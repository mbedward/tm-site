tm.driver <- function(param.table, session.settings=NULL, database=NULL) {

# Drives tm.site.sqlite.

# Arguments:
#
# param.table   - A data.frame where each row specifies tm.site arguments and the 
#                 number of replicates to run. The following col names are expected:
#                   spp
#                   initialcohorts
#                   rain
#                   fire
#                   firefunc
#                   ovmatrix
#                   ovgeneral
#                   recruitment
#                   thinning
#                   special
#                   seedlingsurvival
#                   nreps
#                 Note that apart from fire and firefunc, all others may be abbreviated
#                 to the first 3 chars in param.table.
#
#                 Any of the following can be used in the body of param.table to specify
#                 that an argument is not being passed (ie. tm.site.sqlite default will be used):
#                   "" (empty string)
#                   NA or "NA"
#                   "NULL" 
#                 Note: NULL can't be used because R won't let you insert it into a 
#                 character vector.
#
#                 Alternatively, you can omit the column for an optional arg in param.table
#                 which will cause the default value to be used for all runs.
#
#                 If the nreps column is missing then only 1 replicate simulation will be run
#                 for each param combination in param.table. To disable a combination but leave
#                 it in the table, set nreps to 0.
#
# session.settings - Passed directly to tm.site.sqlite for all runs.
#
# database      - Passed directly to tm.site.sqlite for all runs. If not NULL it should be
#                 an open SQLite connection to the database that results will be appended
#                 to. 

  require(RSQLite)

  # edit this to use another version of the model
  MODEL.FN <- tm.site.sqlite

  DBCON <- database
  NEW.DATABASE <- is.null(DBCON)

  PARAM.INFO <- data.frame(
    name=c("spp", "initialcohorts", "rain", "fire", "firefunc", "ovmatrix", "ovgeneral",
           "recruitment", "thinning", "special", "seedlingsurvival", "nreps"),
    mandatory=c(TRUE,  TRUE,  TRUE,   FALSE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
    row.names=c(  "spp", "ini", "rai", "fire", "firef", "ovm", "ovg", "rec", "thi", "spe", "see", "nre") )

  # if the nreps col has been omitted add a column of 1s
  if ( is.na(pmatch("nre", colnames(param.table))) ) {
    param.table = cbind(param.table, nreps=1)
  }

  PARAM.INFO$colnum <- pmatch(rownames(PARAM.INFO), colnames(param.table))
  
  # check for mandatory params that have been left out of the param table
  left.out <- PARAM.INFO$mandatory & is.na(PARAM.INFO$colnum)
  if (any(left.out)) {
    stop("Missing mandatory params in input table:", PARAM.INFO$name[ left.out ])
  }

  # Standardize missing value flags
  param.table[ apply( param.table, 2, function(x) x %in% c("NULL", "NA", "") ) ] <- NA

  # Replace any NAs in the nreps col with 1s
  param.table$nre[ is.na(param.table$nre) ] <- 1

  # Make sure that no mandatory parameters have NAs
  for (i in which(PARAM.INFO$mandatory)) {
    if (!is.na(PARAM.INFO$colnum[i])) {
      if ( any( is.na(param.table[ , PARAM.INFO$colnum[i]]) ) ) {
        stop("Missing values are not valid for mandatory parameter", PARAM.INFO$name[i])
      }
    }
  }


  ###################################################################
  #
  # Helper functions
  #
  ###################################################################

  ###################################################################
  # Returns a copy of a string with any leading and trailing spaces
  # removed
  ###################################################################
  trim <- function(text) {
    gsub("^\\s+|\\s+$", "", text)
  }

  ###################################################################
  # Detects a function call string in the param.table. Function calls
  # are identified by the presence of "<-" or "->" in the character
  # string.
  ###################################################################
  isFunctionCall <- function(text) {
    grepl("<-|->", text)
  }

  ###################################################################
  # Extracts the function call from a character representation of
  # the call with one of the following forms:
  #
  #     obj <- fn(...)
  #     fn(...) -> obj
  #     <- fn(...)
  #     fn(...) ->
  #
  # Returns an expression for the function call.
  ###################################################################
  getFunctionExpr <- function(text) {
    if (grepl("<-", text)) {
      r <- regexpr("<-", text)
      func <- trim(substring(text, r+2))
      
    } else {
      r <- regexpr("->", text)
      func <- trim(substring(text, 1, r-1))
    }

    parse(text=func)
  }

  ###################################################################
  # Gets a value from the param table, taking into account
  # possibility of missing cols and NAs
  ###################################################################
  getArgValue <- function(argName, index) {
    val <- NULL

    colNum <- PARAM.INFO[argName, "colnum"]
    if ( !is.na(colNum) ) {
      charVal <- param.table[index, colNum]
      if (!is.na(charVal)) {
        if (isFunctionCall(charVal)) {
          val <- getFunctionExpr(charVal)
        } else {
          val <- as.name( charVal )
        }
      }
    }

    val
  }


  ###################################################################
  # Constructs a call to the tree model
  ###################################################################
  createCall <- function(table.row) {
    L <- list( MODEL.FN,
        SpeciesParams = as.name( param.table$spp[table.row] ),
        initial = getArgValue("ini", table.row ),
        rain = getArgValue("rai", table.row ),
        session.settings = session.settings,
        database = DBCON )

    val <- getArgValue("fire", table.row)
    if (!is.null(val)) L$scheduled.fires <- val

    val <- getArgValue("firef", table.row)
    if (!is.null(val)) L$fire.func <- val

    val <- getArgValue("ovm", table.row)
    if (!is.null(val)) L$overlap.matrix <- val

    val <- getArgValue("ovg", table.row)
    if (!is.null(val)) L$ov.gen <- val

    val <- getArgValue("rec", table.row)
    if (!is.null(val)) L$recruitment <- val

    val <- getArgValue("thi", table.row)
    if (!is.null(val)) L$thinning <- val

    val <- getArgValue("spe", table.row)
    if (!is.null(val)) L$special <- val

    val <- getArgValue("see", table.row)
    if (!is.null(val)) L$seedling.survival <- val

    as.call( L )
  }



  #############################################################################
  #
  # Process the param table
  #
  #############################################################################

  for (i in 1:nrow(param.table)) {
    # skip the combination if nreps <= 0
    if (param.table$nre[i] <= 0) next

    tmcall <- createCall(i)

    for (rep in 1:param.table$nre[i]) {

      if (NEW.DATABASE) {
        # Run the first replicate, then recreate the call to contain 
        # the new database connection
        DBCON <- eval(tmcall)
        tmcall <- createCall(i)
        NEW.DATABASE <- FALSE

      } else {
        eval(tmcall)
      }
    }
  }

  # return the database connection
  invisible( DBCON )
}
