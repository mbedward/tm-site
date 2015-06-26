#' Runs \code{tmRun} in batch mode.
#' 
#' Generating functions take two parameters: major and minor run number. Each
#' function is free to interpret these in any way, or ignore them completely.
#' For example, in a study of rainfall effects, major number may relate to an
#' overall average rainfall value while the minor number is simply the replicate
#' index within each rainfall average.
#' 
#' @param funs A named list of generating functions, where the names correspond 
#'   to \code{\link{tmRun}} argument names. Each function takes two integer
#'   parameters (major and minor run number) and returns an object appropriate
#'   for the particular argument (e.g. a generating function for the \code{'rain'})
#'   argument would return a vector of annual rainfall values.
#' 
#' @examples
#' \dontrun{
#' # List of parameter generating functions, with list element names
#' # corresonding to (abbreviated) tmRun argument names
#' fns <- list(
#'   # Constant objects for Spp and initial.cohorts arguments
#'   Spp = my.params, 
#'   init = my.initial.cohorts,
#'   
#'   # Rainfall function
#'   rain = function(maj, min) {
#'     avg <- ifelse(maj == 1, 500, 600)
#'     runif(100, avg - 100, avg + 100)
#'   }
#'   
#' }
#' 
#' @export
#' 
tmDriver <- function(funs, nmajor=1, nminor=1) {
  
  tmRunArgs <- formals(tmRun)
  MandatoryIndices <- which( sapply(tmRunArgs, is.name) )  # args with no default
  
  fnames <- stringr::str_trim(names(funs))
  
  # Check for duplicates
  #
  if (length(unique(fnames)) < length(fnames)) {
    stop("One or more duplicate names in \'funs\' list")
  }  

  arg.indices <- pmatch(fnames, names(tmRunArgs))

  # Check for names which don't uniquely match arguments
  #
  nas <- is.na(arg.indices)
  if ( any(nas) ) {
    s <- paste(fnames[nas], collapse=", ")
    stop("Cannot match argument name(s): ", s)
  }

  # Check for missing mandatory arguments
  #
  found <- MandatoryIndices %in% arg.indices
  if ( !all(found) ) {
    s <- paste(names(tmRunArgs)[MandatoryIndices[!found]], collapse=", ")
    stop("Missing mandatory argument function(s): ", s)
  }
  
  
  db.out <- NULL
  run.id <- 1
  for (M in 1:nmajor) {
    for (m in 1:nminor) {
      params <- lapply(funs, function(el) if (is.function(el)) el(M, m) else el)
      params$run.id <- run.id
      
      db.rep <- do.call(tmRun, params)
      db.out <- combineDB(db.out, db.rep)
      tmdbClose(db.rep)
      
      run.id <- run.id + 1
    }
  }
  
  db.out
}


combineDB <- function(db1, db2) {
  newdb <- FALSE
  if (is.null(db1)) {
    db1 <- RSQLite::dbConnect(RSQLite::SQLite(), tempfile())
    newdb <- TRUE
  }
  
  for (tbl in RSQLite::dbListTables(db2)) {
    dat <- RSQLite::dbReadTable(db2, tbl)
    RSQLite::dbWriteTable(db1, tbl, dat, append=!newdb, overwrite=newdb)
  }
  
  db1
}
