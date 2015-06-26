#' Runs simulations in batch mode.
#' 
#' This function automates the process of generating parameter values, running 
#' replicate simulations, and aggregating results into a single SQLite database.
#' The number of simulations to run is controlled by the \code{major} and
#' \code{minor} arguments (see Details).
#' 
#' The \code{funs} argument accepts a named list of parameter generators, where 
#' the names indicate which \code{\link{tmRun}} argument the generator pertains 
#' to. A generator can either be a function or an object to use as a constant
#' parameter value. Generating functions take two single integer values as
#' parameters, termed \emph{major} and \emph{minor}. Typically, \emph{major} is
#' used to index alternative parameter levels (e.g. overall average rainfall
#' value) while the minor index controls how many replicate simulations will be
#' run within each major level. However, an individual parameter generating 
#' function is free to interpret these in any way, or ignore them completely.
#' 
#' @param funs A named list of generating functions, where the names correspond 
#'   to \code{\link{tmRun}} argument names. Each function takes two integer 
#'   parameters (major and minor run number) and returns an object appropriate 
#'   for the particular argument (e.g. a generating function for the
#'   \code{'rain'}) argument would return a vector of annual rainfall values.
#'   
#'   Where a parameter value is constant over all simulations, an object can be 
#'   provided directly for the corresponding \code{funs} list element (see
#'   example).
#'   
#' @param majors (integer) Either a single value for the number of major 
#'   increments or a vector of values. If a single value, \code{M}, it will be
#'   treated as the sequence \code{1:M}.
#'   
#' @param minors (integer) Either a single value for the number of minor 
#'   increments or a vector of values. If a single value, \code{m}, it will be
#'   treated as the sequence \code{1:m}.
#'   
#' @examples
#' \dontrun{
#' 
#' # Combinations of average rainfall and annual fire probability
#' # in a data.frame in the global environment
#' param.combos <- expand.grid(
#'   rain.av = c(500, 700), 
#'   fire.prob = c(0.1, 0.05, 0.025))
#' 
#' # Simulation length as a global var
#' SimLen <- 500
#' 
#' # The list of parameter generators, with list element names
#' # corresonding to (abbreviated) tmRun argument names
#' funs <- list(
#'   # Constant object for Spp argument
#'   Spp = my.spp.params, 
#'   
#'   # Function to generate initial cohort table.
#'   # Same behaviour over all simulations so the function
#'   # ignores major and minor arguments.
#'   #
#'   init = function(...) {
#'     Nspp <- length(my.spp.params)
#'     data.frame(sp = 1:Nspp, 
#'                age = sample(20:50, Nspp),
#'                height = runif(Nspp, 5, 10),
#'                n = sample(1:20, Nspp) )
#'   },
#'   
#'   # Simple auto-correlated rainfall function where average 
#'   # rainfall depends on major index
#'   #
#'   rain = function(maj, min) {
#'     avg <- param.combos[maj, "rain.av"]
#'     x <- stats::filter(rnorm(SimLen, avg, sd=100), 
#'                        filter=rep(1, 3), 
#'                        circular=TRUE)
#'     x / 3
#'   },
#'   
#'   # Scheduled fire where annual probability depends on
#'   # major index
#'   #
#'   scheduled.fire = function(maj, min) {
#'     p <- param.combos[maj, "fire.prob"]
#'     fires <- rbinom(SimLen, 1, p)
#'     
#'     # uniform fire intensity in this example
#'     fires[ fires > 0 ] <- 5
#'     
#'     fires
#'   }
#' ) 
#' 
#' # Run the simulations, with 100 replicates for each parameter 
#' # combination
#' con <- tmDriver(funs, major = nrow(param.combos), minor = 100)
#' 
#' }
#' 
#' @export
#' 
tmDriver <- function(funs, majors=1, minors=1, silent=FALSE) {
  
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
  
  
  if (length(majors) == 1) majors = 1:majors
  if (length(minors) == 1) minors = 1:minors
  
  db.out <- NULL
  run.id <- 1
  
  if (!silent) cat("Running simulations \n")
  for (major in majors) {
    for (minor in minors) {
      if (!silent) cat(major, minor, "\n")
      
      params <- lapply(funs, 
                       function(p) { 
                         if (is.function(p)) p(major, minor) 
                         else p
                       })
      
      params$run.id <- run.id
      
      db.rep <- do.call(tmRun, params)
      db.out <- combine_databases(db.out, db.rep)
      tmdbClose(db.rep)
      
      run.id <- run.id + 1
    }
  }
  if (!silent) cat("Finished \n")
  
  db.out
}


combine_databases <- function(db1, db2) {
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
