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
#' If the \code{parallel} argument is \code{TRUE}, simulations will be run
#' concurrently using the \code{foreach} package. To use this option, you must
#' create and register a cluster prior to calling \code{tmDriver} and also 
#' export any objects in the global environment which are referenced by 
#' parameter generators. See the example code below.
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
#' @param parallel (logical) If TRUE, run the simulations in parallel using
#'   the \code{foreach} package. See example.
#'   
#' @return A connection to a SQLite database containing the aggregated
#'   simulation outputs.
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
#' # This example also assumes that there is an object 'spp.params' 
#' # in the global environment
#' 
#' # The list of parameter generators, with list element names
#' # corresonding to (abbreviated) tmRun argument names
#' funs <- list(
#'   # Constant object for Spp argument
#'   Spp = spp.params, 
#'   
#'   # Function to generate initial cohort table.
#'   # Same behaviour over all simulations so the function
#'   # ignores major and minor arguments.
#'   #
#'   init = function(...) {
#'     Nspp <- length(spp.params)
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
#' ### Example of running the above in parallel (3 cores)
#' 
#' # Create a cluster and register it
#' library(doParallel)
#' cl <- makeCluster(3)
#' registerDoParallel(cl)
#' 
#' # Export objects required by the parameter generators in list 'funs'
#' # (if doing this from within a function, add 'envir = environment()' to
#' # the call to clusterExport so that objects within the surrounding
#' # function will be found)
#' #
#' clusterExport(cl, c("SimLen", "params", "param.combos"))
#' 
#' # Run the driver in parallel mode
#' con <- tmDriver(funs, major = nrow(param.combos), minor = 100, parallel = TRUE)
#' 
#' # Finish
#' stopCluster(cl)
#' 
#' }
#' 
#' @export
#' 
tmDriver <- function(funs, majors=1, minors=1, parallel=FALSE) {
  
  if (parallel &&
      !requireNamespace("foreach", quietly=TRUE)) {
    stop("Install the foreach package to run parallel simulations")
  }
  
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

  db.out <- 
    if (parallel)
      driver_parallel(funs, majors, minors)
    else
      driver_serial(funs, majors, minors)
  
  # Add indexes
  #
  RSQLite::dbGetQuery(db.out, 
                      "create index idxcyr on cohortyearly(RunID, SpeciesID, CohortID)")
  
  RSQLite::dbGetQuery(db.out,
                      "create index idxcsum on cohortsummary(RunID, SpeciesID, CohortID)")
  
  RSQLite::dbGetQuery(db.out,
                      "create index idxcdat on commondata(RunID)")
  
  db.out
}


driver_serial <- function(funs, majors, minors) {
  db.out <- NULL
  run.id <- 1
  
  cat("Running simulations \n")
  for (major in majors) {
    for (minor in minors) {
      cat(major, minor, "\n")
      
      db.run <- do_run(funs, major, minor, run.id)
      db.out <- combine_databases(db.out, db.run)
      
      run.id <- run.id + 1
    }
  }
  cat("Finished \n")
  
  db.out
}


driver_parallel <- function(funs, majors, minors) {
  nminor <- length(minors)
  
  dbs <- foreach (major = majors, i = icount(), .combine = c) %:%
    foreach (minor = minors, j = icount()) %dopar% {
      run.id = nminor * (i - 1) + j
      do_run(funs, major, minor, run.id)
    }

  db.out <- NULL
  for (db in dbs) {
    db.out <- combine_databases(db.out, con)
  }
  
  db.out
}


do_run <- function(funs, major, minor, run.id) {
  params <- lapply(funs, 
                   function(p) { 
                     if (is.function(p)) p(major, minor) 
                     else p
                   })
  
  params$run.id <- run.id
  
  do.call(tmRun, params)
}

# Combine two databases.
#
# If db1 is NULL, we simply return db2.
# If both connections are non-NULL, the contents of db2 are
# inserted into db1 and db2 is closed and deleted.
#
# We avoid passing data through R by using an SQL 
# attach / insert / detach workflow.
#
combine_databases <- function(db1, db2) {
  # db1 is allowed to be null, but not db2
  db2 <- ensure_open(db2)
  
  if (is.null(db1)) {
    db2
  }
  else {
    db1 <- ensure_open(db1)
    
    sql <- paste("attach database \'", db2@dbname, "\' as db2", sep="")
    RSQLite::dbGetQuery(db1, sql)
    RSQLite::dbBegin(db1)
    
    tbls <- RSQLite::dbListTables(db2)
    
    for (tbl in tbls) {
      sql <- paste("insert into ", tbl, " select * from db2.", tbl, sep="")
      RSQLite::dbGetQuery(db1, sql)
    }
    
    RSQLite::dbCommit(db1)
    RSQLite::dbGetQuery(db1, "detach database db2")

    # sometimes deleting the temp file fails
    suppressWarnings(
      tmdbClose(db2, deleteDB = TRUE)
    )
    
    db1
  }
}

# Ensure a database connection is open.
# When running in parallel, we end up with many closed
# connections which still point to valid files.
#
ensure_open <- function(db) {
  if (is.null(db))
    stop("db connection is NULL")
  
  # If the db connection is not NULL, but not open, then open it
  if (!is.null(db) && !RSQLite::dbIsValid(db)) {
    db <- RSQLite::dbConnect(RSQLite::SQLite(), db@dbname)
  }
  
  db
}
