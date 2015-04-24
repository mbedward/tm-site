#' Summarize species extant status
#' 
#' For a given parameter set, identified by its integer ID, finds the number 
#' of runs in which each modelled species was extant at a specified time. 
#'
#' @param tmdb an open database connection
#' @param paramSetID identifier of the parameter set to query
#' @param time the simulation time to query
#' 
#' @return A list with the following elements: 
#'   paramSetID (parameter set identifier); 
#'   numRuns (number of runs found for the parameter set);
#'   time (query time);
#'   data (a `data.frame` with columns spID, name, runs.extant, prop)
#'
#' @export
#' 
tmdbGetExtant <- function(tmdb, paramSetID, time) {

  if (!tmdbValidate( tmdb, show=FALSE )) {
    return(invisible(NULL)) 
  }

  if (length(paramSetID) > 1) {
    stop( "This function can only query a single parameter set ID" )
  }

  if (length(time) > 1) {
    stop( "This function can only query a single time" )
  }

  # Get the IDs of runs associated with the parameter set
  sql.runs <- paste("SELECT ID FROM runs WHERE ParamSetID =", paramSetID)
  df.runs <- RSQLite::dbGetQuery(tmdb, sql.runs)
  NRUNS <- nrow(df.runs)
  if (NRUNS == 0) {
    stop("No runs for parameter set ID", paramSetID)
  }


  # In case species were defined in the parameters but never appeared
  # in the modelled stand (ie. were not in any initial cohorts) check
  # the yearly data for species IDs rather than just getting them from
  # the species table. We look at times 0 and 1 in case the database is
  # from a version of the model that didn't write a time 0 record for
  # initial cohorts.
  sp.present.sql <- paste("SELECT DISTINCT SpeciesID FROM cohortyearly",
      "WHERE Time IN (0, 1) AND RunID IN (", paste(df.runs$ID, collapse=","), ")" )

  df <- RSQLite::dbGetQuery(tmdb, sp.present.sql)
  SP.IDS <- sort( unique( df$SpeciesID ) )
  NSPP <- length(SP.IDS)
  
  # Get the species names
  sp.sql <- paste("SELECT Name FROM paramsets AS p JOIN species AS s ON p.SpeciesSetID = s.ID",
      "WHERE p.ID =", paramSetID, "AND SpeciesID IN (", paste(SP.IDS, collapse=","), ") ORDER BY SpeciesID")
  df <- RSQLite::dbGetQuery(tmdb, sp.sql)
  SP.NAMES <- df$Name

  # Get the data (only RunID and SpeciesID are required)
  # Note: we don't bother using SQL outer joins here to identify species with 0 cohorts. It's
  # a lot quicker and easier to just get the basic data and massage it with R
  sql.data <- paste("SELECT DISTINCT RunID, SpeciesID FROM cohortyearly", 
      "WHERE RunID IN (", paste(df.runs$ID, collapse=","), ") AND Time =", time)

  df.data <- RSQLite::dbGetQuery(tmdb, sql.data)

  if (nrow(df.data) == 0) {
    # no runs with data for the given time
    species <- data.frame(SP.IDS, SP.NAMES, 0, 0)

  } else {
    # Count the number of runs for which each species is extant
    # (don't use table() to do this because there will be no count
    # for species that are not in df.data)
    countExtant <- numeric(NSPP)
    for (i in 1:NSPP) {
      countExtant[i] <- sum(df.data$SpeciesID == SP.IDS[i])
    }
    species <- data.frame(SP.IDS, SP.NAMES, countExtant, countExtant / NRUNS)
  }
  colnames(species) <- c("spID", "name", "nruns", "prop")

  # Examine frequency of possible combinations of species 
  # (this code looks complex but it's written to deal with any number of species)
  key <- matrix(0, nrow=2^NSPP, ncol=NSPP)
  k <- 2
  for (i in 1:NSPP) {
    x <- t(combn(NSPP, i))
    for (j in 1:nrow(x)) {
      key[k, x[j,]] <- 1
      k <- k+1
    }
  }

  fnKeyToValue <- function( x ) 1 + sum(x * 2^seq(0, length(x)-1))

  valueToKeyRow <- (1:nrow(key))[ apply(key, 1, fnKeyToValue) ]
  keyCount <- integer(nrow(key))
  
  if (nrow(df.data) > 0) {
    tapply(df.data$SpeciesID, df.data$RunID, 
        function(x) { 
          val <- fnKeyToValue(!is.na(match(SP.IDS, x))); 
          i <- valueToKeyRow[val]; 
          keyCount[i] <<- keyCount[i] + 1 
        }
    )
  }

  # Runs were both species were absent
  keyCount[1] <- NRUNS - sum(keyCount)

  extant.combinations <- data.frame(key, keyCount, keyCount / NRUNS)
  colnames(extant.combinations) <- c(SP.NAMES, "nruns", "prop")

  list(paramSetID = paramSetID, numRuns = NRUNS, time = time, species = species,
       combinations = extant.combinations )
}
