tmdbGetSplat <- function (tmdb, runid=1, time)
{
# Returns the stand data for a specified run and year (a 'splat')

  if (time < 1 || length(time) > 1) {
    stop("the value for time should be a single integer >= 1")
  }

  if (tmdbValidate(con, FALSE)) {
  	sql <- paste("SELECT CanopyRadius, N, SpeciesID FROM cohortyearly WHERE",
  	    "RunID IN (", paste(runid, sep=","), ") AND",
  	    "Time =", time, 
  	    "ORDER BY SpeciesID ASC, CanopyRadius DESC")
  	    
    return( dbGetQuery(tmdb, sql) )
  }
}
