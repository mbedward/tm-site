#' Gets the stand data for a specified run and year (a 'splat').
#' 
#' @param tmdb an open database connection
#' @param runid identifier(s) (integer) of the simulation run(s) to query
#' @param time simulation year (integer)
#' 
#' @return `data.frame` with columns: CanopyRadius, N, SpeciesID
#' 
#' @examples
#' \dontrun{
#' con <- tmdbOpen("results.db")
#' 
#' # query a year across several runs
#' dat <- tmdbGetSplat(con, runid=1:3, time=100)
#' 
#' # close the database connection
#' tmdbClose(con)
#' }
#' 
#' @export
#' 
tmdbGetSplat <- function (tmdb, runid=1, time)
{
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
