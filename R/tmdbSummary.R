#' Summarize a simulation output database.
#' 
#' Queries the output database for the ID value and time length of each
#' run contained within it.
#' 
#' @param tmdb an open database connection
#' 
#' @return A data.frame with columns RunID and TimeLength.
#' 
#' @export
#' 
tmdbSummary <- function(tmdb) {

  if (!tmdbValidate(tmdb, FALSE)) {
    return(invisible(NULL))
  }
  
  df <- RSQLite::dbGetQuery(tmdb, "SELECT RunID, COUNT(*) AS TimeLength FROM commondata GROUP BY RunID")

  df
}
