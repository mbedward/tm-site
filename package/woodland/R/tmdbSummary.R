#' Generate a summary of the contents of a \link{\code{tmRun}} output database.
#' 
#' @param tmdb an open database connection
#' 
#' @return a `data.frame` of summary data describing the simulation parameters
#' 
#' @examples
#' \dontrun{
#' con <- tmdbOpen("results.db")
#' dat <- tmdbSummary(con)
#' tmdbClose(con)
#' }
#' 
tmdbSummary <- function(tmdb) {

  if (!require(RSQLite, quietly=TRUE)) {
    stop("Can't load the RSQLite package or one of its dependents")
  }
  
  if (!tmdbValidate(tmdb, FALSE)) {
    return(invisible(NULL))
  }
  
  df <- dbGetQuery(tmdb, "SELECT COUNT(*) FROM runs")
  if ( is.na(df[1,1]) ) {
    cat("Database is empty \n")
    return(invisible())
  }
  
  numRuns <- df[1,1]
  
  df <- dbGetQuery(tmdb, 
      paste("SELECT p.ID AS ParamSetID, p.InitialCohorts, p.Rain, p.Fire, p.FireFunc,",
            "p.Thinning, p.Special, p.SeedSurv, p.OverlapMatrix, s.Spp, count(runs.ID) as NumRuns",
            "FROM runs JOIN paramsets AS p ON runs.ParamSetID = p.ID",
            "JOIN (select ID, group_concat(Name) as Spp from species group by ID) AS s ON p.SpeciesSetID = s.ID",
            "GROUP BY p.ID"))
  
  df
}
