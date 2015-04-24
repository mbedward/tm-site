#' Gets simulation params from a \code{tmrun} output database
#' 
#' @param tmdb an open database connection
#' @param paramSetID identifier of the parameter set to retrieve, 
#'   or NULL (default) to be prompted
#'   
#' @return a named \code{list} of runtime parameters
#' 
#' @export
#' 
tmdbGetParamSet <- function(tmdb, paramSetID=NULL) {
  ids <- RSQLite::dbGetQuery(tmdb, "SELECT ID FROM paramobjects")[,1]
  if (is.null(paramSetID)) {
    cat("Available parameter set IDs...\n")
    cat(ids, "\n")
    cat("ID to retrieve: ")
    tryCatch(
      paramSetID <- as.integer(scan(n=1, what="character", quiet=TRUE)),
      error=function(e) {})
    if (is.null(paramSetID)) return(NULL)
  }
  if (!(paramSetID %in% ids))
    stop(paramSetID, " is not one of the available IDs")
    
  df <- RSQLite::dbGetQuery(tmdb, 
      paste("SELECT data from paramobjects WHERE ID=", paramSetID))
  unserialize(charToRaw(df[1,1]))
}
