tmdbClose <- function (tmdb, deleteDB=FALSE) 
{
  # Closes the connection to a database and optionally
  # removes the database file.
  #
  # tmdb - an open database connection
  # deleteDB - whether to delete the database file from disk
  
  if (!require(RSQLite, quietly=TRUE)) {
    stop("Can't load the RSQLite package or one of its dependents")
  }

  f <- function() {
    filename <- dbGetInfo(tmdb)$dbname
    dbDisconnect(tmdb)
    if (deleteDB) {
      file.remove(filename)
    }
  }
  
  tryCatch(f(), error=function(e) {
  	cat(paste(deparse(substitute(tmdb)), "is not an open database connection"))})
}
