tmdbSave <- function (tmdb, destFilename, reconnect=FALSE) 
{
  # Copies a database from its current file (probably a temp file)
  # to the specified destination file, after which the connection
  # is closed and the original file deleted.
  #
  # tmdb - open database connection
  # destFilename - a valid destination file name
  # reconnect - if TRUE a new connection is returned for the save file
  #
  # Invisibly returns a new connection (if reconnect is TRUE) or NULL
  # otherwise.

  if (!require(RSQLite, quietly=TRUE)) {
    stop("Can't load the RSQLite package or one of its dependents")
  }
  
  newCon <- NULL

  tryCatch({  
    file.copy(dbGetInfo(tmdb)$dbname, destFilename, overwrite=TRUE)
    tmdbClose(tmdb, delete=TRUE)
    
    if (reconnect) {
      newCon <- dbConnect(dbDriver("SQLite"), destFilename)
    }
  },
  error = function(e){ cat(paste(deparse(substitute(tmdb)), "is not a valid database connection \n")) })
  
  invisible(newCon)
}
