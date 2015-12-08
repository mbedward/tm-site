#' Open an output database from the \code{\link{tmRun}} function.
#' 
#' Attempts to open a connection to the given database and, if
#' successful, checks that the database contains the expected tables
#' by calling \code{\link{tmdbValidate}}.
#'
#' @param path the database file path (if NULL a
#'   dialog will be displayed to choose the file)
#'      
#' @param driver an optional database driver to manage 
#'   the connection
#'   
#' @return the database connection
#' 
#' @seealso \code{\link{tmdbOpen}}, \code{\link{tmdbValidate}}
#' 
#' @examples
#' \dontrun{
#' con <- tmdbOpen("somedir/results.db")
#' 
#' # do interesting things, then...
#' 
#' tmdbClose(con)
#' }
#' 
#' @export
#' 
tmdbOpen <- function (path=NULL, driver=NULL)
{  
  if (is.null(path)) {
  	tryCatch(
      path <- file.choose(),
      error = function(e) {} # dialog cancelled
    )
    
    if (is.null(path)) return( invisible(NULL) )
  }
  
  if (!file.exists(path))
    stop("Can't find database file ", path)
  
  if (is.null(driver)) driver <- RSQLite::SQLite()
  
  con <- RSQLite::dbConnect(driver, path)
  if (!tmdbValidate(con, show=FALSE)) {
    RSQLite::dbDisconnect(con)
  	stop(path, " is not a valid tmRun output database")
  }
  
  # Return the connection to the database
  con
}

# =============================================================================

#' Close a database connection.
#' 
#' Closes the given database connection and optionally
#' deletes the associated database file.
#'
#' @param tmdb an open database connection
#' @param deleteDB whether to delete the database file from disk
#' 
#' @seealso \code{\link{tmdbOpen}}
#'
#' @examples
#' \dontrun{
#' con <- tmdbOpen("somedir/results.db")
#' 
#' # do interesting things, then...
#' 
#' tmdbClose(con)
#' }
#' 
#' @export
#' 
tmdbClose <- function (tmdb, deleteDB=FALSE) 
{
  .ensureConnection(tmdb, is.open=FALSE)
    
  filename <- tmdb@dbname
  suppressWarnings( RSQLite::dbDisconnect(tmdb) )
  if (deleteDB) {
    file.remove(filename)
  }
}

# =============================================================================

#' Save a database to a new file.
#' 
#' Copies a database from its current location (possibly a temporary file)
#' to the given destination file. After copying, the connection is closed 
#' and the original file deleted. If the destination is the same as the 
#' database file for the connection, a warning message is issued and the
#' input connection is returned.
#'
#' @param tmdb an open database connection
#' @param dest destination path
#' @param reconnect if TRUE a new connection to the destination
#'   database file is returned
#'   
#' @return Invisibly returns a new connection (if reconnect is TRUE) 
#'   or NULL otherwise.
#'   
#' @export
#' 
tmdbSave <- function (tmdb, dest, reconnect=FALSE) 
{
  newCon <- NULL
  
  .ensureConnection(tmdb, is.open=TRUE)
  
  if (dest == tmdb@dbname) {
    warning("Destination is same as current database file - skipping copy")
    invisible(tmdb)
    
  } else {
    file.copy(tmdb@dbname, dest, overwrite=TRUE)
    tmdbClose(tmdb, delete=TRUE)
    
    if (reconnect) {
      newCon <- RSQLite::dbConnect(RSQLite::SQLite(), dest)
    }
    
    invisible(newCon)
  }
}

# =============================================================================

#' Check that a database of simulation outputs is valid.
#' 
#' Checks that a database contains the expected tables for \code{\link{tmRun}}
#' output and, optionally, prints a summary of the database contents. 
#' 
#' @param tmdb an open database connection
#' @param show.output TRUE (default) to print summary of database contents
#' 
#' @return TRUE if the database was open and contained the required tables;
#'   FALSE otherwise
#'   
#' @seealso \code{\link{tmdbOpen}}
#' 
#' @export
#' 
tmdbValidate <- function (tmdb, show.output=TRUE) 
{
  .ensureConnection(tmdb, is.open=TRUE)
  
  expected.tables <- c(
    "cohortyearly", 
    "cohortsummary", 
    "commondata", 
    "species")
  
  if (show.output) {
    message( paste("Database file:", tmdb@dbname) )
  }
  
  tables <- RSQLite::dbListTables(tmdb)
  diffs <- setdiff(expected.tables, tables)
  
  if (length(diffs) > 0) {
    if (show.output) {
      message("Missing expected tables: ", paste(diffs, collapse=", "))
    }
    return(FALSE)
  }
  
  if (show.output) {
    for (t in tables) {
      df <- RSQLite::dbGetQuery(tmdb, paste("select count(*) from", t))
      message(paste(t, df[1,1], "recs"))
    }
  }
  
  return (TRUE)
}


# =============================================================================

#' Check that an object is a database connection.
#' 
#' This is intended for internal use only.
#' 
#' @param tmdb the object to check
#' @param is.open TRUE to require that the connection be open
#' 
.ensureConnection <- function(tmdb, is.open) {
  objname <- deparse(substitute(tmdb))
  
  if (is.null(tmdb) | !is(tmdb, "SQLiteConnection"))
    stop(objname, " is not a valid SQLite database connection")
  
  if (is.open && !RSQLite::dbIsValid(tmdb))
    stop(objname, " is not an open connection")    
}
