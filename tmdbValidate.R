tmdbValidate <- function (tmdb, show.output=TRUE) 
{
  # Summarize the contents of an output database from tm.site
  #
  # con - an open connection to a database
  
  if (!require(RSQLite, quietly=TRUE)) {
    stop("Can't load the RSQLite package or one of its dependents")
  }

  if (is.null(tmdb) | !is(tmdb, "SQLiteConnection")) {
  	stop("The first argument must be a connection to an SQLite database")
  }
  
  f <- function() {
    expected.tables <- c("cohortyearly", "cohortsummary", "commondata", "runs", "paramsets", "paramobjects", "species")
    if (show.output) {
      cat( paste("Database file:", dbGetInfo(tmdb)$dbname, "\n") )
    }
  
    tables <- dbListTables(tmdb)
    if (any(is.na(match(expected.tables, tables)))) {
  	  if (show.output) {
  	    cat("Missing one or more expected tables\n")
  	  }
  	  return(FALSE)
    }
  
    if (show.output) {
      for (t in tables) {
        df <- dbGetQuery(tmdb, paste("select count(*) from", t))
  	    cat(paste(t, df[1,1], "recs\n"))
      }
    }
    
    return (TRUE)
  }
  
  flag <- FALSE
  tryCatch(flag <- f(), error=function(e) {
    cat(paste(deparse(substitute(tmdb)), "is not an open database connection \n"))
  })
  
  flag 
}
