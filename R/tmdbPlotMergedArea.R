#' TODO - write me !
#' 
#' @export
#' 
tmdbPlotMergedArea <- function(tmdb, runid, x.same, y.same) {

  sql <- paste( "SELECT RunID, Time, MergedArea FROM commondata WHERE RunID IN (",
                paste(runid, collapse=","), ") GROUP BY RunID, Time" )

  df.data <- RSQLite::dbGetQuery(tmdb, sql)

  ID.PRESENT <- runid %in% df.data$RunID
  NPLOTS <- sum(ID.PRESENT)
  if (NPLOTS == 0) {
    warning("None of the requested run IDs are in the database")
    return(invisible(NULL))
  } else if (NPLOTS < length(runid)) {
    warning("Some requested run IDs are not present in the database")
  }

  old.pars <- par(no.readonly=TRUE)
  par(mfrow=tmdbGetmfrow(NPLOTS))

  old.pars <- par(no.readonly=TRUE)
  par(mfrow=tmdbGetmfrow(NPLOTS))
  
  if (NPLOTS > 1) {
    if (x.same) common.xlim <- c(0, max(df.data$Time))
    if (y.same) common.ylim <- c(0, max(df.data$SpValue, totalValue))
  } else {
    # set the axis params to false so that the limits are
    # set properly below
    x.same <- y.same <- FALSE
  }
  
  #############################################################################
  # Helper function - supresses contiguous zeroes in the data to be plotted
  #############################################################################
  suppressZeroes <- function(y) {
    y[ y <= 0 ] <- NA
    ny <- length(y)
    y[ is.na(y[-ny]) & !is.na(y[-1]) ] <- 0
    y[ which(is.na(y[-1]) & !is.na(y[-ny])) + 1 ] <- 0
    y
  }

  
  #############################################################################
  #
  # Main loop
  #
  #############################################################################
  totalValueCol <- 1
  for (id in runid[ID.PRESENT]) {
    run.data <- df.data[ df.data$RunID == id, ]

    if (x.same) {
      xlim <- common.xlim
    } else {
      xlim <- c(0, max(df.data$Time))
    }

    if (y.same) {
      ylim <- common.ylim
    } else {
      ylim <- c(0, max(df.data$MergedArea))
    }

    plot(df.data$Time, suppressZeroes(df.data$MergedArea), type="l",
         xlim=xlim, xlab="time", ylim=ylim, ylab="Merged canopy area",
         main=paste("Run", id))
  }

  par(old.pars)
}
