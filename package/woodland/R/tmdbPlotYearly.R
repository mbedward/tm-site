tmdbPlotYearly <- function( tmdb, val=c("basalarea", "corearea", "resourceuse", "n"), runid=1, col=c("blue", "green", "purple", "magenta", "cyan"), x.same=TRUE, y.same=TRUE, show.legend=TRUE, pos.legend=c("left", "right"), plot.total=TRUE, col.total="BLACK" ) {
  #
  # Plots yearly summed value (across cohorts by species) for a chosen variable.
  #
  # tmdb - an open connection to a tm.site database
  #
  # val - name (as character) of the variable to plot. The values plotted
  #       will be the sum of individual cohort values for each year.
  #
  # runid - a vector of one or more run ID values, each of which will
  #         be used to retrieve data for a separate plot
  #
  # col - vector of colours for species lines
  #
  # x.same - if TRUE, all plots use the same xlim setting
  #
  # y.same - if TRUE, all plots use the same ylim setting
  #
  # show.legend - if TRUE, legend is added to each plot
  #
  # pos.legend - specifies whether to plot the legend at upper-left or upper-right
  #              of plot area (default is left)
  #
  # plot.total - if TRUE, and there are two or more species in the results,
  #              plot the combined value in addition to that for each species
  #
  # col.total - colour to use for plotting total value
  
  if (!require(RSQLite, quietly=TRUE)) {
    stop("Can't load the RSQLite package or one of its dependents")
  }
  
  if (!tmdbValidate(tmdb, FALSE)) {
    return(invisible(NULL))
  }
  
  ARG.NAMES <- c("basalarea", "corearea", "resourceuse", "n")
  SQL.NAMES <- c("BasalArea", "CoreAreaGeneral", "ResourceUse", "N")
  DISPLAY.NAMES <- c("basal area", "core area", "resource use", "num trees")
  
  valIndex <- match( match.arg(val, ARG.NAMES), ARG.NAMES )

  LEFT <- 0
  RIGHT <- 1
  if (show.legend) {
    pos.legend <- match.arg(pos.legend, c("left", "right"))
    LEG.POS <- ifelse(pos.legend == "left", LEFT, RIGHT)
  }
  
  # query for individual species values 
  sql <- paste("SELECT RunID, Time, SpeciesID, SUM(", SQL.NAMES[valIndex], ") AS SpValue FROM cohortyearly WHERE RunID IN (",
               paste(runid, collapse=","), ") GROUP BY RunID, Time, SpeciesID")             
               
  df.data <- dbGetQuery(tmdb, sql)
  
  # check the number of species in the results
  NSPP <- length(unique(df.data$SpeciesID))
  
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
  
  common.xlim <- NULL
  common.ylim <- NULL
  
  # if there are two or more species and plot.total is TRUE
  # calculate value totalled across species
  if (NSPP < 2) {
    plot.total <- FALSE
  }
  totalValue <- 0
  if (plot.total) {
    totalValue <- with(df.data, tapply(SpValue, list(Time, RunID), sum))  
  }
  
  if (NPLOTS > 1) {
    if (x.same) common.xlim <- c(1, max(df.data$Time))
    if (y.same) common.ylim <- c(0, max(df.data$SpValue, totalValue))
  } else {
  	# set the axis params to false so that the limits are
  	# set properly below
    x.same <- y.same <- FALSE
  }
  
  totalValueCol <- 1
  for (id in runid[ID.PRESENT]) {
  	run.data <- df.data[ df.data$RunID == id, ]
  	years.extant <- table( run.data$SpeciesID )
  	
    if (x.same) {
      xlim <- common.xlim
    } else {
      xlim <- c(1, max(years.extant))
    }
      
    if (y.same) {
      ylim <- common.ylim
    } else {
      ylim <- c(0, max(run.data$SpValue, totalValue[ , totalValueCol]))
    }

    plot(0, type="n", xlab="time", xlim=xlim, 
         ylab=DISPLAY.NAMES[valIndex], ylim=ylim,
         main=paste("Run", id))

    num.spp <- length(years.extant)
    for (sp in 1:num.spp) {
      if (years.extant[sp] > 0) {
        col.index <- ((sp - 1) %% length(col)) + 1
        lines(1:years.extant[sp], run.data$SpValue[ run.data$SpeciesID==sp ], col=col[col.index])
      }
    }
    
    if (plot.total) {
      lines(1:xlim[2], totalValue[ , totalValueCol], col=col.total)
      if (show.legend) {
        legx <- xlim[1]
        if (LEG.POS == RIGHT) {
          legrect <- legend(1, ylim[2], c(paste("sp", 1:num.spp), "total"), c(col[(1:num.spp) %% (length(col)+1)], col.total), plot=FALSE)$rect
          legx <- xlim[2] - legrect$w
        }
        legend(legx, ylim[2], c(paste("sp", 1:num.spp), "total"), c(col[(1:num.spp) %% (length(col)+1)], col.total))
      }
      totalValueCol <- totalValueCol + 1

    } else {
      if (show.legend) {
        legx <- xlim[1]
        if (LEG.POS == RIGHT) {
          legrect <- legend(1, ylim[2], paste("sp", 1:num.spp), col[(1:num.spp) %% (length(col)+1)], plot=FALSE)$rect
          legx <- xlim[2] - legrect$w
        }
        legend(legx, ylim[2], paste("sp", 1:num.spp), col[(1:num.spp) %% (length(col)+1)])
      }
    }
  }
  
  par(old.pars)  
}
