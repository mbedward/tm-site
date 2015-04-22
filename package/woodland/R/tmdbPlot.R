tmdbPlot <- function( tmdb, val=c("mergedarea", "basalarea", "corearea", "resourceuse", "n"), runid=1, 
          col=c("blue", "green", "purple", "magenta", "cyan"), x.same=TRUE, y.same=TRUE, 
          show.legend=TRUE, pos.legend=c("left", "right"), plot.total=TRUE, col.total="BLACK" ) {

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
  
  ARG.NAMES <- c("mergedarea", "basalarea", "corearea", "resourceuse", "n")
  SQL.NAMES <- c("MergedArea", "BasalArea", "CoreAreaGeneral", "ResourceUse", "N")
  DISPLAY.NAMES <- c("merged area", "basal area", "core area", "resource use", "num trees")
  
  valIndex <- match( match.arg(val, ARG.NAMES), ARG.NAMES )

  # If we are plotting merged area we use another function because it is in
  # the commondata table rather than cohortyearly
  if (valIndex == 1) {  
    return( invisible( tmdbPlotMergedArea(tmdb, runid, x.same, y.same) ) )
  }

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

  sql <- paste("SELECT r.ID AS RunID, s.SpeciesID, s.Name FROM species AS s",
               "JOIN paramsets AS p ON s.ID = p.SpeciesSetID",
               "JOIN runs AS r ON p.ID = r.ParamSetID",
               "WHERE r.ID IN (", paste(runid, collapse=","), ") ORDER BY r.ID, s.SpeciesID")

  df.spp <- dbGetQuery(tmdb, sql)
  
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
    spp.ids <- sort(unique( run.data$SpeciesID ) )
    spp.rows <- which( df.spp$RunID == id & df.spp$SpeciesID %in% spp.ids )
    last.year <- tapply(run.data$Time, run.data$SpeciesID, max)
    
    if (x.same) {
      xlim <- common.xlim
    } else {
      xlim <- c(0, max(last.year))
    }
      
    if (y.same) {
      ylim <- common.ylim
    } else if (plot.total) {
      ylim <- c(0, max(run.data$SpValue, totalValue[ , totalValueCol]))
    } else {
      ylim <- c(0, max(run.data$SpValue))
    }

    plot(0, type="n", xlab="time", xlim=xlim, 
         ylab=DISPLAY.NAMES[valIndex], ylim=ylim,
         main=paste("Run", id))

    color.indices <- numeric(length(spp.ids))
    k <- 1
    for (sp.id in spp.ids) {
      sp.index <- match(sp.id, spp.ids)
      color.indices[k] <- ((sp.index - 1) %% length(col)) + 1

      x <- seq(0, last.year[sp.index])
      y <- numeric(length(x))
      ii <- which( run.data$SpeciesID == sp.id )
      y[ run.data$Time[ ii ] + 1 ] <- run.data$SpValue[ ii ]

      lines(x, suppressZeroes(y), col=col[color.indices[k]])
      k <- k+1
    }
    
    if (show.legend) {
      if (plot.total) {
        lbox <- legend(0, 0, c(df.spp$Name[spp.rows], "total"), plot=FALSE)$rect
      } else {
        lbox <- legend(0, 0, df.spp$Name[spp.rows], plot=FALSE)$rect
      }

      LEGW <- lbox$w
      LEGH <- lbox$h

      if (LEG.POS == LEFT) {
        legx <- xlim[1]
      } else {
        legx <- xlim[2] - LEGW
      }

    } else {
      LEGW <- 0
      LEGH <- 0
    }

    if (plot.total) {
      lines(0:xlim[2], totalValue[ , totalValueCol], col=col.total)
      if (show.legend) {
        legend(legx, ylim[2], c(df.spp$Name[spp.rows], "total"), c(col[color.indices], col.total))
      }
      totalValueCol <- totalValueCol + 1

    } else {
      if (show.legend) {
        legend(legx, ylim[2], df.spp$Name[spp.rows], col[color.indices])
      }
    }
  }
  
  par(old.pars)  
}
