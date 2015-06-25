#' Plots yearly summed value (across cohorts by species) for a chosen variable.
#'
#' @param tmdb an open connection to a \code{\link{tmRun}} output database
#'
#' @param val name (character) of the variable to plot. The values plotted
#'   will be the sum of individual cohort values for each year.
#'
#' @param runid a vector of one or more run ID values, each of which will
#'   be used to retrieve data for a separate plot
#'
#' @param show.legend if TRUE, legend is added to each plot
#'
#' @param plot.total if TRUE, and there are two or more species in the results,
#'   plot the combined value in addition to that for each species
#'
#' @importFrom ggplot2 ggplot aes facet_wrap geom_line labs theme theme_bw
#' 
#' @export
#' 
tmdbPlot <- function(tmdb, 
                     val=c("mergedarea", "basalarea", "corearea", "resourceuse", "n"), 
                     runid=1, 
                     show.legend=TRUE, 
                     plot.total=TRUE) {

  if (!tmdbValidate(tmdb, FALSE)) {
    return(invisible(NULL))
  }
  
  ARG.NAMES <- c("mergedarea", "basalarea", "corearea", "resourceuse", "n")
  SQL.NAMES <- c("MergedArea", "BasalArea", "CoreAreaGeneral", "ResourceUse", "N")
  DISPLAY.NAMES <- c("merged area", "basal area", "core area", "resource use", "num trees")
  
  valIndex <- match( match.arg(val, ARG.NAMES), ARG.NAMES )

  # For plotting merged area we use a specific function because the data
  # are in the commondata table rather than cohortyearly
  if (valIndex == 1) {  
    return( tmdbPlotMergedArea(tmdb, runid) )
  }

  # query for individual species values 
  sql <- paste("SELECT RunID, Time, SpeciesID, SUM(", SQL.NAMES[valIndex], 
               ") AS SpeciesValue FROM cohortyearly WHERE RunID IN (",
               paste(runid, collapse=","), ") GROUP BY RunID, Time, SpeciesID")             
               
  df.data <- RSQLite::dbGetQuery(tmdb, sql)

  sql <- paste("SELECT RunID, SpeciesID, Name FROM species",
               "WHERE RunID IN (", paste(runid, collapse=","), ") ORDER BY RunID, SpeciesID")

  df.spp <- RSQLite::dbGetQuery(tmdb, sql)
  NSPP <- length(unique(df.data$SpeciesID))
  if (NSPP < 2) {
    plot.total <- FALSE
  }
  
  NPLOTS <- sum( runid %in% df.data$RunID )
  if (NPLOTS == 0) {
    warning("None of the requested run IDs are in the database")
    return(invisible(NULL))
  } else if (NPLOTS < length(runid)) {
    warning("Some requested run IDs are not present in the database")
  }

  if (plot.total) {
    df.total <- df.data %>%
      dplyr::group_by(RunID, Time) %>% 
      dplyr::summarize(SpeciesID = NSPP+1, SpeciesValue = sum(SpeciesValue))

    df.data <- rbind(df.data, df.total)
  }
  
  df.data <- df.data %>%
    dplyr::left_join(df.spp, by=c("RunID", "SpeciesID")) %>%
    dplyr::mutate(Name = ifelse(is.na(Name), "Total", Name))
  
  g <- ggplot(data=df.data) +
    theme_bw() +
    labs(x="time", y=DISPLAY.NAMES[valIndex]) +
    geom_line(aes(x=Time, y=SpeciesValue, colour=Name))
  
  if (!show.legend) {
    g <- g + theme(legend.position = "none")
  }
  
  if (NPLOTS > 1)
    g <- g + facet_wrap(~ RunID)
  
  g  
}


# Helper for tmdbPlot which handles plotting of merged area
#
tmdbPlotMergedArea <- function(tmdb, runid) {
  
  sql <- paste( "SELECT RunID, Time, MergedArea FROM commondata WHERE RunID IN (",
                paste(runid, collapse=","), ") GROUP BY RunID, Time" )
  
  df.data <- RSQLite::dbGetQuery(tmdb, sql)
  
  NPLOTS <- sum( runid %in% df.data$RunID )
  if (NPLOTS == 0) {
    warning("None of the requested run IDs are in the database")
    return(invisible(NULL))
  } else if (NPLOTS < length(runid)) {
    warning("Some requested run IDs are not present in the database")
  }

  g <- ggplot(df.data) + 
    theme_bw() + 
    labs(x="time", y="merged canopy area") +
    geom_line(aes(x=Time, y=MergedArea))
  
  if (NPLOTS > 1)
    g <- g + facet_wrap(~ RunID)
  
  g
}

# TODO  do we need this function any longer ?
#
suppressContiguousZeroes <- function(y) {
  y[ y <= 0 ] <- NA
  ny <- length(y)
  y[ is.na(y[-ny]) & !is.na(y[-1]) ] <- 0
  y[ which(is.na(y[-1]) & !is.na(y[-ny])) + 1 ] <- 0
  y
}
