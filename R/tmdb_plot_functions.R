#' Plot simulation outputs.
#' 
#' Plots yearly value, summed across cohorts by species, for a chosen variable.
#'
#' @param tmdb an open connection to a \code{\link{tmRun}} output database
#'
#' @param variable The name (character) of the variable to plot: \code{"n",
#'   "mergedarea", "basalarea", "corearea", "resourceuse"} (may be abbreviated).
#'   The values plotted will be the sum of individual cohort values for each
#'   year.
#'   
#' @param run A vector of one or more run ID values (default = 1), each of which
#'   will be used to retrieve data for a separate plot
#'   
#' @param show.legend If TRUE (default), legend is added to each plot
#'   
#' @param plot.total If TRUE (default), and there are two or more species in the
#'   results, plot the combined value in addition to that for each species
#'   
#' @param plot.fire If TRUE (default), adds fire occurrences as a rug plot. 
#'   
#' @param extra.sql Additional conditions to apply when querying the database
#'   for plot data. See example.
#'   
#' @return A ggplot object.
#'   
#' @examples
#' \dontrun{
#' ## Plot the number of trees of each species in runs 1 to 4
#' tmdbPlot(con, plot="n", run=1:4)
#'
#' ## Same plot but for subset of trees with height 5-10m
#' tmdbPlot(con, plot="n", run=1:4, extra.sql="height >= 5 and height <= 10")
#' }
#'
#' @export
#' 
tmdbPlot <- function(tmdb, 
                     variable=c("n", "mergedarea", "basalarea", "corearea", "resourceuse"), 
                     run=1, 
                     show.legend=TRUE, 
                     plot.total=TRUE,
                     plot.fire=TRUE,
                     extra.sql="") {

  if (!tmdbValidate(tmdb, FALSE)) {
    return(invisible(NULL))
  }
  
  ARG.NAMES <- c("n", "mergedarea", "basalarea", "corearea", "resourceuse")
  SQL.NAMES <- c("N", "MergedArea", "BasalArea", "CoreAreaGeneral", "ResourceUse")
  DISPLAY.NAMES <- c("number of trees", "merged area", "basal area", "core area", "resource use")
  
  variable <- match.arg(variable, ARG.NAMES)
  varIndex <- match(variable, ARG.NAMES )

  extra.sql <- stringr::str_trim(extra.sql)

  # For plotting merged area we use a specific function because the data
  # are in the commondata table rather than cohortyearly
  if (variable == "mergedarea") {
    if (extra.sql != "") 
      warning("extra.sql not yet supported for merged canopy area plot")
  
    return( plot_merged_area(tmdb, run, plot.fire) )
  }
  
  if (extra.sql != "") {
    leading.and <- stringr::str_detect(extra.sql, stringr::ignore.case("^and "))
    if (!leading.and) extra.sql <- paste("AND", extra.sql, collapse = " ")
  }

  # query for individual species values 
  sql <- paste("SELECT RunID, Time, SpeciesID, SUM(", SQL.NAMES[varIndex], 
               ") AS SpeciesValue FROM cohortyearly",
               "WHERE RunID IN (", paste(run, collapse=","), ")",
               extra.sql,
               "GROUP BY RunID, Time, SpeciesID")             
               
  df.data <- RSQLite::dbGetQuery(tmdb, sql)

  sql <- paste("SELECT RunID, SpeciesID, Name FROM species",
               "WHERE RunID IN (", paste(run, collapse=","), ") ORDER BY RunID, SpeciesID")

  df.spp <- RSQLite::dbGetQuery(tmdb, sql)
  NSPP <- length(unique(df.data$SpeciesID))
  if (NSPP < 2) {
    plot.total <- FALSE
  }
  
  NPLOTS <- sum( run %in% df.data$RunID )
  if (NPLOTS == 0) {
    warning("None of the requested run IDs are in the database")
    return(invisible(NULL))
  } else if (NPLOTS < length(run)) {
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
    labs(x="time", y=DISPLAY.NAMES[varIndex]) +
    geom_line(aes(x=Time, y=SpeciesValue, colour=Name))
  
  if (!show.legend) {
    g <- g + theme(legend.position = "none")
  }
  
  if (plot.fire) {
    g <- g + fire_rug_plot(tmdb, run)
  }
  
  if (NPLOTS > 1)
    g <- g + facet_wrap(~ RunID)
  
  g
}


# Helper for tmdbPlot which handles plotting of merged area
#
plot_merged_area <- function(tmdb, run, plot.fire) {
  
  sql <- paste( "SELECT RunID, Time, MergedArea FROM commondata WHERE RunID IN (",
                paste(run, collapse=","), ") GROUP BY RunID, Time" )
  
  df.data <- RSQLite::dbGetQuery(tmdb, sql)
  
  NPLOTS <- sum( run %in% df.data$RunID )
  if (NPLOTS == 0) {
    warning("None of the requested run IDs are in the database")
    return(invisible(NULL))
  } else if (NPLOTS < length(run)) {
    warning("Some requested run IDs are not present in the database")
  }

  g <- ggplot(df.data) + 
    theme_bw() + 
    labs(x="time", y="merged canopy area") +
    geom_line(aes(x=Time, y=MergedArea))
  
  if (plot.fire)
    g <- g + fire_rug_plot(tmdb, fun)
  
  if (NPLOTS > 1)
    g <- g + facet_wrap(~ RunID)
  
  g
}


fire_rug_plot <- function(tmdb, run) {
  sql <- paste("SELECT RunID, Time FROM commondata WHERE",
               "RunID IN (", paste(run, collapse=","), ")",
               "AND RealizedFires > 0")
  
  dat <- RSQLite::dbGetQuery(tmdb, sql)
  
  ggplot2::geom_rug(data=dat, aes(x=Time))
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
