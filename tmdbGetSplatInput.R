tmdbGetSplatInput <- function(cohort.data, time, min.radius=0, labels=NULL) {
  irows <- which(cohort.data$Time == time & cohort.data$CanopyRadius >= min.radius)
  x <- cohort.data[irows, ]
  out <- data.frame(radius=x$CanopyRadius, N=x$N)
  if (!is.null(labels)) {
    out$label <- labels[ x$SpeciesID ]
  } else {
  	out$label <- x$SpeciesID
  }
  
  out
}
