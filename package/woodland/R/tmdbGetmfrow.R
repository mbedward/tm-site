tmdbGetmfrow <- function(n) {
  # Suggest values for mfrow to arrange n plots
  nc <- ceiling(sqrt(n))
  nr <- ceiling(n / nc)
  if (length(n) > 1) {
    return (matrix(c(nr, nc), ncol=2))
  } else {
  	return (c(nr, nc))
  }
}
