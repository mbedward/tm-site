#' Find non-overlapping configuration of circles.
#' 
#' Attempts to find a configuration for a given set of circles
#' in a rectangle where there is no overlap between circles.
#' 
#' @param xyr 3-column matrix or data.frame (centre X, centre Y, radius)
#' @param xbound X upper limit (lower assumed 0)
#' @param ybound Y upper limit (lower assumed 0)
#' @param max.iter maximum number of iterations to attempt
#' 
#' @return A list with components:
#'   \describe{
#'     \item{config}{A 3-column matrix or data.frame (centre x, centre y, radius).}
#'     \item{n.iter}{Number of iterations performed.}
#'   }
#' 
#' @export
#' 
packCircles <- function(xyr, xbound, ybound, max.iter) {
  if (!is.data.frame(xyr) || is.matrix(xyr))
    stop("argument xyr must be a data.frame or matrix")
  
  if (ncol(xyr) != 3)
    stop("argument xyr must have 3 columns (x, y, radius)")
  
  m <- as.matrix(xyr)
  n.iter = iterate_layout(m, xbound, ybound, max.iter)

  if (is.data.frame(xyr)) m <- as.data.frame(m)
  list(config = m, n.iter = n.iter)
}
