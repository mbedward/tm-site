#' Generate vertex coordinates for a circle.
#' 
#' Generates vertex coordinates for a circle with given centre and radius.
#' 
#' @param xc centre X
#' @param yc centre Y
#' @param r radius
#' @param npoints number of vertices
#' 
#' @return a 2-column matrix of X and Y values
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' 
#' ## draw some random circles with ggplot
#' library(ggplot2)
#' 
#' xmax = 100
#' ymax = 200
#' rmax <- 20
#' N <- 100
#' 
#' ## Random centre coordinates and sizes
#' xyr <- data.frame(x=runif(N, 0, xbound), 
#'                   y=runif(N, 0, ybound), 
#'                   r=rbeta(N, 1, 5) * rmax)
#' 
#' ## Generate vertices for each circle and accumulate in
#' ## a single data.frame with an id col
#' dat <- do.call("rbind", lapply(1:nrow(m), function(i) {
#'   df <- as.data.frame(circleVertices(m[i,1], m[i,2], m[i,3], 25))
#'   df$id <- i
#'   df
#' }))
#' 
#' ## Draw them
#' ggplot(dat, aes(x, y, group=id)) + 
#'   geom_path(colour="steelblue") + 
#'   coord_equal(xlim=c(0, xbound), ylim=c(0, ybound)) +
#'   theme_bw()
#' }
circleVertices <- function(xc, yc, r, npoints=25) {
  a <- seq(0, 2*pi, length.out = npoints + 1)
  x <- xc + r * cos(a)
  y <- yc + r * sin(a)
  m <- cbind("x" = x, "y" = y)
}
