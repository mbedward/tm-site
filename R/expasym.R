#' Area transformation function
#' 
#' TODO - where do we use this ?
#' 
#' Function has the form:
#' \preformatted{area' = a * (1 - exp(-b * area))}
#' 
#' @param x input area value(s)
#' @param a coefficient
#' @param b coefficient
#' 
#' @export
#' 
expasym <- function (x, a, b) {
  a * (1 - exp(-b * x))
}

#' TODO - write me !
#' 
#' @export
#' 
expasym3d <- function (x, y, a, b, c) {
  a * (1 - exp(-(b * x + c * y)))
}