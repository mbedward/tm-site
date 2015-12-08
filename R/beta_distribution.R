#' Calculates the mean and variance of a Beta distribution.
#' 
#' Given the parameters \code{(a, b)} of a Beta distribution, this 
#' function calculates the mean and variance as:
#' \code{mean = a / (a + b)} and
#' \code{variance = (ab) / ((a+b+1)(a+b)^2)}
#' 
#' @param a first Beta shape parameter
#' @param b second Beta shape parameter
#' @return named vector with values for mean and variance
#' 
#' @export
#' 
betaStats <- function(a, b) {
  c( "mean" = a / (a + b), 
     "variance" = (a * b) / ( (a + b + 1)*(a + b)^2 ) )
}
