#' The Burr XII distribution
#'
#' Density, distribution function and random generation for the Burr Type XII distribution
#' (commonly called more simply the Burr distribution).
#' 
#' The Burr type XII distribution with shape parameters \eqn{c, k} and scale parameter \eqn{s}
#' has density:
#' 
#' \deqn{
#'   (ck/s)[ (x/s)^(c - 1) ] / [ (1 + (x/s)^c)^(k + 1) ]
#' }
#' 
#' for \eqn{x > 0} and \eqn{c,k,s > 0}.
#' 
#' The cumulative probability function is:
#' 
#' \deqn{
#'   1 - ( 1 + (x/s)^c )^(-k) 
#' }
#' 
#' The mean is given by \eqn{\mu(1)} and the variance by \eqn{-\mu(1)^2 + \mu(2)} where:
#' 
#' \deqn{ \mu(i) = (ks^i)Beta( (ck - i)/c, (c + i)/c ) }
#' 
#' The median is: \eqn{ s(2^(1/k) - 1)^(1/c) }
#' 
#' The mode is: \eqn{ s( (c - 1)/(ck + 1) )^(1/c) }
#' 
#' The Burr XII distribution is also known as the generalized log-logistic distribution.
#' 
#' When shape parameter \eqn{c = 1} the Burr XII is the Pareto II distribution.
#' When shape parameter \eqn{k = 1} it is a special case of the Fisk distribution.
#'
#' @param x, q vector of quantiles (> 0)
#' @param c, k shape parameters (> 0)
#' @param scale scale parameter (> 0)
#' @param n number of observations or, if length(n) > 1, the length is
#'   taken to be the number required
#' 
#' @rdname burr_distribution
#' @export
dburr <- function(x, c, k, scale=1) {
  (c * k / scale) * (x/scale)^(c - 1) / ( (1 + (x/scale)^c)^(k + 1) ) 
}

#' @rdname burr_distribution
#' @export
pburr <- function(q, c, k, scale=1) {
  1 - (1 + (q/scale)^c)^(-k)
}

#' @rdname burr_distribution
#' @export
rburr <- function(n, c, k, scale=1) {
  u <- runif(n)
  scale * (u^(-1/k) - 1)^(1/c)
}
