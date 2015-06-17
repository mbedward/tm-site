#' Calculates realized fire intensity value.
#' 
#' This function can be passed to the \code{fire.intensity.func} argument of
#' \code{\link{tmRun}}. Given values for the flammable proportion of stand area
#' \code{p_flam} and incoming fire intensity \code{i_inc}, it calculates the value 
#' of realized fire intensity \code{i_real} as:
#' \preformatted{
#'   i_real = i_inc + beta * (1 - p_flam) for i_inc < threshold 
#'          = i_inc for i_inc >= threshold
#' }
#' 
#' The default values for threshold.intensity (4.5) and beta (-2.14) were derived from
#' field data collected by Janet Cohn on incoming and outgoing intensity (inferred from 
#' eucalypt char heights) and tree basal areas, together with calculations by Karen Ross 
#' of flammable proportions in the field plots, done in the same way that the tree stand 
#' simulation model calculates it, allowing for cases where combined.core.area exceeds 
#' stand area.
#' 
#' @note
#' The \code{fire.intensity.func} argument of \code{\link{tmRun}} expects a function 
#' taking two arguments: flammable proportion and incoming intensity.
#' 
#' @param flammable.prop flammable proportion of stand area
#' @param incoming.intensity incoming fire intensity
#' @param threshold.intensity value of incoming intensity above which the realized value
#'   will equal the incoming value
#' @param beta damping factor (should be negative although this is not checked)
#' 
#' @return realized fire intensity
#' 
#' @export
#' 
realizedFireIntensity <- function ( flammable.prop, incoming.intensity, 
                                    threshold.intensity=4.5, beta=-2.14 ) {
  
  if (length(flammable.prop) > 1)
    warning("Only using first value of flammable.prop")
  
  out <- incoming.intensity
  
  ii <- incoming.intensity <= threshold.intensity
  out[ii] <- pmax( 0, incoming.intensity[ii] + beta * (1 - flammable.prop[1]) )
  
  out
}
