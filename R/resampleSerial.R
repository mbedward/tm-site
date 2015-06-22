#' Resample a vector of serially-autocorrelated values.
#' 
#' This function performs resampling with replacement on an input vector of 
#' (possibly) serially autocorrelated values. Values within randomly placed 
#' windows are collected and concatenated to form the output vector. Window 
#' sizes can be random within specified bounds, or constant. The aim of the 
#' function is to produce a resampled version of the input data while 
#' maintaining the pattern of serial autocorrelation as much as possible. To
#' achieve this, you will probably want to experiment with window sizes, using
#' ACF plots or similar to check results.
#' 
#' By default, the input vector is treated as non-circular. If a randomly placed
#' window extends beyond the right-hand edge of the input vector, it will be 
#' rejected and a new window created. Alternatively, setting the \code{wrap}
#' argument to TRUE allows the input vector to be treated as circular. In this
#' case, a window which extends beyond the right-hand edge of the input vector
#' will wrap around to the left-hand edge.
#' 
#' @param x Vector to resample
#' 
#' @param length.out Length of the output sequence
#' 
#' @param win.size Either a vector of two values giving the lower and
#'   upper bounds of resampling windows, or a single value for constant
#'   window size
#'
#'  @param wrap If TRUE, the input time series is treated as circular 
#'   (see Details).
#'   
#' @param na.action Controls how sampling deals with NA values in the
#'   input vector.
#'   \describe{
#'     \item{"allow"}{No special action: NAs are sampled as any other value.}
#'     \item{"omit"}{If any NAs occur in a window they are simply omitted.}
#'     \item{"reject"}{If any NAs occur in a window, that window is
#'       rejected and another chosen.}
#'   }
#'   
#' @export
#' 
resampleSerial <- function(x, length.out, win.size=c(10, 20), 
                           wrap=FALSE, na.action=c("allow", "omit", "reject")) {
  
  if (length.out < 1)
    stop("length.out must be positive")
  
  Nx <- length(x)
  
  if (Nx < 1) {
    stop("Empty input vector x")
  }  
  
  # Used as a marker value by the getter helper function
  Empty <- rep(x[1], 0)

  # Helper function to retrieve data as linear or circular
  # sequence. If the sequence is linear and the right-hand bound
  # of the window is beyond Nx, an empty vector is returned.
  #
  getter <- function(left, size) {
    right <- left + size - 1
    if (right <= Nx) {
      x[left:right]
    
    } else if (wrap) {
      xr <- x[left:Nx]
      while (length(xr) < size) {
        right <- min(size - length(xr), Nx)
        xr <- c(xr, x[1:right])        
      }
      xr
      
    } else { 
      # for linear, return an empty vector to 
      # indicate failure
      Empty
    }
  }
    
  

  if (Nx == 1) {
    # degenerate case
    warning("Input vector x has only 1 element")
    rep(x[1], length.out)
  }
  else {
    lo <- win.size[1]
    hi <- ifelse( length(win.size) > 1, win.size[2], win.size[1] )
    
    lo <- as.integer( min(lo, hi) )
    hi <- as.integer( max(lo, hi) )
    
    if (lo < 1)
      stop("minimum window size must be > 0")
    
    if (lo > Nx && !wrap)
      stop("Minimum window size (", lo, ") exceeds length of input vector")
    
    # Create vector of window sizes
    #
    if (lo == hi) {
      sz <- rep(lo, ceiling(length.out / lo))
    }
    else {
      sz <- numeric( length.out / lo )
      len <- 0
      k <- 0
      
      while (len < length.out) {
        k <- k + 1
        sz[k] <- sample(lo:hi, 1)
        len <- len + sz[k]
      }
      sz <- sz[1:k]
    }
    
    # Resample input data
    #
    out <- rep(x, length.out = sum(sz))
    
    out.left <- 1
    for (s in sz) {
      repeat {
        in.left <- sample(1:Nx, 1)
        xr <- getter(in.left, s)
        if (length(xr) == s) {
          out.right <- out.left + s - 1
          out[out.left:out.right] <- xr
          out.left <- out.right + 1
          break
        }
      }
    }
    
    out[1:length.out]
  }
}
