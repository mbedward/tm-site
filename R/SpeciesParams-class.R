#' Class to hold species parameters used in simulation.
#' 
#' \code{SpeciesParams} is an S4 class which defines the species parameters used
#' by the \code{\link{tmRun}} simulation function. This replaces the previous use of a
#' named list for the same purpose and provides some type and validity checking
#' of arguments. Note that at the moment there are no associated methods to get
#' and set elements (as is usually the case for S4 classes), instead you deal
#' with the slots directly. Given that this is just a data class that shouldn't
#' be too painful.
#' 
#' @section Tree growth:
#' Changes in tree dimensions are modelled in a very basic fashion. change in
#' tree height is modelled as a function of existing height, moisture
#' (rainfall), and stand resource use. All other dimensions such as trunk
#' diameter, canopy radius and canopy surface area, are derived
#' deterministically from tree height.
#' 
#' Annual growth in tree height is described by:
#' \preformatted{dh = (hmax - height) * g.adj}
#' where 
#' \code{dh} is the increment in height;
#' \code{hmax} is the maximum attainable height for the species; 
#' \code{height} is current height; and \code{g.adj} is adjusted growth rate:
#' \preformatted{g.adj = g * g.rain.adj * g.crowd.adj}
#' where 
#' \code{g} is the ideal growth rate; 
#' \code{g.rain.adj} is the proportional adjustment due to rainfall effect;
#' \code{g.crowd.adj} is the proportional adjustment due to crowding (resource
#' competition).
#' 
#' The rainfall and crowding adjustments to growth are given by generalized logistic
#' functions:
#' \preformatted{g.rain.adj = 1 / (1 + exp( -(b1 + b2 * rain * height^b3)))}
#' \preformatted{g.crowd.adj = 1 / (1 + exp( -(c1 + c2 * combined.resource.use * height^c3)))}
#' 
#' \code{combined.resource.use} is the summed resource use for all extant cohorts.
#' 
#' Trees which have been coppiced have a boosted growth rate which is meant to represent
#' their more extensive root system compared to un-coppiced trees of the same height.
#' \preformatted{g.boosted = max( g, b1 / (1 + b2 * exp(-b3 * hprev)) )}
#' 
#' 
#' @section Seed:
#' Seed production of trees is modelled as a function of height, based on a lookup
#' matrix with columns: tree height, seed production, proportion retained.
#' 
#' 
#' @section Survival:
#' 
#' The probability of fire survival is modelled as a function of tree height and
#' fire intensity using a generalized logistic form:
#' \preformatted{psurv = ( 1 / (1 + exp(-( b1 + b2 * intensity + b3 * height^b4 )))^b5 )} 
#' where
#' \code{intensity} is the realized intensity of the fire in the stand
#' 
#' 
#' @slot name Species name (single element character vector)
#' 
#' @slot tree_desc numeric matrix with three columns: height, canopy radius, 
#'   and measure of resource use (e.g. canopy surface area)
#'   
#' @slot max_height maximum attainable height
#' 
#' @slot height_yr1 initial height for trees in a seedling cohort
#' 
#' @slot height_dbh_pars numeric vector of length 3 giving the parameter values 
#'   for the function relating trunk diameter (dbh) to tree height: 
#'   \preformatted{dbh = exp(b1 + (b2 / (b3 + height)) )}
#'   
#' @slot height_fire_pars numeric vector of length 4 giving the parameters for
#'   the generalized logistic function relating the proportional reduction in tree 
#'   height for survivors of a fire:
#'   \preformatted{prop.reduced = 1 / (1 + exp(-(b1 + b2 * fi + b3 * h)))^b4}
#'   where \code{fi} is fire intensity and \code{h} is current height.
#'   
#' @slot growth_rate rate of increase in height under ideal conditions (see Tree Growth).
#' 
#' @slot growth_rainfall_pars numeric vector of length 3 giving the parameters
#'   for the logistic function relating rainfall and current tree height to
#'   proportional decrease in ideal growth rate (see Tree Growth).
#'   
#' @slot growth_crowding_pars numeric vector of length 3 giving the parameters
#'   for the logistic function relating total stand resource use and current tree height to
#'   proportional decrease in ideal growth rate (see Tree Growth).
#'   
#' @slot canCoppice logical; TRUE if this species can have coppice growth.
#'   
#' @slot coppice_boost_pars numeric vector of length 3 giving the parameters for
#'   the function relating boosted growth rate of coppiced trees to height before
#'   coppicing (see Tree Growth).
#'   
#' @slot max_sprogs single numeric value for maximum number of trees in a new cohort
#'   (ie. when space is unrestricted)
#'   
#' @slot seed_output numeric matrix with three columns: height (e.g. in whole metres);
#'   seeds per tree; and retained proportion of seed.
#'   
#' @slot seed_rainfall.pars numeric vector of length 2 giving the parameters for
#'   the function relating proportional adjustment of seed production to rainfall
#'   (see Seed)
#'   
#' @slot recruit_canopy_fn an optional function to transform stand canopy area
#'   into the area perceived by recruits of this species (ie. to increase or reduce
#'   the inhiting effect of overstorey on recruitment). The function should take
#'   one argument for canopy area and return a single numeric value for transformed
#'   area.
#'   
#' @slot external_seed_fn an optional function to return the number of seeds
#'   of this species entering the stand from external sources in a given year.
#'   The function should take no arguments and return a single numeric value.
#'   
#' @slot survival_prob a vector of 1 or more values for the base rate of annual
#'   survival probability. If the length of the vector is less than the length
#'   of the simulation, the last value will be used for all subsequent time steps.
#'   Hence, if the base rate is constant only a single value need be provided.
#'   
#' @slot survival_rainfall_pars numeric vector of length 3 giving the parameters for
#'   the function relating proportional adjustment of base survival rate to rainfall
#'   (see Survival).
#'   
#' @slot survival_crowding_pars numeric vector of length 3 giving the parameters for
#'   the function relating proportional adjustment of base survival rate to total
#'   stand resource use (see Survival).
#'   
#' @slot survival_fire_pars numeric vector of length 5 giving the parameters for
#'   a generalized logistic function relating probability of fire survival to fire
#'   intensity and tree height (see Survival).
#'   
#  Allow users to create objects with SpeciesParams(...) but not extend the class
#' @export SpeciesParams
#' 
SpeciesParams <- setClass("SpeciesParams",
         slots = c(
           name = "character",
           
           tree_desc = "matrix",
           max_height = "numeric",
           height_yr1 = "numeric",
           height_dbh_pars = "numeric",
           height_fire_pars = "numeric",
           growth_rate = "numeric",
           growth_rainfall_pars = "numeric",
           growth_crowding_pars = "numeric",
           canCoppice = "logical",
           coppice_boost_pars = "numeric",
           
           max_sprogs = "numeric",
           seed_output = "matrix",
           seed_rainfall_pars = "numeric",
           recruit_canopy_fn = "ANY",     # will be checked for NULL or function
           external_seed_fn = "ANY",      # will be checked for NULL or function
           
           survival_prob = "numeric",
           survival_rainfall_pars = "numeric",
           survival_crowding_pars = "numeric",
           survival_fire_pars = "numeric",
           
           non_flammability = "numeric"),
         
         prototype = list(
           name = "unnamed",
           
           tree_desc = matrix(rep(0.0, 3), ncol=3),
           max_height = 0.0,
           height_yr1 = 0.0,
           height_dbh_pars = numeric(3),
           height_fire_pars = numeric(4),
           growth_rate = 0.0,
           growth_rainfall_pars = numeric(3),
           growth_crowding_pars = numeric(3),
           canCoppice = FALSE,
           coppice_boost_pars = numeric(3),
           
           max_sprogs = 0,
           seed_output = matrix(rep(0.0, 3), ncol=3),
           seed_rainfall_pars = numeric(2),
           recruit_canopy_fn = NULL,
           external_seed_fn = NULL,
           
           survival_prob = 0.0,
           survival_rainfall_pars = numeric(3),
           survival_crowding_pars = numeric(3),
           survival_fire_pars = numeric(5),
           
           non_flammability = 0.0)
         
         )

# concatenate text strings
.concat <- function(...) paste(..., collapse="", sep="")

# general checking function for numeric elements
.check_numeric <- function(object, 
                           slotName, 
                           requiredLength = -1, 
                           allowAllZeroes = FALSE,
                           allowNA = FALSE) {
  
  value <- slot(object, slotName)
  
  if (!is(value, "numeric"))
    .concat(slotName, " should be numeric, not ", class(value))
  
  else if (requiredLength > 0 && length(value) != requiredLength)
    .concat(slotName, " should have length ", requiredLength)
  
  else if (anyNA(value) && !allowNA)
    .concat(slotName, " should not have missing values")
  
  else if (!allowAllZeroes && all(value == 0, na.rm = !allowNA)) {
    if (length(value) > 1)
      .concat(slotName, " has all zero values")
    else
      .concat(slotName, " is zero")
  }
  
  else NULL
}


.check_matrix <- function(object, slotName, numCols) {
  value <- slot(object, slotName)
  
  if (ncol(value) != numCols)
    .concat(slotName, " matrix must have ", numCols, " columns: ",
            "height, canopy radius, resource use")
  
  else if (nrow(value) < 1)
    .concat(slotName, " matrix must have 1 or more rows")
  
  else if (!is.numeric(value[1,1]))
    .concat(slotName, " must be a numeric matrix")
  
  else NULL
}


.check_function <- function(object, fnName, optional) {
  fn <- slot(object, fnName)
  
  if (optional) {
    if ( !(is.null(fn) || is(fn, "function")) )
      .concat(fnName, " should be NULL or a function object")
    else
      NULL
    
  } else {
    if ( !is(fn, "function") )
      .concat(fnName, " must be provided as a function object")
    else
      NULL
  }
}


.check_tree_desc <- function(object)
  .check_matrix(object, "tree_desc", 3)


.check_max_height <- function(object) {
  value <- object@max_height
  
  if (value <= 0)
    "max_height must be greater than 0"
  
  else NULL
}


.check_height_yr1 <- function(object) {
  value <- object@height_yr1
  
  if (value <= 0 || value > object@max_height)
    "height_yr1 must be greater than 0 and not greater than max_height"
  
  else NULL
}


.check_height_dbh_pars <- 
  function(object) .check_numeric(object, "height_dbh_pars", 3)

.check_height_fire_pars <- 
  function(object) .check_numeric(object, "height_fire_pars", 4)

.check_growth_rate <- 
  function(object) .check_numeric(object, "growth_rate", 1)

.check_growth_rainfall_pars <- 
  function(object) .check_numeric(object, "growth_rainfall_pars", 3)

.check_growth_crowding_pars <- 
  function(object) .check_numeric(object, "growth_crowding_pars", 3)

.check_coppice <-
  function(object) {
    # Check both the canCoppice logical flag and coppice_boost_pars 
    # numeric vector
    
    flag <- object@canCoppice
    
    if ( !is(flag, "logical") || length(flag) != 1 || is.na(flag) )
      "canCoppice should be TRUE or FALSE"
    
    else if (flag)
      .check_numeric(object, "coppice_boost_pars", 3)
  }

.check_max_sprogs <-
  function(object) .check_numeric(object, "max_sprogs", 1)

.check_seed_output <-
  function(object) .check_matrix(object, "seed_output", 3)

.check_seed_rainfall_pars <-
  function(object) .check_numeric(object, "seed_rainfall_pars", 2)

.check_recruit_canopy_fn <-
  function(object) .check_function(object, "recruit_canopy_fn", optional=TRUE)

.check_external_seed_fn <-
  function(object) .check_function(object, "external_seed_fn", optional=TRUE)

.check_survival_prob <-
  function(object) {
    errs <- .check_numeric(object, "survival_prob")
    
    if (length(errs) > 0) errs
    
    else {
      p <- object@survival_prob
      
      if (length(p) < 1)
        "survival_prob should have at least one value"
      
      else NULL
    }
  }

.check_survival_rainfall_pars <-
  function(object) .check_numeric(object, "survival_rainfall_pars", 3)

.check_survival_crowding_pars <-
  function(object) .check_numeric(object, "survival_crowding_pars", 3)

.check_survival_fire_pars <-
  function(object) .check_numeric(object, "survival_fire_pars", 5)

.check_non_flammability <-
  function(object) .check_numeric(object, "non_flammability", 1, allowAllZeroes=TRUE)

setGeneric("isComplete", function(object) standardGeneric("isComplete"))


#' Check if a \code{SpeciesParams} object is ready for use.
#' 
#' Checks that all required elements of the given \code{SpeciesParams} object
#' have valid values.
#' 
#' @param object the object to check
#' 
#' @return a character vector of error messages for invalid or incomplete
#'   elements; otherwise an empty vector if all elements are valid
#' 
#' @examples
#' \dontrun{
#' params <- new("SpeciesParams", name="Tree")
#' 
#' ## put data for each element in slot
#' params@@max_height <- 30
#' params@@height_yr1 <- 0.1
#' params@@tree_desc <- my.tree.desc.matrix
#' 
#' ## plus the rest, then...
#' errs <- isComplete(params)
#' if (length(errs) > 0)
#'   cat(errs, sep="\n")
#' else
#'   ## params valid to use 
#' }
#' 
#' @export
#' 
setMethod("isComplete", 
          signature="SpeciesParams",
          definition = function(object) {              
              
            fns <- list(
              .check_tree_desc,
              .check_max_height,
              .check_height_yr1,
              .check_height_dbh_pars,
              .check_height_fire_pars,
              .check_growth_rate,
              .check_growth_rainfall_pars,
              .check_growth_crowding_pars,
              .check_coppice,
              .check_max_sprogs,
              .check_seed_output,
              .check_seed_rainfall_pars,
              .check_recruit_canopy_fn,
              .check_external_seed_fn,
              .check_survival_prob,
              .check_survival_rainfall_pars,
              .check_survival_crowding_pars,
              .check_survival_fire_pars,
              .check_non_flammability)
              
            # run all check_XXX functions and return accumulated error 
            # messages, if any
            unlist( lapply(fns, do.call, list(object)) )
          })


setGeneric("hasFunction", function(object, fnName) standardGeneric("hasFunction"))

# partial matching of name to slot name
.match_slotName <- function(object, name) {
  i <- pmatch(name, slotNames(object))
  if (is.na(i)) NA
  else slotNames(object)[i]
}

#' Checks if an optional function has been provided.
#' 
#' Checks if a \linkS4class{SpeciesParams} object has an optional function in
#' the given function slot.
#' 
#' @param object \code{SpeciesParam} object
#' @param fnName name of the function slot (partial matching is used)
#' 
#' @return TRUE if the function slot contains a function object
#' 
#' @examples
#' \dontrun{
#' params <- new("SpeciesParams", name="test")
#' 
#' ## Set one of the optional function objects
#' params@@recruit_canopy_fn <- function(area) area^(1.1)
#' 
#' ## Check for function - partial name is permitted
#' hasFunction(params, "recruit")
#' 
#' ## returns TRUE
#' }
#' 
#' @export
#' 
setMethod("hasFunction",
          "SpeciesParams",
          function(object, fnName) {
            nm <- .match_slotName(object, fnName)
            if (is.na(nm))
              stop("SpeciesParams class has no slot with (patrial) name ", fnName)
            
            !is.null( slot(object, nm) )
          })


setGeneric("getFunctionOrElse", 
           function(object, fnName, fallback) standardGeneric("getFunctionOrElse"))

#' Gets an optional function if present, or a fallback function object.
#' 
#' @param object \code{SpeciesParam} object
#' @param fnName name of the function slot (partial matching is used)
#' @param fallback fallback function object
#' 
#' @return the optional function if present, or the fallback
#' 
#' @examples
#' \dontrun{
#' params <- new("SpeciesParams", "test")
#' 
#' ## Ask for the recruit_canopy_fn object. Since we haven't set one
#' ## we will get the fallback (identity function in this example)
#' fn <- getFunctionOrElse(params, "recruit_canopy_fn", identity)
#' }
#' 
#' @export
#' 
setMethod("getFunctionOrElse",
          "SpeciesParams",
          function(object, fnName, fallback) {
            if (hasFunction(object, fnName)) {
              nm <- .match_slotName(object, fnName)
              slot(object, nm)
            
            } else fallback
          })


#' Show brief description of a SpeciesParams object.
#' 
#' @export
setMethod("show",
          "SpeciesParams",
          function(object) {
            cat("Species parameters for", object@name, "\n")
            invisible(NULL)
          })
