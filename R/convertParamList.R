#' Convert an old-format species parameters object to a SpeciesParams object
#' 
#' Converts an old-format species parameters object, as used by an earlier version of
#' the simulation function, into a \linkS4class{SpeciesParams} object.
#' 
#' @param oldparams named list of parameter values
#' @return a new \code{SpeciesParams} object
#' 
#' @export
#'
convertParamList <- function(oldparams) {
  
  key <- data.frame(
    fromName = c("name", "survival.prob", "survival.rainfall.pars", 
                 "survival.crowding.pars", "survival.fire.pars", "max.sprogs", 
                 "seed.output", "seed.rainfall.pars", "max.height", "growth.rate", 
                 "height.yr1", "height.fire.pars", "growth.rainfall.pars", "growth.crowding.pars", 
                 "external.seed.func", "tree.desc", "non.flammability", "ht.dbh.pars", 
                 "coppice.boost.pars", "recruit.canopy.func"),
    
    toName = c("name", 
               "survival_prob", "survival_rainfall_pars", "survival_crowding_pars", 
               "survival_fire_pars", "max_sprogs", "seed_output", "seed_rainfall_pars", 
               "max_height", "growth_rate", "height_yr1", "height_fire_pars", 
               "growth_rainfall_pars", "growth_crowding_pars", "external_seed_fn", 
               "tree_desc", "non_flammability", "height_dbh_pars", "coppice_boost_pars", 
               "recruit_canopy_fn"),
    
    stringsAsFactors = FALSE
  )
  
  lookup <- function(name) {
    i <- match(name, key$fromName)
    if (is.na(i)) 
      stop(name, " not found in lookup table")
    
    key$toName[i]
  }
  
  slotClass <- function(slotName) {
    getSlots("SpeciesParams")[slotName]
  }
  

  sp <- new ("SpeciesParams")
  for (nm in names(oldparams)) {
    toName <- lookup(nm)
    value <- oldparams[[nm]]
    
    tryCatch(
      slot(sp, toName) <- value,
      
      error = function(...) {
        warning("Wrong class for ", toName, ": expected ", slotClass(toName), ", got ", class(value))
      }
    )
  }
  
  # extra params, not present in the old list format
  sp@canCoppice <- !all(sp@coppice_boost_pars == 0)
  
  sp
}
