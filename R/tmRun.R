#' Simulates dynamics of a woodland stand.
#' 
#' \code{tmRun} simulates the dynamics of cohorts of one or more tree species
#' in a woodland stand. For each cohort the simulation tracks annual values of
#' age, number of trees, height, plus a set of morphometrics derived from height
#' such as DBH and canopy radius. The location of individual trees is not
#' explicitly modelled. The stand environment is defined by input vectors for
#' annual rainfall and fire occurrence / intensity. The simulation period is
#' equal to the length of the rainfall vector.
#' 
#' @section Order of events:
#' The order of events in each year of the simulation is:
#' \enumerate{
#'   \item Optional special actions (coppice ON)
#'   \item Optional thinning of trees
#'   \item Growth of trees (uniform within cohort)
#'   \item Survival (adjustment of numbers within cohorts)
#'   \item Recruitment (addition of new cohorts)
#'   \item Fire (possible mortality and reduction of tree height)
#'   \item Report cohort statistics
#' }
#' 
#' @section Management actions:
#' The simulation can include management actions: tree thinning (proportional reduction
#' of numbers in selected cohors), clearing, and height reduction. Conditions can also
#' be specified to protect cohorts from management actions.
#' 
#' Tree thinning in one or more years is enabled by providing a 5-column
#' data.frame as the \code{thinning} argument, where the columns are: year,
#' species ID, minimum height, maximum height, number of trees to leave after
#' thinning. In a thinning year, cohorts corresponding to the species and
#' height-range are identified; then if the total number of trees across all
#' such cohorts exceeds the number specified in the thinning matrix, cohorts are
#' reduced by a uniform proportion.
#' 
#' Clearing (complete removal of selected cohorts) and height reduction can be 
#' specified for selected years of the simulation by providing a 4-column
#' data.frame as the \code{special} argument, where the columns are: year,
#' species ID, action name, value. Actions name is one of 'remove' (clearing), 
#' 'cut' (height reduction) or 'protect' (set protected status of cohorts). 
#' The meaning of the value column depends on the action specified:
#'   \describe{
#'     \item{cut}{value is the new (reduced) height for selected cohorts}
#'     \item{protect}{value treated as TRUE (set cohorts as protected) if non-zero
#'       and FALSE (unprotect cohorts) if zero}
#'     \item{remove}{value is ignored}
#'   }
#' Setting cohorts as protected insulates them from thinning, clearing and
#' height reduction.
#' 
#' @section Session settings:
#' The optional \code{session.settings} argument takes a named list which can 
#' contain the following elements:
#' \describe{
#'   \item{progress.interval}{(integer; default=0) specifies the interval,
#'     in simulation years, for write a progress message to the console.
#'     A value <= 0 means no messages.}
#'     
#'   \item{display.fire.data}{(logical; default FALSE) if TRUE, enables printing fire data to 
#'     the console.}
#' }
#' 
#' 
#' @param Spp Either a single object or a \code{list} of objects
#'   of class \code{\linkS4class{SpeciesParams}} defining one or more species
#' 
#' @param initial.cohorts Numeric matrix with 4 columns: 
#'   species index, age, height, number of trees.
#'   For simulations with a single species the species index should be 1, while
#'   for multiple species the index value is the position in the list
#'   passed to the \code{Spp} argument.
#'   
#' @param rain Numeric vector of annual rainfall values. The length of this
#'   vector determines the number of years in the simulation.
#'   
#' @param stand.area Area of the stand. This should be expressed in units
#'   compatible with those used for species dimensions.
#'   
#' @param scheduled.fires Numeric vector of fire intensity values. The scale is 
#'   user-defined. If provided, the \code{fire.intensity.func} argument must also be
#'   present.
#'   
#' @param fire.intensity.func Function taking two numeric arguments (flammable stand 
#'   proportion and incoming fire intensity) and returning a single numeric
#'   value for realized fire intensity
#'   
#' @param fire.patchiness.func Function taking a single argument (incoming fire intensity)
#'   and returning a single numeric value for the probability of an individual tree being 
#'   burnt.
#'   
#' @param fire.canopy.func Function taking a single numeric argument for
#'   summed core canopy area, and returning a (possibly) transformed area value.
#'   If not provided, untransformed canopy area will be used.
#'   
#' @param fire.early.prob Probability that fire will occur before recruitment in any given
#'   year. The default value (0) specifies that fires always occur later than
#'   recruitment.
#'   
#' @param overlap.matrix Numeric matrix, with dimensions Nspp x Nspp, specifying the allowable
#'   overlap between seedlings and established canopies. Values are proportions of canopy radius: 
#'   with a value \emph{p} in row \emph{i}, col \emph{j} specifying how far seedlings of species \emph{i} 
#'   can be positioned within an overlying canopy of species \emph{j}. Example, a value of 0.1 
#'   means that a seedling may establish under the outer 10% (in terms of radius) of a canopy;
#'   a value of 0 means no seedlings can establish under a canopy; while a value of -0.1 means
#'   that species are excluded from the canopy and a surrounding buffer of width 10% of canopy
#'   radius.
#' 
#' @param ov.gen For compatibility with previous version of the model. This argument used to
#'   control allowable canopy overlap for seedling establishment prior to the introduction of
#'   the \code{overlap.matrix} argument. Now it simply acts as a scale factor for reporting of
#'   general canopy area but will probably be removed in a subsequent version.
#' 
#' @param recruitment A logical vector indicating for which years of the simulation recruitment
#'   may occur. Can also be a numeric vector where 0 means FALSE and all other values
#'   mean TRUE. If length is less than the simulation period (given by the length of the 
#'   \code{rain} vector) the last value will apply to all subsequent years. 
#' 
#' @param thinning An optional specification of tree removal (aka thinning) to impose in 
#'   specified years of the simulation. The object is a data.frame with five columns:
#'   \enumerate{
#'     \item year
#'     \item species integer ID
#'     \item minimum height of trees to thin
#'     \item maximum height of trees to thin
#'     \item total number of trees to leave
#'   }
#'   See \strong{Management actions} for details on how thinning is applied.
#'   
#' @param special An optional specification of management actions to impose in specified
#'   years of the simulation. The object is a data.frame with four columns:
#'   \enumerate{
#'     \item year
#'     \item species integer ID
#'     \item action
#'     \item value
#'   }
#'   See \strong{Management actions} for details on how these actions are applied.
#' 
#' @param seedling.survival Numeric vector giving the probability of establishment
#'   for seedlings in each year of the simulation. If length is less than the
#'   simulation period the last value applies for subsequent years. The probability
#'   is applied to new seedlings regardless of species, so can be used as a proxy
#'   for general threats such as grazing.
#' 
#' @param session.settings Optional named list of settings for blah blah. See 
#'   \strong{Session settings} for details.
#'   
#' @param database.path Path for the output SQLite database. If not provided
#'   the database will be written to a temporary file, which can then be saved to
#'   a permanent file with \code{\link{tmdbSave}}.
#' 
#' @param run.id An integer identifier that will be included in each of the 
#'   output database tables. Useful when the outputs of mutliple runs are to
#'   be aggregated (e.g. by \code{\link{tmDriver}}).
#'   
#' @return Returns a connection to the output SQLite database.
#' 
#' @export
#' 
#' @importFrom dplyr group_by summarise %>%
#' @importFrom stringr str_trim
#' 
tmRun <- function (Spp, 
                   initial.cohorts, 
                   rain, 
                   stand.area=10000,
                   scheduled.fires=NULL, 
                   fire.intensity.func=NULL, 
                   fire.patchiness.func=NULL,
                   fire.canopy.func=NULL, 
                   fire.early.prob=0,
                   overlap.matrix=0, 
                   ov.gen=0, 
                   recruitment=TRUE, 
                   thinning=NULL, 
                   special=NULL, 
                   seedling.survival=NULL, 
                   session.settings=NULL,
                   database.path=NULL,
                   run.id=1)
{
  requireNamespace("RSQLite", quietly = FALSE)

  #===============================================================
  # Magic numbers (TODO: consider moving these to species params)
  #===============================================================
  
  # Growth rate of coppicing trees is boosted to simulate the effect of a larger
  # root system than would otherwise be the case for trees of the same height.
  # The boosting effect is applied until trees reach the proportion of their 
  # former height specified by COPPICE_BOOST_PROP.
  #
  # This proportion is also used When testing tree survival (both base rate and fire)
  # where probability is a function of tree height. A coppicing tree is treated as 
  # being at its former height, for the purposes of the probability calculation, if 
  # its current height is less than this proportion.
  COPPICE_BOOST_PROP <- 0.7
  
  
  # ====================================================================================
  #  If the spp argument is a single SpeciesParams object, wrap it in a list for
  #  consistency between single and multi-species runs.
  # ====================================================================================
  
  if ( is(Spp, "SpeciesParams") ) {
      Spp <- list(Spp)
  }
  
  
  # ====================================================================================
  #  Constants, global variables and frequently accessed bits
  # ====================================================================================
  
  RUNID <- run.id
  
  SIMULATION.PERIOD <- length(rain)
  
  NUM.SPP <- length(Spp)
  SP.NAMES <- character(NUM.SPP)
  for (i in 1:NUM.SPP) {
    SP.NAMES[i] <- Spp[[i]]@name
  }
  
  HAS.HT.DBH.PARS <- logical(NUM.SPP)
  for (i in 1:NUM.SPP) HAS.HT.DBH.PARS[i] <- !is.null(Spp[[i]]@height_dbh_pars)
    
  HAS.EXTERNAL.SEED.FUNC <- sapply(Spp, 
                                   function(sp) hasFunction(sp, "external_seed_fn"))
  
  # all species flag
  EXTERNAL.SEED <- any(HAS.EXTERNAL.SEED.FUNC)
  
  RECRUIT.CANOPY.FUNC <- vector("list", length=NUM.SPP)
  for (i in 1:NUM.SPP) {
    sp <- Spp[[i]]
    RECRUIT.CANOPY.FUNC[[i]] <- getFunctionOrElse(sp, "recruit_canopy_fn", identity)
  }
  
  NON.FLAMMABILITY <- numeric(NUM.SPP)
  for ( spID in 1:NUM.SPP ) NON.FLAMMABILITY[ spID ] <- Spp[[spID]]@non_flammability
  
  if (is.null(fire.intensity.func)) {
    # Default function returns the incoming fire intensity
    # regardless of stand state
    fire.intensity.func <- function(flam.prop, intensity) { intensity }
  }
  
  if (is.null(fire.patchiness.func)) {
    # Default function returns proportion burnt of 1 for any
    # fire
    fire.patchiness.func <- function(intensity) { 1.0 }
  }
  
  if (is.null(fire.canopy.func)) {
    fire.canopy.func <- function(area) { area }
  }
  
  # Cohorts matrix cols (only cols 2 - 5 are in the matrix of initial cohorts
  # provided by the user)
  #
  colID <- 1; colSpID <- 2;  colAge <- 3;  colHeight <- 4;  colN <- 5;  colProtect <- 6; 
  colCoppiceStage <- 7; colFormerHt <- 8; colCoppiceOn <- 9; colResourceUse <- 10;
  colCanopyRadius <- 11; colGrowthRate <- 12; colSurvP <- 13; colDBH <- 14; colBasalArea <- 15;
  colFirstCoreArea <- 16;
  colGeneralCoreArea <- colFirstCoreArea + NUM.SPP
  
  NUM.COHORTS.COLS <- colGeneralCoreArea
  
  # Previously we created a CohortSummary data.frame and expanded this one row at a time
  # as cohorts were added during the simulation. This is a slow operation in R so now
  # we create a matrix with dimensions sufficient for the maximum number of cohorts that
  # can arise in the simulation
  max.cohorts <- nrow(initial.cohorts) + SIMULATION.PERIOD * NUM.SPP
  CohortSummary <- matrix(0, nrow=max.cohorts, ncol=16)
  
  cscolRun <- 1; cscolID <- 2; cscolSpID <- 3; cscolStartTime <- 4; cscolStartAge <- 5;
  cscolStartN <- 6; cscolStartHt <- 7; cscolStartDBH <- 8; cscolStartBA <- 9;
  cscolFinalTime <- 10; cscolFinalAge <- 11; cscolFinalN <- 12; cscolFinalHt <- 13;
  cscolFinalDBH <- 14; cscolFinalBA <- 15; cscolCoppiceStage <- 16
  
  TOTAL.COHORTS <- 0 # will be updated by NewCohortSummaryRecord


  # ====================================================================================
  #  Helper function - format a multi-line query string
  # ====================================================================================  
  FormatSQL <- function(str) {
    strwrap(str, width = 10000, simplify = TRUE)
  }
  
  # ====================================================================================
  # SQL INSERT statement for writing cohort data
  # ====================================================================================
  
  SQL.WriteCohortData <-
    FormatSQL(
      "INSERT INTO cohortyearly ( 
         RunID, Time, CohortID, SpeciesID, Age, Height, DBH, BasalArea, N, 
         ResourceUse, CanopyRadius, CoppiceStage, FormerHeight, CoppiceOn,
         GrowthAdj, SurvivalAdj, CoreAreaGeneral )
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" )
  
  
  # ====================================================================================
  #  Helper function - Set session settings
  # ====================================================================================  
  SetSession <- function() {
    if (is.null(session.settings)) {
      session.settings <<- list()
    }
    
    # the 'progress.interval' option is a numeric value that sets
    # the time between progress reporint on screen
    if (is.null(session.settings$progress.interval)) {
      session.settings$progress.interval <<- 0 # no progress messages
    }
    
    # the 'display.fire.data' option enables printing fire data to
    # the screen
    if (is.null(session.settings$display.fire.data)) {
      session.settings$display.fire.data <<- FALSE
    }
  }
  
  
  # ====================================================================================
  #  Helper function - Create a new SQLite database for the simulation output.
  # ====================================================================================  
  CreateDatabase <- function(path) {
    con <- RSQLite::dbConnect( RSQLite::SQLite(), path )
    
    # Create the species table
    RSQLite::dbGetQuery(con, FormatSQL("CREATE TABLE species (
                     RunID INTEGER,
                     SpeciesID INTEGER,
                     Name TEXT,
                     PRIMARY KEY (RunID, SpeciesID))" ) )
    
    # Add species data to the table
    df <- data.frame(RunID=RUNID, SpeciesID=1:NUM.SPP, Name=SP.NAMES)
    RSQLite::dbWriteTable(con, "species", df, row.names=FALSE, overwrite=TRUE)
    
    # Create cohort annual data table
    RSQLite::dbGetQuery(con, FormatSQL("CREATE TABLE cohortyearly (
                     RunID INTEGER,
                     Time INTEGER,
                     CohortID INTEGER,
                     SpeciesID INTEGER,
                     Age INTEGER,
                     Height REAL,
                     N INTEGER,
                     ResourceUse REAL,
                     GrowthAdj REAL,
                     SurvivalAdj REAL,
                     CanopyRadius REAL,
                     CoreAreaGeneral REAL,
                     DBH REAL,
                     BasalArea REAL,
                     CoppiceStage INTEGER,
                     FormerHeight REAL,
                     CoppiceOn INTEGER,
                     PRIMARY KEY (RunID, Time, CohortID) )" ) )
    
    # Create cohort summary table
    RSQLite::dbGetQuery(con, FormatSQL( "CREATE TABLE cohortsummary (
                     RunID INTEGER,
                     CohortID INTEGER,
                     SpeciesID INTEGER,
                     StartTime INTEGER,
                     StartAge INTEGER,
                     StartN INTEGER,
                     StartHeight REAL,
                     StartDBH REAL,
                     StartBasalArea REAL,
                     FinalTime INTEGER,
                     FinalAge INTEGER,
                     FinalN INTEGER,
                     FinalHeight REAL,
                     FinalDBH REAL,
                     FinalBasalArea,
                     CoppiceStage INTEGER, 
                     PRIMARY KEY (RunID, CohortID) )" ) )
    
    # Create the common data table
    RSQLite::dbGetQuery(con, FormatSQL("CREATE TABLE commondata (
                     RunID INTEGER,
                     Time INTEGER,
                     ResourceUse REAL, 
                     CoreAreaGeneral REAL,
                     MergedArea REAL,
                     Rain REAL, 
                     ScheduledFires REAL, 
                     RealizedFires REAL, 
                     RealizedPropBurnt REAL,
                     FlammableProp REAL, 
                     SeedlingSurvival REAL,
                     PRIMARY KEY (RunID, Time) )" ) )
    
    # Return the connection
    con
  }  

  
  # ====================================================================================
  #  Helper function - Store a snapshot of the param objects in binary form in the
  #  paramobjects database table
  # ====================================================================================
  StoreParamSnapshot <- function(dbcon, paramSetID) {
    params <- list(Spp = Spp, 
                   initial.cohorts = initial.cohorts, 
                   rain = rain, 
                   stand.area = stand.area,
                   scheduled.fires = scheduled.fires, 
                   fire.intensity.func = fire.intensity.func, 
                   fire.patchiness.func = fire.patchiness.func,
                   fire.canopy.func = fire.canopy.func,
                   fire.early.prob = fire.early.prob,
                   overlap.matrix = overlap.matrix, 
                   ov.gen = ov.gen,
                   recruitment = recruitment,
                   thinning = thinning, 
                   special = special, 
                   seedling.survival = seedling.survival ) 
    
    object.data <- rawToChar(serialize(params, NULL, ascii=TRUE))
    
    # Using a dbGetPreparedQuery seems to work with large blobs of param data whereas using
    # dbGetQuery sometimes fails. Also note the WrapText function isn't required when using
    # dbGetPreparedQuery.
    RSQLite::dbGetPreparedQuery(dbcon, 
                       "INSERT INTO paramobjects (ID, Data) VALUES (?, ?)",
                       data.frame(ID=paramSetID, Data=object.data, stringsAsFactors=FALSE))
  }
  
  
  
  # ====================================================================================
  #  Helper function - Get the Cohorts matrix row index for the given cohort
  # ====================================================================================
  GetCohortRow <- function( cohort.id ) {
    match(cohort.id, Cohorts[ , colID ])
  }
  
  # ====================================================================================
  #  Helper function - Get descriptors (canopy radius and resource use surrogate such
  #       as canopy surface area) for the given cohort
  # ====================================================================================
  
  # order of elements in the vector returned by this function
  tdRadius <- 1; tdResourceUse <- 2; tdDBH <- 3; tdBasalArea <- 4
  
  GetTreeDescriptors <- function( cohort.id )
  {
    # get the cohort data
    cohort.data <- Cohorts[ GetCohortRow(cohort.id), ]
    
    # get the descriptors table in which col1 is height, col2 is
    # canopy radius and col 3 is whatever we're using for the
    # resource use surrogate (e.g. canopy surface area)
    #
    spID <- cohort.data[ colSpID ]
    desc <- Spp[[spID]]@tree_desc
    
    # Match the cohort height to a row in the descriptors table
    # using the heights col (1)
    # (note: we are implicitly rounding up here)
    # If no matching row is found (tree too tall) use the last one
    #
    i <- match( TRUE, desc[,1] >= cohort.data[ colHeight ], nrow(desc) )
    
    canopy.radius <- desc[i,2] 
    resource.use <- desc[i,3] * cohort.data[colN]
    
    # calculate dbh (cm) and basal.area (sq m) if the parameters were provided
    #
    dbh <- 0
    basal.area <- 0
    if (HAS.HT.DBH.PARS[spID]) {
      pars <- Spp[[spID]]@height_dbh_pars
      dbh <- exp(pars[1] + pars[2] / {pars[3] + cohort.data[ colHeight ]})
      basal.area <- pi * (dbh/200)^2 * cohort.data[ colN ]
    }
    
    c(canopy.radius, resource.use, dbh, basal.area)
  }
  
  
  # ====================================================================================
  #  Helper function - Add a cohort to the Cohorts matrix and output database
  # ====================================================================================
  AddCohort <- function( spID, N )
  {
    pars <- Spp[[spID]]
    ID <- NextCohortID
    NextCohortID <<- NextCohortID + 1
    
    # cols: ID, spID, age, height, N, Protect, CoppiceStage, FormerHt, CoppiceOn, ResourceUse, 
    # CanopyRadius
    new.cohort.data <- numeric(NUM.COHORTS.COLS)
    new.cohort.data[c(colID, colSpID, colHeight, colN, colFormerHt)] <- c( ID, spID, pars@height_yr1, N, -1 )
    Cohorts <<- rbind(Cohorts, new.cohort.data)
    
    NewCohortSummaryRecord( ID, spID, YEAR, 0, N, pars@height_yr1, 0, 0, 0 )
    
    # return the new cohort's ID
    ID
  }
  
  
  
  # ====================================================================================
  #  Helper function - Add a cohort to the cohorts summary table in the output database
  # ====================================================================================
  NewCohortSummaryRecord <- function( ID, spID, startTime, startAge, startN, startHeight, startDBH, 
                                      startBasalArea, coppiceStage ) {
    
    # Note - don't use rbind to add the new record because if it is the first (ie. CohortSummary
    # is an empty data.frame) rbind stupidly zaps the col names.
    #
    # MB Sep 2010: The above note doesn't apply now because we create CohortSummary as a matrix
    # with sufficient rows for the max possible number of cohorts to decrease running time.
    
    i <- TOTAL.COHORTS + 1
    CohortSummary[i, ] <<- c(RUNID, ID, spID, startTime, startAge, startN, startHeight, startDBH, startBasalArea, 
                             startTime, startAge, startN, startHeight, startDBH, startBasalArea, coppiceStage)
    TOTAL.COHORTS <<- i
  }
  
  
  # ====================================================================================
  #  Helper function - Archive yearly data and update the summary table for all cohorts
  # ====================================================================================
  WriteCohortData <- function(dbcon) {
    # NB we are only storing 'core.area.general' (the core area of that cohort 
    # calculated using the ov.gen parameter (default=0))
    
    RSQLite::dbBegin( dbcon )
    RSQLite::dbSendPreparedQuery(dbcon, SQL.WriteCohortData,
                        as.data.frame(cbind(RUNID, YEAR, 
                                            Cohorts[ , c(colID, colSpID, colAge, colHeight, colDBH, colBasalArea, colN, colResourceUse, colCanopyRadius, 
                                                         colCoppiceStage, colFormerHt, colCoppiceOn, colGrowthRate, colSurvP, colGeneralCoreArea), drop=FALSE ]))) 
    RSQLite::dbCommit( dbcon )
    
    
    irows <- match(Cohorts[ , colID], CohortSummary[1:TOTAL.COHORTS, cscolID])
    if (any(is.na(irows))) {
      stop("Cohorts recs found without matching CohortSummary recs")
    }
    
    CohortSummary[irows, cscolFinalTime]    <<- YEAR
    CohortSummary[irows, cscolFinalAge]     <<- Cohorts[,colAge]
    CohortSummary[irows, cscolFinalN]       <<- Cohorts[,colN]
    CohortSummary[irows, cscolFinalHt]      <<- Cohorts[,colHeight]
    CohortSummary[irows, cscolFinalDBH]     <<- Cohorts[,colDBH]
    CohortSummary[irows, cscolFinalBA]      <<- Cohorts[,colBasalArea]
    CohortSummary[irows, cscolCoppiceStage] <<- Cohorts[,colCoppiceStage]
  }
  
  
  # ====================================================================================
  #  Helper function - Write out the end-of-simulation summary data
  # ====================================================================================
  WriteSummaryData <- function(dbcon) {
    common.data <- cbind(RUNID, 1:YEAR, 
                         common.resource.use[1:YEAR], 
                         common.core.area[1:YEAR], 
                         common.merged.area[1:YEAR],
                         rain[1:YEAR], 
                         scheduled.fires[1:YEAR], 
                         realized.fires[1:YEAR], 
                         realized.prop.burnt[1:YEAR],
                         flammable.proportions[1:YEAR], 
                         seedling.survival[1:YEAR])
    
    RSQLite::dbWriteTable(dbcon, "commondata", 
                          as.data.frame(common.data), 
                          row.names=FALSE, append=TRUE)
    
    RSQLite::dbWriteTable(dbcon, "cohortsummary", 
                          as.data.frame(CohortSummary[1:TOTAL.COHORTS, ]), 
                          row.names=FALSE, append=TRUE)
  }
  
  # ====================================================================================
  #  Helper function - compare strings ignoring case and leading / trailing white-space
  # ====================================================================================
  stringEqualsIgnoreCase <- function( s1, s2 ) {
    tolower( str_trim(s1) ) == tolower( str_trim(s2) )
  }
  
  # ====================================================================================
  #  Helper function - case-insensitive string match
  # ====================================================================================
  StringMatch <- function( x, table ) {
    match( tolower( str_trim(x) ), tolower( str_trim(table) ) )
  }
  
  # ====================================================================================
  #  Helper function - wraps text in single quotes
  # ====================================================================================
  WrapText <- function(x) paste("'", x, "'", sep="")
  
  # ====================================================================================
  #  Helper function - generate index values for a loop counter, returning NULL if the
  #  'from' value is greater than the 'to' value. This prevents the insidious R error
  #  of "for (i in 1:n)" when n is 0.
  # ====================================================================================
  SafeIndex <- function(from=1, to) {
    if (from > to) return(NULL)
    else return(from:to)
  }
  
  
  
  # ====================================================================================
  #  Model starts here
  # ====================================================================================
  
  SetSession()
  
  # If an overlap matrix wasn't provided, create a default one
  if (is.null(overlap.matrix) || is.vector(overlap.matrix)) {
    val <- ifelse(is.null(overlap.matrix), 0, overlap.matrix[1])
    overlap.matrix <- matrix(val, nr=NUM.SPP, nc=NUM.SPP)
    rownames(overlap.matrix) <- SP.NAMES
    colnames(overlap.matrix) <- SP.NAMES
    
  } else {
    # Ensure that the values in the overlap matrix are in the same order as species
    # in the Spp list. If the species names do not occur in the row and column
    # names, complain.
    m <- matrix(0, nrow=nrow(overlap.matrix), ncol=ncol(overlap.matrix))
    for (i in 1:NUM.SPP) {
      for (j in 1:NUM.SPP) {
        r <- StringMatch( SP.NAMES[i], rownames(overlap.matrix) )
        c <- StringMatch( SP.NAMES[j], colnames(overlap.matrix) )
        if (is.na(r) || is.na(c)) stop(paste(SP.NAMES[i], "should appear in overlap matrix row and col names"))
        
        m[i, j] <- overlap.matrix[r, c] 
      }
    }
    overlap.matrix <- m
  }
  
  # Convert overlap to core proportion
  #
  overlap.matrix <- 1 - overlap.matrix
  ov.gen <- 1 - ov.gen
  
  
  # Previous versions of the model expected a 5 col matrix for initial cohorts,
  # whereas now we require a 4 col matrix (sp, age, height, number).
  # If the input has 5 cols assume it's an old one, ignore the first col, and 
  # issue a message
  
  if ( !is.matrix(initial.cohorts) | ncol(initial.cohorts) < 4 | ncol(initial.cohorts) > 5 ) 
    stop("Expected a 4 col matrix for initial cohorts (or an old 5 col matrix")
  
  initial.cohorts.cols <- 1:4
  if ( ncol(initial.cohorts) == 5 ) {
    initial.cohorts.cols <- 2:5
    cat( "Initial cohorts matrix has 5 cols - assuming it is an old version \n" )
  }
  
  num.initial.cohorts <- nrow(initial.cohorts) 
  
  cat( num.initial.cohorts, "initial cohorts \n" )
  
  max.num.cohorts <- num.initial.cohorts + SIMULATION.PERIOD * NUM.SPP
  
  Cohorts <- matrix(0, nrow=num.initial.cohorts, ncol=NUM.COHORTS.COLS)
  Cohorts[ , colID ] <- 1:num.initial.cohorts
  Cohorts[ , c(colSpID, colAge, colHeight, colN) ] <- initial.cohorts[ , initial.cohorts.cols ]
  Cohorts[ , colFormerHt ] <- -1
  for (i in 1:nrow(Cohorts)) {
    Cohorts[ i, c( colCanopyRadius, colResourceUse, colDBH, colBasalArea ) ] <- GetTreeDescriptors( Cohorts[i, colID] )
  }
  
  NextCohortID <- num.initial.cohorts + 1
  
  #==============================================================
  # Declare various global variables and create the database
  # for simulation output.
  #==============================================================
  common.resource.use <- numeric( SIMULATION.PERIOD )
  common.core.area <- numeric( SIMULATION.PERIOD )  # just stores CoreAreaGeneral values
  common.merged.area <- numeric( SIMULATION.PERIOD )
  
  total.core.area <- numeric( NUM.SPP + 1 )
  names.combined <- paste("CombCoreArea", SP.NAMES, sep="")
  names(total.core.area) <- c( names.combined, "CombCoreAreaGeneral")
  
  realized.fires <- numeric( SIMULATION.PERIOD )
  realized.prop.burnt <- numeric( SIMULATION.PERIOD )
  flammable.proportions <- numeric( SIMULATION.PERIOD )
  
  if (is.null(database.path)) {
    database.path <- tempfile()
    
  } else if (file.exists(database.path)) {
    ok <- file.remove(database.path)
    if (!ok) {
      warning("Cannot delete existing database file ", database.path,
              ". Writing to temp file.")
      
      database.path <- tempfile()
    }
  }
  DBCON <- CreateDatabase(database.path)


  # Add the initial cohorts to the summary table in the output database
  for ( i in 1:nrow(Cohorts) )
  {
    NewCohortSummaryRecord( i, Cohorts[i, colSpID], 0, Cohorts[i, colAge], Cohorts[i, colN], 
                            Cohorts[i, colHeight], Cohorts[i, colDBH], Cohorts[i, colBasalArea], Cohorts[i, colCoppiceStage])
  }
  
  # Write the initial cohorts to the output database as year 0 data
  YEAR <- 0
  WriteCohortData(DBCON)
  
  # create a 'no fires' fire vector if a scheduled fires vector was not provided
  #
  if ( is.null( scheduled.fires ) ) 
  {
    scheduled.fires <- numeric( SIMULATION.PERIOD )
    cat ( "No fires in this simulation \n" )
  }
  else
  {
    if ( length(scheduled.fires) < SIMULATION.PERIOD ) 
      scheduled.fires <- c(scheduled.fires, rep(0, SIMULATION.PERIOD - length(scheduled.fires)))
  }
  
  
  # if seedling.survival is missing, make it 1 for all years
  #
  if ( is.null( seedling.survival ) )
  {
    seedling.survival <- rep(1, SIMULATION.PERIOD )
  }
  else if ( length(seedling.survival) < SIMULATION.PERIOD )
  {
    seedling.survival <- c(seedling.survival, rep(1, SIMULATION.PERIOD - length(seedling.survival)))
  }
  
  # if recruitment is a single value convert it to a yearly vector
  if ( length(recruitment) == 1 ) 
  {
    recruitment <- rep( recruitment, SIMULATION.PERIOD )
  }
  # if recruitment is a vector but too short, pad it out with the last value
  else if ( length(recruitment) < SIMULATION.PERIOD )
  {
    len <- length(recruitment)
    recruitment <- c( recruitment, rep(tail(recruitment, 1), SIMULATION.PERIOD - length(recruitment)) )
  }
  
  # check the thinning arg, if provided, and create a flag array to identify thinning years
  is.thinning.year <- logical( SIMULATION.PERIOD )
  if ( ! is.null( thinning ) )
  {
    if ( !is.matrix( thinning ) ) stop( "Bummer: Expected a 5 col matrix for thinning" )
    if ( ncol( thinning ) != 5 )  stop( "Bummer: Expected a 5 col matrix for thinning" )
    
    thinColYear <- 1
    thinColSpID <- 2
    thinColMinHeight <- 3
    thinColMaxHeight <- 4
    thinColN <- 5
    
    years <- thinning[ , thinColYear ]
    # just in case...
    years <- years[ years <= SIMULATION.PERIOD ]
    
    # Check for duplication of year + spID with overlapping height ranges
    # Note: since we added the option to treat all species (species ID = NA)
    # we run the check on a temp matrix in which such rows are converted to
    # separate species rows.
    #
    NA.rows <- which( is.na( thinning[ , thinColSpID] ) )
    sp.rows <- which( !is.na( thinning[ , thinColSpID] ) )
    
    # This vector will hold the number of the thinning matrix row corresponding
    # to each row of the temp matrix
    temp.row.matrix <- c( sp.rows, rep( NA.rows, each=NUM.SPP ) )
    
    if ( length( NA.rows ) > 0 ) 
    {
      temp <- thinning[ sp.rows, , drop=FALSE ]
      
      for ( row in NA.rows ) 
      {
        temp.row <- thinning[ row, ]
        for ( spID in 1:NUM.SPP )
        {
          temp.row[ thinColSpID ] <- spID
          temp <- rbind( temp, temp.row )
        }
      }
    }
    else
    {
      temp <- thinning
    }
    
    n <- nrow( temp )
    if ( n > 1 )
    {
      for ( i in 1:(n-1) )
      {
        others <- which( temp[(i+1):n, thinColYear] == temp[i, thinColYear] &
                           temp[(i+1):n, thinColSpID] == temp[i, thinColSpID] )
        
        if ( length(others) > 0 )
        {
          # adjust others to be row numbers of thinning matrix
          others <- c( i, others + i )
          # sort height ranges by min height and then just compare max height
          # of each to min height of the one below to check for overlap
          others <- others[ order( temp[ others, thinColMinHeight ] ) ]
          for ( j in 1:(length(others)-1) )
          {
            if ( temp[others[j], thinColMaxHeight] > temp[others[j+1], thinColMinHeight] )
              stop( paste("Bummer: overlapping height ranges for same year and species in rows", 
                          temp.row.index[others[j]], "and", temp.row.index[others[j+1]],
                          "of thinning matrix") )
          }
        }
      }
    }
    
    # If we made it here the thinning matrix was OK
    is.thinning.year[ years ] <- TRUE
  }
  
  # check the 'special' argument, if provided
  is.special.year <- logical( SIMULATION.PERIOD )
  
  if ( !is.null( special ) )
  {
    specialColCohortID <- 1
    specialColYear <- 2
    specialColAction <- 3
    specialColValue <- 4
    
    special.actions <- c("remove", "cut", "protect")
    
    special.action.remove <- pmatch( "rem", special.actions )
    special.action.cut <- pmatch( "cut", special.actions )
    special.action.protect <- pmatch( "pro", special.actions )
    
    if ( !is.data.frame( special ) | ncol(special) != 4 ) stop("special should be a 4 col data.frame")
    
    for ( i in 1:nrow( special ) )
    {
      if ( special[i, specialColCohortID] < 1 | special[i, specialColCohortID] > nrow(Cohorts) )
        stop( paste("cohort.id", special[i, specialColCohortID], "out of range in special argument") )
      
      if ( special[i, specialColYear] < 1 | special[i, specialColYear] > SIMULATION.PERIOD )
        stop( paste("year", special[i, specialColYear], "out of range in special argument") )
      
      if ( is.na( pmatch(special[i, specialColAction], special.actions) ) )
        stop( paste("unknown action", special[i, specialColAction], "in special argument") )
      
      if ( pmatch(special[i, specialColAction], special.actions) == special.action.cut )
      {
        if ( special[i, specialColValue] <= 0 ) stop( paste("height value for row", i, "should be positive") )
      }
    }
    
    is.special.year[ special[ , specialColYear ] ] <- TRUE
  }
  
  
  #==============================================================
  # Yearly loop
  #==============================================================
  for ( YEAR in 1:length( rain ) )
  {
    if ( session.settings$progress.interval > 0 && 
         (YEAR %% session.settings$progress.interval == 0 || YEAR == SIMULATION.PERIOD) ) {
      cat( "Year ", YEAR, "\n" )
      flush.console()
    }
    
    #==============================================================
    # Special treatment applied to one or more initial cohorts
    #==============================================================
    if ( is.special.year[ YEAR ] )
    {
      m.rows <- which( special[, specialColYear] == YEAR )
      for ( i in m.rows )
      {
        cohort.id <- special[i, specialColCohortID]
        irow <- GetCohortRow( cohort.id )
        if ( Cohorts[irow, colN] > 0 )
        {
          i.action <- pmatch( special[i, specialColAction], special.actions )

          if ( i.action == special.action.remove ) 
          {
            Cohorts[irow, colN] <- 0
          }
          
          else if ( i.action == special.action.protect )
          {
            Cohorts[irow, colProtect] <- (special[i, specialColValue] != 0)
          } 
          
          else if ( i.action == special.action.cut )
          {
            cut.h <- min( Cohorts[irow, colHeight], special[i, specialColValue] )

            # If the species can coppice, record current height to use
            # for boosted growth
            spID <- Cohorts[irow, colSpID]
            pars <- Spp[[ spID ]]
            if ( pars@canCoppice && cut.h <= pars@height_yr1 )
            {
              Cohorts[irow, colCoppiceStage] <- Cohorts[irow, colCoppiceStage] + 1
              Cohorts[irow, colCoppiceOn] <- 1
              
              # when a cohort coppices again before reaching the original former 
              # height, former height stays as the original former height, otherwise
              # it is changed to the immediate previous height:
              if ( Cohorts[irow, colCoppiceStage] > 1 && 
                   Cohorts[irow, colHeight] < Cohorts[irow, colFormerHt] ) 
              {
                Cohorts[irow, colFormerHt] <- Cohorts[irow, colFormerHt]
              } else {
                Cohorts[irow, colFormerHt] <- Cohorts[irow, colHeight]
              }
            }
            
            Cohorts[irow, colHeight] <- cut.h
            cat( "cut cohort", cohort.id, "in year", YEAR, "to height", cut.h, "\n" )
          }          
        }
        else
        {
          cat( "cohort", special[i, specialColCohortID], 
               "died before special action in year", YEAR, "\n" )
        }
      }
    }
    
    
    #==============================================================
    # Thinning treatment
    # (note: protected cohorts ARE NOW PROTECTED FROM THINNING)
    #==============================================================
    if ( is.thinning.year[ YEAR ] )
    {
      m.rows <- which( thinning[, thinColYear] == YEAR )
      for ( i in m.rows )
      {
        i.cohorts <- which ( Cohorts[,colHeight] > thinning[i,thinColMinHeight] & 
                               Cohorts[,colHeight] <= thinning[i,thinColMaxHeight] & 
                               Cohorts[,colN] > 0  &
                               Cohorts[,colProtect]==0 )
        
        if ( length(i.cohorts) > 0 & !is.na( thinning[i, thinColSpID] ) )
        {
          i.cohorts <- i.cohorts[ Cohorts[i.cohorts, colSpID] == thinning[i,thinColSpID] ]
        }
        
        if ( length(i.cohorts) > 0 )
        {
          n.sum <- sum( Cohorts[i.cohorts, colN] )
          n.adj <- thinning[i, thinColN] / n.sum
          for ( j in i.cohorts )
          {
            if (round( Cohorts[j, colN] * n.adj ) < Cohorts[j, colN])
            {
              Cohorts[j, colN] <- round( Cohorts[j, colN] * n.adj )
            }
          }
        }      
      }
    }
    
    #==============================================================
    # Tree variables
    #==============================================================
    for ( i in SafeIndex(to=nrow(Cohorts)) ) 
    {
      if ( Cohorts[i, colN] > 0 )
      {
        Cohorts[ i, c( colCanopyRadius, colResourceUse, colDBH, colBasalArea ) ] <- GetTreeDescriptors( Cohorts[i, colID] )
        
        # for each cohort we calculate the core area perceived by each seedling species and in general, 
        for ( spID in 1:NUM.SPP )
        {
          overlap.value <- overlap.matrix[ spID, Cohorts[i, colSpID] ]
          Cohorts[i, colFirstCoreArea + spID - 1] <- Cohorts[i,colN] * pi * (overlap.value * Cohorts[i, colCanopyRadius])^2
        }
        # we calculate that cohort's general core.area, using the ov.gen par
        Cohorts[i, colGeneralCoreArea] <- Cohorts[i,colN] * pi * (ov.gen * Cohorts[i, colCanopyRadius])^2
      }
    }
    
    total.resource.use <- sum( Cohorts[ , colResourceUse ] )
    
    for ( k in 1:(NUM.SPP+1) ) {
      total.core.area[k] <- sum( Cohorts[ , colFirstCoreArea + k - 1] )
    }
    
    # An estimate of the merged canopy area of the stand is made using
    # the fire canopy function and the unscaled total core area
    #
    total.merged.area <- fire.canopy.func( total.core.area[NUM.SPP + 1] )
    
    for ( i in SafeIndex(to=nrow(Cohorts)) )
    {
      if ( Cohorts[i, colN] <= 0 ) next
      
      pars <- Spp[[ Cohorts[i, colSpID] ]]
      
      #==============================================================
      # Growth
      #==============================================================
      h <- Cohorts[i, colHeight]
      former.h <- Cohorts[i, colFormerHt]
      adj.pars <- pars@growth_rainfall_pars
      g.rain.adj <- 1 / {1 + exp( -{ adj.pars[1] + adj.pars[2] * rain[YEAR] * h^adj.pars[3] } ) }
      
      adj.pars <- pars@growth_crowding_pars
      g.crowd.adj <- 1 / {1 + exp( -{adj.pars[1] + adj.pars[2] * total.resource.use * h^adj.pars[3]} ) }
      
      # Now, if you are coppicing and you're less than COPPICE_BOOST_PROP of former(max) ht,
      # our growth rate is boosted, whereas if you have reached or exceeded that proportion of
      # former height your growth rate is normal and coppicing is turned/stays off 
      # (colCoppiceOn=0). If you're not coppicing, your growth rate is normal:
      
      if ( pars@canCoppice && Cohorts[i, colCoppiceOn] == 1 )  
      {
        # Make sure that we haven't somehow lost former height value while still coppicing
        if (is.na(former.h))
          stop("Coppicing turned on but former height (former.h) is NA")
        
        if ( h < COPPICE_BOOST_PROP * former.h ) 
          # To introduce a slackening off of the boosting effect due to root area 
          # equilibrating with shoot area (based on Karen's unpubl. data)
        { 
          boost.pars <- pars@coppice_boost_pars
          adjusted.boosted.gr <- max( pars@growth_rate, boost.pars[1]/{1 + boost.pars[2] * exp(-boost.pars[3] * former.h)} )
          # this equation and the coefficients fitted to it (in coppice.boost.pars) were from a regression of
          # coppice growth data from Mt Pilot, and directly relate a boosted growth rate to a tree's former height.
          Cohorts[i, colGrowthRate] <- adjusted.boosted.gr * g.rain.adj * g.crowd.adj
          
        } else {  # height >= COPPICE_BOOST_PROP * former.h
          Cohorts[i, colGrowthRate] <- pars@growth_rate * g.rain.adj * g.crowd.adj
          Cohorts[i, colCoppiceOn] <- 0
        }
        
      } else {  # not coppicing
        Cohorts[i, colGrowthRate] <- pars@growth_rate * g.rain.adj * g.crowd.adj
      }
      
      # original 'bodge' curve for height increment vs height
      #h.incr <- max(0, (h * growth.rate * (pars@max_height - h) / pars@max_height) )
      
      # current curve based on two parameter exponential heights vs age formula
      # for ideal conditions...
      #    h(t) = a( 1 - exp(-ct) )
      # which gives height increment vs height formula
      #    delta.h = c(a - h)
      #
      h.incr <- Cohorts[i, colGrowthRate] * (pars@max_height - h)
      
      Cohorts[i, colHeight] <- h + h.incr
      
      
      #==============================================================
      # Survival
      #==============================================================
      #print( "Survival" );  flush.console()
      p.base <- pars@survival_prob[ min(length( pars@survival_prob ), Cohorts[i, colAge]) ]
      
      if ( pars@canCoppice && 
           Cohorts[i, colCoppiceOn] == 1  &&  
           h < COPPICE_BOOST_PROP * former.h )
        
        # this is safe as long as former.h is never NA when colCoppiceOn = 1, which should always be true???
      {
        h <- former.h
      }
      
      # inverse logit expression to calculate probability adjustment
      #
      adj.pars <- pars@survival_rainfall_pars
      s.rain.adj <- 1 / {1 + exp( -{ adj.pars[1] + adj.pars[2] * rain[YEAR] * h^adj.pars[3] } ) }
      
      adj.pars <- pars@survival_crowding_pars
      s.crowd.adj <- 1 / {1 + exp( -{adj.pars[1] + adj.pars[2] * total.resource.use * h^adj.pars[3]} ) }
      
      Cohorts[i, colSurvP] <- p.base * s.rain.adj * s.crowd.adj
      
      # only change the number of individuals for non-protected cohorts
      if ( !Cohorts[i, colProtect] )
        Cohorts[i, colN] <- rbinom( 1, Cohorts[i, colN], Cohorts[i, colSurvP] )
    }
    
    
    #==============================================================
    # Recruitment
    #==============================================================
    
    # We always want this vector to be initialized, even for years where there is no recruitment
    # because it is used in the fire sub-model below.
    #
    new.cohort.ids <- integer(NUM.SPP)
    
    if ( recruitment[YEAR] )
    {
      #print( "Recruitment" );  flush.console()
      
      num.stems <- sum(Cohorts[, colN])
      
      for ( spID in 1:NUM.SPP )
      {
        pars <- Spp[[spID]]
        
        seed <- 0
        # Seed production of in-situ trees
        seed.cohorts <- which( Cohorts[, colSpID] == spID )
        if ( length( seed.cohorts ) > 0 ) 
        {
          for ( i in seed.cohorts )
          {
            h <- round( Cohorts[i, colHeight] )
            if ( h >= 1 )
              seed <- seed + Cohorts[i, colN] * pars@seed_output[h, 2] * pars@seed_output[h, 3]
          }
        }
        
        if ( HAS.EXTERNAL.SEED.FUNC[spID] )
        {
          seed <- seed + (pars@external_seed_fn)()
        }
        
        # rainfall effect
        srp <- pars@seed_rainfall_pars
        adj <- 1 / (1 + exp( -( srp[1] + srp[2] * rain[YEAR] ) ) )
        seed <- seed * adj
        
        # balls in bins model
        #
        # we don't want endless seedlings cramming in over the max_sprogs limit
        # which will happen if we just consider cover of existing residents, so
        # we adjust the number of bins by the number of existing trees
        #
        free.bins <- pars@max_sprogs - num.stems
        if ( free.bins <= 0 ) next
        
        num.sprogs <- rbinom( 1, pars@max_sprogs, 1 - exp(-seed / pars@max_sprogs) )
        if ( num.sprogs == 0 ) next
        
        num.sprogs <- rbinom( 1, num.sprogs, free.bins / pars@max_sprogs )
        if ( num.sprogs == 0 ) next
        
        # Reduction in available space (resources) perceived by the new recruits.
        # For this we take the summed scaled canopy area (as seen by the recruiting
        # species) and apply a merging function to calculate total obscuring cover.
        #
        canopy.area <- RECRUIT.CANOPY.FUNC[[spID]]( total.core.area[spID] )
        gap <- stand.area - canopy.area
        
        if ( gap > 0 ) 
        {
          num.sprogs <- rbinom( 1, num.sprogs, gap / stand.area )
          
          # finally we imagine that the value for this year for
          # seedling.survival is something like bunnies grazing on 
          # the seedlings
          #
          num.sprogs <- rbinom( 1, num.sprogs, seedling.survival[YEAR] )
          
          if ( num.sprogs > 0 ) { 
            new.cohort.ids[spID] <- AddCohort( spID, num.sprogs )
          }
        }
      }
    }
    
    
    #==============================================================
    # Fire induced changes
    #==============================================================
    if ( scheduled.fires[YEAR] > 0 )  # if a fire is SCHEDULED in the current year
    {
      # first we calculate a realized fire intensity using the relationships contained in the fire.function
      # (which has as input scheduled intensity and the flammable proportion of the cell). Flammable proportion
      # of the cell is calculated as the difference between the cell area and the contribution of each cohort to
      # what we think of as non-flammable area. (Note that we are re-calculating core area for each cohort here 
      # but we don't use these new values for reporting)
      #
      
      flammable.area <- stand.area
      if (nrow(Cohorts) > 0) {
        # Calculate summed canopy area for each species
        total.core.area.post.growth <- {
          
          canopyAreas <- apply(Cohorts, 1, function(cohort) {
            r <- cohort[colCanopyRadius]
            pi * r * r * cohort[colN]
          })
        
          tapply(canopyAreas, Cohorts[ , colSpID], sum)
        }
        
        # From this point it gets fudgy...
        # We apply the canopy merging function to get total merged canopy area.
        # We then estimate a non-flammable merged area based on the proportion of the non-merged area
        # occupied by each species and their respective non-flammabilities
        
        merged.area <- fire.canopy.func( sum(total.core.area.post.growth) )
        
        non.flammable.area <- sum( merged.area * total.core.area.post.growth / sum(total.core.area.post.growth) * NON.FLAMMABILITY )
        flammable.area <- max(0, stand.area - non.flammable.area)
      }
      
      
      # K.R. Feb 2009: I have turned off two steps previously in the model for fire: (1) The test of: if ( 
      # flammable.area > 0), then continue with the fire part of the model ? because this was an unnecessary extra 
      # step (the influence of zero flammable area is now dealt with in the fire.function); (2) I have also turned off 
      # the step that tests fire.prob against a runif(1) because the fire.func no longer generates a probability, but
      # a realized fire intensity instead (and this fire in the site model is a flaming front that has already 
      # arrived at the stand boundary; it is not a new ignition or spotting ahead of the fire. When we have a 
      # landscape model, it will be appropriate to bring back this test of fire.prob against runif(1) to 
      # determine whether an ignition actually takes off into a fire.
      
      flammable.prop <- flammable.area / stand.area
      
      realized.intensity <- fire.intensity.func( flammable.prop, scheduled.fires[YEAR] )
      realized.fires[YEAR] <- realized.intensity

      prop.burnt <- fire.patchiness.func( realized.intensity )
      realized.prop.burnt[YEAR] <- prop.burnt
      
      flammable.proportions[YEAR] <- flammable.prop
      
      if (session.settings$display.fire.data) {
        print( paste( "total.core.area.post.growth: ", total.core.area.post.growth ) ) 
        print( paste( "flammable.area: ",flammable.area ) )
        print( paste("flam prop:", flammable.prop ) )
        print( paste("realized intensity of: ",realized.intensity, " in year ",YEAR ) )
        flush.console()
      }
      
      # Test if we are modelling this as an early fire (pre-recruitment) or a late fire (post-recruitment)
      is.early.fire <- runif(1) < fire.early.prob
      
      for ( i in SafeIndex(to=nrow(Cohorts)) ) {
        if ( Cohorts[i, colN] <= 0 ) next
        
        # if this is a new cohort and the fire is 'early' then we skip to
        # simulate the cohort having arisen after the fire
        if (is.early.fire && Cohorts[i, colID] %in% new.cohort.ids) next
        
        pars <- Spp[[ Cohorts[i, colSpID] ]]
        h <- Cohorts[i, colHeight]
        former.h <- Cohorts[i, colFormerHt]
        
        if ( pars@canCoppice &&
             Cohorts[i, colCoppiceOn] == 1 &&
             h < COPPICE_BOOST_PROP * former.h )  # last condition just a paranoid double-check
          
          # this is safe as long as former.h is never NA when colCoppiceOn = 1, which should always be true???
        {
          h <- former.h
        }
        
        # Inverse logit expression to calculate probability of survival in fire
        #
        adj.pars <- pars@survival_fire_pars
        p.fire.surv <- ( 1 / ( 1 + exp(-( adj.pars[1] + adj.pars[2] * realized.intensity + adj.pars[3] * h ^ adj.pars[4] ) ) ) ^ adj.pars[5] )
        
        # The overall probability of survival takes into account fire patchiness.
        # It is given by the sum of the probability of escaping the fire plus the
        # probability of being burnt but surviving.
        #
        p.overall.fire.surv <- (1 - prop.burnt) + (prop.burnt * p.fire.surv)
        
        # We only subject non-protected cohorts to the fire.
        #
        if ( !Cohorts[i, colProtect] ) {
          Cohorts[i, colN] <- rbinom( 1, Cohorts[i, colN], p.overall.fire.surv )
        }
        
        # For reporting, adjust the annual survival rate to reflect the impact of any 
        # fire during this timestep. Note, this won't be exactly the same as the above
        # rbinom result.
        #
        Cohorts[i, colSurvP] <- Cohorts[i, colSurvP] * p.overall.fire.surv
        
        if ( Cohorts[i, colN] > 0 ) {          
          if ( pars@canCoppice ) {
            adj.pars <- pars@height_fire_pars
            h <- Cohorts[i, colHeight]

            # THIS IS THE FINAL GENERALIZED LOGISTIC FUNCTION MICHAEL FITTED IN JULY 2010:
            fire.height.adj <-  ( 1 / ( 1 + exp( -( adj.pars[1] + adj.pars[2] * realized.intensity + adj.pars[3] * h ) ) ) ^ adj.pars[4] )
            
            new.h <- max( (h * fire.height.adj), pars@height_yr1 )

            # We need to bring all initial coppice heights up to at least height.yr1 because otherwise the current 
            # resprout.propn pars give really tiny new.hts which do not grow! (like 1x10^-92)!!!
            # (note: this height change can affect protected cohorts)            
                        
            # We boost all surviving coppicing trees which are below the boost threshold of their former
            # (max) height, not just the currently coppiced ones (ie. colCoppiceOn and colCoppiceStage now
            # actually mean any trees of a coppicing species with fire height reductions).  
            #
            # Karen's note:
            # ???I SHOULD DO THE SAME FOR CUT-'N-COPPICED TREES - THINK ABOUT AND DO THIS LATER WHEN HAVE TIME??

            # this will tell us how many fires a cohort has survived through its lifetime
            Cohorts[i, colCoppiceStage] <- Cohorts[i, colCoppiceStage] + 1
            
            Cohorts[i, colCoppiceOn] <- 1
            
            # Karen's note:
            #  Cohorts[i, colFormerHt] <- max( Cohorts[i, colFormerHt], Cohorts[i, colHeight], na.rm=TRUE ) 
            # We thought we could simplify it to the above 'max' test, but we can't because the test has to be on 70% of
            # former ht, and then we'd have to store 70% of former ht, which gets messy for the "cut-'n-coppice" stuff we do 
            # earlier in the tm script, so I have stayed with the current 'if else' test as follows:
            
            # When a cohort is fire affected again before reaching the boosting threshold of the original 
            # former height, former height remains as the original former height, otherwise it is changed 
            # to the immediate previous height:
            if ( Cohorts[i, colCoppiceStage] > 1 && 
                 Cohorts[i, colHeight] < COPPICE_BOOST_PROP * Cohorts[i, colFormerHt] ) {
              Cohorts[i, colFormerHt] <- Cohorts[i, colFormerHt]
            } else {
              Cohorts[i, colFormerHt] <- Cohorts[i, colHeight]
            }
            
            # Only now this is all done, can we update the cohorts table with the post-fire adjusted height:
            Cohorts[i, colHeight] <- new.h
            
          } # close canCoppice loop
        } # close NON-ZERO COHORT loop
      } # close COHORT loop
    } # close FIRE loop 
    
    
    #==============================================================
    # Record end data for any cohorts that have been lost, then
    # remove then from the Cohorts array
    #==============================================================
    m.rows <- which(Cohorts[ , colN ] <= 0)
    if (length(m.rows) > 0) {
      Cohorts <- Cohorts[ -m.rows, , drop=FALSE ]
    }
    
    #==============================================================
    # Report and increment cohort ages
    #==============================================================
    
    if (nrow(Cohorts) > 0) {
      WriteCohortData(DBCON)
      Cohorts[, colAge] <- Cohorts[, colAge] + 1
    }
    
    common.resource.use[YEAR] <- total.resource.use
    common.core.area[YEAR] <- total.core.area[NUM.SPP+1]  # CoreAreaGeneral
    common.merged.area[YEAR] <- total.merged.area
    
    # Check for loss of all cohorts and break out early if we are not using
    # external seed input
    if (nrow(Cohorts) == 0 && !EXTERNAL.SEED) {
      if (YEAR < SIMULATION.PERIOD) {
        print(paste("All cohorts extinct at year", YEAR))
        break
      }
    }
    
  }  # END OF yearly loop
  
  WriteSummaryData(DBCON)
  
  # clean up and return the connection object for the output database
  gc()
  DBCON
}
