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
#'
#' @param Spp either a single object or a \code{list} of objects
#'   of class \code{\linkS4class{SpeciesParams}} defining one or more species
#' 
#' @param initial.cohorts numeric matrix with 4 columns: 
#'   species index, age, height, number of trees.
#'   For simulations with a single species the species index should be 1, while
#'   for multiple species the index value is the position in the list
#'   passed to the \code{Spp} argument.
#'   
#' @param rain numeric vector of annual rainfall values. The length of this
#'   vector determines the number of years in the simulation.
#'   
#' @param scheduled.fires numeric vector of fire intensity values. The scale is 
#'   user-defined. If provided, the \code{fire.func} argument must also be
#'   present.
#'   
#' @param fire.func a function taking two numeric arguments (flammable stand 
#'   proportion and incoming fire intensity) and returning a single numeric
#'   value for realized fire intensity
#'   
#' @param fire.canopy.func a function taking a single numeric argument for
#'   summed core canopy area, and returning a (possibly) transformed area value.
#'   If not provided, untransformed canopy area will be used.
#'   
#' @param the probability that fire will occur before recruitment in any given
#'   year. The default value (0) specifies that fires always occur later than
#'   recruitment.
#'   
#' @param overlap.matrix WRITE ME !
#' 
#' @param ov.gen WRITE ME !
#' 
#' @param recruitment WRITE ME !
#' 
#' @param thinning WRITE ME !
#' 
#' @param special WRITE ME !
#' 
#' @param seedling.survival WRITE ME !
#' 
#' @param session.settings WRITE ME !
#' 
#' @param database an optional existing SQLite database connection to write results
#'   and metadata to. If not provided, a new database will be created.
#'   
#' @return a connection to the output SQLite database. If the \code{database}
#'   argument was NULL (default) then this will point to a temporary file which
#'   can be saved using the \code{\link{tmdbSave}} function. If an open 
#'   connection to an existing database was provided via the \code{database}
#'   argument, then this same connection will be returned.
#' 
#' @export
#' 
#' @importFrom dplyr group_by summarise %>%
#' @importFrom stringr str_trim
#' 
tmRun <- function (Spp, 
                   initial.cohorts, 
                   rain, 
                   scheduled.fires=NULL, 
                   fire.func=NULL, 
                   fire.canopy.func=NULL, 
                   fire.early.prob=0,
                   overlap.matrix=0, 
                   ov.gen=0, 
                   recruitment=TRUE, 
                   thinning=NULL, 
                   special=NULL, 
                   seedling.survival=NULL, 
                   session.settings=NULL, 
                   database=NULL )
{
  
  requireNamespace("RSQLite", quietly = FALSE)

  # Programming notes
  # 
  # The following hacks are used to speed up simulations:
  #
  # 1. Matrices are used in preference to data.frames for internal tables.
  #
  # 2. Vectors are used in preference to lists.
  #
  # 3. String comparisons are kept out of the yearly loop because they are so
  #    so slow.
  #
  # 4. Output data that are small enough to be cached are only written to the 
  #    SQLite database at the end of the run.
  #
  # 5. '{' is used instead of '(' in numeric expressions with multiple parentheses.
  #    For some reason R processes '{' nesting slightly faster than '('.
  #
  # 6. We take any opportunities to replace for-loops with vector processing.
  #
  # 7. Long SQL query text is created once then re-used.
  #
  # 8. For yearly cohort data, dbBegin and dbCommit are used resulting
  #    in slightly faster output than with SQLite's default auto-commit mode.

  
  # ====================================================================================
  #  Get the names of objects passed as args. Any arguments that are expressions are
  #  evaluated at this stage.
  # ====================================================================================
  
  ARG.INFO <- data.frame(
    arg.name = c("initial.cohorts", "rain", "scheduled.fires", "fire.func", "fire.canopy.func", "fire.early.prob",
                 "thinning", "special", "seedling.survival", "overlap.matrix"),
    is.expr = FALSE,
    obj.name = "",
    row.names = c("init", "rain", "fire", "fireFunc", "fireCanopyFunc", "fireEarlyProb", "thinning", "special", "seedSurv", "overlap"),
    stringsAsFactors = FALSE)
  
  for (i in 1:nrow(ARG.INFO)) {
    obj <- get(ARG.INFO$arg.name[i])
    
    if (is.null(obj)) {
      ARG.INFO$obj.name[i] <- "NULL"
      
    } else if (is.expression(obj)) {
      ARG.INFO$obj.name[i] <- as.character(obj)
      ARG.INFO$is.expr[i] <- TRUE
      
      # Evaluate the expression, with the results being assigned to an object
      # of name given by arg.name
      e <- parse( text=paste( ARG.INFO$arg.name[i], "<-", as.character(obj) ) )
      eval(e)
      
    } else {
      e <- parse( text=paste( "deparse(substitute(", ARG.INFO$arg.name[i], "))") )
      ARG.INFO$obj.name[i] <- eval(e)
    }
  }
  
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
  
  # This will be initialized in the WriteMetadata function but we define it here
  # so that it doesn't end up being <<-'d into the global environment
  RUNID <- NA
  
  CELL.AREA <- 10000
  
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
  
  if (is.null(fire.func)) {
    fire.func <- function(flam.prop, intensity) { intensity }
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
  max.cohorts <- nrow(initial.cohorts) + length(rain)*NUM.SPP
  CohortSummary <- matrix(0, nrow=max.cohorts, ncol=16)
  
  cscolRun <- 1; cscolID <- 2; cscolSpID <- 3; cscolStartTime <- 4; cscolStartAge <- 5;
  cscolStartN <- 6; cscolStartHt <- 7; cscolStartDBH <- 8; cscolStartBA <- 9;
  cscolFinalTime <- 10; cscolFinalAge <- 11; cscolFinalN <- 12; cscolFinalHt <- 13;
  cscolFinalDBH <- 14; cscolFinalBA <- 15; cscolCoppiceStage <- 16
  
  TOTAL.COHORTS <- 0 # will be updated by NewCohortSummaryRecord
  
  
  # ====================================================================================
  # SQL INSERT statement for writing cohort data
  # ====================================================================================
  
  SQL.WriteCohortData <-
    paste("INSERT INTO cohortyearly (",
          "RunID, Time, CohortID, SpeciesID, Age, Height, DBH, BasalArea, N, ResourceUse, CanopyRadius, CoppiceStage, FormerHeight, CoppiceOn,",
          "GrowthAdj, SurvivalAdj, CoreAreaGeneral )",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
  
  
  # ====================================================================================
  #  Helper function - Set session settings
  # ====================================================================================  
  SetSession <- function() {
    if (is.null(session.settings)) {
      session.settings <<- list()
    }
    
    # the 'test.run' option disables all metadata storage if TRUE
    if (is.null(session.settings$test.run)) {
      session.settings$test.run <<- FALSE
    }
    
    # the 'display.time.interval' option is a numeric value that sets
    # the time between progress reporint on screen
    if (is.null(session.settings$display.time.interval)) {
      session.settings$display.time.interval <<- 10
    }
    
    # the 'display.fire.data' option enables printing fire data to
    # the screen
    if (is.null(session.settings$display.fire.data)) {
      session.settings$display.fire.data <<- FALSE
    }
  }
  
  # ====================================================================================
  #  Helper function - Connect to the output database if one was provided or open a
  #                    new database
  # ====================================================================================  
  ConnectToDatabase <- function() {
    if (!is.null(database)) {
      con <- database
      
    } else { 
      con <- CreateNewDatabase()
    }
    
    # Store parameter metadata. This also sets the global RUNID variable
    WriteMetadata(con)
    
    # Return connection
    con
  }
  
  # ====================================================================================
  #  Helper function - Create a new SQLite database for the simulation output.
  #  Note: some of the table definitions include foreign keys but RSQLite doesn't
  #  seem to enforce them (perhaps have to recompile the package ?).
  # ====================================================================================  
  CreateNewDatabase <- function() {
    con <- RSQLite::dbConnect( RSQLite::SQLite(), tempfile() )
    
    # Create cohort annual data table
    RSQLite::dbGetQuery(con,
               paste("CREATE TABLE cohortyearly (",
                     "RunID INTEGER REFERENCES runs(ID),",
                     "Time INTEGER,",
                     "CohortID INTEGER,",
                     "SpeciesID INTEGER,",
                     "Age INTEGER,",
                     "Height REAL,",
                     "N INTEGER,",
                     "ResourceUse REAL,",
                     "GrowthAdj REAL,",
                     "SurvivalAdj REAL,",
                     "CanopyRadius REAL,",
                     "CoreAreaGeneral REAL,",
                     "DBH REAL,",
                     "BasalArea REAL,",
                     "CoppiceStage INTEGER,",
                     "FormerHeight REAL,",
                     "CoppiceOn INTEGER,", 
                     "PRIMARY KEY (RunID, Time, CohortID) )"))
    
    # Create cohort summary table
    RSQLite::dbGetQuery(con,
               paste("CREATE TABLE cohortsummary (",
                     "RunID INTEGER REFERENCES runs(ID),",
                     "CohortID INTEGER,",
                     "SpeciesID INTEGER,",
                     "StartTime INTEGER,",
                     "StartAge INTEGER,",
                     "StartN INTEGER,",
                     "StartHeight REAL,",
                     "StartDBH REAL,",
                     "StartBasalArea REAL,",
                     "FinalTime INTEGER,",
                     "FinalAge INTEGER,",
                     "FinalN INTEGER,",
                     "FinalHeight REAL,",
                     "FinalDBH REAL,",
                     "FinalBasalArea,",
                     "CoppiceStage INTEGER,", 
                     "PRIMARY KEY (RunID, CohortID) )"))
    
    # Create the common data table
    RSQLite::dbGetQuery(con,
               paste("CREATE TABLE commondata (",
                     "RunID INTEGER REFERENCES runs(ID),",
                     "Time INTEGER,",
                     "ResourceUse REAL,", 
                     "CoreAreaGeneral REAL,",
                     "MergedArea REAL,",
                     "Rain REAL,", 
                     "ScheduledFires REAL,", 
                     "RealisedFires REAL,", 
                     "FlammableProp REAL,", 
                     "SeedlingSurvival REAL,",
                     "PRIMARY KEY (RunID, Time) )"))
    
    # Create the run description table
    RSQLite::dbGetQuery(con,
               paste("CREATE TABLE runs (",
                     "ID INTEGER PRIMARY KEY,",
                     "ParamSetID INTEGER REFERENCES paramsets(ID),",
                     "Replicate INTEGER,",
                     "UNIQUE(ParamSetID, Replicate) )"))
    
    # Create the param sets table
    RSQLite::dbGetQuery(con, 
               paste("CREATE TABLE paramsets (",
                     "ID INTEGER PRIMARY KEY,",
                     "SpeciesSetID INTEGER REFERENCES species(ID),",
                     "InitialCohorts TEXT,",
                     "Rain TEXT,",
                     "Fire TEXT,",
                     "FireFunc TEXT,",
                     "FireCanopyFunc TEXT,",
                     "FireEarlyProb TEXT,",
                     "Thinning TEXT,",
                     "Special TEXT,",
                     "SeedSurv TEXT,",
                     "OverlapMatrix TEXT )"))
    
    # Create the species table that lists species IDs and names
    RSQLite::dbGetQuery(con,
               paste("CREATE TABLE species (",
                     "ID INTEGER,",
                     "SpeciesID INTEGER,",
                     "Name TEXT,",
                     "PRIMARY KEY (ID, SpeciesID))"))
    
    # Create the paramobjects table that stores a full copy of the parameters.
    # This has a one-to-one relationship with records in the paramsets table and
    # the only reason we use a separate table is to avoid getting loads of binary
    # crap returned when we do queries with "select * from paramsets".
    RSQLite::dbGetQuery(con,
               paste("CREATE TABLE paramobjects (",
                     "ID INTEGER PRIMARY KEY REFERENCES paramsets(ID),",
                     "Data BLOB )"))
    
    # Return the connection
    con
  }
  
  # ====================================================================================
  #  Helper function - Store run metadata. This is called after the database has been
  #  opened or created and the RUNID value has been set.
  # ====================================================================================
  WriteMetadata <- function(dbcon) {
    
    # look for the species combination in the species table
    df <- RSQLite::dbGetQuery(
      dbcon, "SELECT ID, SpeciesID, Name FROM species order by ID, SpeciesID")
    
    matches <- df %>%
      group_by(ID) %>%
      summarise(found = all(Name == SP.NAMES))
    
    if (!any(matches$found)) {
      # record this new species combination
      spSetID <- 1
      df <- RSQLite::dbGetQuery(dbcon, "SELECT MAX(ID) FROM species")
      if (!is.na(df[1,1])) {
        spSetID <- df[1,1] + 1
      }
      
      for (spid in 1:NUM.SPP) {
        sql <- paste("INSERT INTO species (ID, SpeciesID, Name) VALUES (",
                     paste(spSetID, spid, WrapText(SP.NAMES[spid]), sep=","), ")", sep="")
        RSQLite::dbGetQuery(dbcon, sql)
      }
    } else { 
      # integrity check
      if (sum(matches$found) != 1) {
        stop("More than one set in the species table matches. This should never happen")
      }
      
      # get the id of the existing combination 
      spSetID <- matches$ID[ matches$found ]
    }
    
    # search for this parameter combination in the paramsets table
    sql <- paste("SELECT ID FROM paramsets WHERE SpeciesSetID = ", spSetID,
                 " AND InitialCohorts = ", WrapText(ARG.INFO["init", "obj.name"]),
                 " AND Rain = ", WrapText(ARG.INFO["rain", "obj.name"]),
                 " AND Fire = ", WrapText(ARG.INFO["fire", "obj.name"]),
                 " AND FireFunc = ", WrapText(ARG.INFO["fireFunc", "obj.name"]),
                 " AND FireCanopyFunc = ", WrapText(ARG.INFO["fireCanopyFunc", "obj.name"]),
                 " AND FireEarlyProb = ", WrapText(ARG.INFO["fireEarlyProb", "obj.name"]),
                 " AND Thinning = ", WrapText(ARG.INFO["thinning", "obj.name"]),
                 " AND Special = ", WrapText(ARG.INFO["special", "obj.name"]),
                 " AND SeedSurv = ", WrapText(ARG.INFO["seedSurv", "obj.name"]), 
                 " AND OverlapMatrix = ", WrapText(ARG.INFO["overlap", "obj.name"]), sep = "")
    
    df <- RSQLite::dbGetQuery(dbcon, sql)
    
    if (nrow(df) == 0) {
      # new combination: assign a param set ID, store the names and store a 
      # snapshot of the param objects
      paramSetID <- 1
      df <- RSQLite::dbGetQuery(dbcon, "SELECT MAX(ID) FROM paramsets")
      if (!is.na(df[1,1])) {
        paramSetID <- df[1,1] + 1
      }
      
      # (note the use of as.list(ARG.INFO$obj.name) to ensure that we get
      # arg object names in separate columns)
      RSQLite::dbGetPreparedQuery(dbcon, 
                         paste("INSERT INTO paramsets",
                               "(ID, SpeciesSetID, InitialCohorts, Rain, Fire, FireFunc, FireCanopyFunc, FireEarlyProb,",
                               "Thinning, Special, SeedSurv, OverlapMatrix)",
                               "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"),
                         data.frame(paramSetID, spSetID, as.list(ARG.INFO$obj.name)))
      
      StoreParamSnapshot(dbcon, paramSetID)
      
    } else {
      # existing combination
      paramSetID <- df[1,1]
    }
    
    # Record the run information in the runs table
    df <- RSQLite::dbGetQuery(dbcon,
                     paste("SELECT MAX(Replicate) FROM runs WHERE ParamSetID =", paramSetID))
    
    repNum <- 1
    if (!is.na(df[1,1])) {
      repNum <- df[1,1] + 1
    }
    
    RSQLite::dbGetQuery(dbcon,
               paste("INSERT INTO runs (ParamSetID, Replicate) VALUES (", paramSetID, ",", repNum, ")"))
    
    # Finally retrieve the global RUNID value
    df <- RSQLite::dbGetQuery(dbcon,
                     paste("SELECT ID FROM runs WHERE ParamSetID =", paramSetID, "AND Replicate =", repNum))
    
    if (nrow(df) != 1 || is.na(df[1,1])) {
      stop("Error writing to the runs table")
    }
    
    RUNID <<- df[1,1]
  }
  
  
  # ====================================================================================
  #  Helper function - Store a snapshot of the param objects in binary form in the
  #  paramobjects database table
  # ====================================================================================
  StoreParamSnapshot <- function(dbcon, paramSetID) {
    params <- list(SpeciesParams=Spp, initial.cohorts=initial.cohorts, rain=rain, fire=scheduled.fires, 
                   fire.func=fire.func, thinning=thinning, special=special, seedling.survival=seedling.survival, 
                   overlap.matrix=overlap.matrix, ov.gen=ov.gen, recruitment=recruitment) 
    
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
                         realised.fires[1:YEAR], 
                         flammable.proportions[1:YEAR], 
                         seedling.survival[1:YEAR])
    
    RSQLite::dbWriteTable(dbcon, "commondata", as.data.frame(common.data), row.names=FALSE, append=TRUE)
    
    RSQLite::dbWriteTable(dbcon, "cohortsummary", as.data.frame(CohortSummary[1:TOTAL.COHORTS, ]), row.names=FALSE, append=TRUE)
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
  
  # A few sections of the model give special treatment to eucalypts (e.g. coppicing)
  is.euc <- logical(NUM.SPP)
  for (i in 1:NUM.SPP) {
    is.euc[i] <- stringEqualsIgnoreCase(SP.NAMES[i], "Eucalyptus")
  } 
  
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
  
  max.num.cohorts <- num.initial.cohorts + length(rain)*NUM.SPP
  
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
  common.resource.use <- numeric( length(rain) )
  common.core.area <- numeric( length(rain) )  # just stores CoreAreaGeneral values
  common.merged.area <- numeric( length(rain) )
  
  total.core.area <- numeric( NUM.SPP + 1 )
  names.combined <- paste("CombCoreArea", SP.NAMES, sep="")
  names(total.core.area) <- c( names.combined, "CombCoreAreaGeneral")
  
  realised.fires <- numeric( length(rain) )
  flammable.proportions <- numeric( length(rain) )
  
  # Connect to the output database. This will be either a user-supplied database or a 
  # newly created one.
  DBCON <- ConnectToDatabase()
  
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
    scheduled.fires <- numeric( length(rain) )
    cat ( "No fires in this simulation \n" )
  }
  else
  {
    # if a fire vector was provided then a fire function must be too
    if ( is.null( fire.func ) ) 
      stop( "scheduled.fires argument was provided but fire.func is missing" )    
    
    if ( length(scheduled.fires) != length(rain) ) 
      scheduled.fires <- c(scheduled.fires, rep(0, length(rain) - length(scheduled.fires)))
  }
  
  
  # if seedling.survival is missing, make it 1 for all years
  #
  if ( is.null( seedling.survival ) )
  {
    seedling.survival <- rep(1, length(rain) )
  }
  else if ( length(seedling.survival) < length(rain) )
  {
    seedling.survival <- c(seedling.survival, rep(1, length(rain) - length(seedling.survival)))
  }
  
  # if recruitment is a single value convert it to a yearly vector
  if ( length(recruitment) == 1 ) 
  {
    recruitment <- rep( recruitment, length(rain) )
  }
  # if recruitment is a vector but too short, pad it out with the last value
  else if ( length(recruitment) < length(rain) )
  {
    len <- length(recruitment)
    recruitment <- c( recruitment, rep(tail(recruitment, 1), length(rain) - length(recruitment)) )
  }
  
  # check the thinning arg, if provided, and create a flag array to identify thinning years
  is.thinning.year <- logical( length(rain) )
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
    years <- years[ years <= length(rain) ]
    
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
  is.special.year <- logical( length(rain) )
  
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
      
      if ( special[i, specialColYear] < 1 | special[i, specialColYear] > length(rain) )
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
    if ( YEAR %% session.settings$display.time.interval == 0 | YEAR == length(rain) ) {
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
          else if ( i.action == special.action.cut )
          {
            cut.h <- min( Cohorts[irow, colHeight], special[i, specialColValue] )
            pars <- Spp[[ Cohorts[irow, colSpID] ]]
            if ( is.euc[Cohorts[irow, colSpID]] & cut.h <= pars@height_yr1 )
            {
              Cohorts[irow, colCoppiceStage] <- Cohorts[irow, colCoppiceStage] + 1
              Cohorts[irow, colCoppiceOn] <- 1
              
              # when a cohort coppices again before reaching the original former height, former height stays                               
              # as the original former height, otherwise it is changed to the immediate previous height:
              if (Cohorts[irow, colCoppiceStage] > 1 & Cohorts[irow, colHeight] < Cohorts[irow, colFormerHt] ) 
              {
                Cohorts[irow, colFormerHt] <- Cohorts[irow, colFormerHt]
              } else {
                Cohorts[irow, colFormerHt] <- Cohorts[irow, colHeight]
              }
            }
            Cohorts[irow, colHeight] <- cut.h
            print( paste( "cut cohort ", cohort.id, "in year ", YEAR, "to height", cut.h ) )
          }          
          else if ( i.action == special.action.protect )
          {
            Cohorts[irow, colProtect] <- (special[i, specialColValue] != 0)
          } 
        }
        else
        {
          print( paste( "cohort", special[i, specialColCohortID], "died before special action in year", YEAR ) )
          flush.console()
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
      
      # Now, if you are a euc with coppicing turned on and you're still <70% of former(max) ht, your growth rate is 
      # boosted, whereas if you have reached or exceeded 70% of former height your growth rate is normal and
      # coppicing is turned/stays off (colCoppiceOn=0); and if you're not a euc, your growth rate is normal:
      
      if ( is.euc[Cohorts[i, colSpID]] & Cohorts[i, colCoppiceOn] == 1 )  
        # this is safe as long as former.h is never NA when colCoppiceOn = 1, which should always be true???
      {
        if ( h < 0.7 * former.h ) 
          # to introduce a slackening off of the boosting effect due to root area equilibrating with shoot area 
          #which we estimated might happen at 70% of former ht (see coppice spreadsheet)
        { 
          boost.pars <- pars@coppice_boost_pars
          adjusted.boosted.gr <- max( pars@growth_rate, boost.pars[1]/{1 + boost.pars[2] * exp(-boost.pars[3] * former.h)} )
          # this equation and the coefficients fitted to it (in coppice.boost.pars) were from a regression of
          # coppice growth data from Mt Pilot, and directly relate a boosted growth rate to a tree's former height.
          Cohorts[i, colGrowthRate] <- adjusted.boosted.gr * g.rain.adj * g.crowd.adj
        } else if ( h >= 0.7 * former.h ) {
          Cohorts[i, colGrowthRate] <- pars@growth_rate * g.rain.adj * g.crowd.adj
          Cohorts[i, colCoppiceOn] <- 0
        }
      } else {
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
      
      if ( is.euc[Cohorts[i, colSpID]] & Cohorts[i, colCoppiceOn] == 1  &  h < 0.7 * former.h )  
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
        gap <- CELL.AREA - canopy.area
        
        if ( gap > 0 ) 
        {
          num.sprogs <- rbinom( 1, num.sprogs, gap / CELL.AREA )
          
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
      # first we calculate a realised fire intensity using the relationships contained in the fire.function
      # (which has as input scheduled intensity and the flammable proportion of the cell). Flammable proportion
      # of the cell is calculated as the difference between the cell area and the contribution of each cohort to
      # what we think of as non-flammable area. (Note that we are re-calculating core area for each cohort here 
      # but we don't use these new values for reporting)
      #
      # The current fire.func looks like this:
      #
      # function ( flammable.prop, scheduled.fire.intensity )  **CHANGE REGNS AND THIS DESCRIPTION WHEN FINALISED**
      # {
      # A function that returns the realised.fire.intensity in a particular year. The function was derived on the basis 
      # of actual data (Janet Cohn's data for incoming and outgoing intensity (represented by eucalypt char heights) and 
      # Basal Areas, and my calculations of flammable proportions in her quadrats), and on a combination of models fitted 
      # to this data. This calculation of flammable prop was done in the same way as the model calculates it (ie. takes 
      # account of when the total.core.area exceeds the cell area).
      #
      # if ( scheduled.fire.intensity > Threshold ) 
      #  { return( scheduled.fire.intensity  ) } # realised.fire.int above threshold is same as scheduled
      # else if (scheduled.fire.intensity <= Threshold ) 
      #  { return( scheduled.fire.intensity + b1 * (1 - flammable.prop ) ) } # dampen realised.fire.int below threshold
      # }
      # and we are using Threshols = 4.5 and b1 = -1.4 
      
      flammable.area <- CELL.AREA
      if (nrow(Cohorts) > 0) {
        total.core.area.post.growth <-
          tapply( apply(Cohorts, 1, function(co) co[colN] * co[colCanopyRadius] * co[colCanopyRadius] * pi ), Cohorts[ , colSpID], sum )
        
        # From this point it gets fudgy...
        # We apply the canopy merging function to get total merged canopy area.
        # We then estimate a non-flammable merged area based on the proportion of the non-merged area
        # occupied by each species and their respective non-flammabilities
        
        merged.area <- fire.canopy.func( sum(total.core.area.post.growth) )
        
        non.flammable.area <- sum( merged.area * total.core.area.post.growth / sum(total.core.area.post.growth) * NON.FLAMMABILITY )
        flammable.area <- max(0, CELL.AREA - non.flammable.area)
      }
      
      
      # K.R. Feb 2009: I have turned off two steps previously in the model for fire: (1) The test of: if ( 
      # flammable.area > 0), then continue with the fire part of the model ? because this was an unnecessary extra 
      # step (the influence of zero flammable area is now dealt with in the fire.function); (2) I have also turned off 
      # the step that tests fire.prob against a runif(1) because the fire.func no longer generates a probability, but
      # a realised fire intensity instead (and this fire in the site model is a flaming front that has already 
      # arrived at the stand boundary; it is not a new ignition or spotting ahead of the fire. When we have a 
      # landscape model, it will be appropriate to bring back this test of fire.prob against runif(1) to 
      # determine whether an ignition actually takes off into a fire.
      
      flammable.prop <- flammable.area / CELL.AREA
      realised.fire.intensity <- fire.func( flammable.prop, scheduled.fires[YEAR] )
      realised.fires[YEAR] <- realised.fire.intensity
      flammable.proportions[YEAR] <- flammable.prop
      
      if (session.settings$display.fire.data) {
        print( paste( "total.core.area.post.growth: ", total.core.area.post.growth ) ) 
        print( paste( "flammable.area: ",flammable.area ) )
        print( paste("flam prop:", flammable.prop ) )
        print( paste("realised.fire.intensity of: ",realised.fires[YEAR], " in year ",YEAR ) )
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
        
        if ( is.euc[Cohorts[i, colSpID]] &
               Cohorts[i, colCoppiceOn] == 1  &  
               h < 0.7 * former.h )  #  this last condition is not necessary but could be a good double-check.
          
          # this is safe as long as former.h is never NA when colCoppiceOn = 1, which should always be true???
        {
          h <- former.h
        }
        
        # inverse logit expression to calculate probability of survival in fire
        adj.pars <- pars@survival_fire_pars
        # NB currently we're using different forms for Euc and Callitris...this might change when we reassess Euc fire survival:
        if (is.euc[Cohorts[i, colSpID]]) {
          fire.surv <- 1 / (1 + exp( -( adj.pars[1] + adj.pars[2] * realised.fire.intensity * h^adj.pars[3] ) ) ) 
        } else {
          # For Callitris we use a generalized logistic with ht raised to a power
          fire.surv <- ( 1 / ( 1 + exp(-( adj.pars[1] + adj.pars[2] * realised.fire.intensity + adj.pars[3] * h ^ adj.pars[4] ) ) ) ^ adj.pars[5] )
        }  
        
        #print( paste(pars$name, "fire survival", fire.surv) ); flush.console()
        
        # only apply the survival prob to non-protected cohorts
        if ( !Cohorts[i, colProtect] ) Cohorts[i, colN] <- rbinom( 1, Cohorts[i, colN], fire.surv )
        
        # adjust the annual survival to reflect the impact of any fire during the timestep
        Cohorts[i, colSurvP] <- Cohorts[i, colSurvP] * fire.surv  # NB this won't be exactly the same as the rbinom result above.
        
        if ( Cohorts[i, colN] > 0 ) {
          # height adjustment for survivors; although this is not specific to eucs, at present callitris height_fire_pars are null (Inf, 0, 0, 0)
          adj.pars <- pars@height_fire_pars
          h <- Cohorts[i, colHeight]
          # fire.height.adj <- 1 / (1 + exp( -( adj.pars[1] + adj.pars[2] * realised.fire.intensity  * h^adj.pars[3] ) ) )
          # THIS IS THE FINAL GENERALIZED LOGISTIC FUNCTION MICHAEL FITTED IN JULY 2010:
          fire.height.adj <-  ( 1 / ( 1 + exp( -( adj.pars[1] + adj.pars[2] * realised.fire.intensity + adj.pars[3] * h ) ) ) ^ adj.pars[4] )
          
          #print( paste(pars$name, "fire height adj", fire.height.adj) ); flush.console()
          
          new.h <- max( (h * fire.height.adj), pars@height_yr1 )
          # We need to bring all initial coppice heights up to at least height.yr1 because otherwise the current 
          # resprout.propn pars give really tiny new.hts which do not grow! (like 1x10^-92)!!!
          # (note: this height change can affect protected cohorts)
          
          if (is.euc[Cohorts[i, colSpID]]) {
            # We boost all surviving Eucs which are <70% of their former (max) ht, not just truly
            # coppiced ones: so throughout tmRun, colCoppiceOn and colCoppiceStage now actually
            # mean any eucs with fire height reductions.  
            # ???I SHOULD DO THE SAME FOR CUT-'N-COPPICED TREES - THINK ABOUT AND DO THIS LATER WHEN HAVE TIME??
            Cohorts[i, colCoppiceStage] <- Cohorts[i, colCoppiceStage] + 1  # this will tell us how many fires a cohort has survived through its 'lifetime'.
            Cohorts[i, colCoppiceOn] <- 1
            
            #  Cohorts[i, colFormerHt] <- max( Cohorts[i, colFormerHt], Cohorts[i, colHeight], na.rm=TRUE ) 
            # We thought we could simplify it to the above 'max' test, but we can't because the test has to be on 70% of
            # former ht, and then we'd have to store 70% of former ht, which gets messy for the "cut-'n-coppice" stuff we do 
            # earlier in the tm script, so I have stayed with the current 'if else' test as follows:
            
            # When a cohort is fire affected again before reaching 70% of the original former height, former height stays 
            # as the original former height, otherwise it is changed to the immediate previous height:
            if (Cohorts[i, colCoppiceStage] > 1 & Cohorts[i, colHeight] < 0.7 * Cohorts[i, colFormerHt] ) {
              Cohorts[i, colFormerHt] <- Cohorts[i, colFormerHt]
            } else {
              Cohorts[i, colFormerHt] <- Cohorts[i, colHeight]
            }
            
            # Only now this is all done, can we update the cohorts table with the post-fire adjusted height:
            Cohorts[i, colHeight] <- new.h
            
          }    # close EUC loop
        }   # close NON-ZERO COHORT loop
      }  # close COHORT loop
    }     # close FIRE loop 
    
    
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
      if (YEAR < length(rain)) {
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
