tm.site.sqlite <- function (SpeciesParams, 
                            initial.cohorts, 
                            rain, 
                            scheduled.fires = NULL, 
                            fire.func = function(flam.prop, fire) fire,
                            overlap.matrix = 0, 
                            ov.gen = 0, 
                            recruitment = TRUE, 
                            thinning = NULL, 
                            special = NULL, 
                            seedling.survival = NULL, 
                            session.settings = NULL, 
                            database = NULL ) 
{
  #tm.site
  
  # eg tm.site(list(callitris.params, euc.params), test.cohorts, rainvec, recruitment=TRUE)
  #
  # The simulation period is equal to the length of the rain vector.
  #
  # ORDER OF EVENTS:
  # (1) Preparatory stuff
  #     yearly loop starts ->
  # (2) Special Actions (coppice ON)
  # (3) Thinning Treatments
  # (4) Prepare Tree Variables
  # (5) Growth (coppice OFF)
  # (6) Survival
  # (7) Recruitment
  # (8) Fire (adjust survival; adjust height; coppice ON)
  # (9) Report
  #     -> yearly loop ends
  # (10) Assemble results
  #
  # The argument scheduled.fires is a vector of scheduled fire intensities with 0 meaning no fire and 
  # length=length(rain). If the scheduled.fires arg is not provided this is interpreted as no fires.
  # The argument fire.func is a function that calculates realised fire intensity from scheduled fire intensity,
  # and flammable proportion (ie. the function you provide must look like f( prop, scheduled intens) and
  # realised intensity is returned. 
  #
  # The argument recruitment is either a boolean value indicating whether any recruitment
  # will be modelled, or a vector of boolean values indicating possibility of recruitment
  # for each year of the simulation.
  #
  # The thinning argument is a matrix with the following cols:
  #   year
  #   species.id
  #   min.height
  #   max.height
  #   num.trees
  #
  # Each row specifies that, for a given species (or all if NA) in a given year, the number of trees 
  # with height: min.height < h <= max.height is to be reduced to the value of num.trees.
  # Where there are multiple cohorts in the height range, culling of trees is done evenly
  # across cohorts by, for each cohort: new.n <- round( n * num.trees / n.total )
  # 
  # The 'special' argument is a data.frame that prescribes treatments for given cohorts.  The
  # cohorts must be part of the initial stand specified in the Cohorts argument (see above).
  # The data.frame has the the following cols:
  #   cohort.id  - an integer corresponding to the row for this cohort in the Cohorts matrix
  #   year       - (integer) year when the treatment is to be applied
  #   action     - (character) one of "remove", "cut", "protect" (may be abbreviated)
  #   value      - (numeric) if action = "cut" this is the new height of the cohort;
  #                else if action = "protect" a non-zero value means protect cohort while
  #                a zero value means remove protection
  #
  #
  # A cohort may be listed more than once.  For example, this...
  #
  #   cohort.id  year  action  value
  #           3    10     cut    1.5 
  #           3    10     pro      1
  #           3   100     rem     NA
  #
  # ...means reduce trees in cohort 3 to 1.5m in height and ensure that they survive
  # until year 100, at which time they are removed.
  #
  # Actions specified in the 'special' matrix are carried out at the beginning of the simulation year. The Special 
  # actions arg was originally added to apply known actions to Lunt et al (2006) pre-settlement trees. Special 
  # actions may be useful in other applications as well. In general the special action "cut" is not appropriate for 
  # Callitris, as it results in the tree surviving in the first instance, but being treated as a seedling of the cut 
  # height, which is not really what Callitris does (it generally dies when cut), so it is best to do the "remove"  
  # action for Callitris. Also note that in a situation where a fire occurs in the same year as a special action cut, 
  # if a tree does not coppice when cut, but then does coppice when burnt, the former height recorded is the cut 
  # height (plus that year's growth) and not the actual former height in the previous year. This is reasonable given 
  # that if a tree is cut and then burnt in the same year, it is likely to have a lower survival and growth rate 
  # than you might have expected for a tree that coppiced post-fire without having been first cut. However, if the 
  # tree coppices twice in the same year (first when cut, then when burnt), the former height is the previous 
  # year's height). This slight discrepancy was not thought serious enough to fix, but note that it exists.
  #
  # The overlap.matrix is a grid of pair-wise overlap values used in calculations of core area for recruitment 
  # under asymmetric competition (where species exert different competitive influences on conspecific seedlings 
  # vs seedlings of other species). The matrix must have column and row names corresponding exactly to the 
  # species name parameter values in SpeciesParams. It must have all the species in SpeciesParams, but doesn't 
  # matter if it has additional species not in SpeciesParams, or if the order is different to SpeciesParams, as 
  # the script uses 'match' to look up the correct values. Also, the matrix must be arranged with the rows 
  # equalling the seedling response, and the columns equalling the overstorey (existing cohort) effects. For 
  # e.g. cell [1,2] has rowname "Callitris" and colname "Eucalyptus". It therefore contains the ov value used 
  # to calculate the effect of existing Eucalyptus cohorts on potential Callitris seedlings. 
  
  # The ov.gen parameter is a general overlap value (default = 0) used in 2 places: to calculate flammable 
  # proportion of the stand in fire years, and to report in the outputs a general core.area value for each 
  # cohort, plus a combined general core area. SHOULD OV = 0 ALWAYS FOR FLAMMABLE AREA ???
  #
  
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
  # 8. For yearly cohort data, dbBeginTransaction and dbCommit are used resulting
  #    in slightly faster output than with SQLite's default auto-commit mode.
  
  # ====================================================================================
  #  Dependencies
  # ====================================================================================
  
  require(RSQLite)
  
  # ====================================================================================
  #  Get the names of objects passed as args. Any arguments that are expressions are
  #  evaluated at this stage.
  # ====================================================================================
  
  ARG.INFO <- data.frame(
    arg.name = c("initial.cohorts", "rain", "scheduled.fires", "fire.func", "thinning", "special", "seedling.survival", "overlap.matrix"),
    is.expr = FALSE,
    obj.name = "",
    row.names = c("init", "rain", "fire", "fireFunc", "thinning", "special", "seedSurv", "overlap"),
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
  #  Examine the SpeciesParams arg to check if it is a 'naked' param object or a list
  #  of one or more objects
  # ====================================================================================
  
  nms <- names(SpeciesParams)
  if ( !is.null(nms) ) {
    # At the moment we just do a quick and dirty check by looking for
    # a sample of expected names
    if ( all(c("name", "survival.prob", "survival.rainfall.pars", "growth.rate", "height.yr1") %in% nms) ) {
      # we have a naked params object - wrap it in a list
      SpeciesParams <- list(SpeciesParams)
    }
  }
  
  
  # ====================================================================================
  #  Constants
  # ====================================================================================
  
  # This will be initialized in the WriteMetadata function but we define it here
  # so that it doesn't end up being <<-'d into the global environment
  RUNID <- NA
  
  NUM.SPP <- length(SpeciesParams)
  SP.NAMES <- character(NUM.SPP)
  for (i in 1:NUM.SPP) {
    SP.NAMES[i] <- SpeciesParams[[i]]$name
  }
  
  HAS.HT.DBH.PARS <- logical(NUM.SPP)
  for (i in 1:NUM.SPP) HAS.HT.DBH.PARS[i] <- !is.null(SpeciesParams[[i]]$ht.dbh.pars)
  
  HAS.EXTERNAL.SEED.FUNC <- logical(NUM.SPP)
  for (i in 1:NUM.SPP) HAS.EXTERNAL.SEED.FUNC[i] <- !is.null( SpeciesParams[[i]]$external.seed.func )
  
  # all species flag
  EXTERNAL.SEED <- any(HAS.EXTERNAL.SEED.FUNC)
  
  # Cohorts matrix cols (only cols 2 - 5 are in the matrix of initial cohorts
  # provided by the user)
  #
  colID <- 1; colSpID <- 2;  colAge <- 3;  colHeight <- 4;  colN <- 5;  colProtect <- 6; 
  colCoppiceStage <- 7; colFormerHt <- 8; colCoppiceOn <- 9; colResourceUse <- 10;
  colCanopyRadius <- 11; colGrowthRate <- 12; colSurvP <- 13; colDBH <- 14; colBasalArea <- 15;
  colFirstCoreArea <- 16;
  colGeneralCoreArea <- colFirstCoreArea + NUM.SPP
  
  NUM.COHORTS.COLS <- colGeneralCoreArea
  
  cell.area <- 10000
  
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
  # Reformatting of parameters for increased speed
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
      DBCON <<- database
      
    } else { 
      DBCON <<- CreateNewDatabase()
    }
    
    # Store parameter metadata. This also sets the global RUNID variable
    WriteMetadata()
  }
  
  # ====================================================================================
  #  Helper function - Create a new SQLite database for the simulation output.
  #  Note: some of the table definitions include foreign keys but RSQLite doesn't
  #  seem to enforce them (perhaps have to recompile the package ?).
  # ====================================================================================  
  CreateNewDatabase <- function() {
    con <- dbConnect( dbDriver("SQLite"), tempfile() )
    
    # Create cohort annual data table
    dbGetQuery(con,
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
    dbGetQuery(con,
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
    dbGetQuery(con,
               paste("CREATE TABLE commondata (",
                     "RunID INTEGER REFERENCES runs(ID),",
                     "Time INTEGER,",
                     "ResourceUse REAL,", 
                     paste(names(combined.core.area), "REAL,", collapse=" "), 
                     "Rain REAL,", 
                     "ScheduledFires REAL,", 
                     "RealisedFires REAL,", 
                     "FlammableProp REAL,", 
                     "SeedlingSurvival REAL,",
                     "PRIMARY KEY (RunID, Time) )"))
    
    # Create the run description table
    dbGetQuery(con,
               paste("CREATE TABLE runs (",
                     "ID INTEGER PRIMARY KEY,",
                     "ParamSetID INTEGER REFERENCES paramsets(ID),",
                     "Replicate INTEGER,",
                     "UNIQUE(ParamSetID, Replicate) )"))
    
    # Create the param sets table
    dbGetQuery(con, 
               paste("CREATE TABLE paramsets (",
                     "ID INTEGER PRIMARY KEY,",
                     "SpeciesSetID INTEGER REFERENCES species(ID),",
                     "InitialCohorts TEXT,",
                     "Rain TEXT,",
                     "Fire TEXT,",
                     "FireFunc,",
                     "Thinning,",
                     "Special,",
                     "SeedSurv,",
                     "OverlapMatrix )"))
    
    # Create the species table that lists species IDs and names
    dbGetQuery(con,
               paste("CREATE TABLE species (",
                     "ID INTEGER,",
                     "SpeciesID INTEGER,",
                     "Name TEXT,",
                     "PRIMARY KEY (ID, SpeciesID))"))
    
    # Create the paramobjects table that stores a full copy of the parameters.
    # This has a one-to-one relationship with records in the paramsets table and
    # the only reason we use a separate table is to avoid getting loads of binary
    # crap returned when we do queries with "select * from paramsets".
    dbGetQuery(con,
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
  WriteMetadata <- function() {
    # look for the species combination in the species table
    df <- dbGetQuery(DBCON, "SELECT ID, SpeciesID, Name FROM species order by ID, SpeciesID")
    if (nrow(df) == 0) {
      found <- FALSE
    } else {
      l <- unstack(df, Name ~ ID)
      found <- sapply(l, function(x) length(x) == NUM.SPP && all(x == SP.NAMES) )
    }
    
    if (!any(found)) {
      # record this new species combination
      spSetID <- 1
      df <- dbGetQuery(DBCON, "SELECT MAX(ID) FROM species")
      if (!is.na(df[1,1])) {
        spSetID <- df[1,1] + 1
      }
      
      for (spid in 1:NUM.SPP) {
        sql <- paste("INSERT INTO species (ID, SpeciesID, Name) VALUES (",
                     paste(spSetID, spid, WrapText(SP.NAMES[spid]), sep=","), ")", sep="")
        dbGetQuery(DBCON, sql)
      }
    } else { 
      # integrity check
      if (sum(found) != 1) {
        stop("More than one set in the species table matches. This should never happen")
      }
      
      # get the id of the existing combination 
      spSetID <- as.integer( names(l)[found] )
    }
    
    # search for this parameter combination in the paramsets table
    sql <- paste("SELECT ID FROM paramsets WHERE SpeciesSetID = ", spSetID,
                 " AND InitialCohorts = ", WrapText(ARG.INFO["init", "obj.name"]),
                 " AND Rain = ", WrapText(ARG.INFO["rain", "obj.name"]),
                 " AND Fire = ", WrapText(ARG.INFO["fire", "obj.name"]),
                 " AND FireFunc = ", WrapText(ARG.INFO["fireFunc", "obj.name"]),
                 " AND Thinning = ", WrapText(ARG.INFO["thinning", "obj.name"]),
                 " AND Special = ", WrapText(ARG.INFO["special", "obj.name"]),
                 " AND SeedSurv = ", WrapText(ARG.INFO["seedSurv", "obj.name"]), 
                 " AND OverlapMatrix = ", WrapText(ARG.INFO["overlap", "obj.name"]), sep = "")
    
    df <- dbGetQuery(DBCON, sql)
    
    if (nrow(df) == 0) {
      # new combination: assign a param set ID, store the names and store a 
      # snapshot of the param objects
      paramSetID <- 1
      df <- dbGetQuery(DBCON, "SELECT MAX(ID) FROM paramsets")
      if (!is.na(df[1,1])) {
        paramSetID <- df[1,1] + 1
      }
      
      # (note the use of as.list(ARG.INFO$obj.name) to ensure that we get
      # arg object names in separate columns)
      dbGetPreparedQuery(DBCON, 
                         paste("INSERT INTO paramsets",
                               "(ID, SpeciesSetID, InitialCohorts, Rain, Fire, FireFunc,",
                               "Thinning, Special, SeedSurv, OverlapMatrix)",
                               "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"),
                         data.frame(paramSetID, spSetID, as.list(ARG.INFO$obj.name)))
      
      StoreParamSnapshot(paramSetID)
      
    } else {
      # existing combination
      paramSetID <- df[1,1]
    }
    
    # Record the run information in the runs table
    df <- dbGetQuery(DBCON,
                     paste("SELECT MAX(Replicate) FROM runs WHERE ParamSetID =", paramSetID))
    
    repNum <- 1
    if (!is.na(df[1,1])) {
      repNum <- df[1,1] + 1
    }
    
    dbGetQuery(DBCON,
               paste("INSERT INTO runs (ParamSetID, Replicate) VALUES (", paramSetID, ",", repNum, ")"))
    
    # Finally retrieve the global RUNID value
    df <- dbGetQuery(DBCON,
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
  StoreParamSnapshot <- function(paramSetID) {
    params <- list(SpeciesParams=SpeciesParams, initial.cohorts=initial.cohorts, rain=rain, fire=scheduled.fires, 
                   fire.func=fire.func, thinning=thinning, special=special, seedling.survival=seedling.survival, 
                   overlap.matrix=overlap.matrix, ov.gen=ov.gen, recruitment=recruitment) 
    
    object.data <- rawToChar(serialize(params, NULL, ascii=TRUE))
    
    # Using a dbGetPreparedQuery seems to work with large blobs of param data whereas using
    # dbGetQuery sometimes fails. Also note the WrapText function isn't required when using
    # dbGetPreparedQuery.
    dbGetQuery(DBCON, "INSERT INTO paramobjects (ID, Data) VALUES (?, ?)",
               data.frame(ID=paramSetID, Data=object.data))
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
    tree.desc <- SpeciesParams[[spID]]$tree.desc
    
    # Match the cohort height to a row in the descriptors table
    # using the heights col (1)
    # (note: we are implicitly rounding up here)
    # If no matching row is found (tree too tall) use the last one
    #
    i <- match( TRUE, tree.desc[,1] >= cohort.data[ colHeight ], nrow(tree.desc) )
    
    canopy.radius <- tree.desc[i,2] 
    resource.use <- tree.desc[i,3] * cohort.data[colN]
    
    # calculate dbh (cm) and basal.area (sq m) if the parameters were provided
    #
    dbh <- 0
    basal.area <- 0
    if (HAS.HT.DBH.PARS[spID]) {
      pars <- SpeciesParams[[spID]]$ht.dbh.pars
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
    pars <- SpeciesParams[[spID]]
    ID <- NextCohortID
    NextCohortID <<- NextCohortID + 1
    
    # cols: ID, spID, age, height, N, Protect, CoppiceStage, FormerHt, CoppiceOn, ResourceUse, 
    # CanopyRadius
    new.cohort.data <- numeric(NUM.COHORTS.COLS)
    new.cohort.data[c(colID, colSpID, colHeight, colN, colFormerHt)] <- c( ID, spID, pars$height.yr1, N, -1 )
    Cohorts <<- rbind(Cohorts, new.cohort.data)
    
    # For the moment we are not setting the tree descriptors for a new cohort so that
    # this version of the model produces the same data as the older versions.
    #
    # treeDesc <- GetTreeDescriptors( ID )
    # Cohorts[ nrow(Cohorts), c(colCanopyRadius, colResourceUse, colDBH, colBasalArea) ] <<-
    #     c(treeDesc$radius, treeDesc$resource.use, treeDesc$dbh, treeDesc$basal.area)
    #
    # new.cohort.core.area <- numeric(NUM.SPP + 1)
    # for (sp in 1:NUM.SPP ) {
    #  overlap.value <- overlap.matrix[ sp, spID ]
    #  new.cohort.core.area[sp] <- N * pi * (overlap.value * treeDesc$radius)^2
    # }
    # new.cohort.core.area[NUM.SPP+ 1] <- N * pi * (ov.gen * treeDesc$radius)^2
    # Cohorts[ nrow(Cohorts), colFirstCoreArea:colGeneralCoreArea ] <<- new.cohort.core.area
    
    NewCohortSummaryRecord( ID, spID, YEAR, 0, N, pars$height.yr1, 0, 0, 0 )
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
  WriteCohortData <- function() {
    # NB we are only storing 'core.area.general' (the core area of that cohort 
    # calculated using the ov.gen parameter (default=0))
    
    dbBeginTransaction( DBCON )
    dbSendPreparedQuery(DBCON, SQL.WriteCohortData,
                        as.data.frame(cbind(RUNID, YEAR, 
                                            Cohorts[ , c(colID, colSpID, colAge, colHeight, colDBH, colBasalArea, colN, colResourceUse, colCanopyRadius, 
                                                         colCoppiceStage, colFormerHt, colCoppiceOn, colGrowthRate, colSurvP, colGeneralCoreArea), drop=FALSE ]))) 
    dbCommit( DBCON )
    
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
  WriteSummaryData <- function() {
    common.data <- cbind(RUNID, 1:YEAR, 
                         common.resource.use[1:YEAR], 
                         common.core.area[1:YEAR, , drop=TRUE], 
                         rain[1:YEAR], 
                         scheduled.fires[1:YEAR], 
                         realised.fires[1:YEAR], 
                         flammable.proportions[1:YEAR], 
                         seedling.survival[1:YEAR])
    
    dbWriteTable(DBCON, "commondata", as.data.frame(common.data), row.names=FALSE, append=TRUE)
    
    dbWriteTable(DBCON, "cohortsummary", as.data.frame(CohortSummary[1:TOTAL.COHORTS, ]), row.names=FALSE, append=TRUE)
  }
  
  # ====================================================================================
  #  Helper function - Trim leading and/or trailing whitespace from a string
  # ====================================================================================
  StringTrim <- function( s ) {
    sub('^[ \t]*([^ \t]*)[ \t]*$', '\\1', s)
  }
  
  # ====================================================================================
  #  Helper function - String comparison that ignores case and leading or trailing blanks
  # ====================================================================================
  StringEquals <- function( s1, s2 ) {
    tolower( StringTrim(s1) ) == tolower( StringTrim(s2) )
  }
  
  # ====================================================================================
  #  Helper function - A robust match function for character objects using StringEquals
  # ====================================================================================
  StringMatch <- function( x, table ) {
    match( tolower( StringTrim(x) ), tolower( StringTrim(table) ) )
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
    is.euc[i] <- StringEquals(SP.NAMES[i], "Eucalyptus")
  } 
  
  # If an overlap matrix wasn't provided, create a default one
  if (is.null(overlap.matrix) || is.vector(overlap.matrix)) {
    val <- ifelse(is.null(overlap.matrix), 0, overlap.matrix[1])
    overlap.matrix <- matrix(val, nr=NUM.SPP, nc=NUM.SPP)
    rownames(overlap.matrix) <- SP.NAMES
    colnames(overlap.matrix) <- SP.NAMES
    
  } else {
    # Ensure that the values in the overlap matrix are in the same order as species
    # in the SpeciesParams list. If the species names do not occur in the row and column
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
    print( "Initial cohorts matrix has 5 cols - assuming it is an old version" )
    flush.console()
  }
  
  num.initial.cohorts <- nrow(initial.cohorts) 
  
  print( paste(num.initial.cohorts, "initial cohorts") );  flush.console()
  
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
  realised.fires <- numeric( length(rain) )
  flammable.proportions <- numeric( length(rain) )
  
  combined.core.area <- numeric( NUM.SPP + 1 )
  names.combined <- paste("CombCoreArea", SP.NAMES, sep="")
  names(combined.core.area) <- c( names.combined, "CombCoreAreaGeneral")
  
  common.core.area <-  matrix( NA, length(rain), NUM.SPP + 1 )
  colnames(common.core.area) <- names(combined.core.area)
  
  # Connect to the output database. This will be either a user-supplied database or a 
  # newly created one.
  ConnectToDatabase()
  
  # Add the initial cohorts to the summary table in the output database
  for ( i in 1:nrow(Cohorts) )
  {
    NewCohortSummaryRecord( i, Cohorts[i, colSpID], 0, Cohorts[i, colAge], Cohorts[i, colN], 
                            Cohorts[i, colHeight], Cohorts[i, colDBH], Cohorts[i, colBasalArea], Cohorts[i, colCoppiceStage])
  }
  
  # Write the initial cohorts to the output database as year 0 data
  YEAR <- 0
  WriteCohortData()
  
  # create a 'no fires' fire vector if a scheduled fires vector was not provided
  #
  if ( is.null( scheduled.fires ) ) 
  {
    scheduled.fires <- numeric( length(rain) )
    print ( paste ( "a schedule of fires was not provided, so assuming no fires during simulation" ) );  flush.console()
  }
  else
  {
    # if a fire vector was provided then a fire function must be too
    if ( is.null( fire.func ) ) stop( "Bummer: fire vector provided but fire.func is missing" )    
    
    if ( length(scheduled.fires) != length(rain) ) stop("OOPS - Hey, the scheduled fires vec is not the same length as the rain vec!")
    #      scheduled.fires <- c(scheduled.fires, rep(0, length(rain) - length(scheduled.fires)))
    #      print ( paste ( "The scheduled fires vec was shorter than the rain.vec and the former has been topped up with zeros" ) ) 
    #      NB I turned the above topping up thing off because safer to do it manually so know what's what. 
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
      cat( paste("Year", YEAR, "\n") )
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
            pars <- SpeciesParams[[ Cohorts[irow, colSpID] ]]
            if ( is.euc[Cohorts[irow, colSpID]] & cut.h <= pars$height.yr1 )
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
    
    combined.resource.use <- sum( Cohorts[ , colResourceUse ] )
    
    for ( k in 1:(NUM.SPP+1) ) {
      combined.core.area[k] <- sum( Cohorts[ , colFirstCoreArea + k - 1] )
    }
    
    
    for ( i in SafeIndex(to=nrow(Cohorts)) )
    {
      if ( Cohorts[i, colN] <= 0 ) next
      
      pars <- SpeciesParams[[ Cohorts[i, colSpID] ]]
      
      #==============================================================
      # Growth
      #==============================================================
      h <- Cohorts[i, colHeight]
      former.h <- Cohorts[i, colFormerHt]
      adj.pars <- pars$growth.rainfall.pars
      g.rain.adj <- 1 / {1 + exp( -{ adj.pars[1] + adj.pars[2] * rain[YEAR] * h^adj.pars[3] } ) }
      
      adj.pars <- pars$growth.crowding.pars
      g.crowd.adj <- 1 / {1 + exp( -{adj.pars[1] + adj.pars[2] * combined.resource.use * h^adj.pars[3]} ) }
      
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
          boost.pars <- pars$coppice.boost.pars
          adjusted.boosted.gr <- max( pars$growth.rate, boost.pars[1]/{1 + boost.pars[2] * exp(-boost.pars[3] * former.h)} )
          # this equation and the coefficients fitted to it (in coppice.boost.pars) were from a regression of
          # coppice growth data from Mt Pilot, and directly relate a boosted growth rate to a tree's former height.
          Cohorts[i, colGrowthRate] <- adjusted.boosted.gr * g.rain.adj * g.crowd.adj
        } else if ( h >= 0.7 * former.h ) {
          Cohorts[i, colGrowthRate] <- pars$growth.rate * g.rain.adj * g.crowd.adj
          Cohorts[i, colCoppiceOn] <- 0
        }
      } else {
        Cohorts[i, colGrowthRate] <- pars$growth.rate * g.rain.adj * g.crowd.adj
      }
      # original 'bodge' curve for height increment vs height
      #h.incr <- max(0, (h * growth.rate * (pars$max.height - h) / pars$max.height) )
      
      # current curve based on two parameter exponential heights vs age formula
      # for ideal conditions...
      #    h(t) = a( 1 - exp(-ct) )
      # which gives height increment vs height formula
      #    delta.h = c(a - h)
      #
      h.incr <- Cohorts[i, colGrowthRate] * (pars$max.height - h)
      
      Cohorts[i, colHeight] <- h + h.incr
      
      
      #==============================================================
      # Survival
      #==============================================================
      #print( "Survival" );  flush.console()
      p.base <- pars$survival.prob[ min(length( pars$survival.prob ), Cohorts[i, colAge]) ]
      
      if ( is.euc[Cohorts[i, colSpID]] & Cohorts[i, colCoppiceOn] == 1  &  h < 0.7 * former.h )  
        # this is safe as long as former.h is never NA when colCoppiceOn = 1, which should always be true???
      {
        h <- former.h
      }
      
      # inverse logit expression to calculate probability adjustment
      #
      adj.pars <- pars$survival.rainfall.pars
      s.rain.adj <- 1 / {1 + exp( -{ adj.pars[1] + adj.pars[2] * rain[YEAR] * h^adj.pars[3] } ) }
      
      adj.pars <- pars$survival.crowding.pars
      s.crowd.adj <- 1 / {1 + exp( -{adj.pars[1] + adj.pars[2] * combined.resource.use * h^adj.pars[3]} ) }
      
      Cohorts[i, colSurvP] <- p.base * s.rain.adj * s.crowd.adj
      
      # only change the number of individuals for non-protected cohorts
      if ( !Cohorts[i, colProtect] )
        Cohorts[i, colN] <- rbinom( 1, Cohorts[i, colN], Cohorts[i, colSurvP] )
    }
    
    
    #==============================================================
    # Recruitment
    #==============================================================
    
    if ( recruitment[YEAR] )
    {
      #print( "Recruitment" );  flush.console()
      
      num.stems <- sum(Cohorts[, colN])
      
      for ( spID in 1:NUM.SPP )
      {
        pars <- SpeciesParams[[spID]]
        
        seed <- 0
        # Seed production of in-situ trees
        seed.cohorts <- which( Cohorts[, colSpID] == spID )
        if ( length( seed.cohorts ) > 0 ) 
        {
          for ( i in seed.cohorts )
          {
            h <- round( Cohorts[i, colHeight] )
            if ( h >= 1 )
              seed <- seed + Cohorts[i, colN] * pars$seed.output[h, 2] * pars$seed.output[h, 3]
          }
        }
        
        if ( HAS.EXTERNAL.SEED.FUNC[spID] )
        {
          seed <- seed + pars$external.seed.func()
        }
        
        # rainfall effect
        adj <- 1 / {1 + exp( -sum( pars$seed.rainfall.pars * c(1, rain[YEAR]) ) )}
        seed <- seed * adj
        
        # balls in bins model
        #
        # we don't want endless seedlings cramming in over the max.sprogs limit
        # which will happen if we just consider cover of existing residents, so
        # we adjust the number of bins by the number of existing trees
        #
        free.bins <- pars$max.sprogs - num.stems
        if ( free.bins <= 0 ) next
        
        num.sprogs <- rbinom( 1, pars$max.sprogs, 1 - exp(-seed / pars$max.sprogs) )
        if ( num.sprogs == 0 ) next
        
        num.sprogs <- rbinom( 1, num.sprogs, free.bins / pars$max.sprogs )
        if ( num.sprogs == 0 ) next
        
        # reduction in available space (resources) perceived by the new recruits,
        # (using the column of combined.core.area specific to that seedling sp:
        #
        # consider porosity here ?
        gap <- cell.area - combined.core.area[spID]
        
        if ( gap > 0 ) 
        {
          num.sprogs <- rbinom( 1, num.sprogs, gap / cell.area )
          
          # finally we imagine that the value for this year for
          # seedling.survival is something like bunnies grazing on 
          # the seedlings
          #
          num.sprogs <- rbinom( 1, num.sprogs, seedling.survival[YEAR] )
          
          if ( num.sprogs > 0 ) AddCohort( spID, num.sprogs )
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
      # account of when the combined.core.area exceeds the cell area).
      #
      # if ( scheduled.fire.intensity > Threshold ) 
      #  { return( scheduled.fire.intensity  ) } # realised.fire.int above threshold is same as scheduled
      # else if (scheduled.fire.intensity <= Threshold ) 
      #  { return( scheduled.fire.intensity + b1 * (1 - flammable.prop ) ) } # dampen realised.fire.int below threshold
      # }
      # and we are using Threshols = 4.5 and b1 = -1.4 
      
      flammable.area <- cell.area
      
      combined.core.area.post.growth <- 0
      for ( i  in SafeIndex(to=nrow(Cohorts)) )
      {
        combined.core.area.post.growth <- combined.core.area.post.growth + ( Cohorts[i, colN] * pi * (ov.gen * Cohorts[i, colCanopyRadius])^2 )
      }
      
      for ( spID in 1:NUM.SPP )
      {
        non.flammability <- SpeciesParams[[spID]]$non.flammability
        
        for ( i in which( Cohorts[,colSpID] == spID ) )
        { 
          if ( combined.core.area.post.growth <= cell.area ) {
            flammable.area <- flammable.area - Cohorts[i, colN] * pi * (ov.gen * Cohorts[i, colCanopyRadius])^2 * non.flammability
          } else if ( combined.core.area.post.growth > cell.area ) {
            flammable.area <- flammable.area - ( cell.area * ( Cohorts[i, colN] * pi * (ov.gen * Cohorts[i, colCanopyRadius])^2 * non.flammability ) / 
                                                   combined.core.area.post.growth )
          }
        }
      }
      
      # K.R. Feb 2009: I have turned off two steps previously in the model for fire: 
      # (1) The test of: if (flammable.area > 0), then continue with the fire part of the model 
      # because this was an unnecessary extra step (the influence of zero flammable area is now 
      # dealt with in the fire.function); 
      # (2) I have also turned off the step that tests fire.prob against a runif(1) because the 
      # fire.func no longer generates a probability, but a realised fire intensity instead (and 
      # this fire in the site model is a flaming front that has already arrived at the stand 
      # boundary; it is not a new ignition or spotting ahead of the fire. When we have a 
      # landscape model, it will be appropriate to bring back this test of fire.prob against 
      # runif(1) to determine whether an ignition actually takes off into a fire.
      
      flammable.prop <- flammable.area / cell.area
      realised.fire.intensity <- fire.func( flammable.prop, scheduled.fires[YEAR] )
      realised.fires[YEAR] <- realised.fire.intensity
      flammable.proportions[YEAR] <- flammable.prop
      
      if (session.settings$display.fire.data) {
        print( paste( "combined.core.area.post.growth: ",combined.core.area.post.growth ) ); flush.console()
        print( paste( "flammable.area: ",flammable.area ) ); flush.console()    
        print( paste("flam prop:", flammable.prop ) ); flush.console()
        print( paste("realised.fire.intensity of: ",realised.fires[YEAR], " in year ",YEAR ) ); flush.console()
      }
      
      for ( i in SafeIndex(to=nrow(Cohorts)) ) {
        if ( Cohorts[i, colN] <= 0 ) next
        
        pars <- SpeciesParams[[ Cohorts[i, colSpID] ]]
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
        adj.pars <- pars$survival.fire.pars
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
          # height adjustment for survivors; although this is not specific to eucs, at present callitris height.fire.pars are null (Inf, 0, 0, 0)
          adj.pars <- pars$height.fire.pars
          h <- Cohorts[i, colHeight]
          # fire.height.adj <- 1 / (1 + exp( -( adj.pars[1] + adj.pars[2] * realised.fire.intensity  * h^adj.pars[3] ) ) )
          # THIS IS THE FINAL GENERALIZED LOGISTIC FUNCTION MICHAEL FITTED IN JULY 2010:
          fire.height.adj <-  ( 1 / ( 1 + exp( -( adj.pars[1] + adj.pars[2] * realised.fire.intensity + adj.pars[3] * h ) ) ) ^ adj.pars[4] )
          
          #print( paste(pars$name, "fire height adj", fire.height.adj) ); flush.console()
          
          new.h <- max( (h * fire.height.adj), pars$height.yr1 )
          # We need to bring all initial coppice heights up to at least height.yr1 because otherwise the current 
          # resprout.propn pars give really tiny new.hts which do not grow! (like 1x10^-92)!!!
          # (note: this height change can affect protected cohorts)
          
          if (is.euc[Cohorts[i, colSpID]]) {
            # We boost all surviving Eucs which are <70% of their former (max) ht, not just truly
            # coppiced ones: so throughout tm.site, colCoppiceOn and colCoppiceStage now actually
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
    
    # Note: at the moment we skip writing output for years with
    # no cohorts (shouldn't happen if external.seed is TRUE but
    # might be an issue later if that arg is converted to a probability
    # or vector)
    if (nrow(Cohorts) > 0) {
      WriteCohortData()
      Cohorts[, colAge] <- Cohorts[, colAge] + 1
    }
    
    common.resource.use[YEAR] <- combined.resource.use
    common.core.area[YEAR, ] <- combined.core.area
    
    # Check for loss of all cohorts and break out early if we are not using
    # external seed input
    if (nrow(Cohorts) == 0 && !EXTERNAL.SEED) {
      if (YEAR < length(rain)) {
        print(paste("All cohorts extinct at year", YEAR))
        break
      }
    }
    
  }  # END OF yearly loop
  
  WriteSummaryData()
  
  # clean up and return the connection object for the output database
  gc()
  DBCON
}
