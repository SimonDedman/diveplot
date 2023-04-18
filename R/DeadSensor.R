# Dead sensor check, Simon Dedman, 10/10/2019 simondedman@gmail.com
# Screen each tag's sensor data for dead zones and convert values to NA: avoids incorrect values biasing analyses
# Also report first & last working dates: save to tblfgprocessingstatus: allows for window/subset of tag import, reducing overhead & processing time waste

# library(compiler)
# cmpfun(DeadSensor)
# DeadSensor(loadlistcompare = FALSE) # or DeadSensor(machine = "/media/Seagate/Work/", loadlistcompare = FALSE)

DeadSensor <- function(machine = "/home/simon/Documents/Si Work/", loadlistcompare = TRUE, molasave = TRUE) {
  library(lubridate)
  library(dplyr) #for lag else uses stats version
  library(tidyr) #pivot_wider
  library(beepr)
  library(stringr) # str remove at bottom
  library(stringi) # stri_sub at bottom
  library(foreach)
  library(doMC)
  library(progress)
  options(error = function() beep(9))  # give warning noise if it fails
  # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/1_extracted") #per saveloc in MolaFoldersExtractLoop.R
  saveloc = paste0(machine, "Blocklab/abft_diving/2_cleaned/") #ensure trailing /slash
  # loadloc = "/mnt/molaHomeDedman/abft_diving/1_extracted/"
  # saveloc = "/mnt/molaHomeDedman/abft_diving/2_cleaned/"
  whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #

  setwd(loadloc)
  loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above

  #loadlist comparison chunk####
  if (loadlistcompare) {
    setwd(saveloc)
    savelist <- list.files(pattern = whichfiles) #list files based on specified pattern above
    print("in loadlist but not savelist, not processed?")
    print(loadlist[which(!loadlist %in% savelist)])
    loadlist <- loadlist[which(!loadlist %in% savelist)] # update loadlist for rerun
    setwd(loadloc)
  } # close loadlistcompare

  if (molasave) {
    dir.create(paste0(saveloc, "molasave"))
    PATpopups <- read.csv(paste0(loadloc, "/PATpopups.csv"))
    PATpopups <- PATpopups$x # to character vector
  }

  # loadlist <- loadlist[96:167] #96 missed, 5107036 155mb, 125 5112003 381mb
  mycores <- 8 #Nautilus
  # mycores <- 16 #Poseidon

  #connectTOPP load dbases, crop, keep in mem for lookups later

  {library(dbplyr)
    library(magrittr)
    library(DBI)
    library(RPostgreSQL)
    library(RH2)
    print("generate con object with connectTOPP.R")

    #Data start & end points from mola tables####
    #Data Start Point####
    deployment = tbl(con, "tblfgarchivaldeployment") %>%
      collect() %>%
      filter(commonname == "Atlantic Bluefin Tuna") %>% # subset rows
      select(toppid, tagcode, taggingdate, taggingtime) # subset columns. taggingdate & time formats all over the place?
    # see http://blocklab.tunaresearch.org/database-tables/fishgroup/archival-deployment
    # taggingtime 	Local time at deployment of tag. Various formats, mostly 00:00:00 or 24h clock. TZ unknown
    deployment$fishID <- paste0(deployment$toppid, "_", deployment$tagcode)
    deployment <- deployment[which(deployment$fishID %in% str_sub(loadlist, 1, -9)),] # could filter by toppid %in% all-but-last-8 of loadlist
    deployment$taggingdate <- parse_date_time(deployment$taggingdate, orders = c("Y-m-d", "m/d/Y")) # fix YYYY-MM-DD & M/D/YYYY formats
    deployment$taggingtime[is.na(deployment$taggingtime)] <- "00:00:00" #convert NAs to 00:00:00
    deployment$tagdatetimeUTCmin5 <- parse_date_time(deployment$taggingtime, orders = c("H:M:S", "H:M"), tz = "America/New_York") # prepends all with 0-01-01 date
    date(deployment$tagdatetimeUTCmin5) <- deployment$taggingdate # replace 0-01-01 with actual date
    deployment %<>% select(toppid, tagdatetimeUTCmin5)
    deployment <- as.data.frame(deployment) #remove tibble awfulness for later

    #Data End Point####
    processing = tbl(con, "tblfgprocessingstatus") %>%
      collect() %>%
      select(toppid, date_last_depth, date_last_etemp, date_last_itemp, date_last_light, date_last_lon, no_pdt, no_tad, no_tat)
    # no tagcode only fgadkey which I haven't got in df_i. Doesn't look to cause issues tho ideally would fix
    processing <- processing[which(processing$toppid %in% substr(loadlist, 1, 7)),] # could filter by toppid %in% first7 of loadlist

    # date_last_depth 	The last date of good time series depth data if there was sensor or battery failure.
    processing$date_last_depth <- parse_date_time(processing$date_last_depth, orders = c("m/d/Y", "m/d/Y H:M:S"), tz = "America/New_York")
    # date_last_etemp 	The last date of good time series external temperature data if there was sensor or battery failure.
    processing$date_last_etemp <- parse_date_time(processing$date_last_etemp, orders = c("m/d/Y", "m/d/Y H:M:S"), tz = "America/New_York")
    # date_last_itemp 	The last date of good time series internal temperature data if there was sensor or battery failure.
    processing$date_last_itemp <- parse_date_time(processing$date_last_itemp, orders = c("m/d/Y", "m/d/Y H:M:S"), tz = "America/New_York")
    # date_last_light 	The last date of good time series light data if there was sensor or battery failure.
    processing$date_last_light <- parse_date_time(processing$date_last_light, orders = c("m/d/Y", "m/d/Y H:M:S"), tz = "America/New_York")
    # date_last_lon 	The last date of good light level longitude data if there was sensor or battery failure.
    processing$date_last_lon <- parse_date_time(processing$date_last_lon, orders = c("m/d/Y", "m/d/Y H:M:S"), tz = "America/New_York")
    # # What to do with these? Also should be 0 or 1, last two have 2s.
    # # no_pdt 	PAT tag specific. Is PDT data missing or corrupt? Yes = 1 and no = 0.
    # unique(processing$no_pdt)
    # # no_tad 	PAT tag specific. Is time at depth data missing or corrupt? Yes = 1 and no = 0. 2 = duplicate datetime. Could be leapyear/DST, or worse
    # unique(processing$no_tad)
    #   # no_tat 	PAT tag specific. Is time at temperature data missing or corrupt? Yes = 1 and no = 0. 2 = duplicate datetime. Could be leapyear/DST, or worse
    # unique(processing$no_tat)
    processing <- as.data.frame(processing) #remove tibble awfulness for later

    recovery = tbl(con, "tblfgsatrecovery") %>%
      collect() %>%
      select(toppid, rtagnumber, popdate_geoloc, actpopdate) # subset columns. taggingdate & time formats all over the place?
    # http://blocklab.tunaresearch.org/database-tables/fishgroup/sat-recovery
    # popdate_geoloc [popdate_geo]	This is the correct popoff date identified with tag data. Necessary when a tag floats at the surface failing to recognize popup.
    # popdate = actpopdate 	The popup date of the tag.
    recovery$fishID <- paste0(recovery$toppid, "_", recovery$rtagnumber)
    recovery <- recovery[which(recovery$fishID %in% str_sub(loadlist, 1, -9)),] # could filter by toppid %in% all-but-last-8 of loadlist
    recovery$popdate_geoloc <- parse_date_time(recovery$popdate_geoloc, orders = c("m/d/Y", "m/d/Y H:M:S"), tz = "America/New_York")
    recovery$actpopdate <- parse_date_time(recovery$actpopdate, orders = c("m/d/Y", "m/d/Y H:M:S", "m/d/y", "m/d/Y H:M"), tz = "America/New_York")
    # use popdate_geoloc else actpopdate
    recovery <- as.data.frame(recovery) #remove tibble awfulness for later

    deployall = tbl(con, "tblabft_deploy_rec_all") %>%
      collect() %>%
      select(toppid, tagcode, mod_popdate, popdate, harvdate)
    deployall$fishID <- paste0(deployall$toppid, "_", deployall$tagcode)
    deployall <- deployall[which(deployall$fishID %in% str_sub(loadlist, 1, -9)),] # could filter by toppid %in% all-but-last-8 of loadlist

    # date_last <- mod_popdate else popdate else harvdate (kinda useless).
    # if date_last < current last date of tag data, replace (harvdate mostly wont be)
    # could have multiple rows with the same toppid e.g. 5102410 i <- loadlist[53]
    # doesn't matter, will then deliver a list of >1 to date_last_all's min() which works fine
    deployall <- as.data.frame(deployall) #remove tibble awfulness for later

    DBI::dbDisconnect(con)
    rm(list = c("con", "connectTOPP"))
  }

  #parallel####
  registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
  pbTracker <- function(pb,i,mycores) {if (i %% mycores == 0) {pb$tick()}} #i & mycores need to be numbers
  pb <- progress_bar$new(
    format <- " progress [:bar] :percent eta: :eta",
    total <- length(loadlist) / mycores, clear = FALSE, width = 60)
  writeLines(c(""), "log.txt")


  #i loop####
  # foreach(i = loadlist, .errorhandling = "pass") %dopar% { #parallel #i <- loadlist[1]
  # sink("log.txt", append = TRUE) #sink output to log
  for (i in loadlist) { #sequential # i <- loadlist[2]
    # Loops through each file. Input files have been extracted from mola folders with MolaFoldersExtractLoop.R
    print(paste0(which(loadlist == i), "/", length(loadlist), " ", i))
    df_i <- readRDS(i)

    # whichi <- which(i %in% loadlist)
    # pbTracker(pb, whichi, mycores)

    # df_i$Date <- date(df_i$DateTimeUTCmin5) # do time to YMD stripping HMS
    df_i$Date <- as.Date(df_i$DateTimeUTCmin5, tz = "America/New_York") # using date() changes days at UTC midnight only
    # If DateTimeUTCmin5 isn't coded with a timezone, or the tz is the default, UTC. Not the case here:
    # input files saved as RDS & carry the tz

    #datefirstx crop####
    if (substr(i, 1, 7) %in% deployment$toppid) { #if i's toppid is in deployment
      date_first <- deployment[deployment$toppid == substr(i, 1, 7), "tagdatetimeUTCmin5"]
      if (!is.na(date_first)) df_i <- df_i[df_i$DateTimeUTCmin5 >= date_first,]
    }

    #datelastx crops####
    date_last_all <- min(c(deployall[deployall$toppid == substr(i, 1, 7), "mod_popdate"],
                           deployall[deployall$toppid == substr(i, 1, 7), "popdate"],
                           deployall[deployall$toppid == substr(i, 1, 7), "harvdate"],
                           recovery[recovery$toppid == substr(i, 1, 7), "actpopdate"],
                           recovery[recovery$toppid == substr(i, 1, 7), "popdate_geoloc"],
                           processing[processing$toppid == substr(i, 1, 7), "date_last_depth"]),
                         na.rm = T)
    if (!is.infinite(date_last_all)) {
      if (date_last_all < max(df_i$DateTimeUTCmin5, na.rm = T)) df_i <- df_i[df_i$DateTimeUTCmin5 <= date_last_all,]
    }

    #Pre VV TV LV cleaning####
    #Depth####
    #Depth values out of range####
    df_i[which(df_i$Depth.m. > 1600), "Depth.m."] <- NA # NA out depth rows > 1600

    #Light####
    #Light values out of range####
    df_i[which(df_i$Lt <= 1), "Lt"] <- NA #dead code
    df_i[which(df_i$Lt > 500), "Lt"] <- NA #some weird codes at 2047

    #IntTemp####
    #IntTemp values out of range####
    if (!all(is.na(df_i$IntTemp.C.))) df_i[which(df_i$IntTemp.C. <= 0.01), "IntTemp.C."] <- NA
    if (!all(is.na(df_i$IntTemp.C.))) df_i[which(df_i$IntTemp.C. == 45), "IntTemp.C."] <- NA # dead value, 5112003 reports this 2017-06-23

    #ExtTemp####
    #ExtTemp values out of range####
    df_i[which(df_i$ExtTemp.C. <= -2.4), "ExtTemp.C."] <- NA #dead code but 0.04 legit readings in Walli
    # Seen numerous negative readings off the Grand Banks, saline water freezes around 2.4, 2.6 is the coldest measured.
    # How to screen out cold-but-legit from failed? (min) temp vs depth
    # plot(AllDailies$MinETemp24h, AllDailies$MaxDepth24h)
    # No obvious pattern, few outliers, leave it.
    # Save old data as separate cols first so it's retained? James idea, allows later comparison, recalibration?


    #ExtTemp broken giving NAs & bad values####
    daystats <- df_i %>%
      group_by(Date) %>%
      summarise(MinETemp24h = min(ExtTemp.C., na.rm = T), #Gives Inf on days with only NA
                MeanETemp24h = mean(ExtTemp.C., na.rm = T)) %>% # Impossible/incredible unlikely to be 3 or 3.2 without being broken
      ungroup()
    # Date of first Inf i.e. whole day of NA
    DeadFrom1 <- daystats$Date[first(which(is.infinite(daystats$MinETemp24h)))] # will return daystats-length vec of NA if no Infs
    # or 3 or 3.2 which is a dead code for 5197era WC tags
    DeadFrom2 <- daystats$Date[first(c(which(daystats$MeanETemp24h == 3), which(daystats$MeanETemp24h == 3.2)))] # ditto
    DeadFrom <- min(DeadFrom1, DeadFrom2, na.rm = TRUE) # if both NA, returns Inf
    # After the first instance of a 3 or 3.2 or full day of NAs, kill the sensor.
    # Arguably this is VERY aggressive & data lossy. And 3.2 or 3 could be complete chance, e.g. 5112003 2012-10-25
    # Could have a threshold number of 3 or 3.2 readings per day above whch the sensor's considered dead?
    # But what if the fish is just in water that's colder than the sensor minimum?
    if (!is.infinite(DeadFrom)) df_i[which(df_i$Date >= DeadFrom), "ExtTemp.C."] <- NA
    # These fish have mean extemp <=2m depth of 3 or 3.2, see /media/Seagate/Work/Blocklab/abft_diving/X_PlotsMisc/DeadSensors/ETemp3tracks/DailyMeanExtU2M_3_3-2.csv
    # [1] "5100129_98518_NO99_W12D" "5100132_98508_NO99_W13D" "5100133_98512_NO99_W11D" "5197125_97103_NO97_W12D" "5100134_98516_NO99_W13D" "5197124_97102_NO97_W12D"
    # [7] "5197055_97028_NO97_W12C" "5100127_98507_NO99_W14D" "5197051_97017_NO97_W12D" "5197054_97027_NO97_W11D" "5197118_97089_NO97_W12D" "5197052_97019_NO97_W12D"
    # [13] "5197058_97037_NO97_W12D" "5199104_98504_NO99_W13D" "5197060_97048_NO97_W12D" "5197057_97030_NO97_W12D" "5197105_97011_NO97_W11C" "5197153_97112_NO97_W12D"
    # [19] "5197111_97022_NO97_W12C" "5197059_97038_NO97_W12D" "5197050_97016_NO97_W12C"
    # 3 or 3.2 is probably the min threshold for some sensors
    # 5100127 1999-04-24 hits the threshold with supercold surface water but is otherwise fine.
    # But from 1999-07-27 ~23:00 it flatlines at 3
    # Filtering min(3) kills everything that hits the min. Mean(3) should catch these.
    # 5100132 no date_last_etemp or is wrong, 3 or 3.2 all day every day for 290 days. 5100134 for 267 days

    # if (!length(DeadFrom1) == 0) df_i[which(df_i$Date >= DeadFrom1), "ExtTemp.C."] <- NA
    # high temp bug, >40.25 (40.25 is sensor max, returns ">40.25" which gets converted to 40.25 in MolaFoldersExtractLoop)
    # 5112003 lots of 45s, just NA those
    # Also 246 rows between 40 & 44.99. None contiguous. 204 btwn 40.26 & 44.99
    # NA out anything > 40.25
    df_i[which(df_i$ExtTemp.C. > 40.25), "ExtTemp.C."] <- NA
    rm(list = c("DeadFrom1", "daystats")) #"DeadFrom" "DeadFrom2",

    #ExcessTemp VV TV LV calcs####
    # ExcessTemp calcs for caught fish
    df_i$ExcessTemp.C. <- df_i$IntTemp.C. - df_i$ExtTemp.C. #Thermal excess

    # VertVelo calc. Speed = d/t. vv = depth2-depth1 / time2-time1
    dep1 <- df_i$Depth.m.[1:(nrow(df_i) - 1)] #subset depth except last row
    dep2 <- df_i$Depth.m.[2:(nrow(df_i))] # depth except first row
    time1 <- df_i$DateTimeUTCmin5[1:(nrow(df_i) - 1)] #time except last row
    time2 <- df_i$DateTimeUTCmin5[2:(nrow(df_i))] # time except first
    vv <- (dep2 - dep1)/as.numeric(difftime(time1 = time2, #diff in depth / diff in time
                                            time2 = time1,
                                            units = "mins")) #metres/min
    df_i$VertVelo <- rep(NA, nrow(df_i)) #ensures first row is NA
    df_i$VertVelo[2:(nrow(df_i))] <- vv #from second row so velocities align with after movement rather than before

    # TempVelo per VertVelo (extTemp)
    temp1 <- df_i$ExtTemp.C.[1:(nrow(df_i) - 1)] #subset depth except last row
    temp2 <- df_i$ExtTemp.C.[2:(nrow(df_i))] # depth except first row
    tv <- (temp2 - temp1)/as.numeric(difftime(time1 = time2, #diff in depth / diff in time
                                              time2 = time1,
                                              units = "mins")) #metres/min
    df_i$TempVelo <- rep(NA, nrow(df_i)) #ensures first row is NA
    df_i$TempVelo[2:(nrow(df_i))] <- tv #from second row so velocities align with after movement rather than before

    # LtVelo per VertVelo (Lt)
    lt1 <- df_i$Lt[1:(nrow(df_i) - 1)]
    lt2 <- df_i$Lt[2:(nrow(df_i))]
    lv <- (lt2 - lt1)/as.numeric(difftime(time1 = time2,
                                          time2 = time1,
                                          units = "mins")) #Lt/min
    df_i$LtVelo <- rep(NA, nrow(df_i))
    df_i$LtVelo[2:(nrow(df_i))] <- lv

    rm(list = c("dep1", "dep2", "temp1", "temp2", "lt1", "lt2", "time1", "time2", "tv", "vv", "lv")) # remove objects
    is.na(df_i) <- do.call(cbind,lapply(df_i, is.infinite)) #convert Inf & -Inf to NA
    is.na(df_i) <- do.call(cbind,lapply(df_i, is.nan)) #convert NaN to NA


    #Post VV TV LV Cleaning####
    #Depth####
    #VV out of range####
    df_i[which(df_i$VertVelo > 900), "VertVelo"] <- NA # outliers above 850 / below same
    df_i[which(df_i$VertVelo < -900), "VertVelo"] <- NA # outliers above 850 / below same

    #Depth spikes 1 row####
    # if VV > 300 & vv next row <-300 it's a bug. Could be less than 300?
    df_i$VVspike <- rep(NA, nrow(df_i))
    df_i$VVspike <- (df_i$VertVelo > 300 & lead(df_i$VertVelo, n = 1) < -300) # need to also NA the next row's VV
    df_i[which(df_i$VVspike), "Depth.m."] <- NA
    df_i[which(df_i$VVspike), "VertVelo"] <- NA
    df_i[which(df_i$VVspike) + 1, "VertVelo"] <- NA

    #Depth spikes multirow####
    # NA depth & VV rows from: VVbug == TRUE & df_i$VertVelo > 675 until VVbug == TRUE & df_i$VertVelo < -675
    # If the next VVbug==TRUE is also VertVelo > 675 the fish could have slightly (actually) ascended so negative VV cant be as high
    df_i$VVbugNeg <- rep(NA, nrow(df_i))
    df_i$VVbugPos <- rep(NA, nrow(df_i))
    # High polling rate tags can exceed this criteria of >700, <-650
    # Bluefin top speed 43.5mph = 19.4 m/s = 1164m/m (vv)
    # e.g. 5107036_07A0144_P_D 2008-02-29 10:44:00 to 10:44:04
    # 262.7 - 238.7 = 24m in 2s = 12m/s, 720m/m = 26.8mph
    # In high PR tags, high VV can occur BUT should be next to similar values, e.g. -675,-720,-600, not 0,-720,0
    # Include condition that, to be a bug, VV must be high AND be in isolation
    df_i$VVrowbefore <- lag(df_i$VertVelo, n = 1)
    df_i$VVrowafter <- lead(df_i$VertVelo, n = 1)
    #mean of neighbouring rows without current row
    df_i$NeighbourRowMeans <- rowMeans(df_i[,c("VVrowbefore","VVrowafter")], na.rm = T)
    # current row as a % of neighbouring rows: scale of discrepancy
    # abs() since directionality of denominator a function of arbitrary shallow wiggles
    df_i$RowPctMean <- abs(df_i$VertVelo / df_i$NeighbourRowMeans)
    # all 0s = mean 0 = RowPctMean NaN, convert to 0
    df_i$RowPctMean[which(is.nan(df_i$RowPctMean))] <- 0
    # before 0 current 1 after -1 mean 0, RowPctMean = 1/0 = Inf or -Inf,
    # convert to 0; can't be a 1 row spike because code above removed them
    df_i$RowPctMean[which(is.infinite(df_i$RowPctMean))] <- 0
    # highest reasonable RowPctMean, 4.5: 600/4.5=133, 133 600 133, would be a very spiky burst
    # 3.6 seen in 5107036 @ vv600. 600&3+: 0 seen. 600&2: 8 seen.
    df_i$VVbugPos <- df_i$VertVelo > 600 & df_i$RowPctMean >= 4.5 #654 deepdive
    df_i$VVbugNeg <- df_i$VertVelo < -600 & df_i$RowPctMean >= 4.5 #-720 for 5107036
    posbugindex <- which(df_i$VVbugPos)
    negbugindex <- which(df_i$VVbugNeg)
    if (length(posbugindex) != 0 & length(negbugindex) != 0) { #both must be nonzero length booleans
      # Starting with the first posbugindex row, if the next highest number in negbug index is lower than the next highest number in posbug index that should be a connected pair?
      NArangePairs <- data.frame(NAfrom = posbugindex,
                                 NAto = rep(NA, length(posbugindex)))
      total <- (length(posbugindex) - 1)
      print("calculating multirow depth failures")
      pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
      for (j in posbugindex[1:total]) {
        setTxtProgressBar(pb, which(posbugindex %in% j)) # update progress bar
        # the next highest number in negbug index
        nexthighestNBI <- negbugindex[which(negbugindex > j)[1]] #first entry in negbugindex greater than j, return the number
        # the next highest number in posbugindex
        nexthighestPBI <- posbugindex[which(posbugindex %in% j) + 1] # index position of j in posbugindex, add one, that value
        # unless nexthighestPBI is NA (may happen near end of series),
        # will fail at last j because there isn't an j+1, hence length(-1) index at top
        # if nexthighestNBI < nexthighestPBI that should be a connected pair
        # record both values (j & nexthighestNBI)
        if (!is.na(nexthighestNBI)) if (nexthighestNBI < nexthighestPBI) NArangePairs[which(posbugindex %in% j), 2] <- nexthighestNBI
      }
      close(pb)
      rm(j)
      # for the last row, there is no nexthighestPBI, j should be retained, single entry
      # as long as it isn't NA: might give constant reads of e.g. 867.5 & have one final surface flick before
      # giving only deep values then crashing. So there's no matching surface return
      finalNBI <- negbugindex[which(negbugindex > posbugindex[total + 1])[1]]
      if (!is.na(finalNBI)) NArangePairs[nrow(NArangePairs), 2] <- finalNBI
      # Remove rows with NAs in "NAto"
      NArangePairs <- NArangePairs[which(!is.na(NArangePairs$NAto)),]
      # NArangePairs is a list of subsets of df_i to blank depth & VV for.
      # FromRow & BetweenRows: Depth & VV
      # ToRow: only VV
      # From & To are both row indexes of the same df_i. Make 2 vectors
      # VV: seq from to
      seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to")) #Vectorise function
      NArangePairs$NAVV <- rep(NA, nrow(NArangePairs))
      NArangePairs$NAVV <- seq2(from = NArangePairs$NAfrom, to = NArangePairs$NAto, by = 1)
      df_i[unlist(NArangePairs$NAVV), "VertVelo"] <- NA
      # depth: seq from to-1
      NArangePairs$NAdepth <- rep(NA, nrow(NArangePairs))
      NArangePairs$NAdepth <- seq2(from = NArangePairs$NAfrom, to = (NArangePairs$NAto - 1), by = 1)
      df_i[unlist(NArangePairs$NAdepth), "Depth.m."] <- NA
      rm(list = c("posbugindex", "negbugindex", "NArangePairs", "total", "pb", "nexthighestNBI", "nexthighestPBI", "finalNBI")) # remove objects
    } # close boolean length if

    #PROBLEM####
    # Depth flatlines deep
    # Notably 5105027 ~860-880m from 2005-09-08 # actually dead from 2005-08-31 which is date_last_depth in processing, why not fixed?
    # Also spikes deep, high VV why not caught?

    #STILL TO FIX####
    # 5105027 defo broken tag depth spikes not cleaned 363 obs
    # 5105041 3 obv, depth missing from ts plots, already cleaned? Then why this remains? 2005-01-13 -19 -02-02
    # 5105065 ~ 30 obs, defo broken tag depth spikes not cleaned

    #Light####
    #Light spikes 1 row####
    # if LV < -100 & vv next row > 100 it's a bug. Could be less than 100? See plot
    # Could be bioluminescence?
    # Little difference in numbers between 50 & 100, stark inflection elbow around +-20
    # see /home/simon/Documents/Si Work/Blocklab/abft_diving/X_PlotsMisc/DeadSensors/5104527_LVsort.png
    # But that's 1 fish, what if earlier tag models have loads of values >20 / <-20? r 50 or 100 or whatever?
    # What's gained by removing this? Commented out next section

    # df_i$LVspike <- rep(NA, nrow(df_i))
    # df_i$LVspike <- (df_i$LtVelo < -50 & lead(df_i$LtVelo, n = 1) > 50) # need to also NA the next row's VV
    # df_i[which(df_i$LVspike), "Lt"] <- NA
    # df_i[which(df_i$LVspike), "LtVelo"] <- NA
    # df_i[which(df_i$LVspike) + 1, "LtVelo"] <- NA
    #
    # #Light spikes multirow####
    # # NA depth & VV rows from: VVbug == TRUE & df_i$LtVelo > 675 until VVbug == TRUE & df_i$LtVelo < -675
    # # If the next VVbug==TRUE is also LtVelo > 675 the fish could have slightly (actually) ascended so negative VV cant be as high
    # df_i$LVbugPos <- df_i$LVbugNeg <- rep(NA, nrow(df_i))
    # df_i$LVbugPos <- df_i$LtVelo > 90 #654 deepdive
    # df_i$LVbugNeg <- df_i$LtVelo < -90
    # posbugindex <- which(df_i$LVbugPos)
    # negbugindex <- which(df_i$LVbugNeg)
    # if (length(posbugindex) != 0 & length(negbugindex) != 0) { #both must be nonzero length booleans
    #   # Starting with the first posbugindex row, if the next highest number in negbug index is lower than the next highest number in posbug index that should be a connected pair?
    #   NArangePairs <- data.frame(NAfrom = negbugindex,
    #                              NAto = rep(NA, length(negbugindex)))
    #   total <- (length(negbugindex) - 1)
    #   print("calculating multirow light failures")
    #   pb <- txtProgressBar(min = 0, max = total, style = 3) # create progress bar
    #   for (k in negbugindex[1:total]) {
    #     setTxtProgressBar(pb, which(negbugindex %in% k)) # update progress bar
    #     # the next highest number in negbug index
    #     nexthighestPBI <- posbugindex[which(posbugindex > k)[1]] #first entry in posbugindex greater than k, return the number
    #     # the next highest number in posbugindex
    #     nexthighestNBI <- negbugindex[which(negbugindex %in% k) + 1] # index position of k in negbugindex, add one, that value
    #     # will fail at last k because there isn't an k+1, hence length(-1) index in 'total'
    #     # unless nexthighestPBI is NA (may happen near end of series),
    #     # if nexthighestPBI < nexthighestNBI that should be a connected pair
    #     # record both values (k & nexthighestPBI)
    #     if (!is.na(nexthighestPBI)) if (nexthighestPBI < nexthighestNBI) NArangePairs[which(negbugindex %in% k), 2] <- nexthighestPBI
    #   }
    #   close(pb)
    #   rm(k)
    #   # for the last row, there is no nexthighestPBI, k should be retained, single entry
    #   # as long as it isn't NA: might give constant reads of e.g. 867.5 & have one final surface flick before
    #   # giving only deep values then crashing. So there's no matching surface return
    #   finalPBI <- posbugindex[which(posbugindex > negbugindex[total + 1])[1]]
    #   if (!is.na(finalPBI)) NArangePairs[nrow(NArangePairs), 2] <- finalPBI
    #   # Remove rows with NAs in "NAto"
    #   NArangePairs <- NArangePairs[which(!is.na(NArangePairs$NAto)),]
    #   # NArangePairs is a list of subsets of df_i to blank depth & LV for.
    #   # FromRow & BetweenRows: Depth & LV
    #   # ToRow: only LV
    #   # From & To are both row indexes of the same df_i. Make 2 vectors
    #   # LV: seq from to
    #   seq2 <- Vectorize(seq.default, vectorize.args = c("from", "to")) #Vectorise function again in case depth loop not run
    #   NArangePairs$NALV <- rep(NA, nrow(NArangePairs))
    #   NArangePairs$NALV <- seq2(from = NArangePairs$NAfrom, to = NArangePairs$NAto, by = 1)
    #   df_i[unlist(NArangePairs$NALV), "LtVelo"] <- NA
    #   # Light: seq from to-1
    #   NArangePairs$NAlight <- rep(NA, nrow(NArangePairs))
    #   NArangePairs$NAlight <- seq2(from = NArangePairs$NAfrom, to = (NArangePairs$NAto - 1), by = 1)
    #   df_i[unlist(NArangePairs$NAlight), "Lt"] <- NA
    #   rm(list = c("posbugindex", "negbugindex", "NArangePairs", "total", "pb", "nexthighestNBI", "nexthighestPBI", "finalPBI", "seq2")) # remove objects
    # } # close boolean length if

    #Flat light signal below threshold####
    # Arguably not an actual problem: light below threshold bottoms out. Can use the values.
    # Therefore comment out this section
    # # bump times backwards by 12 hours? currently night ~= 18:00 - 06:00 would be 06:00 to 18:00 same day
    # # tomorrow's morning now counts as today's night
    # df_i$DateTimeMinus12 <- df_i$DateTimeUTCmin5 - 12*60*60
    # df_i$DateNight <- date(df_i$DateTimeMinus12)
    # df_i$isday <- rep(TRUE, nrow(df_i))
    # #DateTimeMinus12 from 06:00 to 18:00 = FALSE i.e. night
    # df_i[hour(df_i$DateTimeMinus12) >= 6 & hour(df_i$DateTimeMinus12) < 18, "isday"] <- FALSE
    #
    # # create night stats for 4 criteria filter of bad light values at night
    # #Nightstats####
    # Nightstats <- df_i %>%
    #   group_by(DateNight, isday) %>%
    #   summarise(DNMinLight = min(Lt, na.rm = TRUE),
    #             DNMeanLight = mean(Lt, na.rm = TRUE),
    #             # DNMaxLight = max(Lt, na.rm = TRUE),
    #             # DNNApctLight = length(which(is.na(Lt))) / length(Lt) * 100,
    #             DNRangeDiffLight = diff(range(Lt, na.rm = T)),
    #             DNRangeDiffDepth = diff(range(Depth.m., na.rm = T)),
    #             DNMeanLV = mean(abs(LtVelo), na.rm = TRUE),
    #             DNMaxLV = max(abs(LtVelo), na.rm = TRUE)) %>%
    #   ungroup() %>% #Freq circumvents tag sampling interval difference problem
    #   pivot_wider(names_from = isday,
    #               values_from = c(DNMinLight,
    #                               DNMeanLight,
    #                               # DNMaxLight,
    #                               # DNNApctLight,
    #                               DNRangeDiffLight,
    #                               DNRangeDiffDepth,
    #                               DNMeanLV,
    #                               DNMaxLV)) %>%
    #   rename(MinLightNight = DNMinLight_FALSE, #used
    #          MinLightDay = DNMinLight_TRUE,
    #          MeanLightNight = DNMeanLight_FALSE, #used
    #          MeanLightDay = DNMeanLight_TRUE,
    #          # MaxLightNight = DNMaxLight_FALSE,
    #          # MaxLightDay = DNMaxLight_TRUE,
    #          # NApctLightNight = DNNApctLight_FALSE,
    #          # NApctLightDay = DNNApctLight_TRUE,
    #          RangeDiffLightDay = DNRangeDiffLight_TRUE,
    #          RangeDiffDepthDay = DNRangeDiffDepth_TRUE,
    #          MeanLVNight = DNMeanLV_FALSE, #used
    #          MeanLVDay = DNMeanLV_TRUE,
    #          MaxLVNight = DNMaxLV_FALSE, #used
    #          MaxLVDay = DNMaxLV_TRUE) %>%
    #   select(DateNight, MinLightNight, MeanLightNight, RangeDiffLightDay, RangeDiffDepthDay, MeanLVNight, MaxLVNight) #MaxLightNight, NApctLightNight, NApctLightDay,
    # is.na(Nightstats) <- do.call(cbind,lapply(Nightstats, is.infinite)) #convert Inf & -Inf to NA
    # is.na(Nightstats) <- do.call(cbind,lapply(Nightstats, is.nan)) #convert NaN to NA
    # Nightstats$MinLightNightU70 <- Nightstats$MinLightNight < 70 # boolean
    # Nightstats$MeanLightNightU95 <- Nightstats$MeanLightNight < 95
    # Nightstats$MaxLVNightO90 <- Nightstats$MaxLVNight > 90
    # Nightstats$MeanLVNightU.75 <- Nightstats$MeanLVNight < 0.75
    # Nightstats$Criteria4 <- apply(Nightstats[,c("MinLightNightU70",
    #                                             "MeanLightNightU95",
    #                                             "MaxLVNightO90", # max abs light velocity
    #                                             "MeanLVNightU.75")], #mean light velocity
    #                               1, #rows
    #                               function(x) length(which(x))) #sums TRUEs
    # NightCrit34 <- Nightstats[which(Nightstats$Criteria4 >= 3), "DateNight"] # Days scoring 3 or 4 should be NA'd
    # NightCrit34 <- NightCrit34$DateNight #remove from tibble to vector
    # # subset to alltrue dates AND isday=FALSE, NA out Lt
    # if (length(NightCrit34) != 0) {df_i[which(df_i$DateNight %in% NightCrit34 & !df_i$isday), "Lt"] <- NA}

    # Light stalk flatlined but depth variable (i.e. not legit isolume follow)
    # 5100132 2000-04-18 & onwards
    # Nightstats$RangeDiffLightDay < 40 $ Nightstats$RangeDiffDepthDay > 30?
    # DeadLightDays <- Nightstats[which(Nightstats$RangeDiffLightDay <= 22), "DateNight"] # Dates where light stalk range < 22 during day
    # This is wrong: using Nightstats's days means you're using half of today & half of tomorrow:
    # day1 1200-1800 & day2 0600-1200. Use daystats instead
    df_i$isday <- rep(FALSE, nrow(df_i))
    df_i[hour(df_i$DateTimeUTCmin5) >= 6 & hour(df_i$DateTimeUTCmin5) < 18, "isday"] <- TRUE

    Daystats <- df_i %>%
      group_by(Date, isday) %>%
      summarise(DNRangeDiffLight = diff(range(Lt, na.rm = T))) %>%
      ungroup() %>% #Freq circumvents tag sampling interval difference problem
      filter(isday) %>%
      select(-isday) %>%
      rename(RangeDiffLightDay = DNRangeDiffLight)
    is.na(Daystats) <- do.call(cbind,lapply(Daystats, is.infinite)) #convert Inf & -Inf to NA
    is.na(Daystats) <- do.call(cbind,lapply(Daystats, is.nan)) #convert NaN to NA

    DeadLightDays <- Daystats[which(Daystats$RangeDiffLightDay <= 22), "Date"] # Dates where light stalk range < 22 during day
    # L~590 will then kill the rest of the data based on NA%
    DeadLightDays <- DeadLightDays$Date #remove from tibble to vector
    if (length(DeadLightDays) != 0) {df_i[which(df_i$DateNight %in% DeadLightDays), "Lt"] <- NA}

    #Flat light signal 2nd pass####
    # # shrink time by 6 hours so it's only the middle of the night, not contaminated by the dawndusk boundary
    # df_i$isday <- rep(TRUE, nrow(df_i))
    # #DateTimeMinus12 from 09:00 to 15:00 = FALSE i.e. central night
    # df_i[hour(df_i$DateTimeMinus12) >= 9 & hour(df_i$DateTimeMinus12) < 15, "isday"] <- FALSE
    # #subset to isday = F since you don't need day values in the group summaries next
    # dfiNight <- df_i[which(!df_i$isday),]
    # ShortNightstats <- dfiNight %>%
    #   group_by(DateNight) %>%
    #   summarise(MinLight = min(Lt, na.rm = TRUE),
    #             MeanLight = mean(Lt, na.rm = TRUE),
    #             MeanLV = mean(abs(LtVelo), na.rm = TRUE)) %>%
    #   ungroup()
    # is.na(ShortNightstats) <- do.call(cbind,lapply(ShortNightstats, is.infinite)) #convert Inf & -Inf to NA
    # is.na(ShortNightstats) <- do.call(cbind,lapply(ShortNightstats, is.nan)) #convert NaN to NA
    #
    # ShortNightstats$MinLightU75 <- ShortNightstats$MinLight <= 75
    # ShortNightstats$MeanLightU95 <- ShortNightstats$MeanLight < 95
    # ShortNightstats$MeanLVU.17 <- ShortNightstats$MeanLV < 0.17
    # ShortNightstats$Criteria3 <- apply(ShortNightstats[,c("MinLightU75",
    #                                                       "MeanLightU95",
    #                                                       "MeanLVU.17")],
    #                                    1, #rows
    #                                    function(x) length(which(x))) #sums TRUEs
    # ShortNightCrit34 <- ShortNightstats[which(ShortNightstats$Criteria3 >= 3), "DateNight"] # Days scoring 3 or 4 should be NA'd
    # ShortNightCrit34 <- ShortNightCrit34$DateNight #remove from tibble to vector
    # # expand isday back up to 12 hours
    # df_i[hour(df_i$DateTimeMinus12) >= 6 & hour(df_i$DateTimeMinus12) < 18, "isday"] <- FALSE
    # # subset to alltrue dates AND isday=FALSE AND Lt < 100, NA out Lt
    # if (length(ShortNightCrit34) != 0) {df_i[which(df_i$DateNight %in% ShortNightCrit34 &
    #                                                  !df_i$isday &
    #                                                  df_i$Lt < 100),
    #                                          "Lt"] <- NA}
    #   rm(list = c("Nightstats", "NightCrit34", "ShortNightstats")) # remove objects

    #Daystats Daily summaries1####
    Daystats <- df_i %>%
      group_by(Date) %>%
      summarise(DailyMinDepth = min(Depth.m., na.rm = TRUE),
                DailyMeanDepth = mean(Depth.m., na.rm = TRUE),
                DailyMaxDepth = max(Depth.m., na.rm = TRUE), #first over 1? 2? 5? NA all before. Report first date.
                DailyRangeDiffDepth = diff(range(Depth.m., na.rm = T)),
                DailyVarDepth = var(Depth.m., na.rm = T),
                DailyNApctDepth = length(which(is.na(Depth.m.))) / length(Depth.m.) * 100,
                DailyMinLight = min(Lt, na.rm = TRUE),
                DailyMeanLight = mean(Lt, na.rm = TRUE),
                DailyMaxLight = max(Lt, na.rm = TRUE),
                DailyRangeDiffLight = diff(range(Lt, na.rm = T)),
                DailyNApctLight = length(which(is.na(Lt))) / length(Lt) * 100,
                DailyMinIntTemp = min(IntTemp.C., na.rm = TRUE),
                DailyMeanIntTemp = mean(IntTemp.C., na.rm = TRUE),
                DailyMaxIntTemp = max(IntTemp.C., na.rm = TRUE),
                DailyRangeDiffIntTemp = diff(range(IntTemp.C., na.rm = T)),
                DailyNApctIntTemp = length(which(is.na(IntTemp.C.))) / length(IntTemp.C.) * 100,
                DailyMinExtTemp = min(ExtTemp.C., na.rm = TRUE),
                DailyMeanExtTemp = mean(ExtTemp.C., na.rm = TRUE),
                DailyMaxExtTemp = max(ExtTemp.C., na.rm = TRUE),
                DailyRangeDiffExtTemp = diff(range(ExtTemp.C., na.rm = T)),
                DailyNApctExtTemp = length(which(is.na(ExtTemp.C.))) / length(ExtTemp.C.) * 100,
                DailyMinXsTemp = min(ExcessTemp.C., na.rm = TRUE),
                DailyMeanXsTemp = mean(ExcessTemp.C., na.rm = TRUE),
                DailyMaxXsTemp = max(ExcessTemp.C., na.rm = TRUE),
                DailyRangeDiffXsTemp = diff(range(ExcessTemp.C., na.rm = T)),
                DailyMinVV = min(abs(VertVelo), na.rm = T),
                DailyMeanVV = mean(abs(VertVelo), na.rm = T),
                DailyMaxVV = max(abs(VertVelo), na.rm = T),
                DailyRangeDiffVV = diff(range(abs(VertVelo), na.rm = T)),
                DailyMinTV = min(abs(TempVelo), na.rm = T),
                DailyMeanTV = mean(abs(TempVelo), na.rm = T),
                DailyMaxTV = max(abs(TempVelo), na.rm = T),
                DailyRangeDiffTV = diff(range(abs(TempVelo), na.rm = T))) %>%
      ungroup()
    is.na(Daystats) <- do.call(cbind,lapply(Daystats, is.infinite)) #convert Inf & -Inf to NA
    is.na(Daystats) <- do.call(cbind,lapply(Daystats, is.nan)) #convert NaN to NA

    # Unless all DailyMeanXsTemp are NA e.g. if no inttemp already e.g. PAT tags
    if (!all(is.na(Daystats$DailyMeanXsTemp))) {
      # NA out days where int temp & ext temp are the same, i.e. before tag on & after fish caught dead
      Daystats[which(Daystats$DailyMeanXsTemp < 0), "DailyMeanXsTemp"] <- NA #0.6 based on 5103487, could be 2 from that fish, check others
      # NA all IntTemps if they're reading the same as ExtTemp.
      if (max(Daystats$DailyMeanXsTemp, na.rm = TRUE) < 1) df_i$IntTemp.C. <- rep(NA, length(df_i$IntTemp.C.))
    }

    #Daystats cleaning####
    # DailyMaxDepth specifically 979.5####
    # NA out that whole day's Depth.m. 5103423 PAT tag. Look at other PAT tags
    NAdates <- Daystats[which(Daystats$DailyMaxDepth == 979.5), "Date"]
    NAdates <- NAdates$Date
    if (length(NAdates) != 0) {df_i[which(df_i$Date %in% NAdates), "Depth.m."] <- NA}

    # DailyMinDepth never under 200 = broken tag, NA those days
    NAdates <- Daystats[which(Daystats$DailyMinDepth >= 200), "Date"]
    NAdates <- NAdates$Date
    if (length(NAdates) != 0) {df_i[which(df_i$Date %in% NAdates), "Depth.m."] <- NA}

    #Get date_lasts from tables####
    date_last_depth <- as.Date(processing[processing$toppid == substr(i, 1, 7), "date_last_depth"])
    date_last_etemp <- as.Date(processing[processing$toppid == substr(i, 1, 7), "date_last_etemp"])
    date_last_itemp <- as.Date(processing[processing$toppid == substr(i, 1, 7), "date_last_itemp"])
    date_last_light <- as.Date(processing[processing$toppid == substr(i, 1, 7), "date_last_light"])
    # date_last_lon <- as.Date(processing[processing$toppid == substr(i, 1, 7), "date_last_lon"])

    # NA%test, based on NA percentages####
    # TS plots plot NA as 3 for exttemp?
    # Daystats na.rm=T hides NAs which conceals problems unless they're ALL NA. DailyNApct addresses this
    Daystats <- as.data.frame(Daystats) # remove awful tibble
    # last good row in daystats, can index to dates as daystats rownumber, date column
    date_last_depth <- min(date_last_depth, Daystats[last(which(Daystats$DailyNApctDepth < 10)), "Date"], na.rm = T)
    date_last_etemp <- min(date_last_etemp, Daystats[last(which(Daystats$DailyNApctExtTemp < 10)), "Date"], na.rm = T)
    date_last_itemp <- min(date_last_itemp, Daystats[last(which(Daystats$DailyNApctIntTemp < 10)), "Date"], na.rm = T)
    date_last_light <- min(date_last_light, Daystats[last(which(Daystats$DailyNApctLight < 10)), "Date"], na.rm = T)
    # if all are NA, need to ensure code NAs out everything, isn't expecting a start/end date
    # Values test
    date_last_depth <- min(Daystats[last(which(Daystats$DailyMaxDepth > 20)), "Date"],
                           Daystats[last(which(Daystats$DailyMaxDepth < 1600)), "Date"],
                           date_last_depth, na.rm = TRUE)
    # date_last_etemp <- min(Daystats[last(which(Daystats$DailyMinExtTemp > 7)), "Date"], # lowest legit 8.11 but adding safety buffer
    #                        date_last_etemp, na.rm = TRUE)
    date_last_itemp <- min(Daystats[last(which(Daystats$DailyMinIntTemp > 7)), "Date"], # lowest legit 8.11 but adding safety buffer
                           date_last_itemp, na.rm = TRUE)
    date_last_light <- min(Daystats[last(which(Daystats$DailyMinLight > 1)), "Date"], #Could make this 11 given lowest during deepdive was 12
                           date_last_light, na.rm = TRUE)

    # VertVelo test, NA out depth value corresponding over-high VV value in row (and over LOW/negative?)
    ## Test this: VV calc, NA test highs, VV calc2, excessive lows remain? If so make process: VV calc, NA test high & low, VVcalc2

    # if (is.na(date_last_itemp)) date_last_itemp <- first(df_i$Date) #means last GOOD date is the first day. Which is untrue. Leave as NA?
    # date_first_depth <- date_first_light <- date_first_itemp <- date_first_etemp <- first(df_i$Date) #initially set all firsts as first date

    date_first_depth <- Daystats$Date[first(which(Daystats$DailyMaxDepth > 5))] # 1st over 5?

    # NA out dead data after date_last_x even if the df is ongoing
    # Also kill ExcessTemp.C. after the last date of either int or ext, whichever's earlier
    df_i$Depth.m.[which(df_i$Date > date_last_depth)] <- NA
    df_i$Lt[which(df_i$Date > date_last_light)] <- NA # rownames(df_i) <- NULL if row numbers don't match
    df_i$ExtTemp.C.[which(df_i$Date > date_last_etemp)] <- NA # NA all after last working day
    df_i$ExtTemp.C.[which(df_i$Date == date_last_etemp & (df_i$ExtTemp.C. == 3 | df_i$ExtTemp.C. == 3.2))] <- NA # NA those on last working day which are 3 or 3.2
    df_i$IntTemp.C.[which(df_i$Date > date_last_itemp)] <- NA
    date_last_eitemp <- min(date_last_etemp, date_last_itemp, na.rm = T)
    df_i$ExcessTemp.C.[which(df_i$Date > date_last_eitemp)] <- NA

    # Crop df_i to date_last_all
    date_last_all <- max(date_last_depth,
                         date_last_etemp,
                         date_last_itemp,
                         date_last_light,
                         na.rm = TRUE)
    if (!is.infinite(date_first_depth) & !is.infinite(date_last_all)) {
      gooddepthdays <- interval(start = date_first_depth, end = date_last_all) #set interval
      df_i <- df_i[which(df_i$Date %within% gooddepthdays), ]
    } #subset

    # # Crop df_i again within days
    # # THIS KILLS ALL WORKING SENSORS AFTER DEPTH SENSOR DIES!!!
    # time_first_depth <- df_i$DateTimeUTCmin5[first(which(df_i$Depth.m. > 5))] # 1st over 5?
    # time_last_depth <- df_i$DateTimeUTCmin5[last(which(df_i$Depth.m. > 5))] # last over 5?
    # # irrelevant for buddy depth sensors but should catch most normal tags which are caught, popup or wash up.
    # if (!is.infinite(time_first_depth) & !is.infinite(time_last_depth)) {
    #   gooddepthtimes <- interval(start = time_first_depth, end = time_last_depth) #set interval
    #   df_i2 <- df_i[which(df_i$DateTimeUTCmin5 %within% gooddepthtimes), ]
    # }#subset

    #save date_*st_sensor to tblfgprocessingstatus####
    #won't work in parallel####
    if (!exists("sensordates")) { #if this is the first i, create sensordates object
      sensordates <- data.frame(eventid = strsplit(i, split = "_")[[1]][1], # eventid_pttnumber = folder/file names in mola
                                pttnumber = strsplit(i, split = "_")[[1]][2],
                                date_first_depth = date_first_depth, #was time_first_depth but now killed, also below
                                date_last_depth = date_last_depth, #was time_last_depth but now killed, also below
                                date_last_light = date_last_light,
                                date_last_itemp = date_last_itemp,
                                date_last_etemp = date_last_etemp,
                                stringsAsFactors = FALSE)
    } else { #else add to existing object
      sensordatesadd <- data.frame(eventid = strsplit(i, split = "_")[[1]][1], # have to create df first else it'll fail with
                                   pttnumber = strsplit(i, split = "_")[[1]][2], #Error in as.POSIXlt.character(x, tz, ...) :
                                   date_first_depth = date_first_depth, #character string is not in a standard unambiguous format
                                   date_last_depth = date_last_depth,
                                   date_last_light = date_last_light,
                                   date_last_itemp = date_last_itemp,
                                   date_last_etemp = date_last_etemp,
                                   stringsAsFactors = FALSE)
      sensordates <- rbind(sensordates,
                           sensordatesadd,
                           stringsAsFactors = FALSE) # add to existing file
    } #close if else

    df_i <- df_i[, 1:10] # remove VVspike VVbugPos VVbugNeg VVrowbefore VVrowafter NeighbourRowMeans RowPctMean LVspike LVbugPos LVbugNeg DateTimeMinus12 DateNight isday
    df_i$Index <- 1:nrow(df_i) # Create index to rejoin later. Lowercase name bad as its a zoo function
    # Save df_i####
    saveRDS(object = df_i, file = paste0(saveloc, i))


    # molasave section####
    if (molasave) { #also save output as csv to mola
      # Date(EST)	Time(EST)	Depth(m)	Lt	IntTemp(C)	ExtTemp(C)	Julian(EST)	Year	Month	Day	Hour	Min	Sec
      eventid_ptt <- paste0(strsplit(i, split = "_")[[1]][1], "_", strsplit(i, split = "_")[[1]][2])
      mfr <- switch(str_sub(i, -7, -7), "L" = "ltd2310/", "N" = "NMT/", "P" = "PAT/", "W" = "WC/") # convert LNPW to folder names
      if (eventid_ptt %in% PATpopups) mfr <- "PAT/popup/" # lookup PAT popups against list and force into popup subfolder
      dir.create(paste0(saveloc, "molasave/", mfr,  eventid_ptt, "/processing/"), recursive = T) # create directory & all parent directories to it
      write.csv(df_i, file = paste0(saveloc, "molasave/", mfr,  eventid_ptt, "/processing/", eventid_ptt, "_clean.csv"), row.names = F)
      # "/mnt/TOPP/Tuna/ABFT/Recovery/WC/5100127_98507/processing/5100127_98507_clean.csv" # file 6
    } # close molasave


    rm(list = c("Daystats", "df_i", "date_first", "date_first_depth",
                "date_last_all", "date_last_depth", "date_last_etemp",
                "date_last_itemp", "date_last_light", "gooddepthdays",
                "gooddepthtimes", "time_first_depth", "time_last_depth",
                "NAdates", "eventid_ptt", "mfr"))
    gc()

  } #close i loop
  # close(pb)

  if (!loadlistcompare) { # only do sensordates if running all files else it'll overwrite with only the few processed files not already done
    setwd(saveloc)
    dir.create("sensordates")
    setwd("sensordates")
    write.csv(sensordates, file = "sensordates.csv", row.names = F) #save
  }

  rm(list = ls()) #remove all objects
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  gc()
} # close function
# afterwards: join all dfs as big.table. Read csvs in with fread save with fwrite (or saveRDS)
