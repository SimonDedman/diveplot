# Simon Dedman simondedman@gmail.com started 2019.06.17
# 24h day, night & day stats
# cmpfun(Daynightstats)
# Daynightstats(loadlistcompare = FALSE) # or Daynightstats(machine = "/media/Seagate/Work/")
Daynightstats <- function(machine = "/home/simon/Documents/Si Work/", loadlistcompare = TRUE) {
  library(lubridate)
  library(dplyr) #for lag else uses stats version. & first
  library(tidyr) #pivot_wider
  library(magrittr) # for toppid line at bottom
  library(beepr)
  library(stringr)
  library(foreach)
  library(doMC)
  # library(compiler)
  options(error = function() beep(9))  # give warning noise if it fails

  # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/7_DO2_append/") #ensure trailing /slash
  saveloc = paste0(machine, "Blocklab/abft_diving/8_Daynightstats/") #ensure trailing /slash
  whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #

  setwd(loadloc) #run all as chunk
  loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above

  #loadlist comparison chunk####
  if (loadlistcompare) {
    setwd(saveloc)
    savelist <- list.files(pattern = whichfiles) #list files based on specified pattern above
    print("Archival tags in loadlist but not savelist, to be processed")
    print(loadlist[which(!loadlist %in% savelist)])
    loadlist <- loadlist[which(!loadlist %in% savelist)] # update loadlist for rerun
    setwd(loadloc)
  } # close loadlistcompare

  if (!length(loadlist) == 0) { # only run the file processing code if there are files to be processed.
    # mycores <- 8 #Nautilus
    # # mycores <- 16 #Poseidon
    # registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
    # foreach(i = loadlist) %dopar% {
    for (i in loadlist) { #sequential # i <- loadlist[1] # loadlist  i<- loadlist[82]
      df_i <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object
      print(paste0(which(loadlist == i), " of ", length(loadlist), "; calculating Daynightstats for ", i))

      # Hour stats for DepthRangeHr for Hrs50mLesDepRange
      df_i$HourFloor <- floor_date(df_i$DateTimeUTCmin5, unit = "hour") #overwrites existing column only for next line
      Hourstats <- df_i %>%  #mutate(HourFloor = floor_date(DateTimeUTCmin5, unit = "hour")) %>%
        group_by(HourFloor) %>%
        summarise(DepthRangeHr = max(Depth.m., na.rm = TRUE) - min(Depth.m., na.rm = TRUE)) %>%
        ungroup()
      df_i[duplicated(df_i$HourFloor),"HourFloor"] <- NA  # remove duplicates, column back as it was
      df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
                        y = Hourstats)
      rm(Hourstats)

      # Day stats
      Daystats <- df_i %>%
        group_by(Date) %>%
        summarise(lat = mean(lat, na.rm = T),
                  lon = mean(lon, na.rm = T),
                  age = mean(age, na.rm = T),
                  dawn = mean(dawn, na.rm = T), # mean, but only 1 per day, just excludes NAs  #needs SpikeDive loop
                  sunrise = mean(sunrise, na.rm = T),  #needs SpikeDive loop
                  sunset = mean(sunset, na.rm = T), #needs SpikeDive loop
                  dusk = mean(dusk, na.rm = T), #needs SpikeDive loop
                  daylength = mean(daylength, na.rm = T), #needs SpikeDive loop
                  DawnSpike = mean(DawnSpike, na.rm = T), #needs SpikeDive loop
                  DuskSpike = mean(DuskSpike, na.rm = T), #needs SpikeDive loop
                  SpikeDives = mean(SpikeDives, na.rm = T), #needs SpikeDive loop
                  DO2 = mean(DO2, na.rm = T), #needs DO2 loop
                  MinDepth24h = min(Depth.m., na.rm = T),  # 2020.03.25
                  MeanDepth24h = mean(Depth.m., na.rm = T),
                  MaxDepth24h = max(Depth.m., na.rm = T),
                  NaPctDepth24h = length(which(is.na(Depth.m.))) / length(Depth.m.) * 100, # 0:100
                  Hrs50mLesDepRange = sum(DepthRangeHr <= 50, na.rm = T), #0:24
                  MeanLt24h = mean(Lt, na.rm = T),
                  NaPctLt24h = length(which(is.na(Lt))) / length(Lt) * 100, # 0:100
                  MaxITemp24h = max(IntTemp.C., na.rm = T),
                  MeanITemp24h = mean(IntTemp.C., na.rm = T),
                  MinITemp24h = min(IntTemp.C., na.rm = T),
                  RangeITemp24h = diff(range(IntTemp.C., na.rm = T)),
                  NaPctITemp24h = length(which(is.na(IntTemp.C.))) / length(IntTemp.C.) * 100, # 0:100
                  MaxETemp24h = max(ExtTemp.C., na.rm = T),
                  MeanETemp24h = mean(ExtTemp.C., na.rm = T),
                  MeanETemp24hU1M = mean(ExtTemp.C.[Depth.m. <= 1], na.rm = T), # 2020.03.25
                  MeanETemp24hU2M = mean(ExtTemp.C.[Depth.m. <= 2], na.rm = T), # 2020.03.25
                  MinETemp24h = min(ExtTemp.C., na.rm = T),
                  NaPctETemp24h = length(which(is.na(ExtTemp.C.))) / length(ExtTemp.C.) * 100, # 0:100
                  MaxXsTemp24h = max(ExcessTemp.C., na.rm = T),
                  MeanXsTemp24h = mean(ExcessTemp.C., na.rm = T),
                  MinXsTemp24h = min(ExcessTemp.C., na.rm = T),
                  MeanVV24h =  mean(abs(VertVelo), na.rm = T),
                  MaxVV24h = max(abs(VertVelo), na.rm = T),
                  MeanTV24h = mean(abs(TempVelo), na.rm = T),
                  MaxTV24h = max(abs(TempVelo), na.rm = T),
                  SurfFreq24h = table(cut(Depth.m., breaks = c(-Inf,10,Inf)))[[1]]/length(Depth.m.), # Freq visits 2 surface= frequency of depth <= 10m/ all depths
                  MinSST24h = min(mean_sst_gl, na.rm = T), # needs ILDtableJoinLoop
                  MeanSST24h = mean(mean_sst_gl, na.rm = T), # needs ILDtableJoinLoop
                  MaxSST24h = max(mean_sst_gl, na.rm = T), # needs ILDtableJoinLoop
                  MeanILD24h = mean(ild_depth_gl, na.rm = T), # needs ILDtableJoinLoop
                  MeanILDtemp24h = mean(ild_temp_gl, na.rm = T), # needs ILDtableJoinLoop
                  ild_dive_cnt_desc_gl_midnight = first(ild_dive_cnt_desc_gl), #00:00 covers 00:00-06:00 value only; needs ILDtableJoinLoop
                  ild_dive_cnt_desc_gl_Sum = sum(ild_dive_cnt_desc_gl, na.rm = T)) %>% # needs ILDtableJoinLoop
        ungroup()

      is.na(Daystats) <- do.call(cbind,lapply(Daystats, is.infinite)) #convert inf & -inf to NA
      is.na(Daystats) <- do.call(cbind,lapply(Daystats, is.nan)) #convert inf & -inf to NA

      if (all(is.na(df_i$isday))) { #if all isday is NA, can't do daynightstats normally so popoulate NA blanks
        Daynightstats <- data.frame(Date = as.Date(Daystats$Date),
                                    MeanDepthNight = rep(NA, length(Daystats$Date)),
                                    MeanDepthDay = rep(NA, length(Daystats$Date)),
                                    MaxDepthNight = rep(NA, length(Daystats$Date)),
                                    MaxDepthDay = rep(NA, length(Daystats$Date)),
                                    NaPctDepthNight = rep(NA, length(Daystats$Date)),
                                    NaPctDepthDay = rep(NA, length(Daystats$Date)),
                                    MeanLtNight = rep(NA, length(Daystats$Date)),
                                    MeanLtDay = rep(NA, length(Daystats$Date)),
                                    NaPctLtNight = rep(NA, length(Daystats$Date)),
                                    NaPctLtDay = rep(NA, length(Daystats$Date)),
                                    MinITempNight = rep(NA, length(Daystats$Date)),
                                    MinITempDay = rep(NA, length(Daystats$Date)),
                                    RangeITempNight = rep(NA, length(Daystats$Date)),
                                    RangeITempDay = rep(NA, length(Daystats$Date)),
                                    NaPctITempNight = rep(NA, length(Daystats$Date)),
                                    NaPctITempDay = rep(NA, length(Daystats$Date)),
                                    MeanETempNight = rep(NA, length(Daystats$Date)),
                                    MeanETempDay = rep(NA, length(Daystats$Date)),
                                    MeanETempU2MNight = rep(NA, length(Daystats$Date)),
                                    MeanETempU2MDay = rep(NA, length(Daystats$Date)),
                                    MinETempNight = rep(NA, length(Daystats$Date)),
                                    MinETempDay = rep(NA, length(Daystats$Date)),
                                    NaPctETempNight = rep(NA, length(Daystats$Date)),
                                    NaPctETempDay = rep(NA, length(Daystats$Date)),
                                    MeanVVNight = rep(NA, length(Daystats$Date)),
                                    MeanVVDay = rep(NA, length(Daystats$Date)),
                                    MaxVVNight = rep(NA, length(Daystats$Date)),
                                    MaxVVDay = rep(NA, length(Daystats$Date)),
                                    MeanTVNight = rep(NA, length(Daystats$Date)),
                                    MeanTVDay = rep(NA, length(Daystats$Date)),
                                    MaxTVNight = rep(NA, length(Daystats$Date)),
                                    MaxTVDay = rep(NA, length(Daystats$Date)),
                                    SurfFreqNight = rep(NA, length(Daystats$Date)),
                                    SurfFreqDay = rep(NA, length(Daystats$Date)))
      } else { # all isday is NOT NA, CAN do daynightstats normally so do so
        Daynightstats <- df_i %>%
          group_by(Date, isday) %>%
          summarise(DNMeanDepth = mean(Depth.m., na.rm = T), # depthday_mean, depthnight_mean
                    DNMaxDepth = max(Depth.m., na.rm = T), # depthday_max, depthnight_max
                    DNNAPctDepth = length(which(is.na(Depth.m.))) / length(Depth.m.) * 100, # 0:100
                    DNMeanLt = mean(Lt, na.rm = T), # lightday_mean, lightnight_mean
                    DNNAPctLt = length(which(is.na(Lt))) / length(Lt) * 100, # 0:100
                    DNMinITemp = mean(IntTemp.C., na.rm = T), # inttempday_min, inttempnight_min
                    DNRangeITemp = diff(range(IntTemp.C., na.rm = T)), # inttempday_range, inttempnight_range. distance of min to max nightly internal temps
                    DNNAPctITemp = length(which(is.na(IntTemp.C.))) / length(IntTemp.C.) * 100, # 0:100
                    DNMeanETemp = mean(ExtTemp.C., na.rm = T), # tempday_mean, tempnight_mean
                    DNMeanETempU2M = mean(ExtTemp.C.[Depth.m. <= 2], na.rm = T), # tempday_mean, tempnight_mean
                    DNMinETemp = mean(ExtTemp.C., na.rm = T), # inttempday_min, inttempnight_min
                    DNNAPctETemp = length(which(is.na(ExtTemp.C.))) / length(ExtTemp.C.) * 100, # 0:100
                    DNMeanVV = mean(abs(VertVelo), na.rm = T), # vvday_mean, vvnight_mean
                    DNMaxVV = max(abs(VertVelo), na.rm = T), # vvday_max, vvnight_max
                    DNMeanTV = mean(abs(TempVelo), na.rm = T), # tvday_mean, tvnight_mean / tempchange_day, tempchange_night
                    DNMaxTV = max(abs(TempVelo), na.rm = T), # vvday_max, vvnight_max
                    DNSurfFreq = table(cut(Depth.m., breaks = c(-Inf,10,Inf)))[[1]]/length(Depth.m.)) %>% #surfaceday_freq, surfacenight_freq.
          ungroup() %>% #Freq circumvents tag sampling interval difference problem
          pivot_wider(names_from = isday,
                      values_from = c(DNMeanDepth,
                                      DNMaxDepth,
                                      DNNAPctDepth,
                                      DNMeanLt,
                                      DNNAPctLt,
                                      DNMinITemp,
                                      DNRangeITemp,
                                      DNNAPctITemp,
                                      DNMeanETemp,
                                      DNMeanETempU2M,
                                      DNMinETemp,
                                      DNNAPctETemp,
                                      DNMeanVV,
                                      DNMaxVV,
                                      DNMeanTV,
                                      DNMaxTV,
                                      DNSurfFreq)) %>%
          rename(MeanDepthNight = DNMeanDepth_FALSE,
                 MeanDepthDay = DNMeanDepth_TRUE,
                 MaxDepthNight = DNMaxDepth_FALSE,
                 MaxDepthDay = DNMaxDepth_TRUE,
                 NaPctDepthNight = DNNAPctDepth_FALSE,
                 NaPctDepthDay = DNNAPctDepth_TRUE,
                 MeanLtNight = DNMeanLt_FALSE,
                 MeanLtDay = DNMeanLt_TRUE,
                 NaPctLtNight = DNNAPctLt_FALSE,
                 NaPctLtDay = DNNAPctLt_TRUE,
                 MinITempNight = DNMinITemp_FALSE,
                 MinITempDay = DNMinITemp_TRUE,
                 RangeITempNight = DNRangeITemp_FALSE,
                 RangeITempDay = DNRangeITemp_TRUE,
                 NaPctITempNight = DNNAPctITemp_FALSE,
                 NaPctITempDay = DNNAPctITemp_TRUE,
                 MeanETempNight = DNMeanETemp_FALSE,
                 MeanETempDay = DNMeanETemp_TRUE,
                 MeanETempU2MNight = DNMeanETempU2M_FALSE,
                 MeanETempU2MDay = DNMeanETempU2M_TRUE,
                 MinETempNight = DNMinETemp_FALSE,
                 MinETempDay = DNMinETemp_TRUE,
                 NaPctETempNight = DNNAPctETemp_FALSE,
                 NaPctETempDay = DNNAPctETemp_TRUE,
                 MeanVVNight = DNMeanVV_FALSE,
                 MeanVVDay = DNMeanVV_TRUE,
                 MaxVVNight = DNMaxVV_FALSE,
                 MaxVVDay = DNMaxVV_TRUE,
                 MeanTVNight = DNMeanTV_FALSE,
                 MeanTVDay = DNMeanTV_TRUE,
                 MaxTVNight = DNMaxTV_FALSE,
                 MaxTVDay = DNMaxTV_TRUE,
                 SurfFreqNight = DNSurfFreq_FALSE,
                 SurfFreqDay = DNSurfFreq_TRUE) %>%
          select(-contains("_NA"))  #NAs isday's create NA summary columns
        is.na(Daynightstats) <- do.call(cbind,lapply(Daynightstats, is.infinite)) #convert inf & -inf to NA
        is.na(Daynightstats) <- do.call(cbind,lapply(Daynightstats, is.nan)) #convert inf & -inf to NA
      } # close if else all NA daystats
      #number of dives would be useful####
      # Need path analysis, vertex removal: edelhoff2016path 'line simplification'
      # [12,133 ]

      Daynightstats <- left_join(x = Daystats,
                                 y = Daynightstats,
                                 by = "Date")

      #Save individuals, append alldailies####
      Daynightstats$Stock <- as.character(str_sub(i, -6,-6)) # penultimate letter before .Rds: TF.Rds)
      Daynightstats$fishID <- str_remove(i, whichfiles) #get fishID add as column
      saveRDS(object = Daynightstats, file = paste0(saveloc,i))
      rm(list = c("df_i", "Daynightstats", "Daystats")) # remove objects
    } #close i
  } # close if there are files to be processed if

  setwd(saveloc) # so list.files pulls the daynightstats summary Rds's not the DO2 minutely Rds's
  mycores <- 8 #Nautilus
  # mycores <- 16 #Poseidon
  registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
  catf <- function(..., file="log.txt", append=TRUE){cat(..., file = file, append = append)} # avoids sink stack full error https://stackoverflow.com/a/34254839/3975144
  print("Building AllDailies_Daynightstats object, will ingest any new satellite tag files")
  all_dfs <- foreach(j = list.files(pattern = whichfiles),
                     .errorhandling = "pass",
                     .combine = rbind,
                     .inorder = TRUE,
                     .multicombine = TRUE) %dopar% { # j <- list.files(pattern = whichfiles)[1]
                       # for (j in list.files(pattern = whichfiles)) { #Original input file list. sequential # j <- list.files(pattern = whichfiles)[1]
                       catf(paste("\n","Starting iteration",j,"\n")) # avoids sink stack full error https://stackoverflow.com/a/34254839/3975144
                       eachRdsFile <- readRDS(j) #assign file to name, prepend with x to avoid numerical named object
                       # print(paste0(which(list.files(pattern = whichfiles) == j), " of ", length(list.files(pattern = whichfiles)), "; appending AllDailies_Daynightstats.Rds with ", j))
                       # if (!exists("all_dfs")) { #if this is the first j, create all_dfs object
                       #   all_dfs <- eachRdsFile
                       # } else { #else add eachRdsFile to existing object
                       #   all_dfs <- rbind(all_dfs, eachRdsFile) # add to existing file
                       #   gc()
                       # } #close if else
                     } #close j

  alldfscolnames <- colnames(all_dfs) # needed for later in pipe
  alldfsposixcolnames <- all_dfs %>% select(where(is.POSIXt)) %>% colnames # ditto
  all_dfs %<>%
    mutate(Index = 1:nrow(.), # add Index col
           toppid = as.numeric(str_sub(fishID, 1, 7))) %>% # add toppid
    # There are multiple duplicate/dupe dfs from the same fish when doubletagged.
    # These need to be collapsed to one row per fishday.
    group_by(toppid, Date) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE),
              across(where(~ is.character(.) | is.POSIXt(.)), first)) %>% # library(lubridate)
    # across(where(is.list),  ~ list(unlist(., recursive = FALSE))) # need the first non null element of the list. WORKS.
    # From https://stackoverflow.com/questions/16896376/extract-non-null-elements-from-a-list-in-r
    # Don't have any list columns at this stage so shouldn't be needed ever.
    select(toppid, alldfscolnames, Index) %>% # reorder cols to match original
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)), #convert NaN to NA. POSIX needs lubridate
           across(where(~ is.character(.)), ~ ifelse(is.nan(.), NA, .))) # https://community.rstudio.com/t/why-does-tidyrs-fill-work-with-nas-but-not-nans/25506/5
  #  | is.POSIXt(.) no nans in this column so not required and may cause the conversion to numeric
  # mutate(across(all_of(alldfsposixcolnames), as.POSIXct, tz = "America/New_York", origin = "1970-01-01")) %>%

  all_dfs <- as.data.frame(all_dfs) #remove tibble, so future stuff isn't broken
  saveRDS(object = all_dfs, file = paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_Daynightstats.Rds"))
  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  gc() #garbage collection, free up memory
} # close function
