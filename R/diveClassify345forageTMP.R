#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simon Dedman simondedman@gmail.com started 2019.06.13
# 1. manually classify each of 8 dive types (hereafter BB8):
#     1. transition/transit/travel
#     2. glide
#     3. oscillation / Forage dive A: 100m depth, v. steep thermocline
#     4. oscillationB: 300m depth, depth thermocline to bottom of dive profile
#     5. oscillationC: To ocean bottom: feeding on cod etc.
#     6. Diel Vertical Migration DVM
#     7. Deep dive
#     8. Spike dive, sunrise/sunset
# Will likely require additional secondary data e.g. sunset/sunrise/day/night

# 2. Test classification skill against plots, and Wilson2009 where applicable
# 3. Tweak 1 as required until satisfied. Devise algorithmic rules for tweaking
# 4. Automatically classify N divetypes using TOOLS: collate notes, choose candidate tools
# 5. Run 4 & compare to BB8. Different?
#    YES: are differences justifiable?
#         YES: describe new types, formalise the process of testing?
#         NO: Tweak 4, rerun
#    NO: Continue
# 6. Run 4 on ABFT in loop, save
# 7. Test against PBFT, Marlin, etc: descending similarity
# 8. Release as package?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cmpfun(ForageDives)
# ForageDives() # or ForageDives(machine = "/media/Seagate/Work/")
ForageDives <- function(machine = "/home/simon/Documents/Si Work/", loadlistcompare = TRUE) { #run whole thing (as function)
  library(zoo) # na.locf
  library(lubridate)
  library(stringr)
  library(dplyr)
  library(tidyr) #pivot_wider
  library(magrittr)
  library(beepr)
  library(purrr)
  library(parallel)
  library(doMC)
  options(error = function() beep(9))  # give warning noise if it fails

  # machine <- "/home/simon/Documents/Si Work/" #Nautilus
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/6_GL_ILD_append/") #per saveloc in MolaFoldersExtractLoop.R
  saveloc = paste0(machine, "Blocklab/abft_diving/10_ForageDive/") #ensure trailing /slash
  whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #
  setwd(loadloc)
  loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above
  AllDailies <- readRDS(paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_DeepDive.Rds")) #for OceanDepth Shelf & join later

  #loadlist comparison chunk####
  if (loadlistcompare) {
    setwd(saveloc)
    savelist <- list.files(pattern = whichfiles) #list files based on specified pattern above
    print("in loadlist but not savelist, to be processed")
    print(loadlist[which(!loadlist %in% savelist)])
    loadlist <- loadlist[which(!loadlist %in% savelist)] # update loadlist for rerun
    setwd(loadloc)
  } # close loadlistcompare

  mycores <- 8 #Nautilus
  # mycores <- 16 #Poseidon
  registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
  catf <- function(..., file = paste0(saveloc, "log.txt"), append = TRUE){cat(..., file = file, append = append)} # avoids sink stack full error https://stackoverflow.com/a/34254839/3975144
  # for (i in loadlist) { #sequential # i <- loadlist[1]
  foreach(i = loadlist) %dopar% { # i <- loadlist[1]
    catf(paste("\n","Starting iteration",i,"\n")) # avoids sink stack full error https://stackoverflow.com/a/34254839/3975144
    # print(paste0(which(loadlist %in% i), " of ", length(loadlist), "; ", i)) #progress printer
    df_i <- readRDS(i)

    # check polling rate####
    time1 <- df_i$DateTimeUTCmin5[1:(nrow(df_i) - 1)] #time except last row
    time2 <- df_i$DateTimeUTCmin5[2:(nrow(df_i))] # time except first
    pr <- mean(as.numeric(difftime(time1 = time2, time2 = time1, units = "mins")), na.rm = TRUE) #metres/min
    # pr = polling rate in minutes
    if (pr < 1.9) {
      # downsample to 2m
      df_i$twomin <- floor_date(df_i$DateTimeUTCmin5, unit = "2 minutes")
      df_i <- df_i %>%
        group_by(twomin) %>% # group_by(floor_date(df_i$DateTimeUTCmin5, unit = "2 minutes"))
        summarise_all(first) %>%
        ungroup() %>%
        select(-twomin)
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
    } # close pr if
    # should have high average absolute VertVelo e.g. per hour
    # average vert velo per day vs per night?
    # need to interpolate e.g. vvday to whole day section. Already have meanVV24h

    # Greg used 10m as threshold
    # Barb dscussed other depths, 5, 15 20 etc. Potentially could assess number against visuals
    # Possibly different depths depending on SST? mean daily sst? within day period?

    # New England Forage diving:  SST < 15°C, SBT <10°C
    # Spawning Forage diving: >23°C SST, lots of diving: 20 dives in 6 hours.
    # Spawning at night. Reverse top hat behaviour at dawn for spawning fish after
    # a night of spawning, ask James/Barb for the ‘Golden Fish’, best tags for these
    # data. [asked 2019/06/24]


    # U-shaped dives####
    # Simply find the negative space within the dive profile:
    # surface, >150m for >30mins, surface
    df_i$O10m <- df_i$Depth.m. > 10
    # pr should be 2 mins so TRUE vectors over length 30/2=15 qualify
    tmprle <- rle(df_i$O10m)
    tmprle <- data.frame(values = tmprle$values,
                         lengths = tmprle$lengths) #in 2 min rows - remember this script doesn't save dfi_i for use later!
    if (nrow(tmprle) > 1) { # skip if only FALSE row i.e. no TRUE rows as well i.e. no Udives
      tmprle %<>%
        rowwise() %>%
        mutate(O10mO30min = all(lengths >= 15, #30 mins
                                values)) %>%
        ungroup()
      if (any(tmprle$O10mO30min)) { # if no Udives >= 15 mins, skip to end of both ifs
        tmprle$lastrow <- cumsum(tmprle$lengths) # final row of each section
        tmprle$midpointrow <- floor(tmprle$lastrow - (tmprle$lengths / 2)) # rounded down midpoint row of each section
        # Can use this to get time thus position a label on TS plot
        # Urowslengths <- as.data.frame(tmprle[tmprle$O10mO30min, c("midpointrow", "lengths")]) # untibble
        # what do I want, midpoint row time,
        # make a range of rows to get mean depth or U bottom (& for times?)
        tmprle$firstrow <- tmprle$lastrow - tmprle$lengths + 1 #firstrow:cumsumlength is row range

        tmprle <- tmprle[tmprle$O10mO30min,] # remove blanks

        tmprle <- cbind(tmprle, df_i[tmprle$midpointrow, "Date"])
        # tmprle$Date <- df_i[tmprle$midpointrow, "Date"] # for some bizarre reason this col gets named Date.Date on the 77th loop!
        tmprle <- cbind(tmprle, df_i[tmprle$midpointrow, "DateTimeUTCmin5"]) # same nonsense
        #need this to be second of day not posix which causes nightmares later####
        colnames(tmprle)[7:8] <- c("Date", "Umidtime") #requiring renaming in a tibble/df shared way since it'll fail differently on the normal dfs otherwise
        # tmprle$Umidtime <- tmprle$Umidtime - floor_date(tmprle$Umidtime, unit = "days") # convert from POSIX to mins of the day
        tmprle$Umidtime <- difftime(tmprle$Umidtime, floor_date(tmprle$Umidtime, unit = "days"), units = "mins") # convert from POSIX to mins of the day
        # tmprle$Umidtime <- make_difftime(tmprle$Umidtime, units = "mins") # force mins if not already? Just divides mins by 60 to make hours instead!

        tmprle$UdurMins <- tmprle$lengths * 2

        # applyify this section, incomplete####
        # tmprle %<>% # successfully makes a list col of start:end rows
        #   rowwise() %>%
        #   mutate(Udiverow = list(firstrow:lastrow)) %>% # start:end pairs in list
        #   ungroup()
        #
        # tmprle$Umeandepth <- as.numeric(rep(NA, nrow(tmprle)))
        #
        #
        # df_i$Udiverow <- rep(FALSE, nrow(df_i)) # blank (false) column
        # df_i[unlist(tmprle$Udiverow), "Udiverow"] <- TRUE # unlisted start:end pairs = all Udiverows in df_i, now tagged
        # # then which() each df_i row for its membership of a tmprle row, and tag another column with THAT
        # rownames(df_i[which(df_i$Udiverow),]) # same as unlist(tmprle$Udiverow) but as.character
        # sapply(tmprle$Udiverow, "[", 1)
        # between(49050, tmprle$firstrow[1], tmprle$lastrow[1])
        # then group by that col and mean the depths
        # could I group_by df_i using tmprle$Udiverow directly??
        # label each subsequent T/F in tmprle before removing falses then....
        ## group by....
        # lengths of row vectors in Udiverow, fromfirstrow to lastrow (is just 'lengths')
        # can I EXPAND (T/F & Index) by length to get a df_i-length vector of T/F & index? THen cbind df_i & group_by?
        # you already HAVE a df_i length vector of T/F, it's df_i$O150m
        # rowwise tmprle, RBIND (newdf colA = unlist Udiverow, colB = row index)
        # tmprleindex <- data.frame(dficol = as.numeric(NA),
        #                           midpointrowgp = as.numeric(NA))
        #
        # tmprle %>% # successfully makes a list col of start:end rows
        #   rowwise() %>%
        #   mutate(tmprleindex$dficol <- rbind(tmprleindex$dficol, unlist(Udiverow)),
        #          tmprleindex$midpointrowgp <- rbind(tmprleindex$midpointrowgp, midpointrow)) %>% # start:end pairs in list
        #   ungroup()
        #
        # tmprle %>%
        #   rowwise() %>%
        #   tmprleindex$dficol <- rbind(unlist(Udiverow)) %>%
        #   tmprleindex$midpointrowgp <- rbind(midpointrow) %>% # start:end pairs in list
        #   ungroup()
        #
        # tmprle %>%
        #   rowwise() %>%
        #   rep(.$midpointrow, .$lengths) # In rep(., .$midpointrow, .$lengths) : first element used of 'length.out' argument
        # # using rep(tmprle,) whole df not midpointrow
        #
        # tmprle %>%
        #   rowwise() %>%
        #   rep("midpointrow", .$lengths)

        # for loop fallback
        for (j in 1:nrow(tmprle)) { # had to remove tibble despite it working for 76 loops
          tmprle$Umeandepth[j] <- mean(as.data.frame(df_i[(tmprle$firstrow[j]:tmprle$lastrow[j]), "Depth.m."])[,1], na.rm = T)
        } # close for j

        tmprle %<>%
          select(7:10) %>% # only keep last 4
          filter(Umeandepth >= 150) %>%
          group_by(Date) %>% # group_by date & summarise each as a list of itself
          summarise(Umidtime = list(Umidtime), #is POSIXct but looks like numeric in RStudio data viewer? Converted to mins above, so not POSIX? Added to Posix in TSplots
                    UdurMins = list(UdurMins),
                    Umeandepth = list(round(Umeandepth)))
        # append to alldailies as a list vector as I did the others then use it in TSplots (near end)
      } # close any tmprle$O10mO30min
    } # close if nrow tmprle

    # Wilson 2009: V-shaped profiles — irregular profiles with frequent Forage dives and no obvious diel patterns

    #3 oscillationA####
    # 100m depth, v. steep thermocline
    # max(depth) <= 100 #too shallow probably?
    # mean(depth) == 50 #? +/- x? within a range? Why include this? VertVelo will ensure It's diving

    # thermocline steepness? ild_depth_gl <= 40 #?

    # High vertical velocity:
    # TOTAL VertVelo for each row in an hour / 6hr period? Would relate to tag sample interval, so do mean
    # High meanVV = more diving up/down
    # Mean per hour would be tighter res than 6, won't get as diluted
    # Number of Foragedivehours per day as the metric?

    # High ild_dive_cnt_desc_gl? 6 hourly. What if thermocline is too deep?
    # If I use thermocline to define this dive AND oceandepth to define typeC then
    # What if the thermocline is 'insufficiently' steep, those Forage dives will be ignored by all tests

    df_i$HourFloor = floor_date(df_i$DateTimeUTCmin5, unit = "hour") # fills whole series not just first entry
    df_i$ild_depth_gl <- na.locf(df_i$ild_depth_gl, na.rm = F) # last obs carried forward, pastes over intermediate NAs but leaves first set
    df_i$ild_depth_gl <- na.locf(df_i$ild_depth_gl, na.rm = F, fromLast = T) # pastes backwards from first obs over first set of NAs

    df_i$fishID <- str_remove(i, whichfiles)
    df_i[,c("OceanDepth", "Shelf")] <- df_i %>%
      left_join(AllDailies, by = c("Date", "fishID")) %>%
      select(OceanDepth, Shelf)
    # df_i not used after hourstats next line
    Hourstats <- df_i %>%
      group_by(HourFloor) %>%
      summarise(Date = min(Date, na.rm = TRUE),
                MeanVVHr = mean(abs(VertVelo), na.rm = TRUE),
                MaxDepthHr = max(Depth.m., na.rm = TRUE),
                MeanDepthHr = mean(Depth.m., na.rm = TRUE),
                DepthDiffHr = diff(range(Depth.m., na.rm = TRUE)), # High VV & high diff(depth) = more Foragey
                ild_depth_gl = mean(ild_depth_gl, na.rm = T),
                OceanDepth = first(OceanDepth),
                Shelf = first(Shelf)) %>%
      ungroup()
    Hourstats$MeanVVHr[is.nan(Hourstats$MeanVVHr)] <- 0 #convert NaN to 0, column(s) only
    Hourstats$MeanDepthHr[is.nan(Hourstats$MeanDepthHr)] <- 0 #convert NaN to 0, column(s) only

    # # plot abs(VertVelo) against depth & time
    # plot(df_i$DateTimeUTCmin5[131972:132691], df_i$Depth.m.[131972:132691], type = "l")
    # lines(df_i$DateTimeUTCmin5[131972:132691], abs(df_i$VertVelo)[131972:132691], col = "red")

    # shallow depth bouncing, ILD <= 20
    # plot(Hourstats$HourFloor[57:80], Hourstats$MaxDepthHr[57:80], type = "l", ylim = c(0,40)) #also [4400:4423] Forage day
    # lines(Hourstats$HourFloor[57:80], Hourstats$MeanVVHr[57:80], type = "l", col = "red")
    # lines(Hourstats$HourFloor[57:80], Hourstats$ild_depth_gl[57:80], type = "l", col = "blue")
    # legend("topright", legend = c("MaxFishDepthHr", "MeanVVhr", "ILDepth6Hr"),col = c("black", "red", "blue"),lty = 1, bty = "n")
    #
    # plot(Hourstats$HourFloor[4400:4423], Hourstats$MaxDepthHr[4400:4423], type = "l", ylim = c(0,150)) #also [4400:4423] Forage day
    # lines(Hourstats$HourFloor[4400:4423], Hourstats$MeanVVHr[4400:4423], type = "l", col = "red")
    # lines(Hourstats$HourFloor[4400:4423], Hourstats$ild_depth_gl[4400:4423], type = "l", col = "blue")
    # legend("topright", legend = c("MaxFishDepthHr", "MeanVVhr", "ILDepth6Hr"),col = c("black", "red", "blue"),lty = 1, bty = "n")
    #
    # Hourstats$Hour <- hour(Hourstats$HourFloor)
    # plot(Hourstats$Hour, Hourstats$meanlight, type = "p") #also [4400:4423] Forage day
    #
    # Hourstats$ForagehourA <- rep(NA, nrow(Hourstats))
    # Hourstats$ForagehourA <- ifelse(Hourstats$MeanVVHr >= 5 & Hourstats$MaxDepthHr <= 200, TRUE, FALSE)

    # Hourstats$Foragehour <- Hourstats$MeanVVHr >= 5 #logical T/F
    # Hourstats$Foragehour <- between(Hourstats$MeanVVHr, 4.5, 4.9999) # 4.5 is a reasonably compelling threshold
    Hourstats$Foragehour <- Hourstats$MeanVVHr >= 4.5 # metres per MINUTE
    # Add an extra stipulation to avoid two dives averaging to >4.5 e.g. 5100107 1999-02-05-0930?
    # Or allow since why else is it diving twice through the mixed layer?
    Hourstats$ForageDepth <- rowMeans(Hourstats[,c("MeanDepthHr", "MaxDepthHr")], na.rm = TRUE) #midpoint between max & mean= forage zone
    # Collects data whether it's a forage hour or not, see next group_by

    Hourstats %<>%
      rowwise() %>%
      mutate(ForagehourA = all(Foragehour, # 100m, v steep thermocline
                               ForageDepth <= 120,  # could be 100, 200
                               ild_depth_gl <= 40,
                               abs(OceanDepth - ForageDepth > 30)), # not foraging too near bottom. Based on daily latlon oceandepth with error
             ForagehourB = all(Foragehour, # 300m, deep thermocline to bottom of dive profile
                               between(ForageDepth, 120, 360),  # 360 is 1.2X 300 like 120 is for 100
                               ild_depth_gl >= ForageDepth,
                               abs(OceanDepth - ForageDepth > 30)),
             # We want to find when they're feeding on the bottom.
             # ForagehourC = all(Foragehour, # Bathymetrically limited. C mutually exclusive from A&B
             #                   Shelf, # use this? Subsumed by the next one & disallows off-shelf bottom hunting if exists
             #                   abs(OceanDepth - ForageDepth < 30)) # 30 is a guess.
             ForagehourC = all(ForageDepth > 25, # so they're not just cruising on the surface. 25 a guess
                               # Shelf, # use this? Subsumed by the next one & disallows off-shelf bottom hunting if exists
                               abs(OceanDepth - ForageDepth < 30), # 30 is a guess.
                               # DepthDiffHr < , # low depth diff? will remove hours mostly at bottom with 1 surface interval
                               MeanVVHr <= 3) # 3 a guess
      ) %>% # close mutate
      ungroup() # remove rowwise

    # Add ild_depth_gl criteria? But if ild > criteria those dives will be ignored by all tests too
    # ild_depth will be unknown & LOCF'd from previous if fish wiggles at surface?
    # Not sure GL's code

    # if (MeanVVHr >= 5 & MaxDepthHr <= 200) df_i$ForagehourA2 <- TRUE else FALSE

    # Add min depth > ? Not really Forage diving by doing a lot of wiggling near the surface.
    # No just set better MeanVVHr criteria
    # Highly energetic surface wiggling covers the range of Forage diving
    # Conceptually this IS Forage diving. Surface foraging behaviour,
    #  difference is just depth of prey

    # How to disambiguate bouncing from spawning. Sp has high VV bouncing all day
    # low int temp range all day but Foragediving MIGHT also
    # high (>23) int temp but Foragediving MIGHT also
    # max depth <200, same as typeAForage
    # Forage diving might be happening on spawndive days after spawning. They gotta eat.
    # Given the infrequency of spawning AND that spawning will be tagged as well (separately)
    # there is a low penalty for erroniously double-tagging spawning as also bouncing.

    # Hourstats$ForagehourB <- rep(NA, nrow(Hourstats))
    # Hourstats$ForagehourB <- ifelse(Hourstats$MeanVVHr >= 5 & Hourstats$MaxDepthHr >= 100, TRUE, FALSE)

    Daystats <- Hourstats %>%
      group_by(Date, Foragehour) %>% # group by foragehour gives daily summaries for forage & nonforagehours
      summarise(ForageHours = sum(Foragehour, na.rm = TRUE),
                ForageDepth = mean(ForageDepth, na.rm = TRUE),
                ForageDepthDiff = mean(DepthDiffHr, na.rm = TRUE)) %>%
      ungroup() %>%
      filter(Foragehour) %>% #foragehour=true i.e. discard nonforagehours
      select(-Foragehour) # remove redundant grouper

    Daystats$fishID <- str_remove(i, whichfiles)

    DaystatsExtra <- Hourstats %>%
      filter(Foragehour) %>%
      group_by(Date) %>%
      summarise(WhichForageHours = list(hour(HourFloor)),
                ForageHoursDepth = list(round(ForageDepth, 1)))
    Daystats %<>% left_join(DaystatsExtra) #Joining, by = "Date"

    DaystatsForageA <- Hourstats %>%
      filter(ForagehourA) %>%
      group_by(Date) %>%
      summarise(WhichForageHoursA = list(hour(HourFloor)))
    Daystats %<>% left_join(DaystatsForageA) #Joining, by = "Date"
    DaystatsForageB <- Hourstats %>%
      filter(ForagehourB) %>%
      group_by(Date) %>%
      summarise(WhichForageHoursB = list(hour(HourFloor)))
    Daystats %<>% left_join(DaystatsForageB) #Joining, by = "Date"
    DaystatsForageC <- Hourstats %>%
      filter(ForagehourC) %>%
      group_by(Date) %>%
      summarise(WhichForageHoursC = list(hour(HourFloor)))
    Daystats %<>% left_join(DaystatsForageC) #Joining, by = "Date"

    if (ncol(tmprle) == 4) { #i.e. if tmprle ran earlier
      Daystats %<>% left_join(tmprle) #Joining, by = "Date"
    } else { # bind blank rows so each df & alldailies have the same ncols
      Daystats <- cbind(Daystats, data.frame(Umidtime = rep(NA, nrow(Daystats)),
                                             UdurMins = rep(NA, nrow(Daystats)),
                                             Umeandepth = rep(NA, nrow(Daystats))))
    }


    saveRDS(object = Daystats, file = paste0(saveloc, i))
    rm(list = c("df_i", "tmprle", "j", "Hourstats", "Daystats", "DaystatsExtra",
                "DaystatsForageA", "DaystatsForageB", "DaystatsForageC")) # remove objects
  } #close i

  # Error in { : task 1 failed - "object 'C_parse_period' not found"
  # problem with lubridate/util.R bug with unloading AOR re-sourcing packages
  # Restart RStudio, See https://github.com/tidyverse/lubridate/issues/406

  # Error in { : task 1 failed - "object '_dplyr_assert_all_allow_list' not found"

  setwd(saveloc) # so list.files pulls the daynightstats summary Rds's not the DO2 minutely Rds's
  # mycores <- 8 #Nautilus
  # # mycores <- 16 #Poseidon
  # registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
  # catf <- function(..., file="log.txt", append=TRUE){cat(..., file=file, append=append)} # avoids sink stack full error https://stackoverflow.com/a/34254839/3975144
  all_dfs <- foreach(j = list.files(pattern = whichfiles),
                     .errorhandling = "pass",
                     .combine = rbind,
                     .inorder = TRUE,
                     .multicombine = TRUE) %dopar% { # j <- list.files(pattern = whichfiles)[1]
                       # for (j in list.files(pattern = whichfiles)) { #Original input file list. sequential # j <- list.files(pattern = whichfiles)[1]
                       catf(paste("\n","Starting iteration",j,"\n")) # avoids sink stack full error https://stackoverflow.com/a/34254839/3975144
                       eachRdsFile <- readRDS(j) #assign file to name, prepend with x to avoid numerical named object
                     } #close j

  # all_dfs$Index <- 1:nrow(all_dfs)
  all_dfs <- as.data.frame(all_dfs) #remove tibble, so future stuff isn't broken

  AllDailies <- left_join(AllDailies, all_dfs) # Joining, by = c("Date", "fishID")
  AllDailies[which(is.na(AllDailies$ForageHours)), "ForageHours"] <- 0
  # AllDailies[which(is.na(AllDailies$ForageDepth)), "ForageDepth"] <- 0 #leave as NA, 0 is a value, NA signifies no foragehours hence no foragehour depths
  # AllDailies[which(is.na(AllDailies$ForageDepthDiff)), "ForageDepthDiff"] <- 0 # ditto
  AllDailies$WhichForageHours <- modify_if(AllDailies$WhichForageHours, is.null, ~NA) #purrr from tidyverse, replace NULL with NA in list
  AllDailies$ForageHoursDepth <- modify_if(AllDailies$ForageHoursDepth, is.null, ~NA) # NAs are class(logical) while other values are integer. Problem?
  AllDailies$WhichForageHoursA <- modify_if(AllDailies$WhichForageHoursA, is.null, ~NA)
  AllDailies$WhichForageHoursB <- modify_if(AllDailies$WhichForageHoursB, is.null, ~NA)
  AllDailies$WhichForageHoursC <- modify_if(AllDailies$WhichForageHoursC, is.null, ~NA)
  AllDailies$Umidtime <- modify_if(AllDailies$Umidtime, is.null, ~NA)
  AllDailies$UdurMins <- modify_if(AllDailies$UdurMins, is.null, ~NA)
  AllDailies$Umeandepth <- modify_if(AllDailies$Umeandepth, is.null, ~NA)
  # AllDailies$ForageHours[is.infinite(AllDailies$ForageHours)] <- 0 # no infinites
  AllDailies$ForageDepth[is.infinite(AllDailies$ForageDepth)] <- NA #convert Inf to NA
  AllDailies$ForageDepthDiff[is.infinite(AllDailies$ForageDepthDiff)] <- NA #convert NaN to 0, column(s) only

  saveRDS(object = AllDailies, file = paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_ForageDive.Rds"))

  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))
  gc() #garbage collection, free up memory

} #close whole thing

#   source('~/Dropbox/Blocklab Monterey/Blocklab/htmlplots.R')
#   setwd(paste0(machine, "Blocklab/abft_diving/10_ForageDive/")) #X_PlotsMisc/UShapedSquid
#   samplesize <- 50
#   AllDailies %<>% arrange(-ForageHours)
#   for (i in 0:24) {
#     divetyperesults <- AllDailies %>%
#       filter(fishID != "5105027_C0221_NO05_L13D" &  # remove bad fish
#              fishID != "5105065_C0279_NO05_L12D" &  # remove bad fish
#              fishID != "5197124_97102_NO97_W12D" &  # remove bad fish
#              fishID != "5100134_98516_NO99_W13D" &  # remove bad fish
#              # fishID != "5111028_10P0577_CA11_P32D" &  # remove bad fish
#              ForageHours == i)
#     if (nrow(divetyperesults) > samplesize) { # sample if many results
#       samplerows <- sample(nrow(divetyperesults), size = samplesize)
#       divetyperesults <- divetyperesults[samplerows,] # I think this causes the problems?
#     }
#     htmlplots(savename = paste0("ForageHours", i),
#               plotsloc = "../X_PlotsMisc/TSplotMapUpgrade/",
#               fishID = divetyperesults$fishID,
#               dates = divetyperesults$Date)
#   }
# # 2019-12-10 all 24h ones look good, checking full set
#   divetyperesults <- AllDailies %>% # n = 417
#     filter(fishID != "5105027_C0221_NO05_L13D" &  # remove bad fish
#              fishID != "5105065_C0279_NO05_L12D" &  # remove bad fish
#              fishID != "5197124_97102_NO97_W12D" &  # remove bad fish
#              fishID != "5100134_98516_NO99_W13D" &  # remove bad fish
#              # fishID != "5111028_10P0577_CA11_P32D" &  # remove bad fish
#              ForageHours == 24)
#   htmlplots(savename = paste0("ForageHours", 24),
#             plotsloc = "../X_PlotsMisc/TSplotMapUpgrade/",
#             fishID = divetyperesults$fishID,
#             dates = divetyperesults$Date)

# 5112003_L231-0194_NO12_L03D_2013-09-05 function of high PR STILL!! (but 3 others from same fish are legit)
# Could mean loads of same fish's hours are counted as Forage when they aren't. But I should see that.
# And similar for other fish.

# hist(AllDailies$ForageHours)
# unique(AllDailies[which(AllDailies$ForageHours == 24), "fishID"])
# Daystats$YrDay <- yday(Daystats$Date)
# plot(Daystats$YrDay, Daystats$lightRange)
# plot(Daystats$YrDay, Daystats$lightmean)
# plot(Daystats$lightmean, Daystats$lightRange)

# Join Daystats to df_i
# df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
#                   y = Daystats)

#Possibly Forage diving doesn't need to be pre-specified into 3 depth categories
# (100m with steep thermocline, 300m with gentle thermocline, ocean bottom):
# if the pattern is similar and - crucially - is distinct from other behavioural
# types, then I could try to write code which identifies Forage diving
# regardless of depth, and also returns the (mean daily? mean hourly? max hourly*) depth of
# the prey. This might be more informative than pre-specifying what we're expecting,
# since it could reveal unexpected patterns, and also obviates having separate
# behaviour types for the same thing, which might complicate analyses later
# (compared to spawning, e.g.).
# *depth metric: per 4/5/05: foraging @ 450m: mean hourly would capture tight
# wiggles around that depth (correctly locating prey) but the same would HALVE
# the depth where tuna diving down to hunt then returning to surface.
#
# So then what defines Forage diving?
#1. MeanVVHr >5?
# Single deep dive within an hour won't have a high hourly mean
# Has to be a lot of vertical activity within an hour
# What would that be for other than foraging?
# Except Spawning which I need to define but is just Foragedive + extra criteria
# MeanVVHrMax rasters: Med fish means get up to 534, GOM 45.
# 5116043 15-apr-2017 meanVVhr is ~60 IIRC, v high, but patterns don't look like it. Function of polling rate?
# Check this again - SHOULD be pollingrate independent but other rate metrics seem not to be?

#AllDailies tests####
# # loop 5-wide bins from 0-30 sampling max 30 instances of that range of meanVV
# xfrom <- seq(from = 0, to = 25, by = 5)
# xto <- xfrom + 5
# xrange <- data.frame(xfrom = xfrom,
#                      xto = xto)
# for (i in 1:nrow(xrange)) {
#   divetyperesults <- AllDailies %>%
#     select(Date, fishID, MeanVV24h, MaxVV24h, ild_dive_cnt_desc_gl_Sum, MeanDepth24h, NaPctDepth24h) %>%
#     arrange(-MeanVV24h) %>%
#     filter(fishID != "5105027_C0221_NO05_L13D" &  # remove bad fish
#              fishID != "5105065_C0279_NO05_L12D" &  # remove bad fish
#              fishID != "5197124_97102_NO97_W12D" &  # remove bad fish
#              # fishID != "5100134_98516_NO99_W13D" &  # remove bad fish
#              # fishID != "5111028_10P0577_CA11_P32D" &  # remove bad fish
#              between(MeanVV24h, xrange[i,1], xrange[i,2]) & # keep html page size down for memory
#              NaPctDepth24h < 50) # less than 50% depth values NA
#   if (nrow(divetyperesults) > 30) { # sample if many results
#     samplerows <- sample(nrow(divetyperesults), size = 30)
#     divetyperesults <- divetyperesults[samplerows,]
#   }
#   binrange <- paste0(xrange[i,1], "-", xrange[i,2])
#   htmlplots(savename = paste0("MeanVV", binrange),
#             plotsloc = "../X_PlotsMisc/TSplotMapUpgrade/",
#             fishID = divetyperesults$fishID,
#             dates = divetyperesults$Date)
# }

#Checked days notes####
# 5111050_11A0583_CA11_P22D_2011-12-13 # clean Foragedive
# 5117003_16P1983_CA17_P02D_2018-03-08 # 24h >5vv/hr but simply a function of polling rate (5s)
# 5112003_L231-0194_NO12_L03D_2012-03-27 # same. Flat. Downsample tags to 2min & recalc VV first? Just check VV?
# post VV fix downsample:
# 5113029_11P0766_CA13_P31D_2013-11-19 vv still due to PR? NO, plotting doesn't match up to ForageHours. Redo.
# tmp <- all_dfs %>%
#   dplyr::filter(fishID == "5113029_11P0766_CA13_P31D") %>%
#   dplyr::select(Date, ForageHours)
# Is correct here thus is correct in AllDailies_ForageDive (whcih has been merged to main)

#2. Some element of surface contact / proximity.
# Avoids energetic wiggling at depth. But why? Why would they be at depth, diving
# regularly, if not hunting? See 4/5/05: foraging @ 450m
# DiveType2: eating at the mixed layer (deep)?
# Also doesn't penalise same behaviour at surface, where surface criteria passes automatically

#3. Just a threshold number of ForageHours = a Foragediving day.
# Do I even need this? Instead of a binary, just have 0:24
# Then return the mean/max depth of the ForageHours
# Threshold: requires MeanVVHr to be correct.
# 28/1/04 1hr but surface wiggles all day
# 25/1/04 0hr

#4. Some concept of straight up & down. But this requires sufficient depth.
# Can be created with min depth but then tuna can't forage in < 100m depth?
# We know they do in Port Hood.
# DiveType2: eating at the mixed layer (shallow)
# Hourstats$ForagehourB <- rep(NA, nrow(Hourstats))
# Hourstats$ForagehourB <- Hourstats$MeanVVHr >= 5 & Hourstats$MaxDepthHr >= 100 #logical T/F

# 5104527 mid nov04 crazy energetic diving to 1000m then 24th shallows, to 27th
# almost at surface, 9pm drops off, goes berserk again
# 30th nov 04, surface breaks

# Defining foraging diving.
# Eating @ mixed layer = wiggling, high VV, not necessarily > or < certain depth
# Bouncing to 100m, 300m, deeper: function of preydepth & oceandepth, same driver
# High VV, high depth range (only if it has sufficient depth to dive into),
# not necessarily going to surface (e.g. ForagehourB):
# not classic Foragedive pattern but presumably is foraging
# Foragedive pattern relates to reoxygenation requirement relates to DO2 level & preydepth
# Create & annotate gallery of examples, email Barb/James. [did] Other sources?

# Test VV and depth range against steplength / li5day / kmeans

# 10/5/05 & around then: mesopelagic follow. VV not super high.
# Light level flat throughout the day, could be a criteria.

#Read Lawson 2010 p257. His thermal excess is just int-ext!

#4 oscillationB####
#300m depth, deep thermocline to bottom of dive profile
# max(depth) <= 300 #350?
# mean(depth) == 150 #?
# ild_depth_gl >= ?

# HourFloor
# VV should be lower...? Why. Check depth.
# if (mean(VertVelo) >= X & max(depth, na.rm = T) <= 350) df_i$ForagehourB <- TRUE


#5 oscillationC####
#To ocean bottom: feeding on cod etc.
#   DistToBot <- OceanDepth - max(depth) # will depend on resolution of ocean layer?
#   # Could also do as a similarity %?
#   # Also need to get OceanDepth for each day, isn't in 4.csvs
#   # Test in shelf areas
#   if (mean(VertVelo) >= X & DistToBot <= 50) df_i$ForagehourC <- TRUE
# # If max dive depth tuna > oceandepth on shelf then just label days as shelf?
#   # Or have criteria as OceanDepth < X (shelf depth or max dive depth)
#   # Max dive depth 1522m. Cap at 1000m? Most can get there, 6 & 8 y/os got to 1400 & 1200
#   # Or look for specifically depth restricted dives?
#   AllDailies$DistToBottom <- AllDailies$OceanDepth - AllDailies$DailyMaxDepth
# nrow(AllDailies[AllDailies$DistToBottom < 0,]) #16140 rows with dailymaxdepth > oceandeth, boo
# Still try to find a way to tag these.
# Either depth restricted depth profiles via dive profile analysis,
# or ForageDepth is within X% of bottom

#Mackerel Forage####
#low depth, <x?
# minimal thermocline penetrations, above thermocline
# VV = forage mode

# MegaHigh VVs Deep Forage####
# Search for other high mean hourly VVs
# Megahigh VVs ~ 40 in Nov, diving to 800m, same for vv=30
# Often at 02 to dusk & dawn to 22?, or all night
# Oct to Jan, >34.6
# Can differentiate with max depth

# isolume mesopelagic forage####
# 5104527 6/5/05 3 BHA 13 BHB 93.7 lightmean. Good example. See other days around then also
# Sort by Daystats lightRange lowest to highest
# 2005/05/19 lightRange 162 becoming high enough that is starting to not describe this behaviour
# 2004/12/08 160 is isolume with occasional big up/downs

#DiveMove section####
# library(diveMove)
# #read in file, convert from df_i
# dfidedupe <-  df_i[-which(duplicated(df_i$DateTimeUTCmin5)),] # time stamps must not contain duplicate values
#
# tdrX <- createTDR(time = dfidedupe$DateTimeUTCmin5, #Object of class POSIXct, time stamp for every reading.
#                   depth = dfidedupe$Depth.m., #Object of class ‘numeric’, depth (m) readings.
#                   #concurrentData = df_i[, -c(1:3)], #Object of class data.frame, optional data collected concurrently.
#                   dtime = 120, #Object of class ‘numeric’, sampling interval in seconds.
#                   file = "dfidedupe") #Object of class ‘character’, string indicating the file where the data comes from.
#
# # How to determine depth threshold for a dive?
# # diveMove::calibrateDepth(dive.thr = ?)
# dcalib <- calibrateDepth(tdrX,
#                          dive.thr = 10, # key param, see how changes affect results, default 4?
#                          zoc.method = "offset",
#                          offset = 0, #no correction (already done)
#                          descent.crit.q = 0, # numeric: critical quantile of
#                          # rates of descent below which descent is deemed to have ended
#                          ascent.crit.q = 0, #...and started
#                          knot.factor = 3) # numeric scalar that multiplies the
# # number of samples in the dive. This is used to construct the time predictor
# # for the derivative, paper eg 20 default 3. Play with this?
#
# # dive.thr=3 knot.factor = 20:
# # Record is truncated at the beginning and at the end
# # 1 phases detected
# # 5628 dives detected
# # Error: cannot allocate vector of size 87.2 Gb
#
# # dive.thr=10 knot.factor = 3:
# # Record is truncated at the beginning and at the end
# # 1 phases detected
# # 16667 dives detected
# # Error in validObject(.Object) : invalid class “diveModel” object: invalid object
# # for slot "ascent.crit" in class "diveModel": got class "logical", should be or
# # extend class "numeric"
#
# range(dfidedupe$DateTimeUTCmin5)
# dfidedupemini <- dfidedupe[1:43200,]
# # if it continues to be a problem then could loop through days?
# tdrXmini <- createTDR(time = dfidedupemini$DateTimeUTCmin5, #Object of class POSIXct, time stamp for every reading.
#                       depth = dfidedupemini$Depth.m., #Object of class ‘numeric’, depth (m) readings.
#                       #concurrentData = df_i[, -c(1:3)], #Object of class data.frame, optional data collected concurrently.
#                       dtime = 120, #Object of class ‘numeric’, sampling interval in seconds.
#                       file = "dfidedupemini")
# dcalib <- calibrateDepth(tdrXmini,
#                          dive.thr = 10, # key param, see how changes affect results, default 4?
#                          zoc.method = "offset",
#                          offset = 0, #no correction (already done)
#                          descent.crit.q = 0, # numeric: critical quantile of
#                          # rates of descent below which descent is deemed to have ended
#                          ascent.crit.q = 0, #...and started
#                          knot.factor = 3) # numeric scalar that multiplies the
# #success!
#
# dfiexport <- dfidedupe[,1:2]
# saveRDS(object = dfiexport, file = paste0(saveloc,"divemovebigfile.R"))
#
# # email luque about vector size, loop days option, progress bar option after dives detected
# # done https://github.com/spluque/diveMove/issues
# # could do dplyr grouping then divemove per day?
#
# getDAct(x = decalib, y = "dive.activity") #df: divenumber (dive.id),  dive.activity, postdiveintervalnumber
# # levels(diveact$dive.activity)
# # L dry, 0
# # W wet, 587
# # U underwater (above dive depth), 21093
# # D diving, 21520
# # Z trivial wet animal activities, 0
# # factor with information on whether the reading belongs to a dive or a brief aquatic period.
# ?`detPhase-internal`
# hist(diveact$dive.activity)
# summary(diveact$dive.activity)
# # getDAct(x = decalib, y = "dive.phases")
# diveact <- getDAct(dcalib) #gets everything
# divephase <- getDPhaseLab(dcalib) #dive phase per row
# # descent = D
# # descent/bottom = DB
# # bottom = B
# # bottom/ascent = BA
# # ascent = A
# # DA??
# # X = surface
# diveactphase <- cbind(diveact, divephase)
#
# plotDiveModel(dcalib, diveNo = 260) #plot individual dives
# dphases <- getDPhaseLab(dcalib, c(100:300)) #remove 2nd argument
# dives <- extractDive(dcalib, diveNo = c(100:300)) #remove diveNo argument
# plotTDR(dives, phaseCol = dphases)
#
# diveStats() # returns df with final summaries for each dive:
# # dive start, descent end, ascent start
# # duration of dive, descent, bottom, ascent
# # vert dist covered descent, bottom (vert wiggle amount), ascent
# # max depth
# # post dive interval duration
# ## save these as individual files?
#
# # could calculate bouts but is complicated, maybe not worth it since I have
# # other means to do so e.g. meanvv day/night

# # transitdive dplyr code section####
# df_i$HourFloor <- floor_date(df_i$DateTimeUTCmin5, unit = "hour")
#
# Hourstats <- df_i %>%  #mutate(HourFloor = floor_date(DateTimeUTCmin5, unit = "hour")) %>%
#   group_by(HourFloor) %>%
#   summarise(DepthRangeHr = max(Depth.m., na.rm = TRUE) - min(Depth.m., na.rm = TRUE)) %>%
#   ungroup()
#
# df_i[duplicated(df_i$HourFloor),"HourFloor"] <- NA  # delete dupes# remove duplicates
#
# df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
#                   y = Hourstats)
# #%>% select(-HourFloor) # remove join column or retain for later
# rm(Hourstats)
#
# DayStats <- df_i %>%  #mutate(HourFloor = floor_date(DateTimeUTCmin5, unit = "hour")) %>%
#   group_by(Date) %>%
#   # Depth range < 50m per hour for > (e.g.) 20 hours/day = transit day
#   summarise(Hrs50mLesDepRange = sum(DepthRangeHr <= 50, na.rm = T)) %>%
#   ungroup()
#
# df_i[duplicated(df_i$Date),"Date"] <- NA  # delete dupes# remove duplicates
#
# df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
#                   y = DayStats)
# rm(DayStats)
#
# # amt steps etc here
# #StepLength
# #dfi remove NA dates and lats and lons rows
# df_nona <- df_i[!is.na(df_i$Date),] # omit rows with NA values for date, downsample to days only
# df_nona <- df_nona[!is.na(df_nona$lat),] # omit rows with NA values for date, downsample to days only
# df_nona <- df_nona[!is.na(df_nona$lon),] # omit rows with NA values for date, downsample to days only
#
# track <- mk_track(df_nona,
#                   .x = lon,
#                   .y = lat,
#                   .t = DateTimeUTCmin5,
#                   crs = sp::CRS("+init=epsg:4326")) %>%
#   transform_coords(sp::CRS("+init=epsg:6931"))
# stps <- steps(track)
# # step  length  (sl;  in  CRSunits), 6931=m
# # turning angles (ta; in degrees;  notice that it cannot be calculated for stepsthat are not
# # preceded by a valid step), angles are between -pi and pi, *57.29578 to convert to +-180
# df_nona$StepLengthKm <- c(NA, (stps$sl_/1000))
# df_nona$TurnAngleRelDeg <- c(NA, (stps$ta_*57.29578))
#
# track2 <- mk_track(df_nona,
#                    .x = lon,
#                    .y = lat,
#                    .t = DateTimeUTCmin5,
#                    crs = sp::CRS("+init=epsg:4326"))
# #TurnAngleAzimDeg: compass bearing of new direction, can compute for directionality, migration
# TurnAngleAzimDeg <- direction_abs(track2, full_circle = FALSE, zero_dir = "N", lonlat = TRUE, clockwise = TRUE) %>%
#   as_degree
# TurnAngleAzimDeg <- TurnAngleAzimDeg[1:length(TurnAngleAzimDeg) - 1] #remove NA from back
# df_nona$TurnAngleAzimDeg <- c(NA, TurnAngleAzimDeg) # add NA to front
# df_nona <- df_nona %>% select(c(Date, StepLengthKm, TurnAngleRelDeg, TurnAngleAzimDeg))
#
# df_i <- left_join(x = df_i, #join this topp's dive dave (df_i) to this topp's track data (subtracktmp)
#                   y = df_nona)
# df_i$Date <- date(df_i$DateTimeUTCmin5) #rebuild date

# fishID <- str_remove(i, ".csv") #get fishID
# Daystats$fishID <- rep(fishID, nrow(Daystats)) # #add as column
# if (!exists("all_dfs")) { #if this is the first i, create all_dfs object
#   all_dfs <- Daystats
#   #all_dfs$Stock <- as.character(str_sub(i, -6,-6)) # penultimate letter before .Rds: TF.Rds)
#   # Gulf of Mexico = 1
#   # neutral = 2
#   # the Mediterranean = 3
#   # Bahamas/West Atlantic = 4
#   # Slope Sea = 5
#   # Unknown = U
#   # Can be based on deployment, recovery or geolocation data.
# } else { #else add Daystats to existing object
#   all_dfs <- rbind(all_dfs, Daystats) # add to existing file
#   gc()
# } #close if else
#
# Daystats %<>% select(-fishID)
# write.csv(x = Daystats, #Save
#           file = paste0(saveloc, i),
#           row.names = F)
