#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simon Dedman simondedman@gmail.com started 2019.06.13
# 1. manually classify each of 8 dive types (hereafter BB8):
#     1. transition/transit/travel
#     2. glide
#     3. oscillation / bounce dive A: 100m depth, v. steep thermocline
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

# library(compiler)
# cmpfun(SpikeDive)
# SpikeDive() # or SpikeDive(machine = "/media/Seagate/Work/", loadlistcompare = FALSE)

#8 Spike dive, sunrise/sunset####
SpikeDive <- function(machine = "/home/simon/Documents/Si Work/", loadlistcompare = TRUE) {

  if ("dplyr" %in% (.packages())) {detach("package:dplyr", unload = TRUE)}
  library(lubridate)
  library(beepr)
  library(foreach)
  library(doMC)
  library(progress)
  source('~/Dropbox/Galway/Analysis/R/daylength/daylength.R')
  options(error = function() beep(9))  # give warning noise if it fails

  # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/4_latlonage_interpol/") #ensure trailing /slash
  # loadloc = "/mnt/molaHomeDedman/abft_diving/4_latlonage_interpol/" #ensure trailing /slash
  saveloc = paste0(machine, "Blocklab/abft_diving/5_dawnSpike/") #ensure trailing /slash
  # saveloc = "/mnt/molaHomeDedman/abft_diving/5_dawnSpike/" #ensure trailing /slash
  whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #
  mycores <- 8 #Nautilus
  # mycores <- 12 #Aquarius
  # mycores <- 16 #Poseidon
  #library(dplyr) #dplyr RUINS EVERYTHING by making every vector/value a 1*n or 1*1 tibble so you have to use [,1] on everything!
  # Willis2009
  # Spike dives were initially defined as the deepest point in the depth record
  # within a 16-min period around the times of dawn and dusk (defined from the 50%
  # light threshold explained above). The 16-min interval was approximately the
  # minimum duration of the characteristically shaped dive profile.
  #
  # NEVER CLASSIFIES DAWN/DUSK RELATIVE TO SUNRISE/SUNSET!
  # DD classified based on light level, SS from lalon lookup?:
  # For each daily estimated geographic location derived from the light data, we
  # determined the elevation of the sun from the NASA sun elevation model
  # (http://ecco2.jpl.nasa.gov/ data1/matlab/air_sea/sunrise.m) at the time of the
  # spike and calculated the time of the spike in relation to the time of sunset
  # and sunrise for the estimated position.
  #
  # So: spike POSITION is deepest point +-16mins of DD as calculated from lightlevel
  # How to TEST for A spike?
  # 8.1 find dawndusk####
  setwd(loadloc) #run all as chunk
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

  #parallel####
  registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
  pbTracker <- function(pb,i,mycores) {if (i %% mycores == 0) {pb$tick()}} #i & mycores need to be numbers
  pb <- progress_bar$new(
    format <- " progress [:bar] :percent eta: :eta",
    total <- length(loadlist) / mycores, clear = FALSE, width = 60)
  writeLines(c(""), "log.txt")

  #i loop####
  foreach(i = loadlist, .errorhandling = "pass") %dopar% { #parallel #i <- loadlist[1]
    sink("log.txt", append = TRUE) #sink output to log

    # for (i in loadlist) { #sequential # i <- loadlist[1] # loadlist <- loadlist[66:166]
    # whichi <- which(i %in% loadlist)
    # pbTracker(pb, whichi, mycores) #doesnt work

    df_i <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object
    print(paste0(which(loadlist == i), " of ", length(loadlist), "; adding spikedive data to ", i))

    dfidates <- aggregate(df_i[,c("lat", "lon")], #just date lat lon
                          by = list(df_i$Date),
                          mean, na.rm = T)
    colnames(dfidates)[1] <- "Date"
    dfidates$lat[is.nan(dfidates$lat)] <- NA #convert NaN to NA
    dfidates$lon[is.nan(dfidates$lon)] <- NA #convert NaN to NA
    dfidates <- dfidates[!is.na(dfidates$lat),] # omit rows with NA values for lat
    # quicker to call daylength() once per df_i then lookup against that dl than call
    # daylength() every morning and evening for each date
    dl <- daylength(date = dfidates$Date,
                    lat = dfidates$lat,
                    lon = dfidates$lon,
                    tzs = "America/New_York")
    #dl has date lat lon sunrise sunset dawn dusk daylength, mostly "POSIXct" "POSIXt"

    df_i$dawn <- rep(NA, nrow(df_i))
    df_i$dawn <- as.POSIXct(df_i$dawn, tz = "America/New_York")
    attr(df_i$dawn, "tzone") <- "EST"
    df_i$sunrise <- rep(NA, nrow(df_i))
    df_i$sunrise <- as.POSIXct(df_i$sunrise, tz = "America/New_York")
    attr(df_i$sunrise, "tzone") <- "EST"
    df_i$sunset <- rep(NA, nrow(df_i))
    df_i$sunset <- as.POSIXct(df_i$sunset, tz = "America/New_York")
    attr(df_i$sunset, "tzone") <- "EST"
    df_i$dusk <- rep(NA, nrow(df_i))
    df_i$dusk <- as.POSIXct(df_i$dusk, tz = "America/New_York")
    attr(df_i$dusk, "tzone") <- "EST"
    df_i$daylength <- rep(NA, nrow(df_i))
    df_i$isday <- rep(NA, nrow(df_i)) #changed to NA from FALSE, else gives false when there's no latlon
    is.na(df_i) <- do.call(cbind,lapply(df_i, is.infinite)) #convert Inf & -Inf to NA
    df_i$DawnSpike <- rep(NA, nrow(df_i))
    df_i$DuskSpike <- rep(NA, nrow(df_i))
    df_i$SpikeDives <- rep(NA, nrow(df_i))

    k <- 1 # set counter

    #can absolutely parallelise j, is just processing, no external lookups
    # library(parallel)
    # library(doMC)
    # mycores <- detectCores() - 1
    # # registerDoMC(mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
    # foreach(j = dfidates$Date, .errorhandling = "stop") %dopar% { #parallel


    #bigfile section####
    # unique(df_i$Date)
    # length(dfidates$Date)/3 #645.3333
    #
    # split1 <- dfidates$Date[1:645]
    # df_i <- df_i[df_i$Date %in% split1,]
    # dfidates <- dfidates[1:645,]
    # dl <- dl[1:645,] #then do whole section down to bigfile saves
    #
    # split2 <- dfidates$Date[646:1290]
    # df_i <- df_i[df_i$Date %in% split2,]
    # dfidates <- dfidates[646:1290,]
    # dl <- dl[646:1290,]
    #
    # split3 <- dfidates$Date[1291:1936]
    # df_i <- df_i[df_i$Date %in% split3,]
    # dfidates <- dfidates[1291:1936,]
    # dl <- dl[1291:1936,]

    for (j in dfidates$Date) { #loop through dfidates so as to ignore missing latlons?
      #successive row blocks of same day #j <- dfidates$Date[1]
      print(paste0(k, " of ", length(dfidates$Date), "; ", as.Date(j, origin = "1970-01-01"), "; ", i, "; ", which(loadlist == i), " of ", length(loadlist)))
      # pb <- txtProgressBar(min = 0, max = length(1:lastgood), style = 3) # create progress bar
      # dawn/dusk time: when light intensity = 50% difference btwn daily max & min (Willis2009)
      # LtDawnDusk <- (max(df_i[df_i$Date == i,"Lt"], na.rm = T) - min(df_i[df_i$Date == i,"Lt"], na.rm = T))/2
      # this isn't adjusted to the minimum though, so will be skewed downwards by
      # the value of the minimum - you need to add that to the result, which is =mean
      LtDawnDusk <- mean(c(min(df_i[df_i$Date == j,"Lt"], na.rm = T), max(df_i[df_i$Date == j,"Lt"], na.rm = T)))
      # for (j in list(0:11, 12:23)) { #for morning & evening hours of that day #j <- list(0:11, 12:23)[[1]]
      # first check there are any rows today within 60 mins of dawn (& later dusk)
      if (length(df_i[df_i$Date == j & abs((df_i$DateTimeUTCmin5 - dl[dl$date == j, "dawn"])/60) < 60, "Lt"]) != 0) { # if so do calcs
        # find dawndusk time: which (row) has the minimum absolute (modulus)
        # distance to dawndusk light level, i.e. is dawndusk row?
        ## if Lt jumps from low to high in 1 step then the closest could be far from the jump point
        ## this shouldn't happen during natural running, e.g. may be tag brought out of a box
        ## in which case, bother to code for it?
        # if lightlevel at night ~= mean(daily min/max) then surface trips could result in
        # values that happen to be closer to LtDawnDusk than the actual dawndusk curve
        # limit j windows? Lookup ACTUAL DAWN & DUSK for actual latlon that day
        # round to preceeding/succeeding hour? Hour is too much? Test average deviation
        # DiveSSMLengthDiff.R edited to give minmax latlon

        dawnrow <- which.min(abs(LtDawnDusk - df_i[df_i$Date == j & abs((df_i$DateTimeUTCmin5 - dl[dl$date == j, "dawn"])/60) < 60, "Lt"]))
        # df_i[i&j,Lt] is a subset vector, which min gives a position in it. That needs to be translated back to df_i
        dawnrow <- df_i[df_i$Date == j & abs((df_i$DateTimeUTCmin5 - dl[dl$date == j, "dawn"])/60) < 60, "Index"][dawnrow]
        dawnwindow <- interval(df_i[dawnrow,"DateTimeUTCmin5"] - (16*60), df_i[dawnrow,"DateTimeUTCmin5"] + (16*60)) #dawn window of time
        # deepest point within dawnwindow (max depth)
        dawnspike <- which.max(df_i[df_i$DateTimeUTCmin5 %within% dawnwindow, "Depth.m."]) #gives index within subset as before
        dawnspike <- df_i[df_i$DateTimeUTCmin5 %within% dawnwindow, "Index"][dawnspike]
        # near-surface visit (Min depth) immediately (20+-8 mins) post (dawn) & pre (dusk) spike.
        # (20+-8 mins) = 12:28
        dawnsurfacewindow <- interval(df_i[dawnspike,"DateTimeUTCmin5"] + (12*60), df_i[dawnspike,"DateTimeUTCmin5"] + (28*60)) #dawn surface window of time
        # find depth min within this window
        dawnsurf <- which.min(df_i[df_i$DateTimeUTCmin5 %within% dawnsurfacewindow, "Depth.m."]) #gives index within subset as before
        dawnsurf <- df_i[df_i$DateTimeUTCmin5 %within% dawnsurfacewindow, "Index"][dawnsurf]
        # Willis09: spike depth 50–605m, 4min bins for <=20m surfacing sections
        dawnspikedeep <- df_i$Depth.m.[dawnspike] >= 50 #is spike depth >=50m?
        dawnsurfshallow <- df_i$Depth.m.[dawnsurf] <= 20 # is surface depth <=20m
        dawnspikeTF <- dawnspikedeep & dawnsurfshallow
        if (length(dawnspikeTF) == 0) dawnspikeTF <- FALSE # duty cycled tags skip
        # e.g. 8 days every howevermany. GMT to EST conversion means 19:00-23:59 from
        # pre-skip day are missing, as is 00:00-06:59 from first day after skip. Can
        # cause there to be data for the day but no entries during dawn/dusk window
        #
        #vertical velocity
        # highest rate of ascent (asc=negative vv. most neg vv is min)
        # after deepest point of dawn spike: vv between dawnspike & dawnsurf
        # dawnascent <- min(df_i[dawnspike:dawnsurf,"VertVelo"])
        # occurs 32 min before sunrise
        # since there's a relatively static relationship between dawn & spike & sunrise then min.vv already has to be in this range
        # if vv conforms to rules then dawnspikedeep and dawnsurfshallow probably already have had to
        #I'M NOT USING VV FOR TESTING YET####
      } #close if dawn

      if (length(df_i[df_i$Date == j & abs((df_i$DateTimeUTCmin5 - dl[dl$date == j, "dusk"])/60) < 60, "Lt"]) != 0) {
        duskrow <- which.min(abs(LtDawnDusk - df_i[df_i$Date == j & abs((df_i$DateTimeUTCmin5 - dl[dl$date == j, "dusk"])/60) < 60, "Lt"]))
        duskrow <- df_i[df_i$Date == j & abs((df_i$DateTimeUTCmin5 - dl[dl$date == j, "dusk"])/60) < 60, "Index"][duskrow]
        duskwindow <- interval(df_i[duskrow,"DateTimeUTCmin5"] - (16*60), df_i[duskrow,"DateTimeUTCmin5"] + (16*60)) #dusk window of time
        duskspike <- which.max(df_i[df_i$DateTimeUTCmin5 %within% duskwindow, "Depth.m."]) #gives index within subset as before
        duskspike <- df_i[df_i$DateTimeUTCmin5 %within% duskwindow, "Index"][duskspike]
        dusksurfacewindow <- interval(df_i[duskspike,"DateTimeUTCmin5"] - (28*60), df_i[duskspike,"DateTimeUTCmin5"] - (12*60))
        dusksurf <- which.min(df_i[df_i$DateTimeUTCmin5 %within% dusksurfacewindow, "Depth.m."])
        dusksurf <- df_i[df_i$DateTimeUTCmin5 %within% dusksurfacewindow, "Index"][dusksurf]
        duskspikedeep <- df_i$Depth.m.[duskspike] >= 50 #is spike depth >=50m?
        dusksurfshallow <- df_i$Depth.m.[dusksurf] <= 20 # is surface depth <=20m
        duskspikeTF <- duskspikedeep & dusksurfshallow
        if (length(duskspikeTF) == 0) duskspikeTF <- FALSE
        # duskdescent <- max(df_i[dusksurf:duskspike,"VertVelo"]) # UNUSED; highest rate of descent before deepest point of dusk spike occurs 32 min after sunset
      } #close if dusk

      # for dawn: if dive qualifies and shallow qualifies and/or difference is within a range etc then dawn=spike
      # ditto dusk. df_i$spikes = 0,1,2
      #If dawn skipped then dawnspikeTF not created & nextline which fails. Build workaround
      if (!exists("dawnspikeTF")) dawnspikeTF <- FALSE
      if (!exists("duskspikeTF")) duskspikeTF <- FALSE
      df_i[df_i$Date == j,"DawnSpike"][1] <- dawnspikeTF
      df_i[df_i$Date == j,"DuskSpike"][1] <- duskspikeTF
      df_i[df_i$Date == j,"SpikeDives"][1] <- length(which(c(dawnspikeTF,duskspikeTF))) # N spikes per day, append to once-per-day date row
      df_i[df_i$Date == j,"sunrise"][1] <- dl[dl$date == j,"sunrise"] #daily sunrise times etc
      df_i[df_i$Date == j,"sunset"][1] <- dl[dl$date == j,"sunset"]
      df_i[df_i$Date == j,"dawn"][1] <- dl[dl$date == j,"dawn"]
      df_i[df_i$Date == j,"dusk"][1] <- dl[dl$date == j,"dusk"]
      df_i[df_i$Date == j,"daylength"][1] <- dl[dl$date == j,"daylength"]

      # isday append
      daytime <- interval(df_i[df_i$Date == j,"dawn"][1], #from dawn
                          df_i[df_i$Date == j,"dusk"][1]) #to dusk
      df_i[df_i$Date == j & df_i$DateTimeUTCmin5 %within% daytime, "isday"] <- TRUE #mark days for later
      df_i[df_i$Date == j & !df_i$DateTimeUTCmin5 %within% daytime, "isday"] <- FALSE #mark nights for later

      rm(list = c("dawnrow", "dawnspike",
                  "dawnspikedeep", "dawnspikeTF", "dawnsurf", "dawnsurfacewindow",
                  "dawnsurfshallow", "dawnwindow",
                  "duskrow", "duskspike", "duskspikedeep", "duskspikeTF", "dusksurf",
                  "dusksurfacewindow", "dusksurfshallow", "duskwindow")) # remove objects
      k <- k + 1 #counter, using j directly causes problems due to nightmarish handling of dates in R
    } #close j

    #bigfile####
    # saveRDS(object = df_i, file = paste0(saveloc,1,i))
    # saveRDS(object = df_i, file = paste0(saveloc,2,i))
    # saveRDS(object = df_i, file = paste0(saveloc,3,i))
    # df_i1 <- readRDS(paste0(saveloc,1,i))
    # df_i2 <- readRDS(paste0(saveloc,2,i))
    # df_i3 <- readRDS(paste0(saveloc,3,i))
    # df_i <- rbind(df_i1, df_i2, df_i3)
    # rm(df_i1)
    # rm(df_i2)
    # rm(df_i3)

    saveRDS(object = df_i, file = paste0(saveloc,i))
    rm("df_i", "dfidates", "dl", "LtDawnDusk") # remove objects
    gc()
    beep(1) # notify completion of each file
  } #close i
  # close(pb)
  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  gc() #garbage collection, free up memory
} # close function
# More Willis09 notes####
# There is considerable
# variability in the depth of spike dives recorded in these data (50–605 m), but
# the characteristic shape remains consistent and lasted from 15 min to 1 h.
#
# Spike dives are offset by about 30 min on the dark side of sunrise or sunset,
# which corresponds to a sun elevation of about −6º at both times of the day
# (Table 2); that is, the highest rate of ascent (immediately following the
# deepest point of the spike dive) of dawn spike dives occurs approximately 30
# minutes before sunrise, and the highest rate of descent (immediately before
# reaching the deepest point of the spike dive) of the dusk dive occurs
# approximately 30 min after sunset. A plot of relative frequency of depth data
# less than 20 m (Fig. 5) demonstrates that there is a maximum between the main
# depth change and the calculated position of sunrise or sunset; this suggests
# that the fish were more often closer to the surface during this period than at
# other times during the characteristic spike dive pattern.
#
# Proportion of depths <20m, 4 minute bins. Nadir 32minute before sunrise/after sunset
# Peak (most shallow) 12-16 mins before/after: civil twilight, polarized light georeferencing
#
# To test spike against null: we identified the maximum rate of ascent or
# descent within a 16-min interval around a randomly selected time.
#
# Compared spike ascent/descent rates for correlation with fish size. Results
# showed asc/desc speed may be constrained by fish size
