# Diel isolume/mesopelagic prey follow style day pattern, surface at night, deep during day
# Simon Dedman simondedman@gmail.com 2019-11-01

# U shaped diving, Aranda2013spawning  Wilson2009
# ~300m depth, depth mixed layer with no thermal constraint. Mixed layer reaches DSL, ‘window to the mesopelagic’.
# Depth within range (275-325m? How precise?) for X time (per Y hours)
# Depth >200m (citable precedent)
# At certain times per day?should
# Temp >= Z
# Age/length of tuna
#
# Initial period of high negative VV (dive to depth)
# depth attained > 200m
# Midsection of low VV <= ?
#   Midsection depth range >=200m
# Midsection depth as function of ILD / DSL modelled depth, datetime??
#  Terminal ascent, high positive VV
# (for all of these) Max N of these types within a certain time?
#  Bigger fish should be able to do this more than smaller fish


# Aranda:
# In the Bay of Biscay, the tags recorded the
# shallowest daytime behaviour of all regions (18.3 ± 53.6 m),
# with occasional U-shaped profiles up to 400 m deep (Figure 4D).
# In the North Atlantic area (Figure 4E, Table 2), the fish experienced the deepest behaviour during the day (79.9 ±
# 99.5 m), showing frequent U-shaped profiles up to 300-400 m, whereas they displayed a shallow behaviour at nighttime (17.0 ± 32.1 m).
# U-shaped: feeding in the DSL

# Wilson 2009:
# U-shaped dives (also called square-shaped dives) are thought to represent animals locating and exploiting aggregated prey for extended periods each day (Lesage et al. 1999, Schreer et al. 2001, Baechler et al. 2002)
# U-shaped profiles with or without surface returns — regular profiles in which daytime depths are significantly deeper than at night and modal depths are consistent over consecutive days.
# 5100107 1999-03-27, /mnt/TOPP/Tuna/ABFT/Recovery/WC/5100107_98521/TS_pdfs/5100107_98521WP.pdf



# isolume mesopelagic forage####
# From bouncedive notes
# 5104527 6/5/05 3 BHA (bouncehoursA (now ForageHours?)) 13 BHB 93.7 lightmean. Good example. See other days around then also
# /mnt/TOPP/Tuna/ABFT/Recovery/ltd2310/5104527_A2198/TS_pdfs/5104527_A2198WP.pdf
# Sort by Daystats lightRange lowest to highest
# 2005/05/19 lightRange 162 becoming high enough that is starting to not describe this behaviour
# 2004/12/08 160 is isolume with occasional big up/downs

# cmpfun(squidU)
# squidU() # or squidU(machine = "/media/Seagate/Work/")
squidU <- function(machine = "/home/simon/Documents/Si Work/", loadlistcompare = TRUE) {

  library(magrittr)
  library(dplyr)
  library(data.table)
  library(compiler)
  library(beepr)
  if (!"gbm.auto" %in% installed.packages()) {
    library(devtools)
    install_github("SimonDedman/gbm.auto")
  }
  library(gbm.auto)
  # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
  # machine <- "/media/Seagate/Work/" #Poseidon
  AllDailies <- readRDS(file = paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_KMeans.Rds"))
  # mean nightly depth will be << mean daily depth
  AllDailies$DeeperDay <- AllDailies$MeanDepthNight < AllDailies$MeanDepthDay #Emil notes this is the same as the above, replace.
  # mean daily depth will be >= 300? 250? Assessed, 230 is a point where we're still getting some classic (albeit shallow) meso
  #REDUNDANT####
  # Day has to be > 230 & night < 150 hence day > night if those 2 conditions met
  DayDepth <- 230
  AllDailies$DayDepthO <- AllDailies$MeanDepthDay >= DayDepth
  # mean nightly depth will be <= 150? 300 too loose?
  NightDepth <- 150
  AllDailies$NightDepthU <- AllDailies$MeanDepthNight <= NightDepth
  # Light must have at least 50% non-NA readings that night
  AllDailies$MeanLtU <- rep(FALSE, nrow(AllDailies))
  AllDailies$DayMinusNtLtU <- rep(FALSE, nrow(AllDailies))
  AllDailiesOriginal <- AllDailies
  AllDailiesOriginal$DielDive <- as.numeric(rep(NA, nrow(AllDailiesOriginal)))
  AllDailiesOriginal$DVM <- rep(FALSE, nrow(AllDailiesOriginal))
  setDT(AllDailiesOriginal) # convert to data.table without copy


  #loadlist comparison chunk####
  if (loadlistcompare) {
    savetarget <- readRDS(paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_DielDive.Rds"))
    savelist <- paste0(savetarget$fishID, "_", savetarget$Date) #list files based on specified pattern above
    loadlist <- paste0(AllDailies$fishID, "_", AllDailies$Date) #list files based on specified pattern above
    difflist <- which(!loadlist %in% savelist)

    if (length(difflist) == 0) { # if there are no differences, left join existing Diel, to updated (typically) KMeans save, clean, close
      library(dplyr)
      library(magrittr)
      savetarget %<>% dplyr::select(Date, fishID, DeeperDay, DayDepthO, NightDepthU, MeanLtU, DayMinusNtLtU, DielDive, DVM) # remove extra cols
      AllDailies %<>%
        select(-DeeperDay, -DayDepthO, -NightDepthU, -MeanLtU, -DayMinusNtLtU, -DielDive, -DVM) %>% # remove NA column first
        left_join(savetarget) # Joining, by = c("Date", "fishID")
      saveRDS(object = AllDailies, file = paste0(loadloc, "AllDailies_DielDive.Rds"))
      rm(list = ls()) #remove all objects
      beep(8) #notify completion
      stop("Success: Diel file already existed; data were joined successfully")
      lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
      invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))
      gc() #garbage collection, free up memory
    } # close if if (length(difflist) == 0)

    if (length(difflist) != 0) { # if there are fishID days in (newer) DST & not in (older) Chl
      print(paste0(length(savelist[which(savelist %in% loadlist)]), " fishID days present in old AllDailies & also present in new AllDailies; merging in"))
      AllDailiesOriginal <- AllDailies
      setDT(AllDailiesOriginal) # convert to data.table without copy
      setDT(savetarget) # convert to data.table without copy
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), DeeperDay := i.DeeperDay]
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), DayDepthO := i.DayDepthO]
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), NightDepthU := i.NightDepthU]
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), MeanLtU := i.MeanLtU]
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), DayMinusNtLtU := i.DayMinusNtLtU]
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), DielDive := i.DielDive]
      AllDailiesOriginal[savetarget, on = c("Date", "fishID"), DVM := i.DVM]
      print(paste0(length(difflist), " fishID days present in new AllDailies & not present in old"))
      AllDailiesOriginal <<- AllDailiesOriginal # save updated full object for later
      AllDailies <- AllDailies[difflist,] # subset AllDailies to fishIDdays present in new AllDailies & not present in old
      rm("loadlist", "savelist", "difflist")
    } # close if (length(difflist) != 0)
  } # close loadlistcompare

  if (!all(is.na(AllDailies$lat))) { # if not all lats are NA, i.e. there's something to be done

    # Only do light tests if there aren't too many NAs (First TRUE-required condition of subset)
    # TRUE if mean 24h light < 150
    NaPctLtNight <- 50
    MeanLt24h <- 150
    AllDailies[which(AllDailies$NaPctLtNight < NaPctLtNight & AllDailies$MeanLt24h < MeanLt24h), "MeanLtU"] <- TRUE
    # Only do light tests if there aren't too many NAs (First TRUE-required condition of subset)
    # But this means that meso dives will get 2 FALSEs if their light stalk is broken. Could be 14% of meso dives
    # light level variance will be low (value <= Y?) Relevant? For a low mean you need a low variance since you're near-bottoming out the sensors anyhow
    # Diff btwn day & night should be low however. 100 conservative, see DayMinusNightLtSort.png
    DayMinusNtLt <- 100
    AllDailies[which(AllDailies$NaPctLtNight < NaPctLtNight & (AllDailies$MeanLtDay - AllDailies$MeanLtNight) < DayMinusNtLt), "DayMinusNtLtU"] <- TRUE

    # AllDailies$UScore <- apply(AllDailies[,c("DeeperDay",
    #                                          "DayDepthO",
    #                                          "NightDepthU",
    #                                          "MeanLtU",
    #                                          "DayMinusNtLtU")],
    #                            1, #rows
    #                            function(x) length(which(x))) #sums TRUEs
    # plot(sort(AllDailies$MeanLt24h))

    #ALGORITHMIC CRITERIA####
    AllDailies$TFPN <- as.numeric(rep(NA, nrow(AllDailies))) # Was called DielDive previously, this obviates having 2 cols
    AllDailies[which(AllDailies$DeeperDay & AllDailies$DayDepthO & AllDailies$NightDepthU & AllDailies$MeanLtU & AllDailies$DayMinusNtLtU), "TFPN"] <- 1 #5/5 criteria
    # Last 2 will always be FALSE for PAT popups since they have no lightmeter
    AllDailies[which(AllDailies$DeeperDay & AllDailies$DayDepthO & AllDailies$NightDepthU & (AllDailies$NaPctLtDay >= 50 | is.na(AllDailies$NaPctLtDay))), "TFPN"] <- 1
    # or 3/3 depth and:  >=50% daylightNA, OR isNA (no light sensor)

    #PROBLEM####
    #PAT POPUPS FAIL ALL. 7 DAYS PASS FIRST 3 CRITERIA BUT NONE ALSO PASS OTHER CRITERIA = ALL TFPN ISNA
    #AllDailiesTrain (GBM.AUTO SAMPLES) NROW = 0 therefore so fails
    # If so warn beforehand
    if (all(is.na(AllDailies$TFPN))) stop("None of the new fishdays pass criteria for diel dive. Nothing to feel BRT training. Run full dataset with loadlistcompare=F")

    # length(which(AllDailies$TFPN == 1)) # 957/47045
    # length(which(AllDailies$TFPN == 0)) # 3188
    # length(which(AllDailies$DielDive == 1)) # 560 then 733 once Na% added. = 1.55% of fishdays
    # length(which(AllDailies$DielDive == 0)) # 46312
    # temp save for later
    # setDF(AllDailies)
    # saveRDS(object = AllDailies, file = paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_UDive.Rds"))

    # length(which(AllDailies$DeeperDay)) # 25307 / 47045
    # length(which(AllDailies$DayDepthO)) # 250=1316 230=1598, 282 more
    # length(which(AllDailies$NightDepthU)) # 37842
    # length(which(AllDailies$MeanLtU)) # 11757
    # length(which(AllDailies$DayMinusNtLtU)) # 9652
    # length(which(AllDailies$UScore == 5)) #560
    # divetyperesults <- AllDailies[which(AllDailies$UScore == 5),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID")]
    # divetyperesults$TFPN <- rep(1, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/Diel/"))
    # write.csv(x = divetyperesults,
    #           file = "divetyperesults5.csv",
    #           row.names = FALSE)
    # noquote(unique(divetyperesults$fishID)) #64 fish, 560 days
    #html page of results images####
    # source('~/Dropbox/Blocklab Monterey/Blocklab/htmlplots.R')
    # 1. examine positives for false, adjust accordingly, repeat####
    # htmlplots(savename = "UDives5", #n=560
    #           plotsloc = "../TSplotMapUpgrade/",
    #           fishID = divetyperesults$fishID,
    #           dates = divetyperesults$Date)
    # 5104527_A2198_NO04_L12D_2004-10-10 dubious
    # 5111028_10P0577_CA11_P32D_2011-12-06 1 reading per hour?! 15s in file 2011-11-21 img dead, afterwards lowres. Lots of surface time that day but looks fine. Rerun TSplotter
    # Accepting all as TP after review
    # AllDailies$DielDive <- rep(NA, nrow(AllDailies))
    # AllDailies[which(AllDailies$UScore == 5), "DielDive"] <- 1

    # 2. examine negatives for true, adjust accordingly, repeat####
    #Score4####
    # divetyperesults <- AllDailies[which(AllDailies$UScore == 4),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID", "DeeperDay", "DayDepthO",
    #                                 "NightDepthU", "MeanLtU", "DayMinusNtLtU")]
    # htmlplots(savename = "UDives4", plotsloc = "../TSplotMapUpgrade/") #n=5667, 5594 DayDepthO why FEWER??
    # divetyperesults$TFPN <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesults4.csv",
    #           row.names = FALSE)
    # Manually tag group 4 then 3 then 2 then 1 then 0, as binomial
    # Tagged 314/5594 group 4s as 0 or 1, remainder as NA
    # divetyperesults <- read.csv("divetyperesults4.csv")
    # divetyperesults$Date <- as.Date(divetyperesults$Date)
    # divetyperesults$fishID <- as.character(divetyperesults$fishID)


    #Score3####
    # divetyperesults <- AllDailies[which(AllDailies$UScore == 3),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID", "DeeperDay", "DayDepthO",
    #                                 "NightDepthU", "MeanLtU", "DayMinusNtLtU")]
    # htmlplots(savename = "UDives3", plotsloc = "../TSplotMapUpgrade/") #n=4687
    # divetyperesults$TFPN <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesults3.csv",
    #           row.names = FALSE)
    # divetyperesults <- read.csv("divetyperesults3.csv")
    # divetyperesults$Date <- as.Date(divetyperesults$Date)
    # divetyperesults$fishID <- as.character(divetyperesults$fishID)


    #Score2####
    # divetyperesults <- AllDailies[which(AllDailies$UScore == 2),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID", "DeeperDay", "DayDepthO",
    #                                 "NightDepthU", "MeanLtU", "DayMinusNtLtU")]
    # htmlplots(savename = "UDives2", plotsloc = "../TSplotMapUpgrade/") #n=18974
    # divetyperesults$TFPN <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesults2.csv",
    #           row.names = FALSE)
    #REVIEW PLOTS
    # divetyperesults <- read.csv("divetyperesults2.csv")
    # divetyperesults$Date <- as.Date(divetyperesults$Date)
    # divetyperesults$fishID <- as.character(divetyperesults$fishID)


    #Score1####
    # divetyperesults <- AllDailies[which(AllDailies$UScore == 1),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID", "DeeperDay", "DayDepthO",
    #                                 "NightDepthU", "MeanLtU", "DayMinusNtLtU")]
    # htmlplots(savename = "UDives1", plotsloc = "../TSplotMapUpgrade/") #n=8971
    # divetyperesults$TFPN <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesults1.csv",
    #           row.names = FALSE)
    # REVIEW PLOTS
    # divetyperesults <- read.csv("divetyperesults1.csv")
    # divetyperesults$Date <- as.Date(divetyperesults$Date)
    # divetyperesults$fishID <- as.character(divetyperesults$fishID)


    #Score0####
    # divetyperesults <- AllDailies[which(AllDailies$UScore == 0),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID", "DeeperDay", "DayDepthO",
    #                                 "NightDepthU", "MeanLtU", "DayMinusNtLtU")]
    # htmlplots(savename = "UDives0", plotsloc = "../TSplotMapUpgrade/") #n=8259
    # divetyperesults$TFPN <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesults0.csv",
    #           row.names = FALSE)
    #REVIEW PLOTS
    # divetyperesults <- read.csv("divetyperesults0.csv")
    # divetyperesults$Date <- as.Date(divetyperesults$Date)
    # divetyperesults$fishID <- as.character(divetyperesults$fishID)


    # Read in tagged csvs
    divetyperesults0 <- read.csv("divetyperesults0.csv")
    divetyperesults0$Date <- as.Date(divetyperesults0$Date)
    divetyperesults0$fishID <- as.character(divetyperesults0$fishID)
    divetyperesults1 <- read.csv("divetyperesults1.csv")
    divetyperesults1$Date <- as.Date(divetyperesults1$Date)
    divetyperesults1$fishID <- as.character(divetyperesults1$fishID)
    divetyperesults2 <- read.csv("divetyperesults2.csv")
    divetyperesults2$Date <- as.Date(divetyperesults2$Date)
    divetyperesults2$fishID <- as.character(divetyperesults2$fishID)
    divetyperesults3 <- read.csv("divetyperesults3.csv")
    divetyperesults3$Date <- as.Date(divetyperesults3$Date)
    divetyperesults3$fishID <- as.character(divetyperesults3$fishID)
    divetyperesults4 <- read.csv("divetyperesults4.csv")
    divetyperesults4$Date <- as.Date(divetyperesults4$Date)
    divetyperesults4$fishID <- as.character(divetyperesults4$fishID)
    divetyperesults5 <- read.csv("divetyperesults5.csv")
    divetyperesults5$Date <- as.Date(divetyperesults5$Date)
    divetyperesults5$fishID <- as.character(divetyperesults5$fishID)

    # join/merge to alldailies
    setDT(AllDailies) # convert to data.table without copy
    setDT(divetyperesults0) # convert to data.table without copy
    setDT(divetyperesults1)
    setDT(divetyperesults2)
    setDT(divetyperesults3)
    setDT(divetyperesults4)
    setDT(divetyperesults5)
    # join and update "AllDailies" by reference, i.e. without copy
    # https://stackoverflow.com/questions/44930149/replace-a-subset-of-a-data-frame-with-dplyr-join-operations
    AllDailies[divetyperesults0, on = c("Date", "fishID"), TFPN := i.TFPN]
    AllDailies[divetyperesults1, on = c("Date", "fishID"), TFPN := i.TFPN]
    AllDailies[divetyperesults2, on = c("Date", "fishID"), TFPN := i.TFPN]
    AllDailies[divetyperesults3, on = c("Date", "fishID"), TFPN := i.TFPN]
    AllDailies[divetyperesults4, on = c("Date", "fishID"), TFPN := i.TFPN]
    AllDailies[divetyperesults5, on = c("Date", "fishID"), TFPN := i.TFPN]

    # false negatives list, plot, review, any commonalities?
    # divetyperesults <- AllDailies[which(AllDailies$UScore <= 4 & TFPN == 1),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "NaPctLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID", "DeeperDay", "DayDepthO",
    #                                 "NightDepthU", "MeanLtU", "DayMinusNtLtU")]
    # htmlplots(savename = "UDivesFN", plotsloc = "../TSplotMapUpgrade/") #n=8259
    # divetyperesults$Notes <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesultsFN.csv",
    #           row.names = FALSE)

    #Evaluate days which pass all depth criteria but score < 5, i.e. fail on either or both light criteria.
    # divetyperesults <- AllDailies[which(AllDailies$UScore < 5 & AllDailies$DeeperDay & AllDailies$DayDepthO & AllDailies$NightDepthU),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "SurfFreqNight",
    #                                 "SurfFreqDay", "fishID")]
    # htmlplots(savename = "UDivesPassDepthFailLight", plotsloc = "../TSplotMapUpgrade/") #n=510
    # divetyperesults$TFPN <- rep(NA, nrow(divetyperesults))#add TRUE FALSE POSITIVE NEGATIVE column for manual population. 1 or 0 for binomial resvar
    # write.csv(x = divetyperesults,
    #           file = "divetyperesultsPassDepthFailLight.csv",
    #           row.names = FALSE)
    divetyperesultsPDFL <- read.csv("divetyperesultsPassDepthFailLight.csv")
    divetyperesultsPDFL$Date <- as.Date(divetyperesultsPDFL$Date)
    divetyperesultsPDFL$fishID <- as.character(divetyperesultsPDFL$fishID)
    setDT(divetyperesultsPDFL)
    # Lots of fishdays pass the 3 depths and fail light but are still meso.
    # what's the commonality of those which pass depth fail light and aren't meso?
    AllDailies[divetyperesultsPDFL, on = c("Date", "fishID"), TFPN := i.TFPN]
    # PDFLstats <- divetyperesultsPDFL %>%
    #   group_by(TFPN) %>%
    #   summarise(MeanDepth24h = mean(MeanDepth24h, na.rm = T),
    #             MaxDepth24h = mean(MaxDepth24h, na.rm = T),
    #             MeanLt24h = mean(MeanLt24h, na.rm = T),
    #             MeanDepthNight = mean(MeanDepthNight, na.rm = T),
    #             MeanDepthDay = mean(MeanDepthDay, na.rm = T),
    #             MaxDepthNight = mean(MaxDepthNight, na.rm = T),
    #             MaxDepthDay = mean(MaxDepthDay, na.rm = T),
    #             MeanLtNight = mean(MeanLtNight, na.rm = T),
    #             MeanLtDay = mean(MeanLtDay, na.rm = T)) %>%
    #   ungroup()
    # light - obviously - is the difference. mean24h light (150 the threshold) is mean 158 for nonmeso, 122 for meso
    # day only, 199 nonmeso, 130 meso. Bigger diff.
    # Maybe all 5/5 pass,
    # all 3/3 depth pass if NA%Lt > 25? (allow light stalk failure to not prevent meso)
    # divetyperesults <- AllDailies[which(AllDailies$UScore < 5 & AllDailies$DeeperDay & AllDailies$DayDepthO & AllDailies$NightDepthU & AllDailies$NaPctLtDay > 25),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "NaPctLtDay",
    #                                 "fishID", "MeanLtU", "DayMinusNtLtU", "UScore", "TFPN")]
    # length(which(divetyperesults$TFPN == 1)) #166/212 # 166 FNs become TP
    # length(which(divetyperesults$TFPN == 0)) #46/212 # 46 TN become FP, 3.6X ratio of good conversion to bad conversion.

    # divetyperesults <- AllDailies[which(AllDailies$UScore < 5 & AllDailies$DeeperDay & AllDailies$DayDepthO & AllDailies$NightDepthU & AllDailies$NaPctLtDay > 50),
    #                               c("Date", "MeanDepth24h", "MaxDepth24h", "MeanLt24h",
    #                                 "MeanDepthNight", "MeanDepthDay", "MaxDepthNight",
    #                                 "MaxDepthDay", "MeanLtNight", "MeanLtDay", "NaPctLtDay",
    #                                 "fishID", "MeanLtU", "DayMinusNtLtU", "UScore", "TFPN")]
    # length(which(divetyperesults$TFPN == 1)) #137/173 # FNs become TP
    # length(which(divetyperesults$TFPN == 0)) #36/173 # TN become FP, 3.8X ratio of good conversion to bad conversion.
    # Use this 50% NA test, i.e.

    # model####
    # gbm.auto, resvar = AllDailies$DielDive, expvars =
    # expvars <- names(AllDailies)
    # "MeanDepth24h"                  "MaxDepth24h"                   "NaPctDepth24h"                 "MeanLt24h"
    # "NaPctLt24h"                    "MinITemp24h"                   "RangeITemp24h"                 "NaPctITemp24h"                 "MaxETemp24h"
    # "MeanETemp24h"                  "MinETemp24h"                   "NaPctETemp24h"                 "MeanVV24h"                     "MaxVV24h"
    # "MeanTV24h"                     "MaxTV24h"                      "SurfFreq24h"                   "MaxSST24h"                     "MeanILD24h"
    # "MeanILDtemp24h"                "ild_dive_cnt_desc_gl_midnight" "ild_dive_cnt_desc_gl_Sum"      "MeanDepthNight"                "MeanDepthDay"
    # "MaxDepthNight"                 "MaxDepthDay"                   "NaPctDepthNight"               "NaPctDepthDay"                 "MeanLtNight"
    # "MeanLtDay"                     "NaPctLtNight"                  "NaPctLtDay"                    "MinITempNight"                 "MinITempDay"
    # "RangeITempNight"               "RangeITempDay"                 "NaPctITempNight"               "NaPctITempDay"                 "MeanETempNight"
    # "MeanETempDay"                  "MinETempNight"                 "MinETempDay"                   "NaPctETempNight"               "NaPctETempDay"
    # "MeanVVNight"                   "MeanVVDay"                     "MaxVVNight"                    "MaxVVDay"                      "MeanTVNight"
    # "MeanTVDay"                     "MaxTVNight"                    "MaxTVDay"                      "SurfFreqNight"               "SurfFreqDay"
    # "Stock"
    # "LunarPhase"                    "FishLengthCm"                  "TurnAngleRelDeg"               "StepLengthBL"
    # expvarsA <- expvars[c(12:66, 76, 77, 79, 80)]

    # expvarsB <- expvars[c(12:15, 34:43)]
    # "MeanDepth24h"    "MaxDepth24h"     "NaPctDepth24h"   "MeanLt24h"       "MeanDepthNight"  "MeanDepthDay"    "MaxDepthNight"   "MaxDepthDay"     "NaPctDepthNight"
    # "NaPctDepthDay"   "MeanLtNight"     "MeanLtDay"       "NaPctLtNight"    "NaPctLtDay"

    # expvarsC <- c("MeanDepthDay", "NaPctLtNight", "MeanDepthNight", "MeanLt24h", "NaPctLtDay")

    # unique(AllDailies$Stock)
    # AllDailies$Stock <- as.factor(AllDailies$Stock)

    # library(devtools)
    # install_github("SimonDedman/gbm.auto")
    # library(gbm.auto)
    # setwd(paste0(machine, "Blocklab/abft_diving/9_UDive/"))
    # gbm.auto(grids = NULL,
    #          samples = AllDailies,
    #          expvar = expvars,
    #          resvar = "DielDive",
    #          tc = 2,
    #          lr = 0.1, #0l01 default, large n so shrinking
    #          bf = 0.5,
    #          n.trees = 50,
    #          ZI = "CHECK",
    #          fam1 = "bernoulli",
    #          fam2 = "gaussian",
    #          simp = TRUE, #switched off, do it manually after first run
    #          gridslat = 1, # Date
    #          gridslon = 67,  # fishID
    #          multiplot = FALSE,
    #          cols = grey.colors(1, 1, 1),
    #          linesfiles = TRUE,
    #          smooth = FALSE,
    #          savegbm = TRUE,
    #          varint = TRUE,
    #          map = FALSE,
    #          shape = NULL,
    #          RSB = TRUE,
    #          BnW = FALSE,
    #          alerts = TRUE,
    #          pngtype = "cairo-png",
    #          gaus = FALSE)
    # From 56 expvars with simp on
    # Error in if (delta.mean < (alpha * original.deviance.se)) { :
    #     missing value where TRUE/FALSE needed
    #   In addition: There were 13 warnings (use warnings() to see them)
    #   > traceback()
    #   2: gbm.simplify(get(Bin_Best_Model))
    #   1: gbm.auto(grids = NULL, samples = AllDailies, expvar = expvars,
    #               resvar = "DielDive", tc = 2, lr = 0.1, bf = 0.5, n.trees = 50,
    #               ZI = "CHECK", fam1 = "bernoulli", fam2 = "gaussian", simp = TRUE,
    #               gridslat = 2, gridslon = 1, multiplot = FALSE, cols = grey.colors(1,
    #               1, 1), linesfiles = TRUE, smooth = FALSE, savegbm = TRUE,
    #               varint = TRUE, map = FALSE, shape = NULL, RSB = TRUE, BnW = FALSE,
    #               alerts = TRUE, pngtype = "cairo-png", gaus = FALSE)

    # AllDailiesTrain <- AllDailies[which(!is.na(AllDailies$TFPN)),]
    # AllDailiesTest <- AllDailies[which(is.na(AllDailies$TFPN)),]
    # gbm.auto(grids = AllDailiesTest,
    #          samples = AllDailiesTrain,
    #          expvar = expvarsB,
    #          resvar = "TFPN",
    #          tc = 2,
    #          lr = 0.01, #0l01 default, large n so shrinking
    #          bf = 0.5,
    #          n.trees = 50,
    #          ZI = "CHECK",
    #          fam1 = "bernoulli",
    #          fam2 = "gaussian",
    #          simp = TRUE, #switched off, do it manually after first run
    #          gridslat = 1, #Date, want for abundance preds sheet
    #          gridslon = 67, # fishID
    #          multiplot = FALSE,
    #          cols = grey.colors(1, 1, 1),
    #          linesfiles = TRUE,
    #          smooth = FALSE,
    #          savegbm = TRUE,
    #          varint = TRUE,
    #          map = FALSE,
    #          shape = NULL,
    #          RSB = TRUE,
    #          BnW = FALSE,
    #          alerts = TRUE,
    #          pngtype = "cairo-png",
    #          gaus = FALSE)


    #2019-11-27 Note to self####
    # UDive_5CriteriaBRT:
    # Resvar = DielDive. Expert opinion manually crafted criteria.
    # length(which(AllDailies$DielDive == 0)) #46312
    # length(which(AllDailies$DielDive == 1)) #733
    # DeeperDay TRUE (MeanDepthNight < MeanDepthDay)
    # DayDepthO TRUE (MeanDepthDay >= 230)
    # NightDepthU TRUE (MeanDepthNight <= 150)
    # MeanLt24h < 150 (if NaPctLtNight < 50)
    # (MeanLtDay - MeanLtNight) < 100 (if NaPctLtNight < 50)
    # Or if first 3 are true & NaPctLtDay > 50
    #
    # Most important are MeanDepthDay NaPctLtNight MeanDepthNight MeanLt24h NaPctLtDay
    # Plots show: MeanDepthDay >= 230
    # MeanDepthNight <= 150
    # MeanLt24h < 150
    # NaPctLtDay < 50
    # NaPctLtNight < 50
    #I.e. the model finds stark thresholds at the places where I set criteria thresholds.

    #TFPN
    # unique(AllDailies$TFPN) # 0  1 NA
    # setwd(paste0(machine, "Blocklab/abft_diving/9_UDive/"))
    # testTrainResults <- read.csv("./TFPN_TestTrain/Abundance_Preds_only.csv")
    # plot(sort(testTrainResults$PredAbund))
    # plot(sort(testTrainResults$PredAbund)[40000:42900])
    # TestTrain data predicts to 0:1 continuous, NOT binomial.
    # length(which(testTrainResults$PredAbund >= 0.05)) #1065
    # length(which(testTrainResults$PredAbund >= 0.1)) #768 # Very close to the 733 manually tagged above.
    # length(which(testTrainResults$PredAbund >= 0.2)) #504
    # Need to decide a cutoff threshold to assign 0 or 1. Right??
    # Can sort testTrainResults by PredAbund decreasing then htmlplot them
    # Also merge testTrainResults to AllDailies so.... why?
    # Maybe after deciding where to binary, can then merge into the manually tagged to complete the set.
    # setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/UShapedSquid/"))
    # source('~/Dropbox/Blocklab Monterey/Blocklab/htmlplots.R')

    # TTRhighest <- testTrainResults[which(testTrainResults$PredAbund >= 0.05),] #1065
    # TTRhighest <- TTRhighest[order(-TTRhighest[,"PredAbund"]),] #order df descending by colname. For ascending, remove minus
    # htmlplots(savename = "UDivesPredHiToLo", #n=1065
    #           plotsloc = "../TSplotMapUpgrade/",
    #           fishID = TTRhighest$fishID,
    #           dates = TTRhighest$Date)
    # RESULTS ARE WHOLLY UNCONVINCING!!!!


    #TFPN 14 expvars
    # testTrainResults <- read.csv(paste0(machine, "Blocklab/abft_diving/9_DielDive/TFPN_TestTrain_14expvars/Abundance_Preds_only.csv"))
    # This is 42900 rows long. Additional tag returns pumped through the pipeline won't get a DielDive score from this file.
    # Need to re-run the existing BRT model object to predict to the new data
    savewd <- paste0(machine, "Blocklab/abft_diving/9_DielDive/TFPN_TestTrain_14expvars/")
    setwd(savewd)

    expvarsB <- c("MeanDepth24h", "MaxDepth24h", "NaPctDepth24h", "MeanLt24h", "MeanDepthNight",
                  "MeanDepthDay", "MaxDepthNight", "MaxDepthDay", "NaPctDepthNight",
                  "NaPctDepthDay", "MeanLtNight", "MeanLtDay", "NaPctLtNight", "NaPctLtDay")

    #loadlist comparison chunk####
    # if (loadlistcompare) {
    #   savetarget <- readRDS(paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_DielDive.Rds"))
    #   savelist <- paste0(savetarget$fishID, "_", savetarget$Date) #list files based on specified pattern above
    #   loadlist <- paste0(AllDailies$fishID, "_", AllDailies$Date) #list files based on specified pattern above
    #   if (length(which(!loadlist %in% savelist)) != 0) { # if there are fishID days in (newer) Lunar & not in (older) DST
    #     print(paste0(length(which(!loadlist %in% savelist)), " fishID days present in new AllDailies & not present in old"))
    #     AllDailiesBKUP <- AllDailies # save full object for later
    #     AllDailies <- AllDailies[which(!loadlist %in% savelist),] # subset AllDailies to only those fishIDdays
    #     rm("loadlist", "savelist")
    #   } # close loadlist compare length check
    # } # close loadlistcompare

    AllDailiesTrain <- setDF(AllDailies[which(!is.na(AllDailies$TFPN)),])
    AllDailiesTest <- setDF(AllDailies[which(is.na(AllDailies$TFPN)),])
    gbm.auto(grids = AllDailiesTest,
             samples = AllDailiesTrain,
             expvar = expvarsB,
             resvar = "TFPN",
             lr = 0.01, #0.01 default, large n so shrinking
             gridslat = which(colnames(AllDailiesTrain) == "Date"), #Date, want for abundance preds sheet
             gridslon = which(colnames(AllDailiesTrain) == "fishID"), #fishID, want for abundance preds sheet
             multiplot = FALSE,
             savegbm = F,
             loadgbm = savewd, # 2021 update, gbm.auto now uses tempdir by default
             savedir = tempdir(),
             map = FALSE,
             RSB = FALSE,
             alerts = FALSE,
             BnW = FALSE,
             gaus = FALSE)
    detach("package:gbm.auto", unload = TRUE)
    testTrainResults <- read.csv(paste0(machine, "Blocklab/abft_diving/9_DielDive/TFPN_TestTrain_14expvars/TFPN/Abundance_Preds_All.csv"))
    # plot(sort(testTrainResults$PredAbund))
    # plot(sort(testTrainResults$PredAbund)[40000:42900])
    # TestTrain data predicts to 0:1 continuous, NOT binomial.
    # length(which(testTrainResults$PredAbund >= 0.05)) #1183
    # length(which(testTrainResults$PredAbund >= 0.1)) #828 near the 733 manually tagged above.
    # length(which(testTrainResults$PredAbund >= 0.2)) #554
    # setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/UShapedSquid/"))
    # TTRhighest <- testTrainResults[which(testTrainResults$PredAbund >= 0.05),] #1183
    # TTRhighest <- TTRhighest[order(-TTRhighest[,"PredAbund"]),] #order df descending by colname. For ascending, remove minus
    # htmlplots(savename = "UDivesPredHiToLo", #n=1065
    #           plotsloc = "../TSplotMapUpgrade/",
    #           fishID = TTRhighest$fishID,
    #           dates = TTRhighest$Date)
    # Results look better, reasonably believable. Do TFPN testing against my tagged data?
    # Or has that presumably already been done? It's the training data.
    # If I'm going to manaully create criteria which outperform the models then there's no point in doing the models.
    # In which case, incorporate the existing tagged data into AllDailies and move on?
    # Do more manual tagging for thoroughness?
    # ALL the machine tagged days are currently NA, not manually tagged, and so will be discarded.
    # Better to retain machine-tagged days above a threshold than just ignore them all?
    # Maybe that's the move:
    # 1. Manually set criteria based on expert knowledge
    # 2. htmlplot those criteria, manually tag a selection (big as possible)
    # 3. Use manual tags as training set, gbm.auto creates predicted probabilities of being a dive behaviour on the rest
    # 4. Have divedays be a probability. Can always use a threshold for counting later, (see last section, 0.25)
    # but can use probability gradient for plotting smooth surfaces.


    #DielDive = criteria tagged
    # length(which(AllDailies$DielDive == 0)) #46312
    # length(which(AllDailies$DielDive == 1)) #733
    #TFPN = manual tagged. Overlap with DielDive
    # unique(AllDailies$TFPN) # 0  1 NA
    # 6 separate files, criteria5:0, 5 was tagged as all 1s then edited, others tagged 0 then edited
    # 6 files' TFPNs merged in with data.table L240
    # testTrainResults$PredAbund = TFPN's NAs
    # Merge testTrainResults$PredAbund into AllDailies$TFPN by Date fishID
    # Then copy AllDailies$DielDive's 1s to TFPN since DielDive's critera are newer than those used to create TFPN5
    # Typically TFPN5 would be UDive1

    # Merge testTrainResults$PredAbund into AllDailies$TFPN by Date fishID####
    # length(which(AllDailies$TFPN == 0)) #3188
    # length(which(AllDailies$TFPN == 1)) #957
    # length(which(is.na(AllDailies$TFPN))) #42900
    if ("PredAbund" %in% colnames(testTrainResults)) colnames(testTrainResults)[which(colnames(testTrainResults) == "PredAbund")] <- "TFPN"
    testTrainResults <- testTrainResults[,c("Date", "fishID", "TFPN")]
    testTrainResults$Date <- as.Date(testTrainResults$Date)
    testTrainResults$fishID <- as.character(testTrainResults$fishID)
    # setDT(AllDailies) # convert to data.table without copy
    setDT(testTrainResults) # convert to data.table without copy
    AllDailies[testTrainResults, on = c("Date", "fishID"), TFPN := i.TFPN]
    # length(which(is.na(AllDailies$TFPN))) #0
    AllDailies$TFPN <- as.numeric(AllDailies$TFPN)
    #copy AllDailies$DielDive's 1s to TFPN####
    # First check if they're already 1s
    setDF(AllDailies) # so it prints the entire vector not a head/tail summary
    # AllDailies[which(AllDailies$DielDive == 1), "TFPN"] # mostly 1s, a few zeroes. 0s = 5/5 criteria but manually tagged as nonmeso?
    # divetyperesultsCrit5Man0 <- AllDailies[which(AllDailies$DielDive == 1 & AllDailies$TFPN == 0), c("Date", "fishID")]
    # setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/UShapedSquid/"))
    # htmlplots(savename = "UDivesCrit5Man0", #n=36
    #           plotsloc = "../TSplotMapUpgrade/",
    #           fishID = divetyperesultsCrit5Man0$fishID,
    #           dates = divetyperesultsCrit5Man0$Date)
    # All 0s were manually tagged from prior 1s so leave that way.
    # TFPN is thus complete. Remove DielDive & rename TFPN to DielDive
    # AllDailies %<>% select(-DielDive)
    AllDailies %<>% rename(DielDive = TFPN)
    setDF(AllDailies) # release from data.table
    AllDailies <- as.data.frame(AllDailies) # release from dplyr
    AllDailies$DVM <- AllDailies$DielDive > 0.25
    setDT(AllDailies) # convert to data.table without copy
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), DeeperDay := i.DeeperDay]
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), DayDepthO := i.DayDepthO]
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), NightDepthU := i.NightDepthU]
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), MeanLtU := i.MeanLtU]
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), DayMinusNtLtU := i.DayMinusNtLtU]
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), DielDive := i.DielDive]
    AllDailiesOriginal[AllDailies, on = c("Date", "fishID"), DVM := i.DVM]
  } else {# close if (!all(is.na(df_i$lat)))
    print("all new days missing latitude data, can't get external data, nothing to do")
  }
  setDF(AllDailiesOriginal)
  saveRDS(object = AllDailiesOriginal, file = paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_DielDive.Rds"))
  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE)) #crashes R?
  # gc() #garbage collection, free up memory
} # close function

#
# # HTML plotting####
# AllDailies <- readRDS("/home/simon/Documents/Si Work/Blocklab/abft_diving/All_Daily/AllDailies_DielDive.Rds")
# source('~/Dropbox/Blocklab Monterey/Blocklab/htmlplots.R')
# machine <- "/home/simon/Documents/Si Work/" #Nautilus
# # machine <- "/media/Seagate/Work/" #Poseidon
# setwd(paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/Diel/"))
# AllDailies %<>% arrange(-DielDive) # Sort large to small
# divetyperesults <- AllDailies %>%
#   filter(fishID != "5105027_C0221_NO05_L13D" &  # remove bad fish
#            fishID != "5105065_C0279_NO05_L12D" &  # remove bad fish
#            fishID != "5197124_97102_NO97_W12D" &  # remove bad fish
#            fishID != "5100134_98516_NO99_W13D" &  # remove bad fish
#            # fishID != "5111028_10P0577_CA11_P32D" &  # remove bad fish
#            DielDive == 1)
# samplerows <- sample(nrow(divetyperesults), size = 100)
# divetyperesults <- divetyperesults[samplerows,]
# htmlplots(savename = "DielDive",
#           plotsloc = "../TSplotMapUpgrade/",
#           fishID = divetyperesults$fishID,
#           dates = divetyperesults$Date)
#
# # To find DVM days when mean depth day is <230m:
# DeeperDay == TRUE
# NightDepthU == TRUE
# MeanLtU == TRUE
# DayMinusNtLtU == TRUE
# # DayDepthO : ignore
# AllDailies$MeanDepthDay # do in batches of 10? 220:230, e.g.
#
# for (i in 22:0) {
#   j <- i * 10
#   k <- j + 10
#   divetyperesults <- AllDailies %>%
#     filter(DeeperDay == TRUE &
#            NightDepthU == TRUE &
#            MeanLtU == TRUE &
#            DayMinusNtLtU == TRUE &
#            between(MeanDepthDay, j, k) &
#            DielDive == 0 #between(DielDive, 0.01, 0.0999)
#     )
#   n <- nrow(divetyperesults)
#   htmlplots(savename = paste0("MeanDepthDay", j, "-", k, "_n", n),
#             plotsloc = "../TSplotMapUpgrade/",
#             fishID = divetyperesults$fishID,
#             dates = divetyperesults$Date)
# }
#
# hist(divetyperesults$DielDive)
#
# # see MeanDepthDay_DielDivePct.xlsx
#
#
# for (i in 22:0) {
#   j <- i * 10
#   k <- j + 10
#   divetyperesults <- AllDailies %>%
#     filter(between(MeanDepthDay, j, k) &
#            DielDive > 0.25 #between(DielDive, 0.01, 0.0999)
#     )
#   n <- nrow(divetyperesults)
#   htmlplots(savename = paste0("MeanDepthDay", j, "-", k, "_n", n),
#             plotsloc = "../TSplotMapUpgrade/",
#             fishID = divetyperesults$fishID,
#             dates = divetyperesults$Date)
# }
#
# divetyperesults <- AllDailies %>%
#   filter(MeanDepthDay > 230 &
#            DielDive > 0.25 #between(DielDive, 0.01, 0.0999)
#   )
# htmlplots(savename = "MeanDepthDay+230",
#           plotsloc = "../TSplotMapUpgrade/",
#           fishID = divetyperesults$fishID,
#           dates = divetyperesults$Date)

# set DielDive > 0.25 as threshold ####
# reload clean sheet
# machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
# machine <- "/media/Seagate/Work/" #Poseidon
# AllDailies <- readRDS(paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_DielDive.Rds"))
# AllDailies$DVM <- AllDailies$DielDive > 0.25
# saveRDS(object = AllDailies, file = paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_DielDive.Rds"))
