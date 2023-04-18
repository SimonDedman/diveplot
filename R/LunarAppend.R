# Add Lunar phase to data based on date. Simon Dedman 23/09/2019
# Levels: New Waxing Full Waning

LunarAppend <- function(machine = "/home/simon/Documents/Si Work/") {
  library(lunar) #lunar.phase
  library(beepr)
  options(error = function() beep(9))  # give warning noise if it fails
  # library(compiler)
  # cmpfun(LunarAppend)
  # LunarAppend() # or LunarAppend(machine = "/media/Seagate/Work/")

  # machine <- "/home/simon/Documents/Si Work/" #Nautilus
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #ensure trailing /slash
  setwd(loadloc) #run all as chunk
  df_i <- readRDS("AllDailies_Bathy.Rds")
  df_i$LunarPhase <- lunar.phase(x = df_i$Date, shift = -5, name = TRUE) # ,name = 4 # for factor names instead of radians
  saveRDS(object = df_i, file = paste0(loadloc, "AllDailies_Lunar.Rds"))
  rm(list = ls()) #remove all objects
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  gc()
} # close function
