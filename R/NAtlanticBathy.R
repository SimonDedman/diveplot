# With  bathymetry from the North Atlantic (/ anywhere)
# append to existing data by latlon
# Simon Dedman 2019.04.24

# library(ncdf4)


######################################
# UPDATE TO 2020 DATA ####
# https://www.gebco.net/data_and_products/gridded_bathymetry_data/
# or the other one, SRDF or whatever?
######################################



# cmpfun(NAtlanticBathy)
# NAtlanticBathy() # or NAtlanticBathy(machine = "/media/Seagate/Work/")
NAtlanticBathy <- function(machine = "/home/simon/Documents/Si Work/", loadlistcompare = TRUE) {
  library(pbapply) #progress bar apply
  library(parallel) #cluster, detectCores
  # library(doMC) #parallel loops, not used
  library(stringr)
  library(beepr)
  library(data.table)
  options(error = function() beep(9))  # give warning noise if it fails
  # machine <- "/home/simon/Documents/Si Work/" #Nautilus
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #ensure trailing /slash
  # saveloc = paste0(machine, "Blocklab/abft_diving/9_bathy_append/") #ensure trailing /slash
  # loadloc = "/mnt/molaHomeDedman/abft_diving/4_GL_ILD_append/"
  # saveloc = "/mnt/molaHomeDedman/abft_diving/5_bathy_append/"
  # whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #
  mycores <- detectCores() - 1

  #NetCDF subset extract save GEBCO####
  # https://www.bodc.ac.uk/data/open_download/gebco/GEBCO_15SEC/zip/
  #15 arc seconds 12gb expands to 28gb, must be run on sherlock
  # GEBCO <- nc_open(filename = "GEBCO_2019.nc", verbose = T)
  # #Subset to extents # -98 36, 8 65
  # LonIdx <- which(GEBCO$dim$lon$vals < 36 & GEBCO$dim$lon$vals > -98)
  # LatIdx <- which(GEBCO$dim$lat$vals < 65 & GEBCO$dim$lat$vals > 8)
  # # see https://stackoverflow.com/questions/58631421/ncvar-get-cannot-allocate-vector-of-size-for-netcdf4-subset-no-matter-how-smal
  # z <- ncvar_get(nc = GEBCO, #get Z data for those extents
  #                varid = GEBCO$var$elevation,
  #                start = c(LonIdx[1],
  #                          LatIdx[1]),
  #                count = c(length(LonIdx),
  #                          length(LatIdx)),
  #                verbose = T)
  # lon <- GEBCO$dim$lon$vals[LonIdx]  # longitude array indexed by subset zone only
  # lat <- GEBCO$dim$lat$vals[LatIdx]   # latitude array
  # nc_close(GEBCO) #close connection
  # rm(list = c("GEBCO", "LatIdx", "LonIdx"))
  # rownames(z) <- as.character(lon)
  # colnames(z) <- as.character(lat)
  # saveRDS(object = z, file = "z.Rds")
  # saveRDS(object = lon, file = "lon.Rds")
  # saveRDS(object = lat, file = "lat.Rds")
  # library(tidyr) # pivot longer
  # library(magrittr) # %<>%
  # ztbl <- as_tibble(z, rownames = "lon")
  # ztbl %<>% pivot_longer(-lon, names_to = "lat", values_to = "depth") #don't know if I did this originally, but this makes it a 3 column df
  # detach("package:tidyr", unload = TRUE) # else loads df_i as tibble which ruins things later
  # detach("package:magrittr", unload = TRUE) # else loads df_i as tibble which ruins things later
  # saveRDS(object = ztbl, file = "ztbl.Rds")
  # rm(list = c("z", "lat", "lon"))

  setwd(loadloc) #run all as chunk
  # loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above
  # for (i in loadlist) { #sequential, i <- loadlist[1]
  #   df_i <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object
  df_i <- readRDS("AllDailies_Daynightstats.Rds")
  df_i$OceanDepth <- as.numeric(rep(NA, nrow(df_i))) #create blank container vec
  df_i$Shelf <- as.numeric(rep(NA, nrow(df_i))) #create blank container vec
  AllDailies <- df_i # for later if loadlistcompare=F else overwritten
  setDT(AllDailies) # convert to data.table without copy
  # setDT(alldaily) # convert to data.table without copy

  #loadlist comparison chunk####
  if (loadlistcompare) {
    savetarget <- readRDS("AllDailies_Bathy.Rds")
    savelist <- paste0(savetarget$fishID, "_", savetarget$Date) #list files based on specified pattern above
    loadlist <- paste0(df_i$fishID, "_", df_i$Date) #list files based on specified pattern above
    difflist <- which(!loadlist %in% savelist)

    if (length(difflist) == 0) { # if there are no differences, left join existing Bathy, to updated (typically) Daynightstats. save, clean, close
      library(dplyr)
      library(magrittr)
      savetarget %<>% dplyr::select(Date, fishID, OceanDepth, Shelf) # remove extra cols
      df_i %<>%
        select(-OceanDepth, -Shelf) %>% # remove NA columns first
        left_join(savetarget) # Joining, by = c("Date", "fishID")
      saveRDS(object = df_i, file = paste0(loadloc, "AllDailies_Bathy.Rds"))
      rm(list = ls()) #remove all objects
      beep(8) #notify completion
      stop("Success: Bathy file already existed; data were joined successfully")
      lapply(names(sessionInfo()$loadedOnly), require, character.only = TRUE)
      invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE, force = TRUE))
      gc() #garbage collection, free up memory
    } # close if if (length(difflist) == 0)

    if (length(difflist)) {
      print(paste0(length(savelist[which(savelist %in% loadlist)]), " fishID days present in old AllDailies & also present in new AllDailies; merging in"))
      library(data.table)
      AllDailies <- df_i
      setDT(AllDailies) # convert to data.table without copy
      setDT(savetarget) # convert to data.table without copy
      AllDailies[savetarget, on = c("Date", "fishID"), OceanDepth := i.OceanDepth]
      AllDailies[savetarget, on = c("Date", "fishID"), Shelf := i.Shelf]
      print(paste0(length(difflist), " fishID days present in new AllDailies & not present in old"))
      AllDailies <<- df_i # save updated full object for later
      df_i <- df_i[difflist,] # subset df_i to fishIDdays present in new AllDailies & not present in old
      rm("loadlist", "savelist", "difflist")
    } # close if (length(difflist))
  } # close loadlistcompare

  df_i <- df_i[order(-df_i[,"lat"]),] # order df_i by lat
  lastgood <- match(NA, df_i$lat) - 1 #first row with NA, minus 1 = last row of values (only do latlon rows w/ data)
  if (is.na(lastgood)) lastgood <- nrow(df_i) # if there are no NAs in lat lastgood will be NA so make it the whole vector length
  if (!lastgood == 0) { # if lastgood = 0 then all lats are NA so nothing to be done

  # Do processing
  {setwd("/mnt/molaHomeDedman/Basemaps/") #load basemap as chunk
    # GEBCO <- nc_open(filename = "GEBCO_2019_extract.nc", verbose = T)
    # z <- ncvar_get(GEBCO, GEBCO$var$z)
    # lon <- GEBCO$dim$lon$vals # longitude array
    # lat <- GEBCO$dim$lat$vals # latitude array
    # nc_close(GEBCO) #close connection
    # rm(GEBCO)
    # saveRDS(object = z, file = "GEBCO_z.Rds")
    # saveRDS(object = lon, file = "lon.Rds")
    # saveRDS(object = lat, file = "lat.Rds")
    z <- readRDS("GEBCO_z.Rds")
    lon <- readRDS("lon.Rds")
    lat <- readRDS("lat.Rds")
    beep(1)} #notify z loaded

  # GEBCO lookup function####
  if (!all(c(exists("z"), exists("lat"), exists("lon")))) print("Need z matrix and lat & lon arrays loaded")
  getz <- function(x) { # getz function for apply #x is the df
    lon_index <- which.min(abs(lon - x["lon"])) #index pos where bathy lon is closest to df_i$lon
    lat_index <- which.min(abs(lat - x["lat"]))
    z[lon_index, lat_index] #z score for those 2 indices
  } # close getz function

  df_i[(1:lastgood),"OceanDepth"] <- pbapply(df_i[(1:lastgood),c("lon","lat")], #only nonNA rows, only lat lon cols input
                                             1, getz, cl = mycores) #on rows, run function
  # cl either cluster obj via makeCluster (uses parLapply) or integer of cores (uses mclapply)
  df_i <- df_i[order(df_i[,"Index"]),] #reorder by index, dplyr kills this (does it? seems fine 2020.04.20)
  df_i$OceanDepth <- -df_i$OceanDepth #invert values, more deep = larger positive number, matching fish depth column
  # if any values are now negative, because latlon error from sat pings puts hte fish on land and the lookup correctly says "that's a mountain",
  # zero those out, since it's more intellectually honest to say they're acround the coast than it is to just kill then, given how this latlon
  # distance error is the same for ALL PAT popups, but in the middle of the north atlantic it'll be a reasonable value regardless
  if (any(df_i$OceanDepth < 0, na.rm = T)) df_i[which(df_i$OceanDepth < 0), "OceanDepth"] <- 0

  df_i$Shelf <- df_i$OceanDepth <= 130 # add shelf T/F
  df_i <- as.data.frame(df_i)
  setDT(df_i) # convert to data.table without copy
  AllDailies[df_i, on = c("Date", "fishID"), OceanDepth := i.OceanDepth]
  AllDailies[df_i, on = c("Date", "fishID"), Shelf := i.Shelf]
  } else {# close if (!all(is.na(df_i$lat)))
    print("all new days missing latitude data, can't get external data, nothing to do")
  }
  setDF(AllDailies)
  saveRDS(object = AllDailies, file = paste0(loadloc, "AllDailies_Bathy.Rds"))
  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  gc() #garbage collection, free up memory
} # close function
