# TS plot loop: generate top/bottom facet plots of:

#TODO####
# Add light level as element to first plot, convert to depth scale, add second RHS Y axis
# Resize plot to double height (?) Extend depth from 0:1000 to 0:1500, leaving space in the centre for map?
# Add map background
# Move second Y scale up on top plot so int temp isn't often cut out when below 0

# 1D: Map: super opaque as the background; land very light grey
# Fish track could either be a dot for just where it is now,
# or a slightly-darker-grey line with daily dots, and a black arrow for today's point, facing tomorrow's point.
# That last bit might be tricky, and direction will be obvious by skipping to the next few plots,
# but it would probably be better technically if the required info was on the plot.
# For weekly/monthly plots, same thing but with 7/31 dots instead. Maybe only the last one has an arrow

# My question on SO: https://stackoverflow.com/questions/58827777/can-one-overlay-a-line-plot-on-top-of-a-map-in-r
# Make map a background image then plot on top of that
# https://www.engineeringbigdata.com/how-to-add-a-background-image-in-ggplot2-with-r/
# https://stackoverflow.com/questions/42333085/how-to-fit-the-plot-over-a-background-image-in-r-and-ggplot2

# Per day, per week
# https://stackoverflow.com/questions/45746758/arrange-multiple-ggplots-from-a-list-on-multiple-pages-with-specified-page-break

# ocean depth / bathymetry to bottom plot [not attached by 7_DO2_append], Do bathy & lunar append daily pipeline and add these later, OceanDepth
# map to bottom plot
# Lunar cycle: as graphic under each day, bottom plot. Progressing crescent scale (images?) LunarPhase


# Loop code####
# Simon Dedman simondedman@gmail.com started 2019.11.13
# 24h day, night & day stats


# cmpfun(TSplots)
# TSplots() # or TSplots(machine = "/media/Seagate/Work/")
TSplots <- function(machine = "/home/simon/Documents/Si Work/",
                    loadlistcompare = TRUE,
                    labelbehaviour = TRUE) {
  library(data.table) # nafill locf. Masks between, first, last (dplyr) & hour, isoweek, mday, minute, month, quarter, second, wday, week, yday, year (lubridate) so load first
  library(lubridate)
  library(gridExtra) # grid.arrange
  library(dplyr)
  library(tidyr) # replace_na
  library(beepr)
  library(magrittr) # %$%
  library(ggplot2)
  library(fs) # dir_create
  library(stringr) # str_remove
  library(scales) # date_format
  library(foreach)
  library(doMC)
  library(progress)
  library(sf)
  library(grid) # imagegrob
  library(ggspatial)
  library(compiler)
  options(error = function() beep(9))  # give warning noise if it fails

  # machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
  # machine <- "/media/Seagate/Work/" #Poseidon
  loadloc = paste0(machine, "Blocklab/abft_diving/7_DO2_append/") #ensure trailing /slash
  saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/TSplots/") #ensure trailing /slash
  whichfiles <- ".Rds" # "5112003_L231-0194_L_D" #
  mycores <- 4 #Nautilus
  # mycores <- 12 #Aquarius
  # mycores <- 16 #Poseidon

  AllDailies <- readRDS(paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_ForageDive.Rds")) %>%
    select(Date, fishID, OceanDepth, LunarPhase, DVM, DeepDive, kmeans2cluster,
           Hrs50mLesDepRange, ForageHours, ForageDepth, WhichForageHours,
           ForageHoursDepth, WhichForageHoursA, WhichForageHoursB, WhichForageHoursC,
           Umidtime, UdurMins, Umeandepth)

  setwd(loadloc) #run all as chunk
  loadlist <- list.files(pattern = whichfiles) #list files based on specified pattern above

  #loadlist comparison chunk####
  if (loadlistcompare) {
    setwd(saveloc)
    savelist <- list.files(include.dirs = TRUE) #list files based on specified pattern above
    print("in loadlist but not savelist, not processed?")
    print(loadlist[which(!str_remove(loadlist, whichfiles) %in% savelist)])
    loadlist <- loadlist[which(!str_remove(loadlist, whichfiles) %in% savelist)] # update loadlist for rerun
    setwd(loadloc)
  } # close loadlist compare

  registerDoMC(cores = mycores)  #number of CPU cores. 4 laptop 8 pc. should be 8 & 16? Can do 2 threads per core.
  pbTracker <- function(pb,i,mycores) {if (i %% mycores == 0) {pb$tick()}} #i & mycores need to be numbers
  pb <- progress_bar$new(
    format <- " progress [:bar] :percent eta: :eta",
    total <- length(loadlist) / mycores, clear = FALSE, width = 60)
  writeLines(c(""), "log.txt")

  foreach(i = loadlist, .errorhandling = "pass") %dopar% { #parallel #i <- loadlist[1]
    sink("log.txt", append = TRUE) #sink output to log, sink stack is full error
    # for (i in loadlist) { #sequential # i <- loadlist[1] # i <- loadlist[56]
    whichi <- which(i %in% loadlist)
    pbTracker(pb, whichi, mycores) #doesn't work?

    df_i <- readRDS(i) #assign file to name, prepend with x to avoid numerical named object
    print(paste0(which(loadlist == i), " of ", length(loadlist), "; generating timeseries plots for ", i))
    df_i$fishID <- str_remove(i, whichfiles) #get fishID, append to df_i for dir_create later

    # Whole track line####
    # Get whole track, need to do before shrinking df_i in cases of adding a few days
    # else 'whole track' would be just those days
    track <- df_i %>%
      group_by(Date) %>%
      summarise(xlon = mean(lon, na.rm = T),
                ylat = mean(lat, na.rm = T)) %>%
      ungroup()

    # # Missing plots comparison chunk####
    if (loadlistcompare) {
      if (unique(df_i$fishID) %in% savelist) { #if folder already present
        setwd(paste0(saveloc, unique(df_i$fishID)))
        plotlist <- list.files(pattern = ".png") #list files based on specified pattern above
        setwd(loadloc)
        plotlist <- str_sub(str_remove(plotlist, ".png"), start = -10, end = -1)
        daylist <- as.character(unique(df_i$Date))
        plotdays <- as.Date(daylist[which(!daylist %in% plotlist)])
        print(paste0("Number of days to plot: ", length(plotdays)))
        if (length(plotdays) == 0) next# if there are no missing days skip to next
        # <simpleError in eval(c.expr, envir = args, enclos = envir): no loop for break/next, jumping to top level>
        # foreach error: can't use next, see https://stackoverflow.com/questions/7707467/next-with-revolution-rs-foreach-package
        # could have an if isforeachloop return(NULL) else next
        df_i %<>% filter(Date %in% plotdays) #else subset to missing days only
      } # close if folder already present
    } #close loadlistcompare

    df_i %$% dir_create(paste0(saveloc, unique(fishID)))
    df_i %<>%
      left_join(AllDailies, by = c("Date", "fishID")) %>% # Joining, by = c("Date", "fishID"); adds oceandepth & lunar phase
      select(DateTimeUTCmin5, Depth.m., Lt, IntTemp.C., ExtTemp.C., ExcessTemp.C.,
             TempVelo, Date, fishID, lat, lon, age, dawn, dusk, ild_depth_gl,
             OceanDepth, LunarPhase, DawnSpike, DuskSpike, SpikeDives, DVM, DeepDive,
             kmeans2cluster, Hrs50mLesDepRange, ForageHours, ForageDepth,
             WhichForageHours, ForageHoursDepth, WhichForageHoursA,
             WhichForageHoursB, WhichForageHoursC, Umidtime, UdurMins, Umeandepth)

    # Added ExcessTemp.C. for feeding SDA. Need IntTempVelo, calc here:
    if (nrow(df_i) > 1) {# will crash if nrow df_i = 1 (due to 1 plotday missing), skip if so
    time1 <- df_i$DateTimeUTCmin5[1:(nrow(df_i) - 1)] #time except last row
    time2 <- df_i$DateTimeUTCmin5[2:(nrow(df_i))] # time except first
    inttemp1 <- df_i$IntTemp.C.[1:(nrow(df_i) - 1)] #subset depth except last row
    inttemp2 <- df_i$IntTemp.C.[2:(nrow(df_i))] # depth except first row
    itv <- (inttemp2 - inttemp1)/as.numeric(difftime(time1 = time2, #diff in depth / diff in time
                                                     time2 = time1,
                                                     units = "mins")) #metres/min
    df_i$IntTempVelo <- rep(NA, nrow(df_i)) #ensures first row is NA
    df_i$IntTempVelo[2:(nrow(df_i))] <- itv #from second row so velocities align with after movement rather than before
    } # close nrow check

    # Make NA values in DVM FALSE else IF fails
    df_i$DVM %<>% replace_na(FALSE)

    #Transform data for plot####
    df_i$Depth.m. <- 1000 - df_i$Depth.m. #recalibrate depth to the difference from 1000 for 2nd Y axis plotting
    df_i$ild_depth_gl <- 1000 - df_i$ild_depth_gl # same for thermocline depth
    df_i$ild_depth_gl <- nafill(df_i$ild_depth_gl, "locf") # fill in NAs since it's 1 reading per 6 hours, as opposed to bad data which are missing
    df_i$OceanDepth <- 1000 - df_i$OceanDepth # ditto OceanDepth
    depthbreaks <- c(0, 25, 50, 75, 100, 150, 200, 250, 300, 400, 500, 600, 700, 800, 900, 1000) # nice breaks for depth
    depthbreakslabels <- as.character(depthbreaks)
    depthbreaks <- 1000 - depthbreaks
    # 1000/35 = 28.57143 #conversion to have temp plotted on depth scale (which is the coordinate system)

    #Deepdive####
    Deeps <- df_i %>%
      group_by(Date) %>%
      slice(which.min(Depth.m.)) %>%
      ungroup() %>%
      select(Date, DateTimeUTCmin5, Depth.m.) %>%
      rename(DeepestDateTime = DateTimeUTCmin5,
             DeepestDepth = Depth.m.)
    df_i %<>% left_join(Deeps, by = "Date")

    #ForageHours####
    df_i$Hours <- hour(df_i$DateTimeUTCmin5)

    # colours####
    # named colour lookups for plot and legend (scale_colour_manual)
    mycols <- c("Thermocline Depth" = "cyan4", #cyan darkorchid3
                "Ocean Depth" = "chocolate4",
                "Depth" = "blue",
                "Internal Temperature" = "red",
                "External Temperature" = "green",
                "Excess Temperature (smoothed)" = "darkorange1", #https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
                "Light Level" = "magenta")

    #Map Section####
    # You can do all of this before plotting
    # Convert map & position from latlon to cartesian coordinates within our existing plot range
    #x range: 0:1000. lat 10:65
    latrange = c(12.6, 65) # 10 became 12.6 once image cropped to match plot size
    latrangeadj <- latrange - latrange[1] # 0:55 now 0:52.4
    yrange = c(0,1000)
    ylatscalar =  yrange[2] / latrangeadj[2] #18.18182 # 19.08
    # latrangeadj * ylatscalar
    # ((10:65) - 10) * ylatscalar
    # (latrange - 10) * ylatscalar
    # lats <- dfi2$lat * ylatscalar # 1 value per pollingrate due to interpolation

    #y range: 0:00 %H date_format to 23:59:59. lon -95:35
    lonrange = c(-125,65) #c(-95,35)
    lonrangeadj <- lonrange - lonrange[1] # 0:130 # 0:190
    # xrange = c(min(dfi2$DateTimeUTCmin5), max(dfi2$DateTimeUTCmin5)) # "1999-01-04 00:01:00 EST" "1999-01-04 23:59:00 EST"
    # secsdiff <- as.numeric(difftime(xrange[2], xrange[1], units = "secs")) # 23.9667 hours, 86280 secs
    # xlonscalar = secsdiff / lonrangeadj[2] #663.6923 (secs, between each degree of 0:130)
    # lonsecs <- (dfi2$lon + 95) / 130 * secsdiff # lons as seconds past 00:00:00
    # lons <- xrange[1] + lonsecs

    # convert lat to inverted depth
    track$ylatadj <- (track$ylat - latrange[1]) * ylatscalar
    # convert lon to time of day, have to add today's date per ggplot slide, below
    # can only do this bit now, zeroing to today & multiplying by today's number of seconds has to happen below
    track$xlonadj <- (track$xlon - lonrange[1]) / lonrangeadj[2]

    # Basemap
    # natlantic <- sf::read_sf(paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map.shp"))
    # natlantic2 <- sf::st_crop(natlantic, xmin = -125, ymin = 10, xmax = 65, ymax = 65) #xmin = -98, ymin = 10, xmax = 36, ymax = 65
    natlantic_image <- png::readPNG("/home/simon/Dropbox/Blocklab Monterey/Data/Maps/natlantic_outline1465x509alpha.png")
    imagegrob <- rasterGrob(natlantic_image, width = unit(1,"npc"), height = unit(1,"npc"), interpolate = FALSE)

    # Kmeans for latlon point pch####
    df_i$kmeans2cluster %<>% replace_na("NA") # as explicit category name for latlon point

    #Depth & Temp P1####
    p1 <- df_i %>% group_by(Date) %>%
      do(Temp = ggplot(data = .) + #p1<-df_i %>%ggplot()+ #original
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = ild_depth_gl, colour = "Thermocline Depth"), linetype = "dashed", size = rel(0.5)) +
           #thermocline first so it's underneath
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = OceanDepth, colour = "Ocean Depth"), linetype = "dashed", size = rel(0.5)) + #OceanDepth underneath
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = Depth.m., colour = "Depth"), size = rel(0.5)) + # Depth
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = IntTemp.C. * 28.57143, colour = "Internal Temperature"), size = rel(0.5)) + #Internal Temperature
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = ExtTemp.C. * 28.57143, colour = "External Temperature"), size = rel(0.5)) +
           geom_smooth(mapping = aes(x = DateTimeUTCmin5, y = ExcessTemp.C. * 28.57143, colour = "Excess Temperature (smoothed)"), size = rel(0.5),
                       method = "loess", se = FALSE, span = 0.05) + #span controls wiggliness of smoother, fraction of points used to fit each local regression:
           # small numbers make a wigglier curve, larger numbers make a smoother curve.
           # geom_line(mapping = aes(x = DateTimeUTCmin5, y = ExcessTemp.C. * 28.57143, colour = "Excess Temperature"), size = rel(0.5)) +
           {if (!is.na(first(.$DawnSpike))) geom_vline(xintercept = first(.$dawn), linetype = "dashed", colour = "darkgoldenrod1", size = rel(0.6))} + # Dawn
           {if (!is.na(first(.$DawnSpike))) if (first(.$DawnSpike)) geom_vline(xintercept = first(.$dawn), linetype = "solid", colour = "darkgoldenrod1", size = rel(0.6))} + # Dawn
           {if (!is.na(first(.$DawnSpike))) geom_vline(xintercept = first(.$dusk), linetype = "dashed", colour = "darkgoldenrod1", size = rel(0.6))} + # Dusk
           {if (!is.na(first(.$DawnSpike))) if (first(.$DuskSpike)) geom_vline(xintercept = first(.$dusk), linetype = "solid", colour = "darkgoldenrod1", size = rel(0.6))} + # Dusk

           annotate("rect", alpha = 0.2, fill = "grey",  ymin = 1000, ymax = 0, #adds rectangle(s) manually on top to highlight Foraging
                    xmin = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (first(.$WhichForageHours) * 3600),
                    # floordate = base date since first day won't start at 00:00 then add vector of hour integers, convert to seconds= *60*60 aka *3600
                    # WhichForageHours is a vector, annotate() actions each WhichForageHours hour separately like an apply() function i.e. this adds as many
                    # rectangles as there are list entries in today's WhichForageHours
                    xmax = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (first(.$WhichForageHours) * 3600) + 3600) + # add 1 hour

           # ForageHours A/B/C labels @ 900m depth for each qualifying hours
           annotate("text", label = "A", alpha = 0.7, colour = "black", y = 75, # Y=0:1000 going up: ignore axis labels in finished plot
                    x = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (first(.$WhichForageHoursA) * 3600) + 900) + # add 15 mins
           annotate("text", label = "B", alpha = 0.7, colour = "black", y = 75,
                    x = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (first(.$WhichForageHoursB) * 3600) + 1800) + # add 30 mins
           annotate("text", label = "C", alpha = 0.7, colour = "black", y = 75,
                    x = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (first(.$WhichForageHoursC) * 3600) + 2700) + # add 45 mins
           # Umidtime, UdurMins, Umeandepth
           annotate("text", label = paste0("U:", first(.$UdurMins), "mins"), alpha = 0.7, colour = "black", y = 975, # 25m line
                    x = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (as.numeric(first(.$Umidtime)) * 60)) + # mins to secs
           annotate("text", label = paste0(first(.$Umeandepth), "m"), alpha = 0.7, colour = "black", y = 950, # 25m line
                    x = floor_date(first(.$DateTimeUTCmin5), unit = "days") + (as.numeric(first(.$Umidtime)) * 60)) + # mins to secs
           # add a second one, same X lower Y with mean depth

           scale_x_datetime(labels = date_format("%H", tz = "America/New_York"),
                            # limits: ensure "EST", tz, labels tz, and as.Date tz, are all aligned
                            limits = c(as.POSIXct(paste0(as.character(first(.$Date)), " 00:00:00 EST"), tz = "America/New_York"),
                                       as.POSIXct(paste0(as.character(first(.$Date)), " 23:59:59 EST"), tz = "America/New_York")),
                            date_breaks = "1 hour",
                            minor_breaks = NULL,
                            position = "top",
                            expand = expand_scale(mult = 0, add = 180), #limit to 0:23hrs, plus a bit to get the 00:00 line at the end
                            sec.axis = sec_axis(~ . ,
                                                name = NULL,
                                                breaks = c(first(.$dawn), first(.$dusk)), # dawn & dusk positions (in time)
                                                labels = NULL)) + #c("Dawn", "Dusk")
           scale_y_continuous(limits = c(0, 1000), #could set this higher?
                              expand = expand_scale(mult = 0, add = 0),
                              #keep tight to margins; add to keep title underscores off 0m depth lines. But sticks little lines out
                              # Fix this by removing the title and instead generating it within the multiplot?
                              breaks = depthbreaks,
                              minor_breaks = NULL,
                              labels = depthbreakslabels,
                              sec.axis = sec_axis(~ . / 28.57143,
                                                  name = "Temperature (°C)",
                                                  breaks = seq(from = 0, to = 35, by = 5))) +
           labs(y = "Depth (m)",
                x = NULL,
                title = paste0(first(df_i$fishID), "      ", .$Date,  "      Age: ", round(mean(.$age), digits = 2))) + # "Time (hours)"
           theme_minimal() %+replace% theme(axis.title.y.right = element_text(angle = 90),
                                            plot.title = element_text(hjust = 0.5,
                                                                      size = 13),
                                            axis.text.x = element_text(size = rel(1.2)),
                                            axis.text.y = element_text(size = rel(1.2)),
                                            axis.title.x = element_text(size = rel(1.2)),
                                            axis.title.y = element_text(size = rel(1.2),
                                                                        angle = 90),  #overwriting size screws elements
                                            legend.position = c(0.5, 0.01), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
                                            legend.direction = "horizontal",
                                            legend.spacing.x = unit(0.1, 'cm'), #compress spacing between legend items, 0 is min
                                            legend.text = element_text(size = rel(1.2)),
                                            panel.grid.major  = element_line(size = rel(0.5))) +
           scale_colour_manual(name = NULL,
                               values = mycols,
                               guide = guide_legend(override.aes = aes(fill = NA),
                                                    nrow = 1)) # can't change linetype, is fine
      ) # close do

    #Light P2####
    # 1000/450 = 2.222222 #conversion to have temp plotted on depth scale (which is the coordinate system)
    p2 <- df_i %>% group_by(Date) %>%
      do(Light = ggplot(data = .) + #p2<-df_i %>%ggplot()+ #original
           annotation_custom(imagegrob, -Inf, Inf, -Inf, Inf) + # add background image map
           annotate("path", y = track$ylatadj, # add whole fish track
                    x = min(.$DateTimeUTCmin5) +
                      (track$xlonadj *
                         as.numeric(difftime(max(.$DateTimeUTCmin5),
                                             min(.$DateTimeUTCmin5),
                                             units = "secs"))),
                    colour = "grey", size = rel(0.4)) +
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = ild_depth_gl, colour = "Thermocline Depth"), linetype = "dashed", size = rel(0.5)) +
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = OceanDepth, colour = "Ocean Depth"), linetype = "dashed", size = rel(0.5)) +
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = Depth.m., colour = "Depth"), size = rel(0.5)) + # Depth
           geom_line(mapping = aes(x = DateTimeUTCmin5, y = Lt * 2.222222, colour = "Light Level"), size = rel(0.5)) +
           {if (!is.na(first(.$DawnSpike))) geom_vline(xintercept = first(.$dawn), linetype = "dashed", colour = "darkgoldenrod1", size = rel(0.6))} + # Dawn
           {if (!is.na(first(.$DawnSpike))) if (first(.$DawnSpike)) geom_vline(xintercept = first(.$dawn), linetype = "solid", colour = "darkgoldenrod1", size = rel(0.6))} + # Dawn if
           {if (!is.na(first(.$DawnSpike))) geom_vline(xintercept = first(.$dusk), linetype = "dashed", colour = "darkgoldenrod1", size = rel(0.6))} + # Dusk
           {if (!is.na(first(.$DawnSpike))) if (first(.$DuskSpike)) geom_vline(xintercept = first(.$dusk), linetype = "solid", colour = "darkgoldenrod1", size = rel(0.6))} + # Dusk if
           {if (!is.na(any(first(.$DeepestDepth), first(.$DeepestDateTime)))) if (1000 - first(.$DeepestDepth) >= 500) geom_vline(xintercept = first(.$DeepestDateTime), colour = "darkblue", size = rel(0.6), linetype = "solid")} + # Deep .[which.max(.$Depth.m.), "DateTimeUTCmin5"]
           {if (!is.na(any(first(.$DeepestDepth), first(.$DeepestDateTime)))) if (1000 - first(.$DeepestDepth) < 500) geom_vline(xintercept = first(.$DeepestDateTime), colour = "darkblue", size = rel(0.6), linetype = "dashed")} + # Deep .[which.max(.$Depth.m.), "DateTimeUTCmin5"]

           # add latlon point after converting, track
           {if (first(.$kmeans2cluster) == "transit")
             annotate("point",
                      x = min(.$DateTimeUTCmin5) + #centres range at 00:00
                        ((mean(.$lon, na.rm = T) - lonrange[1]) / lonrangeadj[2] * # longitude as % of range e.g. 0.2
                           as.numeric(difftime(max(.$DateTimeUTCmin5), min(.$DateTimeUTCmin5), units = "secs"))), # convert to time secs
                      #x=lons above, all bundled together in 1 line.
                      # 5100107 day1 1999-01-01 mean lat 34.37212 lon-76.4084
                      y = (mean(.$lat, na.rm = T) - latrange[1]) * ylatscalar, pch = 1)} + #pch open circle
           {if (first(.$kmeans2cluster) == "resident")
             annotate("point", x = min(.$DateTimeUTCmin5) + ((mean(.$lon, na.rm = T) - lonrange[1]) / lonrangeadj[2] *
                                                               as.numeric(difftime(max(.$DateTimeUTCmin5), min(.$DateTimeUTCmin5), units = "secs"))),
                      y = (mean(.$lat, na.rm = T) - latrange[1]) * ylatscalar, pch = 20)} + #pch filled circle
           {if (first(.$kmeans2cluster) == "NA")
             annotate("point", x = min(.$DateTimeUTCmin5) + ((mean(.$lon, na.rm = T) - lonrange[1]) / lonrangeadj[2] *
                                                               as.numeric(difftime(max(.$DateTimeUTCmin5), min(.$DateTimeUTCmin5), units = "secs"))),
                      y = (mean(.$lat, na.rm = T) - latrange[1]) * ylatscalar, pch = 4)} + #pch X

           scale_x_datetime(labels = date_format("%H", tz = "America/New_York"),
                            # limits: ensure "EST", tz, labels tz, and as.Date tz, are all aligned
                            limits = c(as.POSIXct(paste0(as.character(first(.$Date)), " 00:00:00 EST"), tz = "America/New_York"),
                                       as.POSIXct(paste0(as.character(first(.$Date)), " 23:59:59 EST"), tz = "America/New_York")),
                            date_breaks = "1 hour",
                            minor_breaks = NULL,
                            position = "top",
                            expand = expand_scale(mult = 0, add = 180), #limit to 0:23hrs, plus a bit to get the 00:00 line at the end
                            sec.axis = sec_axis(~ . ,
                                                name = NULL,
                                                breaks = c(first(.$dawn), first(.$DeepestDateTime), first(.$dusk)), # dawn & dusk positions (in time)
                                                labels = c("Dawn", paste0("Deepest: ", 1000 - first(.$DeepestDepth), "m"), "Dusk"))) +
           scale_y_continuous(limits = c(0, 1000), #could set this higher?
                              expand = expand_scale(mult = 0, add = 0), #keep tight to margins; add to keep title underscores off 0m depth lines
                              breaks = depthbreaks,
                              minor_breaks = NULL,
                              labels = depthbreakslabels,
                              sec.axis = sec_axis(~ . / 2.222222, name = "Light Level",
                                                  breaks = seq(from = 0, to = 450, by = 50))) +
           labs(y = "Depth (m)",
                x = "Time (hours)") + # ,title = paste0(first(df_i$fishID), " - ", first(df_i$Date))
           theme_minimal() %+replace% theme(axis.title.y.right = element_text(angle = 90),
                                            plot.title = element_text(hjust = 0.5,
                                                                      size = 13),
                                            axis.text.x = element_text(size = rel(1.2)),
                                            axis.text.y = element_text(size = rel(1.2)),
                                            axis.title.x = element_text(size = rel(1.2)),
                                            axis.title.y = element_text(size = rel(1.2),
                                                                        angle = 90),  #overwriting size screws elements
                                            legend.position = c(0.5, 0.03), #%dist (of middle? of legend box) from L to R, %dist from Bot to Top
                                            legend.direction = "horizontal",
                                            legend.spacing.x = unit(0.1, 'cm'), #compress spacing between legend items, 0 is min
                                            legend.text = element_text(size = rel(1.2)),
                                            panel.grid.major  = element_line(size = rel(0.5))) +
           {if (first(.$DVM) == TRUE) theme() %+replace% theme(panel.border = element_rect(colour = "red", fill = NA, size = 1))} +
           scale_colour_manual(name = NULL,
                               values = mycols,
                               guide = guide_legend(override.aes = aes(fill = NA))) # can't change linetype, is fine
      ) # close do

    p1 <- cbind(p1, p2[,2]) # join list dfs
    # plot save loop####
    print(paste0("saving daily png plots for ", str_remove(i, whichfiles)))
    if (nrow(p1) > 1) {pb <- txtProgressBar(min = 1, max = nrow(p1), style = 3)} # create progress bar unless it's only 1 day
    for (j in 1:nrow(p1)) { #length of p1 & p2 need to be the same each time. Should be, both grouped on Date
      if (nrow(p1) > 1) {setTxtProgressBar(pb, j)} # update progress bar
      ggsave(file = paste0(saveloc, str_remove(i, whichfiles), "/", str_remove(i, whichfiles), "_", p1$Date[j], ".png"), #plot_df$Date[j]
             plot = arrangeGrob(p1$Temp[[j]], p1$Light[[j]]),
             device = "png",
             path = "",
             scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
             width = 10, #NA default. Manually adjust plot box in RStudio after ggplot() 6.32
             height = 7.3, #NA default; Then ggsave with defaults, changes from 7x7" to e.g. 4
             units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
             dpi = 90, #originally 300, tried 72, 90 is a picturesize vs filesize tradeoff sweetspot
             limitsize = TRUE)
    }
    if (nrow(p1) > 1) {close(pb)} # close for j save loop & progress bar
    # us letter: 8.5 by 11 inches (215.9 by 279.4 mm), A4 8.3 x 11.7in, 210 x 297mm, 1" margin: 7.5 x 10 & 7.3 x 10.7. Min: 7.3 x 10

    # lapply(p1, FUN = )
    # apply(p1, MARGIN = 1, function(d) ggsave(file = paste0(saveloc, str_remove(i, whichfiles), "/", str_remove(i, whichfiles), "_", d[1], ".png"), #plot_df$Date[j]
    #                                         plot = arrangeGrob(d[2], d[3]),
    #                                         device = "png",
    #                                         path = "",
    #                                         scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
    #                                         width = 10, #NA default. Manually adjust plot box in RStudio after ggplot() 6.32
    #                                         height = 7.3, #NA default; Then ggsave with defaults, changes from 7x7" to e.g. 4
    #                                         units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
    #                                         dpi = 90, #originally 300, tried 72, 90 is a picturesize vs filesize tradeoff sweetspot
    #                                         limitsize = TRUE))
    # Error in gList(list(Temp = list(data = list(DateTimeUTCmin5 = c(915213660,:only 'grobs' allowed in "gList"
    # Don't know how to reference the different columns in an apply function on rows
    # https://eriqande.github.io/rep-res-web/lectures/functions_and_lapply.html  # Error in d[, 1] : incorrect number of dimensions

    #     p1 %>% group_by(Date) %>%
    #       do(ggsave(file = paste0(saveloc, str_remove(i, whichfiles), "/", str_remove(i, whichfiles), "_", .$Date, ".png"), #plot_df$Date[j]
    #              plot = arrangeGrob(.$Temp, .$Light),
    #              device = "png",
    #              path = "",
    #              scale = 1.75, #changes how big lines & legend items & axes & titles are relative to basemap. Smaller number = bigger items
    #              width = 6.32, #NA default. Manually adjust plot box in RStudio after ggplot()
    #              height = 4, #NA default; Then ggsave with defaults, changes from 7x7" to e.g.
    #              units = "in", #c("in", "cm", "mm"); 6.32x4, tweak as necessary. Removes canvas whitespace
    #              dpi = 72,
    #              limitsize = TRUE))
    #     # change size to neatly print on a landscape US letter AND A4
    #
    #     # ,
    #     # pointsize = 10,
    #     # bg = "white"
    #
    # # https://stackoverflow.com/questions/31947727/save-multiple-ggplot2-plots-as-r-object-in-list-and-re-displaying-in-grid
    #     #lapply approach
    #
    # # multiple page arrange grob.
    #   ml <- marrangeGrob(pl, nrow=2, ncol=2)
    #   ggsave("multipage.pdf", ml)
    # sink() # close sink https://stackoverflow.com/questions/26296288/how-to-avoid-sink-stack-is-full-error-when-sink-is-used-to-capture-messages
  } #close i / for foreach
  rm(list = ls()) #remove all objects
  beep(8) #notify completion
  invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only = TRUE, unload = TRUE)) #unload detach all packages
  gc() #garbage collection, free up memory
} # close function

#htmlplots per fish####
# #generates HTML file for all plots in all fish directories
# library(stringr)
# source('~/Dropbox/Blocklab Monterey/Blocklab/htmlplots.R')
# machine <- "/home/simon/Documents/Si Work/" #Nautilus/Aquarius
# # machine <- "/media/Seagate/Work/" #Poseidon
# saveloc = paste0(machine, "Blocklab/abft_diving/X_PlotsMisc/TSplots/") #ensure trailing /slash
# setwd(saveloc)
# savelist <- list.dirs(full.names = FALSE, recursive =  FALSE)
# # edit savelist per your preference
# for (k in savelist) {
#   print(paste0(which(savelist == k), " of ", length(savelist), "; generating htmlplots for ", k))
#   setwd(k)
#   dateslist <- str_sub(list.files(pattern = ".png"), -14, -5)
#   htmlplots(savename = k,
#             plotsloc = "../",
#             fishID = k,
#             dates = dateslist)
#   setwd("../")
# }
