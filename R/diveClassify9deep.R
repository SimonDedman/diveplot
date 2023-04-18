#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simon Dedman simondedman@gmail.com started 2019.11.27
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
# cmpfun(DeepDive)
# DeepDive() # DeepDive(machine = "/media/Seagate/Work/")
DeepDive <- function(machine = "/home/simon/Documents/Si Work/",
                         loadlistcompare = TRUE) { # run everything (as function
# machine <- "/home/simon/Documents/Si Work/" #Nautilus
# machine <- "/media/Seagate/Work/" #Poseidon
loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #ensure trailing /slash
setwd(loadloc) #run all as chunk
AllDailies <- readRDS("AllDailies_DielDive.Rds")
AllDailies$DeepDive <- rep(0, nrow(AllDailies))
AllDailies[which(AllDailies$MaxDepth24h >= 500), "DeepDive"] <- 1
saveRDS(object = AllDailies, file = paste0(loadloc, "AllDailies_DeepDive.Rds"))
rm(list = ls()) #remove all objects
# gc() #garbage collection, free up memory
} # close function
