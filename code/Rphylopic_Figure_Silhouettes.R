##Obtaining silhouettes for figures using R phylopic

#Author(s): Marie Gutgesell & Reilly O'Connor
#Version: 2024-07-08

#Pkgs
library(tidyverse)
library(RColorBrewer)
library(beepr)
library(rphylopic)
library(rsvg)
library(grid)

##Citation:
citation("rphylopic")

#Gearty, W. and Jones, L.A. 2023. rphylopic: An R package for fetching, transforming, and visualising PhyloPic silhouettes. Methods in Ecology and Evolution, 14(11), 2700-2708. doi: 10.1111/2041-210X.14221
##### Code #####
#Create data frame for img coords
df <- data.frame(x = c(2, 4), y = c(10, 20))

##oak tree
oak1 <- pick_phylopic(name = "Quercus ilex ballota")
oak1 <- recolor_phylopic(oak1, fill = "darkolivegreen3")
save_phylopic(img = oak1,  bg = "transparent")

##oak tree 2
oak2 <- pick_phylopic(name = "Quercus robur")
save_phylopic(img = oak2,  bg = "transparent")

##mammoth
mammoth <- pick_phylopic(name = "Mammuthus primigenius")
save_phylopic(img = mammoth,  bg = "transparent")

##pine tree
pine <- pick_phylopic(name = "pinus trifoliae")
save_phylopic(img = pine,  bg = "transparent")

##mouse
mouse <- pick_phylopic(name = "rattus exulans")
save_phylopic(img = mouse,  bg = "transparent")


##corn
corn <- pick_phylopic(name = "Zea mays")
save_phylopic(img = corn,  bg = "transparent")

##cod
cod <- pick_phylopic(name = "gadus morhua")
save_phylopic(img = cod,  bg = "transparent")

##human
human <- pick_phylopic(name = "Homo neanderthalensis")
save_phylopic(img = human,  bg = "transparent")
