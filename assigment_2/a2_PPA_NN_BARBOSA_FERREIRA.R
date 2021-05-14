library(spatstat)
library(maptools)
library(sf)
library(sp)
library(foreign)
library(ggplot2)
library(tidyverse)
library(maps)
library(spatialEco)

setwd("G:/My Drive/Master/Copernicus - 15 01/Classes/2ºsemester/Spatial_Statistics/assigment_2")

dt <- st_read("PPA_data/digitised_trees.shp")
st <- st_read("PPA_data/simulated_trees.shp")

#converting to ppp objects
dt_ppp <- as.ppp(dt) 
st_ppp <- as.ppp(st)

#calculates the average nearest neighbor
dt_nndist <- mean(nndist(dt_ppp, k=1))
st_nndist <- mean(nndist(st_ppp,k=1))

#sf objects to sp objects
dt_sp <- as(dt,Class = "Spatial")
st_sp <- as(st,Class = "Spatial")

# Average Nearest Neighbor Ratio 
dt_ann <- nni(dt_sp)
st_ann <- nni(st_sp)

#Histogram of simulated tree heights
st_table <- read.dbf("PPA_data/simulated_trees.dbf")

hist(st_table$HEIGHT, 
     main = "Simulated Trees' Height", 
     xlab = "Height",
     xlim = c(min(st_table$HEIGHT),max(st_table$HEIGHT)),
     col = '#B1FBE2',
     border = '#B9BFBD')

#subgroups of tree heights 
st_small <- st %>% filter(HEIGHT<10)
st_big <- st %>% filter(HEIGHT>10)

#converting to ppp objects
st_subgroup_1_ppp <- as.ppp(st_small) 
st_subgroup_2_ppp <- as.ppp(st_big)

#calculates the average nearest neighbor
group1_avg_nn <- mean(nndist(st_subgroup_1_ppp, k=1))
group2_avg_nn <- mean(nndist(st_subgroup_2_ppp,k=1))

#converting to sp objects
st_subgroup_1_sp <- as(st_small ,Class = "Spatial")
st_subgroup_2_sp <- as(st_big,Class = "Spatial")

# Average Nearest Neighbor Ratio
st_small_ann <- nni(dt_subgroup_1_sp)
st_big_ann <- nni(st_subgroup_2_sp)

#areas that are typical for each of the three point distribution patterns
plot(st_subgroup_1_ppp, use.marks=TRUE, main='',colmap="blue")
maps::map.axes(cex.axis=0.9)

# axis(side=c(1,4), 
#      at = dt_subgroup_1$x,
#      cex.axis=0.9,lwd = 0.0000002,
#      lwd.ticks = 0.000000001,
#      gap.axis = 20,
#      tick = FALSE)

#subsetting by distribution pattern groups
clustered <- subset(st_subgroup_1_ppp, subset=x>673250&y<5189200)
clustered
#plot(clustered,axis=TRUE)

dispersed <- subset(st_subgroup_1_ppp, subset=x<673250&y<5189200)
dispersed
#plot(dispersed,axis=TRUE)

random <-  subset(st_subgroup_1_ppp, subset=y>5189200)
random
#plot(random,axis=TRUE)

#Nearest Neighbor Analysis - Clustered
clustered_avg_nn <- mean(nndist(clustered, k=1))

clustered_sp <- as.SpatialPoints.ppp(clustered)
clustered_ann <- nni(clustered_sp)

#Nearest Neighbor Analysis - Dispersed
dispersed_avg_nn <- mean(nndist(dispersed, k=1))

dispersed_sp <- as.SpatialPoints.ppp(dispersed)
dispersed_ann <- nni(dispersed_sp)

#Nearest Neighbor Analysis - Random
random_avg_nn <- mean(nndist(random,k=1))

random_sp <- as.SpatialPoints.ppp(random)
random_ann <- nni(random_sp)

#Multi distance spatial cluster analysis (Ripley's K)
plot(Kest(clustered))
plot(Kest(dispersed))
plot(Kest(random))
        
#doubts
?as.ppp
?nndist
?read.dbf
?map.axes
?signature

