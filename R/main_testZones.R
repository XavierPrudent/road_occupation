#!/usr/bin/env Rscript

################################################################
## Map flux through the city of Trois-Rivieres using the OD survey
################################################################
##
## Load general packages
source("include_spatialFunctions.R")
source("/Users/lavieestuntoucan/Civilia/tech/general/load_R_pkg.R")
##
## Input data: output of osrm
d <- "out/allTrips_10.RDS"
d <- readRDS(d)
##
## Count 1 for each path
d@data$n <- 1
##
## Aggregate the paths
d.ag <- aggit(d,"n")

##
## Plot it on the map
map.city <- leaflet() %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%
  setView(coord.TroisRivieres$lon, coord.TroisRivieres$lat, zoom = 11)


## color depends on flux
map1 <- NULL
for( i in 1:length(d.ag@lines)){
  i.x <- d.ag@lines[[i]]@Lines[[1]]@coords[,1]
  i.y <- d.ag@lines[[i]]@Lines[[1]]@coords[,2]
  i.n <- d.ag@data$n[i]
  i.col <- col.n(i.n)
  if( is.null(map1) ) MAP <- map.city
  else MAP <- map1
  map1 <- addPolylines(MAP,
                       lng = i.x,
                       lat = i.y,
                       col=i.col,
                       opacity=1)
}
map1


##
## To investigate a single trip
x <- fortify(d@lines[[1]])
plot(x$long,x$lat)


