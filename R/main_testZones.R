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
d <- "out/allTrips_1000_facdep.RDS"
d <- readRDS(d)

##
## Aggregate the paths
d.ag <- aggit(d,"facdep")

##
## Plot it on the map
map.city <- leaflet() %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%
  setView(coord.TroisRivieres$lon, coord.TroisRivieres$lat, zoom = 11)

## color depends on flux
map1 <- NULL
df <- data.frame(i = seq_along(d.ag@lines), n = NA)

for( i in seq_along(d.ag@lines)){
  df$n[i] <- d.ag@data$facdep[i]
}
df <- df %>% arrange(n)

for( i in df$i){
  i.x <- d.ag@lines[[i]]@Lines[[1]]@coords[,1]
  i.y <- d.ag@lines[[i]]@Lines[[1]]@coords[,2]
  i.n <- d.ag@data$facdep[i]
  if( i.n == 0 ) next
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


