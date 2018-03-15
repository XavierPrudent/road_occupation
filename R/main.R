#!/usr/bin/env Rscript

################################################################
## Compute flux through the city of Trois-Rivieres using the OD survey
################################################################
##
## Package dir
setwd("/Users/lavieestuntoucan/civ-3r/road_occupation/R")

##
## Load general packages
source("~/Civilia/tech/general/load_R_pkg.R")

##
## Coords 3 rivières and city map
coord <- data.frame(lon=-72.573058,lat=46.347738)
map <- leaflet() %>%
  addTiles('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png') %>%
  setView(coord$lon, coord$lat, zoom = 13)

##
## OD survey
od <- readRDS("data/enquete_OD_2011/Trv11pv2a.RDS")
##
## Motorised transports
motor <- c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12)
od <- filter(od,mode1 %in% motor | mode2 %in% motor | mode3 %in% motor )
##
## Run OSRM
options(osrm.server = "http://localhost:5000/")
##
## For each trip, estimate the path
trips <- vector("list",length=nrow(od))
for( i in 1:nrow(od)){
  cat(paste(i,"/",nrow(uniq.trips),"\n"))     
  id  <- od$ipere[i]
  o.x <- od$xlonori[i]
  o.y <- od$ylatori[i]
  d.x <- od$xlondes[i]
  d.y <- od$ylatdes[i]
  i.path <- osrmRoute(src = c(1, o.x, o.y), dst = c(2, d.x,d.y), overview = "full", sp = TRUE)
  trips[[i]] <- i.path
}
##
## 
cat("Merge all itineraires...\n")
all.trips <- do.call(rbind, trips)
##
## Save as rds
saveRDS(all.trips, out.data.2)

##
cat("Create the network...\n")
ntw <- aggit(all.trips,"n")
ntw@data <- ntw@data %>% mutate(n.per.day = n/op.days)

## Save as rds
saveRDS(ntw, "out/allTrips.RDS")


