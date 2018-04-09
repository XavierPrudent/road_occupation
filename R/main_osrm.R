#!/usr/bin/env Rscript

################################################################
## Compute flux through the city of Trois-Rivieres using the OD survey
################################################################
##
## Load general packages
source("general/load_R_pkg.R")

##
## OD survey
od <- readRDS("data/enquete_OD_2011/Trv11pv2a.RDS")
##
## Motorised transports
motor <- c(1, 2, 3, 4, 6, 7, 8, 10, 11, 12)
od <- od %>%
filter(mode1 %in% motor | mode2 %in% motor | mode3 %in% motor ) %>%
filter(motif != 12 )
##
## Run OSRM
options(osrm.server = "http://localhost:5000/")
##
## For each trip, estimate the path
#n <- nrow(od)
n <- 5
#n <- 1000
sub <- sample_n(od,n)
trips <- vector("list",length=n)

for( i in 1:n){
  cat(paste(i,"/",n,"\n"))     
  id  <- od$ipere[i]
  o.x <- od$xlonori[i]
  o.y <- od$ylatori[i]
  d.x <- od$xlondes[i]
  d.y <- od$ylatdes[i]
  i.path <- osrmRoute(src = c(1, o.x, o.y), dst = c(2, d.x,d.y), overview = "full", sp = TRUE)
  i.path@data$facdep <- od$facdep[i]
  trips[[i]] <- i.path
}
##
## Merge all itineraires
all.trips <- do.call(rbind, trips)
##
## Save as rds
saveRDS(all.trips, "out/allTrips.RDS")

