
path.gen <- "/Users/lavieestuntoucan/general/"
path.src <- "/Users/lavieestuntoucan/civ-montcalm/tech/calcul-reseau/R/"
path.in <- "/Users/lavieestuntoucan/civ-montcalm/data/data-for-server/"
path.out <- "/Users/lavieestuntoucan/civ-montcalm/tech/calcul-reseau/out/2009-2017/"
##
source(paste0(path.src,"include_spatialFunctions.R"))
source(paste0(path.gen,"load_R_pkg.R"))
##
in.data.1 <- paste0(path.out,"uniqTrips_df.rds")
in.data.2 <- paste0(path.out,"allTrips_spldf.rds")
in.data.3 <- paste0(path.out,"network_spldf.rds")
##
uniq.trips <- readRDS(in.data.1)
all.trips <- readRDS(in.data.2)
ntw <- readRDS(in.data.3)
##
## Zone to test
create.test.zone()
##
## Find interseting paths
list.crx <- vector("list",length=length(all.trips))
for( i in 1:length(all.trips)){
  print(i)
  obj <- as.SpatialLines.SLDF(all.trips)[i]
  df <- data.frame(n = all.trips@data$n[i])
  row.names(df) <- obj@lines[[1]]@ID
  splndf <- SpatialLinesDataFrame(sl = obj, data = df)
  #crx <- intersect(splndf, zone1)
  #crx <- intersect(splndf, zone2)
  crx <- intersect(splndf, zone3)
  if( !is.null(crx)) list.crx[[i]] <- splndf
}
##
## Create a gif
png(file="testzone%02d.png", width=400, height=400)
i.col <- "red"
t.x <- mean(zone4@polygons[[1]]@Polygons[[1]]@coords[,1])
t.y <- mean(zone4@polygons[[1]]@Polygons[[1]]@coords[,2])
for( i in 1:length(list.crx)){
  if( is.null(list.crx[[i]])) next
  if( i < 600 | i > 700) next
  plot(zone4,col="blue",lwd=2)
  if( ! is.null(list.crx[[i]]))plot(list.crx[[i]],col=i.col,add=TRUE)
  text(x=t.x, y=t.y, paste0("It. ", i))
  if( i.col == "red" ) i.col <- "green" else i.col <- "red"
  Sys.sleep(0.5)
}
dev.off()
cmd <- paste0("convert -delay 80 *.png ", path.out, "testZone3.gif")
system(cmd)
file.remove(list.files(pattern=".png"))
##
## Test specific itineraries
i <- 384
obj <- as.SpatialLines.SLDF(all.trips)[i]
df <- data.frame(n = all.trips@data$n[i])
row.names(df) <- obj@lines[[1]]@ID
splndf <- SpatialLinesDataFrame(sl = obj, data = df)
##
## Plot it on the map
map.city <- leaflet() %>%
  addTiles('http://{s}.tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=68c4cd328d3b484091812a76fae093fd') %>%
  setView(coord$lon, coord$lat, zoom = 11)
map1 <- addPolylines(map.city,
                     lng = splndf@lines[[1]]@Lines[[1]]@coords[,1],
                     lat = splndf@lines[[1]]@Lines[[1]]@coords[,2],
                     col="red",
                     opacity=0.8)
map1 <- addCircles(map1,
                   lng = c(uniq.trips[i,]$ori.lon, uniq.trips[i,]$des.lon),
                   lat = c(uniq.trips[i,]$ori.lat, uniq.trips[i,]$des.lat),
                   col="red")
map1



# crx <- intersect(all.trips, zone1)
# LP1 <- crx[crx$Pid == 1, ]
# ntw <- aggit(LP1,"n")

ntw2 <- aggit(all.trips,"n")
ntw2@data <- ntw@data %>% mutate(n.per.day = n/3000)

## Add lines
## color depends on flux
 ntw.dat <- ntw2@data
 ntw.lines <- ntw2@lines 
for( i in 1:length(ntw.lines)){
  i.x <- ntw.lines[[i]]@Lines[[1]]@coords[,1]
  i.y <- ntw.lines[[i]]@Lines[[1]]@coords[,2]
  i.n <- ntw.dat$n.per.day[i]
  i.col <- col.n(i.n)
  if( i == 1 ) MAP <- map.city
  if( i > 1 ) MAP <- map1
  map1 <- addPolylines(MAP,
                       lng = i.x,
                       lat = i.y,
                       col=i.col,
                       opacity=1)
}

map1 <- addCircles(map1,
                   lng = c(uniq.trips$ori.lon,uniq.trips$des.lon),
                   lat = c(uniq.trips$ori.lat,uniq.trips$des.lat),
                   col="red")






