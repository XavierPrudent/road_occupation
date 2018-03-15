
#############################################
## return TRUE if geometries intersect as lines, not points
islines <- function(g1, g2){
  inherits(gIntersection(g1,g2),"SpatialLines")
}

#############################################
## union and merge and disaggregate to make a
## set of non-overlapping line segments
sections <- function(sl){
  disaggregate(gLineMerge(gUnion(sl,sl)))
}

#############################################
## Agregate lines and sum their attributes
aggit <- function(sldf, attr, fun=sum){
  ##
  cat("simplify down to SpatialLines...\n")
  sl = as(sldf, "SpatialLines")
  ##
  cat("Get the line sections that make the network...\n")
  slu = sections(sl)
  ##
  cat("Overlay network with routes...\n")
  overs = over(slu, sl, returnList=TRUE)
  ## 
  cat("Overlay is true if end points overlay, so filter them out...\n")
  overs = lapply(1:length(overs), function(islu){
    Filter(function(isl){
      islines(sl[isl,],slu[islu,])
    }, overs[[islu]])
  })
  ## 
  cat("Aggregate the required attribute by sum...\n")
  aggs = sapply(overs, function(os){fun(sldf[[attr]][os])})
  ## 
  cat("Make a SLDF with the named attribute...\n")
  sldf = SpatialLinesDataFrame(slu, data.frame(Z=aggs))
  names(sldf)=attr
  sldf
}

#############################################
## Pick a color
col.n <- function(x){
  
  if( x >= 0 & x < 1 ) col.x <- "black"
  if( x >= 1 & x < 2 ) col.x <- "blue"
  if( x >= 2 & x < 4 ) col.x <- "yellow"
  if( x >= 4 & x < 8 ) col.x <- "orange"
  if( x >= 8 ) col.x <- "red"
  
  return(col.x)
}

#############################################
## Zone to test
create.test.zone <- function(){
  ##
  ## Coins de la zone1
  coins <- matrix(nrow = 4, ncol=2)
  coins[1,] <- c(-73.727666, 45.940398)
  coins[2,] <- c(-73.729447, 45.941487)
  coins[3,] <- c(-73.731228, 45.940599)
  coins[4,] <- c(-73.728620, 45.939465)
  zone1 <<- spPolygons(coins, attr=data.frame(Pid=1))
  ##
  ## Coins de la zone2
  coins <- matrix(nrow = 4, ncol=2)
  coins[1,] <- c(-73.685453, 45.913863)
  coins[2,] <- c(-73.706996, 45.919177)
  coins[3,] <- c(-73.710816, 45.907323)
  coins[4,] <- c(-73.690174, 45.907622)
  zone2 <<- spPolygons(coins, attr=data.frame(Pid=2))
  ##
  ## Coins de la zone3
  coins <- matrix(nrow = 4, ncol=2)
  coins[1,] <- c(-73.759428, 45.840278 )
  coins[2,] <- c(-73.754246, 45.840394 )
  coins[3,] <- c(-73.753903, 45.835102 )
  coins[4,] <- c(-73.761113, 45.835999 )
  zone3 <<- spPolygons(coins, attr=data.frame(Pid=3))
  ##
  ## Coins de la zone4
  coins <- matrix(nrow = 4, ncol=2)
  coins[1,] <- c( -73.758822, 45.839779)
  coins[2,] <- c( -73.756623, 45.839577)
  coins[3,] <- c( -73.757159, 45.837880)
  coins[4,] <- c( -73.759187, 45.838022)
  zone4 <<- spPolygons(coins, attr=data.frame(Pid=4))
  
  
}

