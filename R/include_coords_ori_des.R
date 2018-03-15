
#####################################################################
## For each adress and poteau, as origin or destination set the lat,lon
set.trips.coords <- function(d, in.data.2, in.data.3, out.data){
  
  ## Coords of private adresses
  c.priv <- readRDS(in.data.2)
  
  ## Coords of poteaux
  c.pot <- read.xlsx(in.data.3)
  c.pot$Arret <- as.integer(c.pot$Arret)
  
  ## List of trips
  #trips <- d %>% group_by(AdresseDepart,AdresseArrivee) %>% summarise(n=n()) # 5472
  
  ## Add priv coords for origin
  trips <- d %>% left_join(c.priv,by=c("AdresseDepart"="initNames")) %>% plyr::rename(c("lon" = "ori.lon", "lat" = "ori.lat")) %>% select(-stops)

  ## Add priv coords for destination
  trips <- trips %>% left_join(c.priv,by=c("AdresseArrivee"="initNames")) %>% plyr::rename(c("lon" = "des.lon", "lat" = "des.lat")) %>% select(-stops)
  
  ## Clean up after merging
  trips <- trips %>% filter(!is.na(AdresseArrivee) & !is.na(AdresseDepart)) # 5472
  
  ## Add poteau coords
  
  ## Standardize the poteau names
  trips$AdresseArrivee <- gsub("POTEAU COLLECTIF # ","P",trips$AdresseArrivee)
  trips$AdresseDepart <- gsub("POTEAU COLLECTIF # ","P",trips$AdresseDepart)
  trips$AdresseArrivee <- gsub("POT","P",trips$AdresseArrivee)
  trips$AdresseDepart <- gsub("POT","P",trips$AdresseDepart)
  trips$AdresseArrivee <- gsub(" ",",",trips$AdresseArrivee,fixed=TRUE)
  trips$AdresseDepart <- gsub(" ",",",trips$AdresseDepart,fixed=TRUE)
  trips$AdresseArrivee <- gsub("-",",",trips$AdresseArrivee,fixed=TRUE)
  trips$AdresseDepart <- gsub("-",",",trips$AdresseDepart,fixed=TRUE)
  trips$AdresseArrivee <- gsub("PO0","P",trips$AdresseArrivee,fixed=TRUE)
  trips$AdresseDepart <- gsub("PO0","P",trips$AdresseDepart,fixed=TRUE)
  trips$AdresseArrivee <- gsub("PO","P",trips$AdresseArrivee,fixed=TRUE)
  trips$AdresseDepart <- gsub("PO","P",trips$AdresseDepart,fixed=TRUE)
  trips$AdresseArrivee <- gsub("P0","P",trips$AdresseArrivee,fixed=TRUE)
  trips$AdresseDepart <- gsub("P0","P",trips$AdresseDepart,fixed=TRUE)
  trips$AdresseArrivee <- gsub("#,0","P",trips$AdresseArrivee,fixed=TRUE)
  trips$AdresseDepart <- gsub("#,0","P",trips$AdresseDepart,fixed=TRUE)
  
  ## Tags of the poteaux as a check
  trips$ori.p <- NA
  trips$des.p <- NA
  
  ## Loop over the poteaux
  for( i in 1:nrow(c.pot)){
    
    ## Descript of poteau in the adress
    i.n <- c.pot$Arret[i]
    #i.n <- ifelse(n<10,paste0("0",n), n)
    
    txt1 <- paste0("P",i.n,",")
    txt2 <- paste0(",#,",i.n,",")
    
    ## Coord
    i.lat <- c.pot$Latitude[i]
    i.lon <- c.pot$Longitude[i]
    ## Poteau as an origin
    trips <- trips %>% 
      mutate(ori.lon = ifelse(grepl(x=AdresseDepart, pattern=txt1) | grepl(x=AdresseDepart, pattern=txt2), i.lon, ori.lon),
             ori.lat = ifelse(grepl(x=AdresseDepart, pattern=txt1) | grepl(x=AdresseDepart, pattern=txt2), i.lat, ori.lat),
             ori.p = ifelse(grepl(x=AdresseDepart, pattern=txt1) | grepl(x=AdresseDepart, pattern=txt2), i.n, ori.p))
    
    
    ## Poteau as a destination
    trips <- trips %>% 
      mutate(des.lon = ifelse(grepl(x=AdresseArrivee, pattern=txt1) | grepl(x=AdresseArrivee, pattern=txt2) , i.lon, des.lon),
             des.lat = ifelse(grepl(x=AdresseArrivee, pattern=txt1) | grepl(x=AdresseArrivee, pattern=txt2), i.lat, des.lat),
             des.p = ifelse(grepl(x=AdresseArrivee, pattern=txt1) | grepl(x=AdresseArrivee, pattern=txt2), i.n, des.p))
    
  }
  
  ## Save the trips with coords
  saveRDS(trips,out.data)
}
