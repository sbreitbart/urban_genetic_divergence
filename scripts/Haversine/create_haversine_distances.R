# import data
Distances <- read.csv(
  here::here("./data/CommonGardenExperiment_2019Data/raw_data/Transect_Milkweed_HaversineData_forjoining.csv"),
  na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",")

# Ref lat and longs are for Yonge & Dundas intersection in downtown Toronto
Distances$Ref_Lat <- "43.656327"
Distances$Ref_Long <- "-79.380904"

# Make lat/long cols numeric
Distances$Latitude <- as.numeric(as.character(Distances$Latitude))
Distances$Longitude <- as.numeric(as.character(Distances$Longitude))
Distances$Ref_Lat <- as.numeric(as.character(Distances$Ref_Lat))
Distances$Ref_Long <- as.numeric(as.character(Distances$Ref_Long))

# Find distances from Yonge/Dundas to sample sites (in meters)
Distances <- Distances %>% mutate(CTD_m = distHaversine(cbind(Longitude, Latitude), cbind(Ref_Long, Ref_Lat)))

# conver to km
Distances$City_dist <- Distances$CTD / 1000

# drop ref lat, long, and city_dist (in m) cols
Distances <- Distances[,-c(7:9)]


#---------
# Export
# --------
write.csv(Distances,
          here::here("./data/CommonGardenExperiment_2019Data/clean_data/clean_haversine_Distances.csv"))
