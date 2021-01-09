
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
##-------------------
# For both data collections (July & August)
herbivores <- read.csv(here("./CommonGardenExperiment_2020Data/raw_data/Herbivores/2020_Datacollection_Herbvivores2020_raw_formatted.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()



#-------------------
# Clean data
#-------------------
# Remove rows without plants, empty columns-----
names(herbivores)[1]<-"Row"
herbivores <- herbivores[!is.na(herbivores$Population), ]


# Make certain columns factors / check classes are correct-----
str(herbivores)

# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
herbivores[factor_cols] <- lapply(herbivores[factor_cols], factor)
herbivores$Dead <- as.factor(herbivores$Dead)

# remove extra row and column cols
herbivores <- herbivores[-c(8,9, 24:32)]
str(herbivores)

# remove dead plants
herbivores <- herbivores %>% filter(., Dead == "0")

# replace NAs with 0s for herbivore cols (9:10, 12:20)
herbivores[,c(9:10, 12:20)][is.na(herbivores[,c(9:10, 12:20)])] <- 0


# make new col to differentiate data collection events (July and Aug)
July_herb_dates <- list("07/13/2020", "07/14/2020", "07/15/2020", "07/16/2020", "07/18/2020", "07/20/2020")
herbivores$Sample <- ifelse(is.element(herbivores$Date, July_herb_dates),"July", "August")
herbivores$Sample <- as.factor(herbivores$Sample)

str(herbivores)



#-------------------
# Export to new csv
#-------------------
write.csv(herbivores,
          here("./CommonGardenExperiment_2020Data/clean_data/2020_insect_herbivores_clean.csv"))