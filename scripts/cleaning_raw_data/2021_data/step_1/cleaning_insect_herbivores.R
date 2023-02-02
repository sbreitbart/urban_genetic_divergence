
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
##-------------------
# For both data collections (July & August)
herbivores <- read.csv(here("./data/CommonGardenExperiment_2021Data/raw_data/2021_DC4_Herbivores_formatted.csv"),
                       header=T, na.strings=c("NO PLANT", "none"),
                       blank.lines.skip=TRUE) %>%
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
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment", "Surveyor")
herbivores[factor_cols] <- lapply(herbivores[factor_cols], factor)

unique(herbivores$Date)

# make new col to differentiate data collection events (Early and late)
herbivores %<>%
  mutate(Sample = case_when(
    startsWith(Date, "7") ~ "Early",
    startsWith(Date, "8") ~ "Late"
  )) %>%
  mutate(Sample = as.factor(Sample))

str(herbivores)



#-------------------
# Export to new csv
#-------------------
write.csv(herbivores,
          here("./data/CommonGardenExperiment_2021Data/partially_cleaned_data/2021_insect_herbivores_partialclean.csv"))
