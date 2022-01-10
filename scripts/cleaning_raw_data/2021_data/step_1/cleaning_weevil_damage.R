
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)
library(magrittr)


#-------------------
# Import data
#-------------------
# at start of season (Data Collection 1)- QUANTITATIVE
weevil_1 <- read.csv(
  here("./CommonGardenExperiment_2021Data/raw_data/2021_DC3_Height_Ramets_ScarLength.csv"),
  header=T, na.strings=c("NO PLANT", "none", ""), blank.lines.skip=TRUE) %>%
  as.data.frame()


# -------------------
# Clean data
#-------------------
# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.
  
c <- is.na(weevil_1$Block) == TRUE
c2 <- c(1:nrow(weevil_1))[c]
weevil_1 <- weevil_1[-c2,]
names(weevil_1)[1]<-"Row"
weevil_1 <- weevil_1[!is.na(weevil_1$Population), ]

rm(c, c2)



# Make certain columns factors / check classes are correct-----
str(weevil_1)

# remove ramet height cols
weevil_1 %<>% select(-c(11:26))

factor_cols <- c(
  "Block", "Population", "Family", "Replicate",
  "Date", "Surveyor")
# make these columns into factors
weevil_1[factor_cols] <- lapply(weevil_1[factor_cols], factor)

str(weevil_1)


#-------------------
# Export to new csv
#-------------------
write.csv(weevil_1,
          here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_weevil_damage_partialclean.csv"))
