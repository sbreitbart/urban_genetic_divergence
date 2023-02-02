
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
#-------------------
# at end of season 
survival <- read.csv(here("./data/CommonGardenExperiment_2020Data/raw_data/Survival/2020_Datacollection7_Survival.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()



#-------------------
# Clean data
#-------------------
# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.
  c <- is.na(survival$Block) == TRUE
  c2 <- c(1:nrow(survival))[c]
  survival <- survival[-c2,]
  names(survival)[1]<-"Row"
  survival <- survival[!is.na(survival$Population), ]
  
rm(c,c2)


# Make certain columns factors / check classes are correct-----
## survival----
str(survival)

# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
survival[factor_cols] <- lapply(survival[factor_cols], factor)
survival$Dead <- as.factor(survival$Dead)

str(survival)



#-------------------
# Export to new csv
#-------------------
write.csv(survival,
          here("./data/CommonGardenExperiment_2020Data/partially_cleaned_data/2020_survival_partialclean.csv"))
