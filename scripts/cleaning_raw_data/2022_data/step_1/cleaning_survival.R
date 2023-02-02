
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)
library(magrittr)


#-------------------
# Import data
#-------------------
# at end of season 
survival <- read.csv(here::here("./data/CommonGardenExperiment_2022Data/raw_data/2022_Survival_final.csv"),
                     header=T, na.strings=c("NO PLANT", "none", ""),
                     blank.lines.skip=TRUE) %>%
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
survival %<>%
  dplyr::mutate(across(3:Survival_LateAugust_Surveyor, as.factor))
  
str(survival)

# check options for survival
unique(survival$Survival_LateJune)
unique(survival$Survival_LateAugust)


#-------------------
# Export to new csv
#-------------------
write.csv(survival,
          here::here("./data/CommonGardenExperiment_2022Data/partially_cleaned_data/2022_survival_partialclean.csv"))
