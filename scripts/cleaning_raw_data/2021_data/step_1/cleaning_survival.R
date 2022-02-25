
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
survival <- read.csv(here::here("./CommonGardenExperiment_2021Data/raw_data/2021_DC5_Herbivory_Survival_final.csv"),
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
factor_cols <- c("Block", "Population", "Family",
                 "Replicate", "Comment",
                 "Alive", "Date", "Surveyor")
survival[factor_cols] <- lapply(survival[factor_cols], factor)

str(survival)

# check options for survival
unique(survival$Alive)
# change "." into NA

survival %<>%
  dplyr::mutate(.,
                Alive = ifelse(survival$Alive == ".",
                                      NA,
                                      2)) %>%
# then change NA into 0
  dplyr::mutate(.,
                Alive = ifelse(is.na(survival$Alive == TRUE),
                               1,
                               0))


#-------------------
# Export to new csv
#-------------------
write.csv(survival,
          here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_survival_partialclean.csv"))
