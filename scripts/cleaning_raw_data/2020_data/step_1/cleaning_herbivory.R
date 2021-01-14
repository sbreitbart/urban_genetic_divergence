
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
##-------------------
# in middle of season 
herbivory_1 <- read.csv(here("./CommonGardenExperiment_2020Data/raw_data/Herbivory/2020_Datacollection3_Herbivory.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()

# towards end of season 
herbivory_2 <- read.csv(here("./CommonGardenExperiment_2020Data/raw_data/Herbivory/2020_Datacollection7_Herbivory.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()




##-------------------
# Clean data
#-------------------

# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.

# herbivory_1-----
  c <- is.na(herbivory_1$Block) == TRUE
  c2 <- c(1:nrow(herbivory_1))[c]
  herbivory_1 <- herbivory_1[-c2,]
  names(herbivory_1)[1]<-"Row"
  herbivory_1 <- herbivory_1[!is.na(herbivory_1$Population), ]  
  
# herbivory_2-----
  c <- is.na(herbivory_2$Block) == TRUE
  c2 <- c(1:nrow(herbivory_2))[c]
  herbivory_2 <- herbivory_2[-c2,]
  names(herbivory_2)[1]<-"Row"
  herbivory_2 <- herbivory_2[!is.na(herbivory_2$Population), ] 
  
rm(c, c2)





# Make certain columns factors / check classes are correct-----
## herbivory_1----
str(herbivory_1)

# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
herbivory_1[factor_cols] <- lapply(herbivory_1[factor_cols], factor)

str(herbivory_1)

## herbivory_2----
str(herbivory_2)

# make these columns into factors
herbivory_2[factor_cols] <- lapply(herbivory_2[factor_cols], factor)

str(herbivory_2)




# combine both herbivory assessment events into one df
# make new df with just row, col... replicate and weevil damage cols
herbivory_both <- left_join(herbivory_1, herbivory_2,
                            by = c("Row", "Column", "Block", "Population", "Family", "Replicate"),
                            suffix = c(".July", ".Sept"))

#-------------------
# Export to new csv
#-------------------
write.csv(herbivory_both,
          here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_herbivory_partialclean.csv"))
