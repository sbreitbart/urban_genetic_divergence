
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
##-------------------
# at start of season
heights_1 <- read.csv(here("./CommonGardenExperiment_2020Data/raw_data/Height/2020_Datacollection1_Height.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()

# at end of season
heights_2 <- read.csv(here("./CommonGardenExperiment_2020Data/raw_data/Height/2020_Datacollection7_Height.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()




##-------------------
# Clean data
#-------------------

# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.

## heights_1-----
c <- is.na(heights_1$Block) == TRUE
c2 <- c(1:nrow(heights_1))[c]
heights_1 <- heights_1[-c2,]
names(heights_1)[1]<-"Row"
heights_1 <- heights_1[!is.na(heights_1$Population), ] # there was no plant in this pot anymore... may have been taken by raccoons
  
## heights_2-----
c <- is.na(heights_2$Block) == TRUE
c2 <- c(1:nrow(heights_2))[c]
heights_2 <- heights_2[-c2,]
names(heights_2)[1]<-"Row"
heights_2 <- heights_2[!is.na(heights_2$Population), ]

rm(c, c2)






# Make certain columns factors / check classes are correct-----
## Heights_1----
str(heights_1)
  
# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
heights_1[factor_cols] <- lapply(heights_1[factor_cols], factor)

# add col for # of stems
heights_1$Ramets <- rowSums(!is.na(heights_1[,c(10:23)]))

# if total height = 0, then change # stems to 0
heights_1$Ramets[heights_1$Total_Height==0] = 0


## Heights_2----
str(heights_2)

# make these columns into factors
heights_2[factor_cols] <- lapply(heights_2[factor_cols], factor)

# make these columns into numeric
heights_2[,9:17] <- sapply(heights_2[,9:17], as.character)
heights_2[,9:17] <- sapply(heights_2[,9:17], as.numeric)


# add col for # of stems
heights_2$Ramets <- rowSums(!is.na(heights_2[,c(9:16)]))

str(heights_2)


# make into one df----
heights_both <- left_join(heights_1, heights_2, by = c("Row", "Column", "Block", "Population", "Family", "Replicate"), suffix = c('_June', '_Sept'))
heights_both <- heights_both[,-c(7:8, 10:23, 27, 29:36, 38)]


# add col for growth rate
heights_both$date_diff <- as.Date(as.character(heights_both$Date_Sept), format="%m/%d/%Y") - as.Date(as.character(heights_both$Date_June), format="%m/%d/%Y")
heights_both$date_diff <- as.numeric(heights_both$date_diff)

heights_both <- as.data.frame(heights_both)




#-------------------
# Export to new csv
#-------------------
write.csv(heights_both, here("./CommonGardenExperiment_2020Data/clean_data/2020_heights_clean.csv"))
