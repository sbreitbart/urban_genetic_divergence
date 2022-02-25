
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)
library(magrittr)


#-------------------
# Import data
##-------------------
# at start of season
heights_1 <- read.csv(here::here("./CommonGardenExperiment_2021Data/raw_data/2021_DC3_Height_Ramets_ScarLength.csv"),
                      header=T, na.strings=c("NO PLANT", "none", "0", "0.00", ""),
                      blank.lines.skip=TRUE) %>%
  as.data.frame()


# at end of season
heights_2 <- read.csv(here::here("./CommonGardenExperiment_2021Data/raw_data/2021_DC6_Height_Ramets_final.csv"),
                      header=T, na.strings=c("NO PLANT", "none", "0", "0.00", ""),
                      blank.lines.skip=TRUE) %>%
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
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment", "Date", "Surveyor")
heights_1[factor_cols] <- lapply(heights_1[factor_cols], factor)

str(heights_1)


# add col for total height
heights_1$Total_Height <- heights_1 %>%
  select(Height_cm_ram1:Height_cm_ram16) %>%
  rowSums(na.rm=TRUE)

# add col for num of stems
heights_1$Ramets <- rowSums(!is.na(heights_1[(11:26)]))


str(heights_1)




## Heights_2----
str(heights_2)

# make these columns into factors
heights_2[factor_cols] <- lapply(heights_2[factor_cols], factor)

# make height cols numeric
heights_2 %>%
  dplyr::select(10:23) %>%
  as.character() %>%
  as.numeric()

# this col is the last to have numeric values in it
heights_2$Height_cm_ram10 %>% unique()

# these cols only have NA; delete them
heights_2$Height_cm_ram14 %>% unique()
heights_2$Height_cm_ram13 %>% unique()
heights_2$Height_cm_ram12 %>% unique()
heights_2$Height_cm_ram11 %>% unique()

heights_2 %<>%
  dplyr::select(-c(Height_cm_ram14,
                   Height_cm_ram13,
                   Height_cm_ram12,
                   Height_cm_ram11))

str(heights_2)


# add col for total height
heights_2$Total_Height <- heights_2 %>%
  select(Height_cm_ram1:Height_cm_ram10) %>%
  rowSums(na.rm=TRUE)

# add col for # of stems
heights_2$Ramets <- rowSums(!is.na(heights_2[,c(10:19)]))


str(heights_2)





# make into one df----
heights_both <- left_join(heights_1, heights_2, by = c("Row", "Column", "Block", "Population", "Family", "Replicate"),
                          suffix = c('_early', '_late')) %>%
  dplyr::select(-c(11:26, 32:41))


# add col for growth rate
# first, make second date same format as first
# heights_both$Date_late <- as.Date(
#   as.character(
#     heights_both$Date_late),
#   format="%d/%m/%Y")
# 
# heights_both$Date_early <- as.Date(
#   as.character(
#     heights_both$Date_early),
#   format="%d/%m/%Y")

heights_both$date_diff <- as.Date(
  as.character(
    heights_both$Date_late),
  format="%m/%d/%Y") -
  as.Date(
    as.character(
      heights_both$Date_early),
    format="%m/%d/%Y")

heights_both$date_diff %<>%
  as.numeric()

heights_both %<>%
  as.data.frame()




#-------------------
# Export to new csv
#-------------------
write.csv(heights_both,
          here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_heights_partialclean.csv"))
