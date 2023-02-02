
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
#-------------------
# at start of season (Data Collection 1)- BINARY
weevil_1 <- read.csv(
  here("./data/CommonGardenExperiment_2020Data/raw_data/Weevil_Damage/2020_Datacollection1_WeevilBinary.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()

# at start of season (Data Collection 1)- QUANTITATIVE
weevil_2 <- read.csv(
  here("./data/CommonGardenExperiment_2020Data/raw_data/Weevil_Damage/2020_Datacollection4_WeevilScarLength.csv"),header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>% as.data.frame()



# -------------------
# Clean data
#-------------------
# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.
  
# weevil_1-----
  c <- is.na(weevil_1$Block) == TRUE
  c2 <- c(1:nrow(weevil_1))[c]
  weevil_1 <- weevil_1[-c2,]
  names(weevil_1)[1]<-"Row"
  weevil_1 <- weevil_1[!is.na(weevil_1$Population), ]
  
# weevil_2-----
  c <- is.na(weevil_2$Block) == TRUE
  c2 <- c(1:nrow(weevil_2))[c]
  weevil_2 <- weevil_2[-c2,]
  names(weevil_2)[1]<-"Row"
  weevil_2 <- weevil_2[!is.na(weevil_2$Population), ]

rm(c, c2)





# Make certain columns factors / check classes are correct-----
## weevil_1----
str(weevil_1)

# remove empty columns
weevil_1 <- weevil_1[,-c(25:29)]

# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
weevil_1[factor_cols] <- lapply(weevil_1[factor_cols], factor)
str(weevil_1)

## weevil_2----
str(weevil_2)

factor_cols_nocomment <- c(
  "Block", "Population", "Family", "Replicate", "Weevil_dam_June", "Dead")
# make these columns into factors
weevil_2[factor_cols_nocomment] <- lapply(weevil_2[factor_cols_nocomment], factor)

str(weevil_2)






# Make new column signifying if any ramets of plant experienced any weevil damage- 0 or 1
## First, select columns pertaining to weevil damage
w_cols <- weevil_1 %>% dplyr::select(contains("Weevil"))
w_colnames <- colnames(w_cols)

## Next, add sums of weevil damage cols.
weevil_1$Weevil_dam_total <- rowSums(weevil_1[w_colnames], na.rm = T)
weevil_1$Weevil_dam_binary <- weevil_1$Weevil_dam_total


## Now change all instances of >=1 to 1 (to signify simple 0/1 damage experienced). If >=1, then at least one ramet experienced weevil damage.
weevil_1 <- weevil_1 %>% mutate(weevil_1,
                                Weevil_dam_binary = ifelse(Weevil_dam_total == "0", "0", "1"))

### make new binary weevil damage column factor
weevil_1$Weevil_dam_binary <- as.factor(weevil_1$Weevil_dam_binary)

# check appropriate columns are factors
str(weevil_1)






#### combine both weevil damage assessment events into one df

# make new df with just row, col... replicate and weevil damage cols
weevil_both <- left_join(weevil_1, weevil_2,
                         by = c("Row", "Column", "Block", "Population", "Family", "Replicate")) %>%
  dplyr::select(.,-c(7:25, 27:28, 30:31)) 
  

#-------------------
# Export to new csv
#-------------------
write.csv(weevil_both,
          here("./data/CommonGardenExperiment_2020Data/partially_cleaned_data/2020_weevil_damage_partialclean.csv"))
