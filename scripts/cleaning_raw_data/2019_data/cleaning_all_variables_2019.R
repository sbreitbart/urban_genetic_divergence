
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)
library(stringi)



#-------------------
# Import data
#-------------------
# data collection 1
DC1 <- read.csv(
  here::here("./CommonGardenExperiment_2019Data/raw_data/DataAnalysis_FirstHeightSurvivalMeasurement_20190610.csv"),
  header=T,
  na.strings=c("NO PLANT", "none"),
  blank.lines.skip=TRUE) %>%
  as.data.frame()

# data collection 2
DC2 <- read.csv(
  here::here("./CommonGardenExperiment_2019Data/raw_data/DataAnalysis_SecondHeightSurvivalMeasurement_20190621.csv"),
  header=T,
  na.strings=c("NO PLANT", "none"),
  blank.lines.skip=TRUE) %>%
  as.data.frame()

# data collection 3- end of season
DC3 <- read.csv(
  here::here("./CommonGardenExperiment_2019Data/raw_data/FINAL_3rdmeasuredheight_survival.csv"),
  header=T,
  na.strings=c("NO PLANT", "none"),
  blank.lines.skip=TRUE) %>%
  as.data.frame()



##-------------------
# Clean all data
#-------------------

# Remove rows without plants, empty columns
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant-
#  ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well
#  as empty spreadsheet columns.
c <- is.na(DC1$Block) == TRUE
c2 <- c(1:nrow(DC1))[c]
DC1 <- DC1[-c2,]
names(DC1)[1]<-"Row"
DC1 <- DC1[!is.na(DC1$Population), ] # there was no plant in this pot anymore... may have been taken by raccoons

c <- is.na(DC2$Block) == TRUE
c2 <- c(1:nrow(DC2))[c]
DC2 <- DC2[-c2,]
names(DC2)[1]<-"Row"
DC2 <- DC2[!is.na(DC2$Population), ] # there was no plant in this pot anymore... may have been taken by raccoons

c <- is.na(DC3$Block) == TRUE
c2 <- c(1:nrow(DC3))[c]
DC3 <- DC3[-c2,]
names(DC3)[1]<-"Row"
DC3 <- DC3[!is.na(DC3$Population), ] # there was no plant in this pot anymore... may have been taken by raccoons

rm(c, c2)

##----------------------------------------
## Prep DC1 and DC2 before joining with DC3
#----------------------------------------
# Calculate total number of ramets per plant for DC2

DC2$Num_Ramets <- rowSums(!is.na(DC2[,c(13:20)]))

# Remove unnecessary columns from DC1 and DC2
DC1 <- DC1[,-c(12:27)]
DC2 <- DC2[,-c(13:20, 23:24)]

# rename notes col
names(DC1)[8]<-"Comment"

# get DC2 measuerer and num ramets for both DCs
DC1_2 <- merge(x = DC1[,c(1:9)], y = DC2[,-c(9:10)],
               by = c("Row", "Column", "Block", "Population", "Family", "Replicate"),
               all.x = TRUE,
               suffix = c('_DC1', '_DC2'))
# reorder cols
DC1_2 <- DC1_2[, c(1:9, 12, 15, 10:11, 13, 16, 14)]





##----------------------------------------
## Prep DC3 before joining with DC1 and DC2
#----------------------------------------

## Make certain columns factors / check classes are correct
str(DC3)
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment",
                 "Dead_DC1", "Dead_DC2", "Dead_DC3", "Exp_rhiz_binary", "Monarch_dam", "Deer_dam")
DC3[factor_cols] <- lapply(DC3[factor_cols], factor)

int_cols <- c("Alive_ram", "Dead_ram")
DC3[int_cols] <- lapply(DC3[int_cols], as.integer)

DC3[c(16,18,20,22,24,26,28,30,32:38)] <- lapply(
  DC3[c(16,18,20,22,24,26,28,30,32:38)], as.numeric)
str(DC3)


## Reclassify Monarch damage & deer damage entries
# Raw data included several indications of damage- "yes", "1,2" (signifying ramets 1 & 2
# experienced damage but, for example, ramet 3 didn't)... so this chunk is about converting
# those indicators of damage into simple yes' and no's.

# MONARCH DAMAGE
# Remove all specific indicators
# can't figure out how to use recode or other more efficient way here
as.list(unique(DC3$Monarch_dam)) #entries are 0 1 2 3 5 all no yes

DC3$Monarch_dam = with(DC3,
                           droplevels(replace(Monarch_dam,
                                              is.na(Monarch_dam) | Monarch_dam =="", "0")))
DC3$Monarch_dam <- gsub('no', '0', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('0', '0', DC3$Monarch_dam)

DC3$Monarch_dam <- gsub('yes', '1', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('all', '1', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('1, 1', '1', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('1, 2', '1', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('2', '1', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('3', '1', DC3$Monarch_dam)
DC3$Monarch_dam <- gsub('5', '1', DC3$Monarch_dam)

as.list(unique(DC3$Monarch_dam)) #entries are 0 1 2 3 5 all no yes

# Make column factor
DC3$Monarch_dam <- factor(DC3$Monarch_dam)


# DEER DAMAGE
DC3$Deer_dam <- factor(DC3$Deer_dam)
as.list(unique(DC3$Deer_dam)) #entries are 0 1 8 n no y yes

DC3$Deer_dam = with(DC3,
                        droplevels(replace(Deer_dam,
                                           is.na(Deer_dam) | Deer_dam =="", "0")))
DC3$Deer_dam <- gsub('no', '0', DC3$Deer_dam)
DC3$Deer_dam <- gsub('n', '0', DC3$Deer_dam)


DC3$Deer_dam <- gsub('8', '1', DC3$Deer_dam)
DC3$Deer_dam <- gsub('y', '1', DC3$Deer_dam)
DC3$Deer_dam <- gsub('yes', '1', DC3$Deer_dam)
DC3$Deer_dam <- gsub('1es', '1', DC3$Deer_dam)

as.list(unique(DC3$Deer_dam)) #entries are 0 1 8 n no y yes

# Make column factor
DC3$Deer_dam <- factor(DC3$Deer_dam)




## Remove empty columns
# These columns are empty (all NA's) as seen from str(DC3)
DC3 <- DC3[,-c(32:38)]



##-------------------
# Calculate herbivory
#-------------------

## Ramet 1
unique(DC3$Herbiv_ram1) 
### Replace non-numeric entries with NA and remove dashes (which symbolize no leaf) 
DC3$Herbiv_ram1 <- gsub('too small', NA, DC3$Herbiv_ram1)
DC3$Herbiv_ram1 <- gsub('too', NA, DC3$Herbiv_ram1)
DC3$Herbiv_ram1 <- gsub('no leaves', NA, DC3$Herbiv_ram1)
DC3$Herbiv_ram1 <- gsub(' ', ",", DC3$Herbiv_ram1)
DC3$Herbiv_ram1 <- gsub(',-', "", DC3$Herbiv_ram1)
DC3$Herbiv_ram1 <- gsub('-', "", DC3$Herbiv_ram1)

unique(DC3$Herbiv_ram1) 

### Calculate mean herbivory for ramet 1 in new column
DC3$Herbivory_mean_DC3 <- sapply(strsplit(as.character(DC3$Herbiv_ram1), ",", fixed=T),
                                function(x) mean(as.numeric(x)))

# IMPORTANT NOTE: In 2019, I measured herbivory on EACH ramet- up to 5 leaves per ramet
# for every ramet in the pot. In 2020, I did this for only one ramet in the pot. So, for
# consistency, I'll only calculate herbivory for first ramet in the pot. I'll remove the
# other ramets' herbivory data from this data frame but will leave them in the raw data file.

DC3 <- DC3[,-c(19,21,23,25,27,29,31)]

# Divide herbivory column by 100 to get percent to use binomial distribution later in glmer
DC3$Herbivory_mean_DC3 <- DC3$Herbivory_mean_DC3/100



##-----------------------------
# Calculate total number of ramets per plant
#-----------------------------
DC3$Num_Ramets_DC3 <- rowSums(!is.na(DC3[,c(16, 18:24)]))


##-----------------------------
# Calculate total plant height
#-----------------------------
DC3$Total_height_DC3 <- NA
DC3$Total_height_DC3 <- rowSums(DC3[,c(16, 18:24)], na.rm=TRUE)

# remove individual ramet height cols
DC3 <- DC3[,-c(16, 18:24, 26)]




#----------
# Join 3 DCs
#-----------
# first, add DC3 suffix to appropriate cols (and rename exposed rhizome col for clarity)
names(DC3)[7]<-"Date_Measured_DC3"
names(DC3)[8]<-"Comment_DC3"
names(DC3)[12]<-"Alive_ramets_DC3"
names(DC3)[13]<-"Dead_ramets_DC3"
names(DC3)[14]<-"Monarch_damage_DC3"
names(DC3)[15]<-"Deer_damage_DC3"
names(DC3)[16]<-"Herbivory_DC3"
names(DC3)[17]<-"Measurer_DC3"
names(DC3)[18]<-"Exposed_rhizome_DC1"

# Join DC1_2 and DC3
DC_all_2019 <- merge(x = DC1_2, y = DC3,
               by = c("Row", "Column", "Block", "Population", "Family", "Replicate"),
               all.x = TRUE)

# reorder cols
DC_all_2019 <- DC_all_2019 %>% 
  dplyr::select(sort(names(.)))

DC_all_2019 <- DC_all_2019[, c(28,3, 2, 26, 16, 27, 1, 4:15, 17:25, 29:31)]


#----------------------------------------------------------
# Find which plants were dead throughout season (all 3 DCs)
#-----------------------------------------------------------
DC_all_2019$Dead_2019 <- NA

DC_all_2019 <- transform(DC_all_2019,
                  Dead_2019 = ifelse(
                    (Dead_DC1 == 'yes' | Dead_DC1 == 'maybe' | Dead_DC1 == 'nothing there') &
                      Dead_DC2 == 1 &
                      Dead_DC3 == 1 &
                      Total_Height_DC1 == 0 &
                      Total_Height_DC2 == 0 &
                      Total_height_DC3 == 0,
                    1,0))





## Calculate relative growth rate
## ---------------------------------------------
# First, find # days between DC1 and DC3
DC_all_2019$date_diff <- as.Date(as.character(DC_all_2019$Date_Measured_DC3), format="%m/%d/%Y") -
  as.Date(as.character(DC_all_2019$Date_Measured_DC1), format="%m/%d/%Y")
DC_all_2019$date_diff <- as.numeric(DC_all_2019$date_diff)

# some plants had heights of 0 in DC1, DC3, or both:
# If they were 0 in DC1 but >0 in DC3: DC1 = 1, since ln(1)=0.
# If they were >0 in DC1 but 0 in DC3: DC3 = 1.
# If DC1 = 0 and DC3 = 0, it will be NA.
DC_all_2019 <- DC_all_2019 %>%
  dplyr::mutate(.,
                rel_growth_rate = case_when(
                  Total_Height_DC1 > 0 & Total_height_DC3 > 0 ~ (log(Total_height_DC3) - log(Total_Height_DC1))/ date_diff,
                  Total_Height_DC1 > 0 & Total_height_DC3 == 0  ~ (1 - log(Total_Height_DC1)) / date_diff,
                  Total_Height_DC1 == 0 & Total_height_DC3 > 0  ~ (log(Total_height_DC3) - 1) / date_diff))

#-----------------------------------------------------------------------------------
# Add urbanization data (Distance from city center, transect IDs, urbanization score, etc.)
#-----------------------------------------------------------------------------------
## Add transect data (e.g. city_dist values)
### Add City_dist values with Haversine formula

# import data
Distances <- read.csv(here::here(
  "./CommonGardenExperiment_2019Data/clean_data/clean_haversine_Distances.csv"),
  na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",")

# Join city_dist to DC3
DC_all_2019 <- merge(x = DC_all_2019, y = Distances, by = "Population", all.x = TRUE)


### Add urb_score data
#--------------------------
# Import urb_index values for each of these rows
urb_scores <- read.csv(
  here::here("./CommonGardenExperiment_2020Data/raw_data/Urbanization_Score/Urbanization_Scores_Table.csv"),  header=T, na.strings=c("","NA"))

# get rid of unnecessary cols, rename Patch ID to Pop_ID for joins
urb_scores <- urb_scores[,c(2,8)]
names(urb_scores)[1] <- "Pop_ID"

# join
DC_all_2019 <- merge(DC_all_2019, y = urb_scores, by = "Pop_ID", all.x = TRUE)

# remove "X" column
DC_all_2019 <- DC_all_2019[,-36]


#-------------------
# Export to new csv
#-------------------
write.csv(DC_all_2019,
          here::here("./CommonGardenExperiment_2019Data/clean_data/clean_data_2019KSR.csv"))
