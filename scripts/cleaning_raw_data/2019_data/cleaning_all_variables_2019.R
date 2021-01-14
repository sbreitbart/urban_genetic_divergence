
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)
library(stringi)



#-------------------
# Import data
##-------------------
# at start of season
all_raw <- read.csv(
  here::here("./CommonGardenExperiment_2019Data/raw_data/FINAL_3rdmeasuredheight_survival.csv"),
  header=T,
  na.strings=c("NO PLANT", "none"),
  blank.lines.skip=TRUE) %>%
  as.data.frame()



##-------------------
# Clean data
#-------------------

# Remove rows without plants, empty columns
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant-
#  ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well
#  as empty spreadsheet columns.
c <- is.na(all_raw$Block) == TRUE
c2 <- c(1:nrow(all_raw))[c]
all_raw <- all_raw[-c2,]
names(all_raw)[1]<-"Row"
all_raw <- all_raw[!is.na(all_raw$Population), ] # there was no plant in this pot anymore... may have been taken by raccoons

rm(c, c2)


## Make certain columns factors / check classes are correct
str(all_raw)
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment",
                 "Dead_DC1", "Dead_DC2", "Dead_DC3", "Exp_rhiz_binary", "Monarch_dam", "Deer_dam")
all_raw[factor_cols] <- lapply(all_raw[factor_cols], factor)

int_cols <- c("Alive_ram", "Dead_ram")
all_raw[int_cols] <- lapply(all_raw[int_cols], as.integer)

all_raw[c(16,18,20,22,24,26,28,30,32:38)] <- lapply(
  all_raw[c(16,18,20,22,24,26,28,30,32:38)], as.numeric)
str(all_raw)


## Reclassify Monarch damage & deer damage entries
# Raw data included several indications of damage- "yes", "1,2" (signifying ramets 1 & 2
# experienced damage but, for example, ramet 3 didn't)... so this chunk is about converting
# those indicators of damage into simple yes' and no's.

# MONARCH DAMAGE
# Remove all specific indicators
# can't figure out how to use recode or other more efficient way here
as.list(unique(all_raw$Monarch_dam)) #entries are 0 1 2 3 5 all no yes

all_raw$Monarch_dam = with(all_raw,
                           droplevels(replace(Monarch_dam,
                                              is.na(Monarch_dam) | Monarch_dam =="", "0")))
all_raw$Monarch_dam <- gsub('no', '0', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('0', '0', all_raw$Monarch_dam)



all_raw$Monarch_dam <- gsub('yes', '1', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('all', '1', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('1, 1', '1', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('1, 2', '1', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('2', '1', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('3', '1', all_raw$Monarch_dam)
all_raw$Monarch_dam <- gsub('5', '1', all_raw$Monarch_dam)

as.list(unique(all_raw$Monarch_dam)) #entries are 0 1 2 3 5 all no yes

# Make column factor
all_raw$Monarch_dam <- factor(all_raw$Monarch_dam)


# DEER DAMAGE
all_raw$Deer_dam <- factor(all_raw$Deer_dam)
as.list(unique(all_raw$Deer_dam)) #entries are 0 1 8 n no y yes

all_raw$Deer_dam = with(all_raw,
                        droplevels(replace(Deer_dam,
                                           is.na(Deer_dam) | Deer_dam =="", "0")))
all_raw$Deer_dam <- gsub('no', '0', all_raw$Deer_dam)
all_raw$Deer_dam <- gsub('n', '0', all_raw$Deer_dam)


all_raw$Deer_dam <- gsub('8', '1', all_raw$Deer_dam)
all_raw$Deer_dam <- gsub('y', '1', all_raw$Deer_dam)
all_raw$Deer_dam <- gsub('yes', '1', all_raw$Deer_dam)
all_raw$Deer_dam <- gsub('1es', '1', all_raw$Deer_dam)

as.list(unique(all_raw$Deer_dam)) #entries are 0 1 8 n no y yes

# Make column factor
all_raw$Deer_dam <- factor(all_raw$Deer_dam)




## Remove empty columns
# These columns are empty (all NA's) as seen from str(all_raw)
all_raw <- all_raw[,-c(32:38)]



##-------------------
# Calculate herbivory
#-------------------

## Ramet 1
unique(all_raw$Herbiv_ram1) 
### Replace non-numeric entries with NA and remove dashes (which symbolize no leaf) 
all_raw$Herbiv_ram1 <- gsub('too small', NA, all_raw$Herbiv_ram1)
all_raw$Herbiv_ram1 <- gsub('too', NA, all_raw$Herbiv_ram1)
all_raw$Herbiv_ram1 <- gsub('no leaves', NA, all_raw$Herbiv_ram1)
all_raw$Herbiv_ram1 <- gsub(' ', ",", all_raw$Herbiv_ram1)
all_raw$Herbiv_ram1 <- gsub(',-', "", all_raw$Herbiv_ram1)
all_raw$Herbiv_ram1 <- gsub('-', "", all_raw$Herbiv_ram1)

unique(all_raw$Herbiv_ram1) 

### Calculate mean herbivory for ramet 1 in new column
all_raw$Herbivory.mean <- sapply(strsplit(as.character(all_raw$Herbiv_ram1), ",", fixed=T),
                                function(x) mean(as.numeric(x)))

# IMPORTANT NOTE: In 2019, I measured herbivory on EACH ramet- up to 5 leaves per ramet
# for every ramet in the pot. In 2020, I did this for only one ramet in the pot. So, for
# consistency, I'll only calculate herbivory for first ramet in the pot. I'll remove the
# other ramets' herbivory data from this data frame but will leave them in the raw data file.

all_raw <- all_raw[,-c(19,21,23,25,27,29,31)]



#-----------------------------------------------------------------------------------
# Add urbanization data (Distance from city center, transect IDs, urbanization score, etc.)
#-----------------------------------------------------------------------------------
## Add transect data (e.g. city_dist values)
### Add City_dist values with Haversine formula

# import data
Distances <- read.csv(here::here(
  "./CommonGardenExperiment_2019Data/clean_data/clean_haversine_Distances.csv"),
  na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",")


# Join city_dist to all_raw
all_raw <- merge(x = all_raw, y = Distances, by = "Population", all.x = TRUE)


#-------------------
# Export to new csv
#-------------------
write.csv(all_raw,
          here::here("./CommonGardenExperiment_2019Data/clean_data/clean_data_2019KSR.csv"))
