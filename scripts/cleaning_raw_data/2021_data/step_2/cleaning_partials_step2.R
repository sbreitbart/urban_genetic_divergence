#-------------------
# Import KSR field data
#-------------------

# 2019 data-----
data_clean_2019 <- read.csv(here::here("./CommonGardenExperiment_2019Data/clean_data/clean_data_2019KSR.csv"),
                            na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -c(1:2))

# 2020 data-----
heights_both <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_heights_partialclean.csv")) %>%
  dplyr::select(., -1)

herbivory_both <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_herbivory_partialclean.csv")) %>%
  dplyr::select(., -1)

survival_2020 <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_survival_partialclean.csv")) %>%
  dplyr::select(., -1)

weevil_both <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_weevil_damage_partialclean.csv")) %>%
  dplyr::select(., -1)

herbivores <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_insect_herbivores_partialclean.csv")) %>%
  dplyr::select(., -1)

reproductive <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_reproductive_partialclean.csv")) %>%
  dplyr::select(., -1)

flowering_2020 <- read.csv(here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_floweringplants_partialclean.csv")) %>%
  dplyr::select(., -1)



# 2021 data-----
heights_both2 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_heights_partialclean.csv")) %>%
  dplyr::select(., -1)

herbivory_both2 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_herbivory_partialclean.csv")) %>%
  dplyr::select(., -1)

survival_2021 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_survival_partialclean.csv")) %>%
  dplyr::select(., -1)

weevil_both2 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_weevil_damage_partialclean.csv")) %>%
  dplyr::select(., -1)

herbivores2 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_insect_herbivores_partialclean.csv")) %>%
  dplyr::select(., -1)

reproductive2 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_reproductive_partialclean.csv")) %>%
  dplyr::select(., -1)

flowering_2021 <- read.csv(here::here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_floweringplants_partialclean.csv")) %>%
  dplyr::select(., -1)




#-------------------
# Clean data- step 2
#-------------------

# SB NOTE 2022: For 2021 (and maybe previous years), I’m doubting my
#  ability to characterize plants as dead. For my previous analyses,
#   I tried to identify which plants had died throughout different
#    parts of the year (initially, after the first summer, after the
#     first winter, etc.) but what I think happens is that the plants
#      shrivel up at different points and then oftentimes come back the
#       next year. I don’t think there’s a big risk of milkweed seeds 
#       from the surrounding non-garden plants getting into the pots 
#       and then germinating, so I think I should abandon my attempt
#        to figure out who’s been dead the whole time and just judge
#         by my end-of-summer surveys. Stop trying to compare the
#          years. Because I end up with “zombies”: plants that I 
#          thought were dead, but weren’t...


# 
# ## Assess survival: Need cleaned heights csv to do this, so I made a second cleaning step.
# #-----------------------------------------------------------------------------------------
# ### Find which plants had grown at season start --> make new survival_both df
# # Isn't a foolproof method for assessing overwinter mortality, but will tell which plants had no visible growth during first data collection in June
# 
# # Make new column signifying if pots contained zero alive plants, or at least one- 0 or 1
# ## First, copy first height column associaed with first ramet (if 0, that means pot was empty/had no alive ramets)
# 
# survival_overwinter <- heights_both[,c(1:10)]
# survival_overwinter$Mortality_binary <- survival_overwinter$Total_Height_June
# 
# ## Now change all instances of >0 to 1. If >=0, then at least one ramet present.
# survival_overwinter <- survival_overwinter %>%
# mutate(., Mortality_binary = ifelse((Mortality_binary == 0 & Ramets_June < 1), "1", "0"))
# 
# ### make new binary mortality column factor
# survival_overwinter$Mortality_binary <- as.factor(survival_overwinter$Mortality_binary)
# 
# # check appropriate columns are factors
# str(survival_overwinter)
# 
# 
# # extract mortality column from survival_overwinter df and add to survival df
# survival_both <- left_join(survival, survival_overwinter, by = c("Row", "Column", "Block", "Population", "Family", "Replicate"))
# # rename cols
# survival_both <- dplyr::rename(survival_both, Dead_Sept = Dead)
# survival_both <- dplyr::rename(survival_both, Dead_June = Mortality_binary)
# survival_both <- dplyr::rename(survival_both, Comment_Sept = Comment)
# # get rid of date columns & June height and # ramets cols
# survival_both <- survival_both[,-c(8, 11:13)]
# # reorder cols
# survival_both <- survival_both[, c(1:6, 10,9,8,7)]
# 
# 
# 
# 
# ### find plants deemed dead during both data collection assessments and years
# 
# # these plants were DEAD for all data collections in 2020
# dead_2020 <- survival_both %>% filter(., Dead_June == 1 & Dead_Sept == 1)
# 
# # shows which plants were dead or alive throughout all data collections in 2020
# survival_both <- transform(survival_both,
#                            dead_2020 = ifelse(Dead_June == 1 & Dead_Sept == 1, 1,0))
# 
# # these plants were dead for all data collections in 2019
# dead_2019 <- data_clean_2019 %>%
#   dplyr::filter(., Dead_2019 == 1)
# 
# # these plants were dead either throughout 2019 OR 2020
# dead_either_2019or2020_fullyear <- full_join(dead_2019[,c(1:6, 31)],
#                                              dead_2020,
#                                              by = c("Row", "Column", "Block", "Population", "Family", "Replicate"))
# 
# # these plants were dead throughout 2019 AND 2020
# # # THESE ARE THE PLANTS THAT HAVE BEEN DEAD FROM THE START, PRESUMABLY
# dead_bothyears <- inner_join(dead_2019[,c(1:6, 31)], dead_2020[,c(1:7,9)],
#                              by = c("Row", "Column", "Block", "Population", "Family", "Replicate")) 
# 
# # new df to see which plants I thought were dead in 2019 but really weren't in 2020
# # SO: should be in dead2019 but NOT in dead2020
# zombies <- anti_join(dead_2019, dead_2020, by = c("Row", "Column"))
# 
# 
# 
# ### Remove plants that were dead since early 2019 from ALL dfs
# #-------------------------------------------------------------------------
# heights_both <- anti_join(heights_both, dead_bothyears, by = c("Row", "Column"))
# herbivory_both <- anti_join(herbivory_both, dead_bothyears, by = c("Row", "Column"))
# survival_2020 <- anti_join(survival_both, dead_bothyears, by = c("Row", "Column"))
# weevil_both <- anti_join(weevil_both, dead_bothyears, by = c("Row", "Column"))
# herbivores <- anti_join(herbivores, dead_bothyears, by = c("Row", "Column"))
# reproductive <- anti_join(reproductive, dead_bothyears, by = c("Row", "Column"))
# 


#----------------------------------------------------------------
# Calculate variables, such as relative growth rate and herbivory,
# with datasets of plants that were alive in 2020
#----------------------------------------------------------------


## heights: Calculate relative growth rate
## ---------------------------------------------
# some plants had heights of 0 in June, Sept, or both:
## If they were 0 in June but >0 in September: June = 1, since ln(1)=0.
# If they were >0 in June but 0 in September: Sept = 1.
# If June = 0 and Sept = 0, it will be NA.

# 2020
heights_both <- heights_both %>%
  dplyr::mutate(.,
                rel_growth_rate = case_when(
                 Total_Height_June > 0 & Total_Height_Sept > 0 ~ (log(Total_Height_Sept) - log(Total_Height_June))/ date_diff,
                 Total_Height_June > 0 & Total_Height_Sept == 0  ~ (1 - log(Total_Height_June)) / date_diff,
                 Total_Height_June == 0 & Total_Height_Sept > 0  ~ (log(Total_Height_Sept) - 1) / date_diff))

# 2021
heights_both2 <- heights_both2 %>%
  dplyr::mutate(.,
                rel_growth_rate = case_when(
                  Total_Height_early > 0 & Total_Height_late > 0 ~ (log(Total_Height_late) - log(Total_Height_early))/ date_diff,
                  Total_Height_early > 0 & Total_Height_late == 0  ~ (1 - log(Total_Height_early)) / date_diff,
                  Total_Height_early == 0 & Total_Height_late > 0  ~ (log(Total_Height_late) - 1) / date_diff))




## weevil damage: standardize weevil scar length by total height
## ------------------------------------------------------------
# 2020

# add June height so I can standardize weevil scar length by total height
weevil_both <- weevil_both %>%
  left_join(., heights_both[,-c(7, 9:15)],
            by = c("Row", "Column", "Block", "Population",
                   "Family", "Replicate")) %>%
  mutate(Scar_length_cm = Scar_length_mm / 10) %>%
  
# standardize weevil scar length by total height
## value may be over 1 because 1) oftentimes there were multiple scars per ramet on different sides, and 2) height was measured in June and scars in July, so the plants were likely taller when scars were measured
mutate(scar_div_Juneheight = Scar_length_cm / (Total_Height_June)) %>%
dplyr::select(., -Scar_length_mm)

# 2021
weevil_both2 %<>%
  left_join(., heights_both2[,c(1:9, 11)],
            by = c("Row", "Column", "Block", "Population",
                   "Family", "Replicate")) %>%
  mutate(Scar_length_cm = Scar_length_mm / 10) %>%
  
  # standardize weevil scar length by total height
  ## value may be over 1 because 1) oftentimes there were multiple scars per
  ## ramet on different sides, and 2) height was measured in June and scars
  ## in July, so the plants were likely taller when scars were measured
  mutate(scar_div_earlyheight = Scar_length_cm / (Total_Height_early)) %>%
  dplyr::select(., -Scar_length_mm)





## herbivory: calculate average herbivory
## ---------------------------------------------
# Clean columns to get average herbivory
# 2020

# JULY------
### Replace non-numeric entries with NA and remove dashes (which symbolize no leaf) 
herbivory_both$Herbivory.Early_mean <- herbivory_both$Herbivory.Early
herbivory_both$Herbivory.Early_mean <- gsub('too small', NA, herbivory_both$Herbivory.Early_mean)

# converts plants we weren't able to assess to NA (from 0)- represents either a lack of plant 
# or no leaves
herbivory_both <- herbivory_both %>%
  mutate(Herbivory.July_mean = na_if(Herbivory.Early_mean, "0"))

herbivory_both$Herbivory.Early_mean <- gsub('-', "", herbivory_both$Herbivory.Early_mean)
herbivory_both$Herbivory.Early_mean <- gsub(' ', "", herbivory_both$Herbivory.Early_mean)

### Calculate mean herbivory for ramet 1 in new column
herbivory_both$Herbivory.July_mean <- sapply(strsplit(
  as.character(herbivory_both$Herbivory.Early_mean), ",", fixed=T),
  function(x) mean(as.numeric(x)))

# SEPT------
### Replace non-numeric entries with NA and remove dashes (which symbolize no leaf) 
herbivory_both$Herbivory.Late_mean <- herbivory_both$Herbivory.Late
herbivory_both$Herbivory.Late_mean <- gsub('too small', NA, herbivory_both$Herbivory.Late_mean)

# converts plants we weren't able to assess to NA (from 0)- represents either a lack of plant or no leaves
herbivory_both <- herbivory_both %>%
  mutate(Herbivory.Late_mean = na_if(Herbivory.Late_mean, "0"))

herbivory_both$Herbivory.Late_mean <- gsub('-', "", herbivory_both$Herbivory.Late_mean)
herbivory_both$Herbivory.Late_mean <- gsub(' ', "", herbivory_both$Herbivory.Late_mean)


### Calculate mean herbivory for ramet 1 in new column
herbivory_both$Herbivory.Late_mean <- sapply(strsplit(as.character(herbivory_both$Herbivory.Sept_mean), ",", fixed=T), function(x) mean(as.numeric(x)))


# Divide herbivory values by 100 to get percentage to use in glmers later w/binomial distr.
herbivory_both$Herbivory.Early_mean <- herbivory_both$Herbivory.Early_mean/100
herbivory_both$Herbivory.Late_mean <- herbivory_both$Herbivory.Late_mean/100





# 2021

# JULY------
herbivory_both2$Herbivory.early_mean <- herbivory_both2$Herbivory.early
unique(herbivory_both2$Herbivory.early)

# converts plants we weren't able to assess to NA
herbivory_both2 %<>%
  mutate(Herbivory.early_mean = na_if(Herbivory.early_mean, "too small")) %>%
  mutate(Herbivory.early_mean = na_if(Herbivory.early_mean, "too withered")) %>%
  mutate(Herbivory.early_mean = na_if(Herbivory.early_mean, "no pot"))

herbivory_both2$Herbivory.early_mean <- gsub('-', "", herbivory_both2$Herbivory.early_mean)
herbivory_both2$Herbivory.early_mean <- gsub(' ', "", herbivory_both2$Herbivory.early_mean)

### Calculate mean herbivory for ramet 1 in new column
herbivory_both2$Herbivory.early_mean <- sapply(strsplit(
  as.character(herbivory_both2$Herbivory.early_mean), ",", fixed=T),
  function(x) mean(as.numeric(x)))





# SEPT------
### Replace non-numeric entries with NA and remove dashes (which symbolize no leaf) 
herbivory_both2$Herbivory.late_mean <- herbivory_both2$Herbivory.late
unique(herbivory_both2$Herbivory.late_mean)

# converts plants we weren't able to assess to NA
herbivory_both2  %<>%
  mutate(Herbivory.late_mean = na_if(Herbivory.late_mean, ""))

herbivory_both2$Herbivory.late_mean <- gsub('-', "", herbivory_both2$Herbivory.late_mean)
herbivory_both2$Herbivory.late_mean <- gsub(' ', "", herbivory_both2$Herbivory.late_mean)


### Calculate mean herbivory for ramet 1 in new column
herbivory_both2$Herbivory.late_mean <- sapply(strsplit(
  as.character(herbivory_both2$Herbivory.late_mean), ",", fixed=T),
  function(x) mean(as.numeric(x)))


# Divide herbivory values by 100 to get percentage to use in glmers later w/binomial distr.
herbivory_both2$Herbivory.early_mean <- herbivory_both2$Herbivory.early_mean/100
herbivory_both2$Herbivory.late_mean <- herbivory_both2$Herbivory.late_mean/100






#-----------------------------------------------------------------------------------
# Add urbanization data (Distance from city center, transect IDs, urbanization score, etc.)
#-----------------------------------------------------------------------------------
## Add transect data (e.g. city_dist values)
### Add City_dist values with Haversine formula

# import data
Distances <- read.csv(here::here(
  "./CommonGardenExperiment_2019Data/clean_data/clean_haversine_Distances.csv"),
  na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",")


### Add transect data to all dfs via merge
# TRIED TO AUTOMATE WITH LAPPLY AND PURRR BUT NOT WORKING YET
# # make list of dfs
df.list <- list(heights_both,
                herbivory_both,
                survival_2020,
                weevil_both,
                herbivores,
                reproductive,
                flowering_2020,

                heights_both2,
                herbivory_both2,
                survival_2021,
                weevil_both2,
                herbivores2,
                reproductive2,
                flowering_2021)
# 
# # Add distances to city center
# for (df in length(df.list)){
#   df %<>% merge(., y = Distances, by = "Population", all.x = TRUE)
# }
# 
# df.list <- lapply(df.list, function(x)
#   x <- merge(x, y = Distances, by = "Population", all.x = TRUE))

# doing it manually
heights_both <- merge(heights_both, y = Distances, by = "Population", all.x = TRUE)
herbivory_both <- merge(herbivory_both, y = Distances, by = "Population", all.x = TRUE)
survival_2020 <- merge(survival_2020, y = Distances, by = "Population", all.x = TRUE)
weevil_both <- merge(weevil_both, y = Distances, by = "Population", all.x = TRUE)
herbivores <- merge(herbivores, y = Distances, by = "Population", all.x = TRUE)
reproductive <- merge(reproductive, y = Distances, by = "Population", all.x = TRUE)
flowering_2020 <- merge(flowering_2020, y = Distances, by = "Population", all.x = TRUE)

heights_both2 <- merge(heights_both2, y = Distances, by = "Population", all.x = TRUE)
herbivory_both2 <- merge(herbivory_both2, y = Distances, by = "Population", all.x = TRUE)
survival_2021 <- merge(survival_2021, y = Distances, by = "Population", all.x = TRUE)
weevil_both2 <- merge(weevil_both2, y = Distances, by = "Population", all.x = TRUE)
herbivores2 <- merge(herbivores2, y = Distances, by = "Population", all.x = TRUE)
reproductive2 <- merge(reproductive2, y = Distances, by = "Population", all.x = TRUE)
flowering_2021 <- merge(flowering_2021, y = Distances, by = "Population", all.x = TRUE)


# # make list of dfs
df.list <- list(heights_both,
                herbivory_both,
                survival_2020,
                weevil_both,
                herbivores,
                reproductive,
                flowering_2020,
                
                heights_both2,
                herbivory_both2,
                survival_2021,
                weevil_both2,
                herbivores2,
                reproductive2,
                flowering_2021)


list_names <- list("heights_both"   ,
                   "herbivory_both" ,
                   "survival_2020"  ,
                   "weevil_both"    ,
                   "herbivores"     ,
                   "reproductive"   ,
                   "flowering_2020" ,
                   
                   "heights_both2"  ,
                   "herbivory_both2",
                   "survival_2021"  ,
                   "weevil_both2"   ,
                   "herbivores2"    ,
                   "reproductive2"  ,
                   "flowering_2021")

names(df.list) <- list_names


df.list <- lapply(df.list, function(i) {
  i %>% mutate(Urb_Rur = 
                 ifelse(Transect_ID == "Rural", "Rural", "Urban"))
})


df.list %>%
  list2env(.GlobalEnv)




### Add urb_score data
# Import urb_index values for each of these rows
urb_scores <- read.csv(
  here::here("./CommonGardenExperiment_2020Data/raw_data/Urbanization_Score/Urbanization_Scores_Table.csv"),  header=T, na.strings=c("","NA"))

# get rid of unnecessary cols, rename Patch ID to Pop_ID for joins
urb_scores <- urb_scores[,c(2,8)]
names(urb_scores)[1] <- "Pop_ID"


# doing it manually
heights_both <- merge(heights_both, y = urb_scores, by = "Pop_ID", all.x = TRUE)
herbivory_both <- merge(herbivory_both, y = urb_scores, by = "Pop_ID", all.x = TRUE)
survival_2020 <- merge(survival_2020, y = urb_scores, by = "Pop_ID", all.x = TRUE)
weevil_both <- merge(weevil_both, y = urb_scores, by = "Pop_ID", all.x = TRUE)
herbivores <- merge(herbivores, y = urb_scores, by = "Pop_ID", all.x = TRUE)
reproductive <- merge(reproductive, y = urb_scores, by = "Pop_ID", all.x = TRUE)
flowering_2020 <- merge(flowering_2020, y = urb_scores, by = "Pop_ID", all.x = TRUE)

heights_both2 <- merge(heights_both2, y = urb_scores, by = "Pop_ID", all.x = TRUE)
herbivory_both2 <- merge(herbivory_both2, y = urb_scores, by = "Pop_ID", all.x = TRUE)
survival_2021 <- merge(survival_2020, y = urb_scores, by = "Pop_ID", all.x = TRUE)
weevil_both2 <- merge(weevil_both2, y = urb_scores, by = "Pop_ID", all.x = TRUE)
herbivores2 <- merge(herbivores2, y = urb_scores, by = "Pop_ID", all.x = TRUE)
reproductive2 <- merge(reproductive2, y = urb_scores, by = "Pop_ID", all.x = TRUE)
flowering_2021 <- merge(flowering_2021, y = urb_scores, by = "Pop_ID", all.x = TRUE)

# ----------------
# Export to new csvs
# ----------------

write.csv(heights_both,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_heights_clean.csv"))
write.csv(herbivory_both,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_herbivory_clean.csv"))
write.csv(survival_2020,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_survival_clean.csv"))
write.csv(weevil_both,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_weevil_damage_clean.csv"))
write.csv(herbivores,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_herbivores_clean.csv"))
write.csv(reproductive,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_reproductive_clean.csv"))
write.csv(flowering_2020,
          here::here("./CommonGardenExperiment_2020Data/clean_data/2020_floweringplants_clean.csv"))




write.csv(heights_both2,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_heights_clean.csv"))
write.csv(herbivory_both2,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_herbivory_clean.csv"))
write.csv(survival_2021,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_survival_clean.csv"))
write.csv(weevil_both2,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_weevil_damage_clean.csv"))
write.csv(herbivores2,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_herbivores_clean.csv"))
write.csv(reproductive2,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_reproductive_clean.csv"))
write.csv(flowering_2021,
          here::here("./CommonGardenExperiment_2021Data/clean_data/2021_floweringplants_clean.csv"))
