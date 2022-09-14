#-------------------
# Import KSR field data
#-------------------
survival_2022 <- read.csv(here::here("./CommonGardenExperiment_2022Data/partially_cleaned_data/2022_survival_partialclean.csv")) %>%
  dplyr::select(., -1)

reproductive2 <- read.csv(here::here("./CommonGardenExperiment_2022Data/partially_cleaned_data/2022_reproductive_partialclean.csv")) %>%
  dplyr::select(., -1)

flowering_2022 <- read.csv(here::here("./CommonGardenExperiment_2022Data/partially_cleaned_data/2022_floweringplants_partialclean.csv")) %>%
  dplyr::select(., -1)


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
survival_2022 <- merge(survival_2022, y = Distances, by = "Population", all.x = TRUE)
reproductive2 <- merge(reproductive2, y = Distances, by = "Population", all.x = TRUE)
flowering_2022 <- merge(flowering_2022, y = Distances, by = "Population", all.x = TRUE)


# # make list of dfs
df.list <- list(survival_2022,
                reproductive2,
                flowering_2022)


list_names <- list("survival_2022"  ,
                   "reproductive2"  ,
                   "flowering_2022")

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
survival_2022 <- merge(survival_2022, y = urb_scores, by = "Pop_ID", all.x = TRUE)
reproductive2 <- merge(reproductive2, y = urb_scores, by = "Pop_ID", all.x = TRUE)
flowering_2022 <- merge(flowering_2022, y = urb_scores, by = "Pop_ID", all.x = TRUE)

# ----------------
# Export to new csvs
# ----------------

write.csv(survival_2022,
          here::here("./CommonGardenExperiment_2022Data/clean_data/2022_survival_clean.csv"))
write.csv(reproductive2,
          here::here("./CommonGardenExperiment_2022Data/clean_data/2022_reproductive_clean.csv"))
write.csv(flowering_2022,
          here::here("./CommonGardenExperiment_2022Data/clean_data/2022_floweringplants_clean.csv"))
