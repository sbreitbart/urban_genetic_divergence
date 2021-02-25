## Load data

library(tidyverse)

## Import data

# 2019 data-----
data_clean_2019 <- read.csv("./CommonGardenExperiment_2019Data/clean_data/clean_data_2019KSR.csv", na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -c(1:2))

# 2020 data-----
heights_both_2020 <- read.csv("./CommonGardenExperiment_2020Data/clean_data/2020_heights_clean.csv") %>%
  dplyr::select(., -c(1:2))

herbivory_both_2020 <- read.csv("./CommonGardenExperiment_2020Data/clean_data/2020_herbivory_clean.csv") %>%
  dplyr::select(., -c(1:2))

survival_2020 <- read.csv("./CommonGardenExperiment_2020Data/clean_data/2020_survival_clean.csv") %>%
  dplyr::select(., -c(1:2))

weevil_both <- read.csv("./CommonGardenExperiment_2020Data/clean_data/2020_weevil_damage_clean.csv") %>%
  dplyr::select(., -c(1:2))

reproductive <- read.csv("./CommonGardenExperiment_2020Data/clean_data/2020_reproductive_clean.csv") %>%
  dplyr::select(., -c(1:2))

flowering_2020 <- read.csv("./CommonGardenExperiment_2020Data/clean_data/2020_floweringplants_clean.csv") %>%
  dplyr::select(., -c(1:2))



## Make combined 2019/2020 dfs

# heights (includes heights and num. ramets)-----
## first make copy of data_clean_2019 with colnames that match heights_both_2020's
heights_2019 <- data_clean_2019 %>%
  dplyr::select(., c(1:6, 11:13, 18:19, 25:41)) %>%
  dplyr::filter(., Dead_2019 == 0) %>%
  dplyr::rename(., Dead = Dead_2019,
                Ramets_midJune = Num_Ramets_DC2, # doing this & following rows to compare btwn years
                Ramets_Sept = Num_Ramets_DC3, 
                Total_Height_midJune = Total_Height_DC2,
                Total_Height_Sept = Total_height_DC3) 
heights_2019$Year = 2019
heights_2019$rel_growth_rate <- as.character(heights_2019$rel_growth_rate)
heights_2019$rel_growth_rate <- as.numeric(heights_2019$rel_growth_rate)


# copy of 2020 data w/colnames that match 2019's
heights_2020 <- heights_both_2020 %>%
  dplyr::select(., -c(16)) %>%
  dplyr::rename(., Ramets_midJune = Ramets_June, # doing this & following rows to compare btwn years
                Total_Height_midJune = Total_Height_June) 
heights_2020$Year = 2020

# combine yearly dfs
heights_19_20 <- plyr::rbind.fill(heights_2019, heights_2020)
heights_19_20$Year <- as.factor(heights_19_20$Year)
heights_19_20$Family <- as.factor(heights_19_20$Family)
heights_19_20$Population <- as.factor(heights_19_20$Population)
heights_19_20$rel_growth_rate <- as.numeric(heights_19_20$rel_growth_rate)

# rm(heights_2019, heights_2020)

heights_19_20$Population <- as.factor(heights_19_20$Population)
heights_19_20$Family <- as.factor(heights_19_20$Family)




# herbivory-----
## first make copy of data_clean_2019 with colnames that match heights_both_2020's
herb_2019 <- data_clean_2019 %>%
  dplyr::select(., c(1:6, 18:19, 21, 32, 35:41)) %>%
  dplyr::filter(., Dead_2019 == 0) %>%
  dplyr::rename(., Dead = Dead_2019,
                Herbivory_mean_Sept = Herbivory_mean_DC3 # doing this to compare btwn years
  ) 
herb_2019$Year = 2019
herb_2019$Herbivory_mean_Sept <- as.character(herb_2019$Herbivory_mean_Sept)
herb_2019$Herbivory_mean_Sept <- as.numeric(herb_2019$Herbivory_mean_Sept)

# copy of 2020 data w/colnames that match 2019's
herb_2020 <- herbivory_both_2020 %>%
  dplyr::select(., -c(18)) %>%
  dplyr::rename(., Herbivory_mean_Sept = Herbivory.Sept_mean # doing thisto compare btwn years
  ) 
herb_2020$Year = 2020

# combine yearly dfs
herbivory_19_20 <- plyr::rbind.fill(herb_2019, herb_2020)

# rm(herb_2019, herb_2020)

herbivory_19_20$Population <- as.factor(herbivory_19_20$Population)
herbivory_19_20$Family <- as.factor(herbivory_19_20$Family)





# survival-----

## first make copy of data_clean_2019 with colnames that match heights_both_2020's
surv_2019 <- data_clean_2019 %>%
  dplyr::select(., c(1:6, 14:16, 25, 32, 35:41)) %>%
  dplyr::rename(., Dead = Dead_2019                ) 
surv_2019$Year = 2019

# copy of 2020 data w/colnames that match 2019's
surv_2020 <- survival_2020 %>%
  dplyr::select(., -c(12)) %>%
  dplyr::rename(., Dead = dead_2020 # doing thisto compare btwn years
  ) 
surv_2020$Year = 2020

# combine yearly dfs
survival_19_20 <- plyr::rbind.fill(surv_2019, surv_2020)

# rm(surv_2019, surv_2020)

survival_19_20$Population <- as.factor(survival_19_20$Population)
survival_19_20$Family <- as.factor(survival_19_20$Family)

