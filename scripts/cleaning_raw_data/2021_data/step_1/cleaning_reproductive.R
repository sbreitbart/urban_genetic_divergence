
# -------------------
# Load packages
#-------------------
library(dplyr)
library(tidyr)
library(here)
library(magrittr)


#-------------------
# Import data
#-------------------
# Data about flowering, pollen removal, inflor and follicle count, + other reproductive traits (Data collection 2)-----
reproductive <- read.csv(here(
  "./CommonGardenExperiment_2021Data/raw_data/2021_DC2_FloralTraits_final.csv"),
  header=T,
  stringsAsFactors=FALSE,
  na.strings=c("NO PLANT", "none", "", "NA"),
  blank.lines.skip=TRUE) %>%
  as.data.frame()


#-------------------
# Clean data
#-------------------
# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.

c <- is.na(reproductive$Block) == TRUE
c2 <- c(1:nrow(reproductive))[c]
reproductive <- reproductive[-c2,]
names(reproductive)[1]<-"Row"
reproductive <- reproductive[!is.na(reproductive$Population), ]  
  
rm(c, c2)





# Make certain columns factors / check classes are correct-----
str(reproductive)


# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
reproductive[factor_cols] <- lapply(reproductive[factor_cols], factor)




# Add col to indicate if plant flowered
reproductive %<>%
  dplyr::mutate(.,
         Flowered2021 = ifelse(is.na(reproductive$Flower_count_I1) == TRUE,
                               0,
                               1)) %>%
  dplyr::mutate(.,
         Flowered2021 = as.factor(Flowered2021))

unique(reproductive$Flowered2021)


# Make subset of only flowering plants
flowering_2021 <- reproductive %>%
  filter(!is.na(Flower_count_I1))

# Remove flowering data from df of all plants
reproductive %<>%
  dplyr::select(-c(13:78))


str(flowering_2021)

# Make certain columns factors / check classes are correct

# Pods, Peduncles, Flower_count, Poll_rem: int
# Hood, Corolla, Flower width/length:      numeric



# find mean flower size
flowering_2021 <- flowering_2021 %>% rowwise() %>%
  # start with hood size
  dplyr::mutate(., Hood.I1F1 = Hood_L.I1F1 * Hood_W.I1F1,
                Hood.I1F2 = Hood_L.I1F2 * Hood_W.I1F2,
                Hood.I1F3 = Hood_L.I1F3 * Hood_W.I1F3,
                Hood.I2F1 = Hood_L.I2F1 * Hood_W.I2F1,
                Hood.I2F2 = Hood_L.I2F2 * Hood_W.I2F2,
                Hood.I2F3 = Hood_L.I2F3 * Hood_W.I2F3,
                Hood.I3F1 = Hood_L.I3F1 * Hood_W.I3F1,
                Hood.I3F2 = Hood_L.I3F2 * Hood_W.I3F2,
                Hood.I3F3 = Hood_L.I3F3 * Hood_W.I3F3) %>%
  dplyr::mutate(., Hood.I1 = mean(c(Hood.I1F1, Hood.I1F2, Hood.I1F3), na.rm = T),
                Hood.I2 = mean(c(Hood.I2F1, Hood.I2F2, Hood.I2F3), na.rm = T),
                Hood.I3 = mean(c(Hood.I3F1, Hood.I3F2, Hood.I3F3), na.rm = T)) %>%
  dplyr::mutate(., Hood_mean = mean(c(Hood.I1, Hood.I2, Hood.I3), na.rm = T)) %>%
  # do same for corollas
    dplyr::mutate(., Corolla.I1F1 = Corolla_L.I1F1 * Corolla_W.I1F1,
                Corolla.I1F2 = Corolla_L.I1F2 * Corolla_W.I1F2,
                Corolla.I1F3 = Corolla_L.I1F3 * Corolla_W.I1F3,
                Corolla.I2F1 = Corolla_L.I2F1 * Corolla_W.I2F1,
                Corolla.I2F2 = Corolla_L.I2F2 * Corolla_W.I2F2,
                Corolla.I2F3 = Corolla_L.I2F3 * Corolla_W.I2F3,
                Corolla.I3F1 = Corolla_L.I3F1 * Corolla_W.I3F1,
                Corolla.I3F2 = Corolla_L.I3F2 * Corolla_W.I3F2,
                Corolla.I3F3 = Corolla_L.I3F3 * Corolla_W.I3F3) %>%
  dplyr::mutate(., Corolla.I1 = mean(c(Corolla.I1F1, Corolla.I1F2, Corolla.I1F3), na.rm = T),
                Corolla.I2 = mean(c(Corolla.I2F1, Corolla.I2F2, Corolla.I2F3), na.rm = T),
                Corolla.I3 = mean(c(Corolla.I3F1, Corolla.I3F2, Corolla.I3F3), na.rm = T)) %>%
  dplyr::mutate(., Corolla_mean = mean(c(Corolla.I1, Corolla.I2, Corolla.I3), na.rm = T)) %>% 
    # do same for total flower L, W
    dplyr::mutate(., Flower.I1F1 = Flower_L.I1F1 * Flower_W.I1F1,
                Flower.I1F2 = Flower_L.I1F2 * Flower_W.I1F2,
                Flower.I1F3 = Flower_L.I1F3 * Flower_W.I1F3,
                Flower.I2F1 = Flower_L.I2F1 * Flower_W.I2F1,
                Flower.I2F2 = Flower_L.I2F2 * Flower_W.I2F2,
                Flower.I2F3 = Flower_L.I2F3 * Flower_W.I2F3,
                Flower.I3F1 = Flower_L.I3F1 * Flower_W.I3F1,
                Flower.I3F2 = Flower_L.I3F2 * Flower_W.I3F2,
                Flower.I3F3 = Flower_L.I3F3 * Flower_W.I3F3) %>%
  dplyr::mutate(., Flower.I1 = mean(c(Flower.I1F1, Flower.I1F2, Flower.I1F3), na.rm = T),
                Flower.I2 = mean(c(Flower.I2F1, Flower.I2F2, Flower.I2F3), na.rm = T),
                Flower.I3 = mean(c(Flower.I3F1, Flower.I3F2, Flower.I3F3), na.rm = T)) %>%
  dplyr::mutate(., Flower_mean = mean(c(Flower.I1, Flower.I2, Flower.I3), na.rm = T)) %>%
  # multiply those 3 components for overall flower size identifier
    dplyr::mutate(., Overall_mean = mean(c(Hood_mean, Corolla_mean, Flower_mean), na.rm = T)) 

# compute mean flower count
flowering_2021$mean_flower_count <- NA
flowering_2021 <- flowering_2021 %>%
  rowwise() %>% 
  dplyr::mutate(., mean_flower_count = mean(c(Flower_count_I1,
                                              Flower_count_I2,
                                              Flower_count_I3),
                                            na.rm = TRUE))

# compute total flower count
flowering_2021 <- flowering_2021 %>%
  rowwise() %>% 
  dplyr::mutate(., total_flower_count = sum(c(Flower_count_I1,
                                              Flower_count_I2,
                                              Flower_count_I3),
                                            na.rm = TRUE))

#-------------------
# Export to new csv
#-------------------
write.csv(reproductive,
          here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_reproductive_partialclean.csv"))
write.csv(flowering_2021,
          here("./CommonGardenExperiment_2021Data/partially_cleaned_data/2021_floweringplants_partialclean.csv"))
