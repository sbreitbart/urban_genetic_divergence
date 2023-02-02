
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
#-------------------
# Data about flowering, pollen removal, inflor and follicle count, + other reproductive traits (Data collection 2)-----
reproductive <- read.csv(here::here(
  "./CommonGardenExperiment_2020Data/raw_data/Reproductive_Traits/2020_Datacollection2_20210110.csv"),
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

# remove columns that were temporary
reproductive <- reproductive[,-c(9:10)]

# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
reproductive[factor_cols] <- lapply(reproductive[factor_cols], factor)






# Add col to indicate if plant flowered
reproductive$Flowered2020 <- reproductive$Flower_count_I1

## Now change all instances of >0 to 1. If >=1, the plant flowered.
reproductive <- reproductive %>% mutate(reproductive,
                                        Flowered2020 = ifelse(Flowered2020 == "0", "0", "1"))
## Change NAs to 0's.
reproductive$Flowered2020[is.na(reproductive$Flowered2020)] <- 0

### make new binary flowering column factor
reproductive$Flowered2020 <- as.factor(reproductive$Flowered2020)

# Make subset of only flowering plants
flowering_2020 <- reproductive %>% filter(!is.na(Flower_count_I1))

# Remove flowering data from df of all plants
reproductive <- reproductive[,-c(9:81)]




# Make certain columns factors / check classes are correct
# THIS ISN'T WORKING, SO DOING IT MANUALLY
# # make these columns into numeric
# flowering_2020 <- flowering_2020 %>%
#   dplyr::mutate_at(vars(contains(c("Hood", "Corolla", "Flower"))),
#                    funs(as.character()))
#                     %>%
#   dplyr::mutate_at(vars(starts_with(c("Hood", "Corolla", "Flower"))),
#                    funs(as.numeric())) %>%
#   
#   # make these columns into integers
#   dplyr::mutate_at(vars(starts_with(c("Flower_count", "Poll_rem", "Pods", "Peduncles"))),
#                    dplyr::funs(as.character())) %>%
#   dplyr::mutate_at(vars(starts_with(c("Flower_count", "Poll_rem", "Pods", "Peduncles"))),
#                    dplyr::funs(as.integer()))



# Pods, Peduncles, Flower_count, Poll_rem: int
# Hood, Corolla, Flower width/length:      numeric

# Convert appropriate column types to integer.
to_int <- c(14:15, # pods, peduncles
            16, 38, 60, # flower count
            23, 30, 37, 45, 52, 59, 67, 74, 81) # pollinaria removed)
flowering_2020[to_int] <- lapply(flowering_2020[to_int], as.integer)
str(flowering_2020)

# Convert appropriate column types to character and then numeric.
to_char_num <- c(17:22, 24:29, 31:36, 39:44, 46:51, 53:58, 61:66, 68:73, 75:80)
flowering_2020[to_char_num] <- lapply(flowering_2020[to_char_num], as.character)
flowering_2020[to_char_num] <- lapply(flowering_2020[to_char_num], as.numeric)
str(flowering_2020)









# find mean flower size
flowering_2020 <- flowering_2020 %>% rowwise() %>%
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
flowering_2020$mean_flower_count <- NA
flowering_2020 <- flowering_2020 %>%
  rowwise() %>% 
  dplyr::mutate(., mean_flower_count = mean(c(Flower_count_I1,
                                              Flower_count_I2,
                                              Flower_count_I3),
                                            na.rm = TRUE))

# compute total flower count
flowering_2020 <- flowering_2020 %>%
  rowwise() %>% 
  dplyr::mutate(., total_flower_count = sum(c(Flower_count_I1,
                                              Flower_count_I2,
                                              Flower_count_I3),
                                            na.rm = TRUE))

# find mean pollinaria removed per plant
poll_avg <- flowering_2020 %>%
  dplyr::select(c(1:6), starts_with("Poll")) %>%
  dplyr::mutate(mean_poll = mean(c_across(7:15), na.rm = T)) %>%
  dplyr::select(1:6, mean_poll) 

# add means to df
flowering_2020 %<>%
  left_join(poll_avg)

#-------------------
# Export to new csv
#-------------------
write.csv(reproductive,
          here::here("./data/CommonGardenExperiment_2020Data/partially_cleaned_data/2020_reproductive_partialclean.csv"))
write.csv(flowering_2020,
          here::here("./data/CommonGardenExperiment_2020Data/partially_cleaned_data/2020_floweringplants_partialclean.csv"))
