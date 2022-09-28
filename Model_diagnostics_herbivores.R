## ------------------------------------------------------------
source("libraries.R")
source("functions.R")


## ------------------------------------------------------------
herbivores_all <- read.csv(here::here("./Joined_annual_data/herbivores.csv")) %>%
  dplyr::select(., -1) %>%
  dplyr::mutate_at(vars(c("Population",
                          "Family",
                          "Replicate",
                          "Block",
                          "Transect_ID",
                          "Sample",
                          "Patch_ID",
                          "Urb_Rur",
                          "Year",
                          "Surveyor",
                          "Date")),
                   as.character) %>%
    dplyr::mutate_at(vars(c("Population",
                            "Family",
                            "Replicate",
                            "Block",
                            "Transect_ID",
                            "Sample",
                            "Patch_ID",
                            "Urb_Rur",
                            "Year",
                            "Surveyor",
                          "Date")),
                     as.factor)

str(herbivores_all)


## ------------------------------------------------------------
# filter out herbivores not surveyed in 2021
# Herbivores surveyed in 2020-2021: Monarchs, L. asclepiadis, L. clivicollis
herbivores <- herbivores_all %>%
  dplyr::select(c(1:7, 10, 13, 19, 22:Surveyor))


## ------------------------------------------------------------
monarch_gr_city_m1 <- glmmTMB(Monarch_Quantity_Observed ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist,
              data = herbivores,
              family = nbinom2,
              REML = F) # may be an outlier, but it's biologically relevant. And anova result doesn't change if it's included or excluded.

# performance::check_model(monarch_gr_city_m1) # looks fine
# car::Anova(monarch_gr_city_m1)


## ------------------------------------------------------------
monarch_gr_usc_m1 <- glmmTMB(Monarch_Quantity_Observed ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score,
              data = herbivores,
              family = nbinom2,
              REML = F) 

# performance::check_model(monarch_gr_usc_m1) # looks fine


## ------------------------------------------------------------
# Full model
monarch_urbsubs_city_m1 <- glmmTMB(Monarch_Quantity_Observed ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist * Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(monarch_urbsubs_city_m1)


# MAIN EFFECTS MODEL
monarch_urbsubs_city_m2 <- glmmTMB(Monarch_Quantity_Observed ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist + Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(monarch_urbsubs_city_m2) # looks fine

# AIC(monarch_urbsubs_city_m1,
#     monarch_urbsubs_city_m2) # m2 better but <2 AIC apart


## ------------------------------------------------------------
# Full model
monarch_urbsubs_usc_m1 <- glmmTMB(Monarch_Quantity_Observed ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score * Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(monarch_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
monarch_urbsubs_usc_m2 <- glmmTMB(Monarch_Quantity_Observed ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score + Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(monarch_urbsubs_usc_m2) # looks fine
# AIC(monarch_urbsubs_usc_m1,
#     monarch_urbsubs_usc_m2) # m2 better but <2 AIC apart


## ------------------------------------------------------------
asclepiadis_gr_city_m1 <- glmmTMB(Liriomyza_asclepiadis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist,
              data = herbivores,
              family = nbinom2,
              REML = F)

# performance::check_model(asclepiadis_gr_city_m1)



## ------------------------------------------------------------
asclepiadis_gr_usc_m1 <- glmmTMB(Liriomyza_asclepiadis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score,
              data = herbivores,
              family = nbinom2,
              REML = F) 

# performance::check_model(asclepiadis_gr_usc_m1)



## ------------------------------------------------------------
# Full model
asclepiadis_urbsubs_city_m1 <- glmmTMB(Liriomyza_asclepiadis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist * Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(asclepiadis_urbsubs_city_m1)


# MAIN EFFECTS MODEL
asclepiadis_urbsubs_city_m2 <- glmmTMB(Liriomyza_asclepiadis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist + Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(asclepiadis_urbsubs_city_m2)
# AIC(asclepiadis_urbsubs_city_m1,
#     asclepiadis_urbsubs_city_m2) # m2 better but <2 AIC apart


## ------------------------------------------------------------
# Full model
asclepiadis_urbsubs_usc_m1 <- glmmTMB(Liriomyza_asclepiadis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score * Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F)

# performance::check_model(asclepiadis_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
asclepiadis_urbsubs_usc_m2 <- glmmTMB(Liriomyza_asclepiadis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score + Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural"),
              family = nbinom2,
              REML = F) 

# performance::check_model(asclepiadis_urbsubs_usc_m2)
# AIC(asclepiadis_urbsubs_usc_m1,
#     asclepiadis_urbsubs_usc_m2) # m2 better but <2 AIC apart


## ------------------------------------------------------------
clivicollis_gr_city_m1 <- glmmTMB(Labidomera_clivicollis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist,
              data = herbivores %>%
                dplyr::filter(Labidomera_clivicollis < 9),
              family = nbinom2,
              REML = F) # remove outliers to maintain homogeneity of variance

# performance::check_model(clivicollis_gr_city_m1)


## ------------------------------------------------------------
clivicollis_gr_usc_m1 <- glmmTMB(Labidomera_clivicollis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score,
              data = herbivores %>%
                dplyr::filter(Labidomera_clivicollis < 9),
              family = nbinom2,
              REML = F) # remove outliers to maintain homogeneity of variance

# performance::check_model(clivicollis_gr_usc_m1)



## ------------------------------------------------------------
# Full model
clivicollis_urbsubs_city_m1 <- glmmTMB(Labidomera_clivicollis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist * Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural" & Labidomera_clivicollis < 20), # remove outlier
              family = nbinom2,
              REML = F)

# performance::check_model(clivicollis_urbsubs_city_m1) 



# MAIN EFFECTS MODEL
clivicollis_urbsubs_city_m2 <- glmmTMB(Labidomera_clivicollis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                City_dist + Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural" & Labidomera_clivicollis < 20), # remove outlier
              family = nbinom2,
              REML = F)

# performance::check_model(clivicollis_urbsubs_city_m2) 

AIC(clivicollis_urbsubs_city_m1,
    clivicollis_urbsubs_city_m2) # m1 better but <2 AIC apart


## ------------------------------------------------------------
# Full model
clivicollis_urbsubs_usc_m1 <- glmmTMB(Labidomera_clivicollis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score * Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural" & Labidomera_clivicollis < 20), # remove outlier
              family = nbinom2,
              REML = F)

# performance::check_model(clivicollis_urbsubs_usc_m1) 



# MAIN EFFECTS MODEL
clivicollis_urbsubs_usc_m2 <- glmmTMB(Labidomera_clivicollis ~
                                Block +
                                (1|Population/Family) +
                                Year +
                                Sample +
                                Urb_score + Transect_ID,
              data = herbivores %>%
                dplyr::filter(Transect_ID != "Rural" & Labidomera_clivicollis < 20), # remove outlier
              family = nbinom2,
              REML = F)

# performance::check_model(clivicollis_urbsubs_usc_m2) 
# AIC(clivicollis_urbsubs_usc_m1,
#     clivicollis_urbsubs_usc_m2) # m2 better but <2 AIC apart


## ------------------------------------------------------------
monarch_mods <- list(

## City_dist / gradient
monarch_gr_city_m1 ,

## Urbanization score / gradient
monarch_gr_usc_m1 ,

## City_dist / urban subtransects
monarch_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
monarch_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
monarch_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
monarch_urbsubs_usc_m2 # Best model

)

names(monarch_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## ------------------------------------------------------------
asclepiadis_mods <- list(

## City_dist / gradient
asclepiadis_gr_city_m1 ,

## Urbanization score / gradient
asclepiadis_gr_usc_m1 ,

## City_dist / urban subtransects
asclepiadis_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
asclepiadis_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
asclepiadis_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
asclepiadis_urbsubs_usc_m2 # Best model

)

names(asclepiadis_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## ------------------------------------------------------------
clivicollis_mods <- list(

## City_dist / gradient
clivicollis_gr_city_m1 ,

## Urbanization score / gradient
clivicollis_gr_usc_m1 ,

## City_dist / urban subtransects
clivicollis_urbsubs_city_m1, # Best model
clivicollis_urbsubs_city_m2, # Qualitatively identical model (<2 AIC away)

## Urbanization score  / urban subtransects
clivicollis_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
clivicollis_urbsubs_usc_m2 # Best model

)

names(clivicollis_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_best",
                     "City_urbsubs_alt",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")

