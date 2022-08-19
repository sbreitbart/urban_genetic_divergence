## -----------------------------------------------------------------
source("libraries.R")
source("functions.R")


## -----------------------------------------------------------------

# LATEX-----
latex <- read.csv(here::here("./CommonGardenExperiment_2020Data/clean_data/2020_latex_clean.csv")) %>%
  dplyr::select(-1) %>%
  dplyr::mutate_at(vars(c(1:7, 9:12, 15, 18, 21)), as.character) %>%
    dplyr::mutate_at(vars(c(1:7, 9:12, 15, 18, 21)), as.factor) %>%
  dplyr::filter(Latex_weight_mg >= 0) %T>% # remove some negative values
  str()

latex %<>%
  dplyr::mutate(Fam_uniq = as.factor(paste0(Population, "_", Family)))

str(latex)


# HERBIVORY-----
herbivory <- read.csv(here::here("./Joined_annual_data/herbivory.csv")) %>%
  dplyr::select(-1) %>%
  dplyr::filter(Year != 2019) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.factor) %T>%
  str()

herbivory %<>%
  dplyr::mutate(Fam_uniq = as.factor(paste0(Population, "_", Family)))



# WEEVIL DAMAGE-----
weevil <- read.csv(here::here("./Joined_annual_data/weevil.csv")) %>%
  dplyr::select(-1) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.factor) %T>%
  str()

weevil %<>%
  dplyr::mutate(Fam_uniq = as.factor(paste0(Population, "_", Family)))


## -----------------------------------------------------------------
# Basic data exploration
# plot(Latex_weight_mg ~ City_dist, data = latex) 
# 
# boxplot(Latex_weight_mg ~ Block, data = latex)

ltx_gr_city_m1 <- glmmTMB(Latex_weight_mg^(1/3) ~
                            Block +
                            (1|Population/Family) +
                            City_dist,
                        data = latex,
                        REML = F)


# performance::check_model(ltx_gr_city_m1)
# plot(DHARMa::simulateResiduals(ltx_gr_city_m1))
# car::Anova(ltx_gr_city_m1)


## -----------------------------------------------------------------
ltx_gr_usc_m1 <- glmmTMB(Latex_weight_mg^(1/3) ~ Block +
                           (1|Population/Family) +
                           Urb_score,
                        data = latex,
                        REML = F)

performance::check_model(ltx_gr_usc_m1)
plot(DHARMa::simulateResiduals(ltx_gr_usc_m1))
car::Anova(ltx_gr_usc_m1)



## -----------------------------------------------------------------
ltx_urbsubs_city_m1 <- glmmTMB(Latex_weight_mg^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(ltx_urbsubs_city_m1)
# plot(DHARMa::simulateResiduals(ltx_urbsubs_city_m1))
# car::Anova(ltx_urbsubs_city_m1)



# MAIN EFFECTS MODEL
ltx_urbsubs_city_m2 <- glmmTMB(Latex_weight_mg^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(ltx_urbsubs_city_m2)
# plot(DHARMa::simulateResiduals(ltx_urbsubs_city_m2))
# car::Anova(ltx_urbsubs_city_m2)

AIC(ltx_urbsubs_city_m1, ltx_urbsubs_city_m2) # m2 best but <2 AIC away


## -----------------------------------------------------------------
ltx_urbsubs_usc_m1 <- glmmTMB(Latex_weight_mg^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(ltx_urbsubs_usc_m1)
# plot(DHARMa::simulateResiduals(ltx_urbsubs_usc_m1))
# car::Anova(ltx_urbsubs_usc_m1)



# MAIN EFFECTS MODEL
ltx_urbsubs_usc_m2 <- glmmTMB(Latex_weight_mg^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(ltx_urbsubs_usc_m2)
# plot(DHARMa::simulateResiduals(ltx_urbsubs_usc_m2))
# car::Anova(ltx_urbsubs_usc_m2)

AIC(ltx_urbsubs_usc_m1, ltx_urbsubs_usc_m2) # m1 best


## -----------------------------------------------------------------
# # Basic data exploration
# plot(Herbivory_mean_early ~ City_dist, data = herbivory) 
# 
# boxplot(Herbivory_mean_early ~ Block, data = herbivory) # anything above 0.1 looks like an outlier
# 
# herbiv_e_gr_dist_m1 <- glmmTMB(Herbivory_mean_early^(1/3) ~
#                                  Block +
#                                  Year +
#                                  (1|Population/Family) +
#                                  City_dist,
#                               data = herbivory,
#                               REML = F)
# 
# plot(DHARMa::simulateResiduals(herbiv_e_gr_dist_m1)) # still problematic even after transforming response (and doesn't converge untransformed)
# 
# hist(herbivory$Herbivory_mean_early, breaks = 50) # zero-inflated?
# 
# # try binomial- still problematic
# plot(
#   DHARMa::simulateResiduals(
#     glmmTMB(Herbivory_mean_early ~
#                   Block +
#                   Year +
#                   (1|Population/Family) +
#                   City_dist,
#                 data = herbivory,
#                 family = binomial,
#                 REML = F)
#     )
#   )
# 
# performance::check_model(
#     glmmTMB(Herbivory_mean_early ~
#                   Block +
#                   Year +
#                   (1|Population/Family) +
#                   City_dist,
#                 data = herbivory,
#                 family = binomial,
#                 REML = F)
#     )

# # TRY BETA FAMILY
# herbivory$Herbivory_mean_early_recode <- herbivory$Herbivory_mean_early
# herbivory$Herbivory_mean_early_recode[herbivory$Herbivory_mean_early_recode == 1] <- 0.999999
# herbivory$Herbivory_mean_early_recode[herbivory$Herbivory_mean_early_recode == 0] <- 0.000001
# 
# test1 <- glmmTMB(Herbivory_mean_early_recode  ~
#                    Block +
#                    Year +
#                    (1|Population/Family) +
#                    City_dist,
#                                 data = herbivory,
#                                 family = beta_family(link="logit"),
#                                REML = F)
# performance::check_model(test1) # heavy right tail
# plot(DHARMa::simulateResiduals(test1))


# TRY HURDLE MODEL-----
# first, create new column with 0/1 indicating any herbivory
herbivory %<>%
  mutate(Herbivory_mean_early_binary = case_when(
    Herbivory_mean_early == 0 ~ 0,
    Herbivory_mean_early > 0 ~ 1)
  )

herbiv_e_gr_dist_m1_bin <- glmmTMB(Herbivory_mean_early_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist,
                              data = herbivory,
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_e_gr_dist_m1_bin)) # looks great
# 
# car::Anova(herbiv_e_gr_dist_m1_bin)


# quantitative model
herbiv_e_gr_dist_m1_quant <- glmmTMB(log(Herbivory_mean_early) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_early_binary == 1),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_e_gr_dist_m1_quant)) # looks great
# performance::check_model(herbiv_e_gr_dist_m1_quant) # looks great
# car::Anova(herbiv_e_gr_dist_m1_quant)


## -----------------------------------------------------------------
# HURDLE MODEL
herbiv_e_gr_usc_m1_bin <- glmmTMB(Herbivory_mean_early_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score,
                              data = herbivory,
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_e_gr_usc_m1_bin)) # looks great
# car::Anova(herbiv_e_gr_usc_m1_bin)


# quantitative model
herbiv_e_gr_usc_m1_quant <- glmmTMB(log(Herbivory_mean_early) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_early_binary == 1),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_e_gr_usc_m1_quant)) # looks great
# performance::check_model(herbiv_e_gr_usc_m1_quant) # looks great
# car::Anova(herbiv_e_gr_usc_m1_quant)


## -----------------------------------------------------------------
# HURDLE MODEL
herbiv_e_urbsubs_dist_m1_bin <- glmmTMB(Herbivory_mean_early_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist * Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_dist_m1_bin)) # looks great
# car::Anova(herbiv_e_urbsubs_dist_m1_bin)


# quantitative model
herbiv_e_urbsubs_dist_m1_quant <- glmmTMB(log(Herbivory_mean_early) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist * Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_early_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_dist_m1_quant)) # looks great
# car::Anova(herbiv_e_urbsubs_dist_m1_quant)


# MAIN EFFECTS MODELS
## BINARY- doesn't converge unless I remove block or year
## if I remove block, p-value for city_dist stays nearly same. If I remove year, it becomes significantly smaller... If I remove block from full model, it doesn't converge. the full model also has a marginally significant interaction of transect*distance, so I'll just use that one.
herbiv_e_urbsubs_dist_m2_bin <- glmmTMB(Herbivory_mean_early_binary ~
                              Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist + Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_dist_m2_bin)) # looks great
# car::Anova(herbiv_e_urbsubs_dist_m2_bin)



# QUANTITATIVE
herbiv_e_urbsubs_dist_m2_quant <- glmmTMB(log(Herbivory_mean_early) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist + Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_early_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_dist_m2_quant)) # looks great
# 
# AIC(herbiv_e_urbsubs_dist_m1_quant, herbiv_e_urbsubs_dist_m2_quant) # m1 better but <2 AIC apart


## -----------------------------------------------------------------
# HURDLE MODEL
herbiv_e_urbsubs_usc_m1_bin <- glmmTMB(Herbivory_mean_early_binary ~
                              Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score * Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)

plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_usc_m1_bin)) # looks great


# quantitative model
herbiv_e_urbsubs_usc_m1_quant <- glmmTMB(log(Herbivory_mean_early) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score * Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_early_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_usc_m1_quant)) # looks great



# MAIN EFFECTS MODELS
## BINARY
herbiv_e_urbsubs_usc_m2_bin <- glmmTMB(Herbivory_mean_early_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score + Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_usc_m2_bin)) # looks great
# 
# AIC(herbiv_e_urbsubs_usc_m1_bin, herbiv_e_urbsubs_usc_m2_bin) # m2 better but <2 AIC apart



# QUANTITATIVE
herbiv_e_urbsubs_usc_m2_quant <- glmmTMB(log(Herbivory_mean_early) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score + Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_early_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_e_urbsubs_usc_m2_quant)) # looks great
# AIC(herbiv_e_urbsubs_usc_m1_quant, herbiv_e_urbsubs_usc_m2_quant) # m1 better but <2 AIC apart


## -----------------------------------------------------------------
## Basic data exploration
# plot(Herbivory_mean_late ~ City_dist, data = herbivory)
# 
# boxplot(Herbivory_mean_late ~ Block, data = herbivory) # anything above 0.23 looks like an outlier (was 0.1 for early)

# HURDLE MODEL
# first, create new column with 0/1 indicating any herbivory
herbivory %<>%
  mutate(Herbivory_mean_late_binary = case_when(
    Herbivory_mean_late == 0 ~ 0,
    Herbivory_mean_late > 0 ~ 1)
  )

herbiv_l_gr_dist_m1_bin <- glmmTMB(Herbivory_mean_late_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist,
                              data = herbivory,
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_l_gr_dist_m1_bin))
# car::Anova(herbiv_l_gr_dist_m1_bin)


# quantitative model
herbiv_l_gr_dist_m1_quant <- glmmTMB(log(Herbivory_mean_late) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_late_binary == 1),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_l_gr_dist_m1_quant))
# performance::check_model(herbiv_l_gr_dist_m1_quant) # looks great
# car::Anova(herbiv_l_gr_dist_m1_quant)



## -----------------------------------------------------------------
# HURDLE MODEL-----
herbiv_l_gr_usc_m1_bin <- glmmTMB(Herbivory_mean_late_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score,
                              data = herbivory,
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_l_gr_usc_m1_bin)) 
# car::Anova(herbiv_l_gr_usc_m1_bin)


# quantitative model
herbiv_l_gr_usc_m1_quant <- glmmTMB(log(Herbivory_mean_late) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_late_binary == 1),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_l_gr_usc_m1_quant))
# performance::check_model(herbiv_l_gr_usc_m1_quant) # looks great
# car::Anova(herbiv_l_gr_usc_m1_quant)


## -----------------------------------------------------------------
# HURDLE MODEL
herbiv_l_urbsubs_dist_m1_bin <- glmmTMB(Herbivory_mean_late_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist * Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_dist_m1_bin)) 

# quantitative model
herbiv_l_urbsubs_dist_m1_quant <- glmmTMB(log(Herbivory_mean_late) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist * Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_late_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models


# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_dist_m1_quant))



# MAIN EFFECTS MODELS
## BINARY
herbiv_l_urbsubs_dist_m2_bin <- glmmTMB(Herbivory_mean_late_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist + Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)

# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_dist_m2_bin)) # looks good enough
# 
# AIC(herbiv_l_urbsubs_dist_m1_bin, herbiv_l_urbsubs_dist_m2_bin) # m2 better but <2 AIC apart



# QUANTITATIVE
herbiv_l_urbsubs_dist_m2_quant <- glmmTMB(log(Herbivory_mean_late) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                City_dist + Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_late_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models

# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_dist_m2_quant)) # looks good enough
# 
# AIC(herbiv_l_urbsubs_dist_m1_quant, herbiv_l_urbsubs_dist_m2_quant) # m2 better but <2 AIC apart



## -----------------------------------------------------------------
# HURDLE MODEL
herbiv_l_urbsubs_usc_m1_bin <- glmmTMB(Herbivory_mean_late_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score * Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)


# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_usc_m1_bin)) # looks good enough


# quantitative model
herbiv_l_urbsubs_usc_m1_quant <- glmmTMB(log(Herbivory_mean_late) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score * Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_late_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models


# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_usc_m1_quant))



# MAIN EFFECTS MODELS
## BINARY
herbiv_l_urbsubs_usc_m2_bin <- glmmTMB(Herbivory_mean_late_binary ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score + Transect_ID,
                              data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                       family = binomial,
                              REML = F)


# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_usc_m2_bin)) # looks good enough
# AIC(herbiv_l_urbsubs_usc_m1_bin, herbiv_l_urbsubs_usc_m2_bin) # m2 better but <2 AIC apart



# QUANTITATIVE
herbiv_l_urbsubs_usc_m2_quant <- glmmTMB(log(Herbivory_mean_late) ~
                               Block +
                               Year + 
                               (1|Population/Family) +
                                Urb_score + Transect_ID,
                              data = herbivory %>%
                        dplyr::filter(
                          Herbivory_mean_late_binary == 1 & Transect_ID != "Rural"),
                              REML = F) # log-transformed after looking at untransformed, sqrt, cube root models


# plot(DHARMa::simulateResiduals(herbiv_l_urbsubs_usc_m2_quant))
# AIC(herbiv_l_urbsubs_usc_m1_quant, herbiv_l_urbsubs_usc_m2_quant) # m2 better but <2 AIC apart


## -----------------------------------------------------------------
# # Basic data exploration
# plot(Scar_length_cm ~ City_dist, data = weevil)
# 
# boxplot(Scar_length_cm ~ Block, data = weevil) # block 4 seems pretty different spread than block 3 and others

# BINARY MODEL-----
# create binary column
weevil$Scar_binary <- weevil$Scar_length_cm
weevil$Scar_binary[weevil$Scar_binary > 0] <- 1


weev_gr_dist_m1_bin <- glmmTMB(Scar_binary ~ Block + Year + (1|Population/Family) + City_dist,
                              data = weevil,
                              family = "binomial"(link = "logit"),
                              REML = F)

# plot(DHARMa::simulateResiduals(weev_gr_dist_m1_bin)) # looks good



# QUANTITATIVE MODEL-----
weev_gr_dist_m1_quant <- glmmTMB(log(Scar_length_cm) ~
                                   Block +
                                   Year +
                                   (1|Population/Family) +
                                   City_dist,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0),
                              REML = F)

# performance::check_model(weev_gr_dist_m1_quant)
# plot(DHARMa::simulateResiduals(weev_gr_dist_m1_quant)) # looks great
# weev_gr_dist_m1_quant %>% car::Anova()


## -----------------------------------------------------------------
# BINARY MODEL-----
weev_gr_usc_m1_bin <- glmmTMB(Scar_binary ~
                                Block + 
                                Year + 
                                (1|Population/Family) + 
                                Urb_score,
                              data = weevil,
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_gr_usc_m1_bin)
# plot(DHARMa::simulateResiduals(weev_gr_usc_m1_bin)) # looks good

# QUANTITATIVE MODEL-----
weev_gr_usc_m1_quant <- glmmTMB(log(Scar_length_cm) ~
                                   Block +
                                   Year +
                                   (1|Population/Family) +
                                   Urb_score,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0),
                              REML = F)

# performance::check_model(weev_gr_usc_m1_quant)
# plot(DHARMa::simulateResiduals(weev_gr_usc_m1_quant))
# weev_gr_usc_m1_quant %>% car::Anova()



## -----------------------------------------------------------------
# BINARY MODEL-----
weev_urbsubs_dist_m1_bin <- glmmTMB(Scar_binary ~
                                      Block + 
                                      Year + 
                                      (1|Population/Family) +
                                      City_dist * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_urbsubs_dist_m1_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m1_bin)) # looks good enough. dharma vignette says, "To remove this pattern, you would need to make the dispersion parameter dependent on a predictor (e.g. in JAGS), or apply a transformation on the data."

# MAIN EFFECTS
weev_urbsubs_dist_m2_bin <- glmmTMB(Scar_binary ~ Block +
                                      Year +
                                      (1|Population/Family) +
                                      City_dist + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_urbsubs_dist_m2_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m2_bin)) # looks good
# 
# AIC(weev_urbsubs_dist_m1_bin, weev_urbsubs_dist_m2_bin) # m2 better but <2AIC away




# QUANTITATIVE MODEL-----
weev_urbsubs_dist_m1_quant <- glmmTMB(log(Scar_length_cm) ~
                                        Block +
                                        Year +
                                        (1|Population/Family) +
                                        City_dist * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# performance::check_model(weev_urbsubs_dist_m1_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m1_quant)) 



# MAIN EFFECTS MODEL
weev_urbsubs_dist_m2_quant <- glmmTMB(log(Scar_length_cm) ~
                                        Block +
                                        Year +
                                        (1|Population/Family) +
                                        City_dist + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# performance::check_model(weev_urbsubs_dist_m2_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m2_quant)) 
# AIC(weev_urbsubs_dist_m1_quant, weev_urbsubs_dist_m2_quant) # m2 best but <2 AIC away


## -----------------------------------------------------------------
# BINARY MODEL-----
weev_urbsubs_usc_m1_bin <- glmmTMB(Scar_binary ~
                                      Block + 
                                      Year + 
                                      (1|Population/Family) +
                                      Urb_score * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_urbsubs_usc_m1_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m1_bin)) # looks good enough. dharma vignette says, "To remove this pattern, you would need to make the dispersion parameter dependent on a predictor (e.g. in JAGS), or apply a transformation on the data."

# MAIN EFFECTS
weev_urbsubs_usc_m2_bin <- glmmTMB(Scar_binary ~ Block +
                                      Year +
                                      (1|Population/Family) +
                                      Urb_score + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_urbsubs_usc_m2_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m2_bin)) # looks good
# AIC(weev_urbsubs_usc_m1_bin, weev_urbsubs_usc_m2_bin) # m2 better but <2AIC away




# QUANTITATIVE MODEL-----
weev_urbsubs_usc_m1_quant <- glmmTMB(log(Scar_length_cm) ~
                                        Block +
                                        Year +
                                        (1|Population/Family) +
                                        Urb_score * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# performance::check_model(weev_urbsubs_usc_m1_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m1_quant))



# MAIN EFFECTS MODEL
weev_urbsubs_usc_m2_quant <- glmmTMB(log(Scar_length_cm) ~
                                        Block +
                                        Year +
                                        (1|Population/Family) +
                                        Urb_score + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# performance::check_model(weev_urbsubs_usc_m2_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m2_quant))
# AIC(weev_urbsubs_usc_m1_quant, weev_urbsubs_usc_m2_quant) # m2 best but <2 AIC away



## -----------------------------------------------------------------
latex_mods <- list(

## City_dist / gradient
ltx_gr_city_m1 ,

## Urbanization score / gradient
ltx_gr_usc_m1,

## City_dist / urban subtransects
ltx_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
ltx_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
ltx_urbsubs_usc_m1 # Best model
)

names(latex_mods) <- c("City_gr", "Usc_gr", "City_urbsubs_alt", "City_urbsubs_best", "Usc_urbsubs")


## -----------------------------------------------------------------
herb_early_mods_binomial <- list(

## City_dist / gradient
herbiv_e_gr_dist_m1_bin ,

## Urbanization score / gradient
herbiv_e_gr_usc_m1_bin,

## City_dist / urban subtransects
herbiv_e_urbsubs_dist_m1_bin, # Best model

## Urbanization score  / urban subtransects
herbiv_e_urbsubs_usc_m1_bin, # Qualitatively identical model (<2 AIC away)
herbiv_e_urbsubs_usc_m2_bin # Best model
)

names(herb_early_mods_binomial) <- c("City_gr",
                            "Usc_gr",
                            "City_urbsubs_best",
                            "Usc_urbsubs_alt",
                            "Usc_urbsubs_best")



herb_early_mods_quant <- list(

## City_dist / gradient
herbiv_e_gr_dist_m1_quant ,

## Urbanization score / gradient
herbiv_e_gr_usc_m1_quant,

## City_dist / urban subtransects
herbiv_e_urbsubs_dist_m1_quant, # Best model
herbiv_e_urbsubs_dist_m2_quant, # Qualitatively identical model (<2 AIC away)

## Urbanization score  / urban subtransects
herbiv_e_urbsubs_usc_m1_quant, # Best model
herbiv_e_urbsubs_usc_m2_quant # Qualitatively identical model (<2 AIC away)
)

names(herb_early_mods_quant) <- c("City_gr",
                            "Usc_gr",
                            "City_urbsubs_best",
                            "City_urbsubs_alt",
                            "Usc_urbsubs_best",
                            "Usc_urbsubs_alt")


## -----------------------------------------------------------------
herb_late_mods_binomial <- list(

## City_dist / gradient
herbiv_l_gr_dist_m1_bin ,

## Urbanization score / gradient
herbiv_l_gr_usc_m1_bin,

## City_dist / urban subtransects
herbiv_l_urbsubs_dist_m1_bin, # Qualitatively identical model (<2 AIC away)
herbiv_l_urbsubs_dist_m2_bin, # Best model

## Urbanization score  / urban subtransects
herbiv_l_urbsubs_usc_m1_bin, # Qualitatively identical model (<2 AIC away)
herbiv_l_urbsubs_usc_m2_bin # Best model
)

names(herb_late_mods_binomial) <- c("City_gr",
                            "Usc_gr",
                            "City_urbsubs_alt",
                            "City_urbsubs_best",
                            "Usc_urbsubs_alt",
                            "Usc_urbsubs_best")



herb_late_mods_quant <- list(

## City_dist / gradient
herbiv_l_gr_dist_m1_quant ,

## Urbanization score / gradient
herbiv_l_gr_usc_m1_quant,

## City_dist / urban subtransects
herbiv_l_urbsubs_dist_m1_quant  , # Qualitatively identical model (<2 AIC away)
herbiv_l_urbsubs_dist_m2_quant  , # Best model

## Urbanization score  / urban subtransects
herbiv_l_urbsubs_usc_m1_quant, # Qualitatively identical model (<2 AIC away)
herbiv_l_urbsubs_usc_m2_quant # Best model
)

names(herb_late_mods_quant) <- c("City_gr",
                            "Usc_gr",
                            "City_urbsubs_alt",
                            "City_urbsubs_best",
                            "Usc_urbsubs_alt",
                            "Usc_urbsubs_best")


## -----------------------------------------------------------------
weev_mods_binomial <- list(

## City_dist / gradient
weev_gr_dist_m1_bin ,

## Urbanization score / gradient
weev_gr_usc_m1_bin,

## City_dist / urban subtransects
weev_urbsubs_dist_m1_bin, # Qualitatively identical model (<2 AIC away)
weev_urbsubs_dist_m2_bin, # Best model

## Urbanization score  / urban subtransects
weev_urbsubs_usc_m1_bin , # Qualitatively identical model (<2 AIC away)
weev_urbsubs_usc_m2_bin # Best model
)

names(weev_mods_binomial) <- c("City_gr",
                      "Usc_gr",
                      "City_urbsubs_alt",
                      "City_urbsubs_best",
                      "Usc_urbsubs_alt",
                      "Usc_urbsubs_best")




weev_mods_quant <- list(

## City_dist / gradient
weev_gr_dist_m1_quant ,

## Urbanization score / gradient
weev_gr_usc_m1_quant,

## City_dist / urban subtransects
weev_urbsubs_dist_m1_quant, # Qualitatively identical model (<2 AIC away)
weev_urbsubs_dist_m2_quant, # Best model

## Urbanization score  / urban subtransects
weev_urbsubs_usc_m1_quant , # Qualitatively identical model (<2 AIC away)
weev_urbsubs_usc_m2_quant # Best model
)

names(weev_mods_quant) <- c("City_gr",
                      "Usc_gr",
                      "City_urbsubs_alt",
                      "City_urbsubs_best",
                      "Usc_urbsubs_alt",
                      "Usc_urbsubs_best")


## -----------------------------------------------------------------

all_models <- list(
latex_mods         , # Latex
herb_early_mods_binomial    , # Herbivory, before flowering:  binomial (part of hurdle models)
herb_early_mods_quant    , # Herbivory, before flowering: quantitative (part of hurdle models)
herb_late_mods_binomial    , # Herbivory, after flowering:  binomial (part of hurdle models)
herb_late_mods_quant    , # Herbivory, after flowering: quantitative (part of hurdle models)
weev_mods_binomial , # Weevil scar: binomial (part of hurdle models)
weev_mods_quant)     # Weevil scar: quantitative (part of hurdle models)

names(all_models) <- c(
"latex_mods"         ,
"herb_early_mods_binomial"    ,
"herb_early_mods_quant"    ,
"herb_late_mods_binomial"     ,
"herb_late_mods_quant"     ,
"weev_mods_binomial" ,
"weev_mods_quant")


## -----------------------------------------------------------------
## Best
### City_dist
latex_mods_best_c <- latex_mods[c(1,4)]

### Urb_score
latex_mods_best_u <- latex_mods[c(2,5)]

## Alt
### City_dist
latex_mods_alt_c <- latex_mods[3]

### Urb_score- NONE



## -----------------------------------------------------------------
## Best
### City_dist
herb_e_bin_mods_best_c <- herb_early_mods_binomial[c(1,3)]

### Urb_score
herb_e_bin_mods_best_u <- herb_early_mods_binomial[c(2,5)]

## Alt
### City_dist- NONE

### Urb_score
herb_e_bin_mods_alt_u <- herb_early_mods_binomial[4]



## -----------------------------------------------------------------
## Best
### City_dist
herb_e_quant_mods_best_c <- herb_early_mods_quant[c(1,3)]

### Urb_score
herb_e_quant_mods_best_u <- herb_early_mods_quant[c(2,5)]

## Alt
### City_dist
herb_e_quant_mods_alt_c <- herb_early_mods_quant[4]

### Urb_score
herb_e_quant_mods_alt_u <- herb_early_mods_quant[6]



## -----------------------------------------------------------------
## Best
### City_dist
herb_l_bin_mods_best_c <- herb_late_mods_binomial[c(1,4)]

### Urb_score
herb_l_bin_mods_best_u <- herb_late_mods_binomial[c(2,6)]

## Alt
### City_dist
herb_l_bin_mods_alt_c <- herb_late_mods_binomial[3]

### Urb_score
herb_l_bin_mods_alt_u <- herb_late_mods_binomial[5]



## -----------------------------------------------------------------
## Best
### City_dist
herb_l_quant_mods_best_c <- herb_late_mods_quant[c(1,4)]

### Urb_score
herb_l_quant_mods_best_u <- herb_late_mods_quant[c(2,6)]

## Alt
### City_dist
herb_l_quant_mods_alt_c <- herb_late_mods_quant[3]

### Urb_score
herb_l_quant_mods_alt_u <- herb_late_mods_quant[5]



## -----------------------------------------------------------------
## Best
### City_dist
weev_bin_mods_best_c <- weev_mods_binomial[c(1,4)]

### Urb_score
weev_bin_mods_best_u <- weev_mods_binomial[c(2,6)]

## Alt
### City_dist
weev_bin_mods_alt_c <- weev_mods_binomial[3]

### Urb_score
weev_bin_mods_alt_u <- weev_mods_binomial[5]


## -----------------------------------------------------------------
## Best
### City_dist
weev_quant_mods_best_c <- weev_mods_quant[c(1,4)]

### Urb_score
weev_quant_mods_best_u <- weev_mods_quant[c(2,6)]

## Alt
### City_dist
weev_quant_mods_alt_c <- weev_mods_quant[3]

### Urb_score
weev_quant_mods_alt_u <- weev_mods_quant[5]

