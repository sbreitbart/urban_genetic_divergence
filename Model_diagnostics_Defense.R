## ------------------------------------------------------------
source("libraries.R")
source("functions.R")


## ------------------------------------------------------------
# SLA & LDMC-----
sla_ldmc <- read.csv(here::here("./CommonGardenExperiment_2020Data/clean_data/2020_sla_ldmc_clean.csv")) %>%
  dplyr::select(., -c(1:2)) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block")), as.factor)

str(sla_ldmc)
sla_ldmc %<>%
  dplyr::mutate(Fam_uniq = paste0(Population, "_", Family))


# LATEX-----
latex <- read.csv(here::here("./CommonGardenExperiment_2020Data/clean_data/2020_latex_clean.csv")) %>%
  dplyr::select(-1) %>%
  dplyr::mutate_at(vars(c(1:7, 9:12, 15, 18, 21)), as.character) %>%
    dplyr::mutate_at(vars(c(1:7, 9:12, 15, 18, 21)), as.factor) %T>%
  str()

latex %<>%
  dplyr::mutate(Fam_uniq = as.factor(paste0(Population, "_", Family)))

str(latex)


# HERBIVORY-----
herbivory <- read.csv(here::here("./Joined_annual_data/herbivory.csv")) %>%
  dplyr::select(-1) %>%
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


## ------------------------------------------------------------
# ldmc_gr_city_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population/Family) + City_dist,
#                         data = sla_ldmc,
#                         REML = F)
# 
# performance::check_model(ldmc_gr_city_m1) # looks like an outlier. Remove and try again

# performance::check_model(update(ldmc_gr_city_m1, . ~ . , data = sla_ldmc %>% dplyr::filter(LDMC < 1)))
# # convergence problem. Take out block

# performance::check_model(update(ldmc_gr_city_m1, . ~ (1|Population/Family) + City_dist , data = sla_ldmc %>%
#                                   dplyr::filter(LDMC < 1))) # better! put back outlier to see if it changes

# performance::check_model(update(ldmc_gr_city_m1, . ~ (1|Population/Family) + City_dist , data = sla_ldmc)) # still an outlier. Keep out. Still a bit right-skewed though, so try sqrt
# 
# performance::check_model(update(ldmc_gr_city_m1, sqrt(LDMC) ~ (1|Population/Family) + City_dist , data = sla_ldmc %>% dplyr::filter(LDMC < 1))) # convergence issue again. Stick with untransformed model


# ldmc_gr_city_m1 <- glmmTMB(LDMC ~ (1|Population/Family) + City_dist,
#                            data = sla_ldmc %>%
#                              dplyr::filter(LDMC < 1.5))
# 
# car::Anova(ldmc_gr_city_m1)
# # returning NaNs. Try making family unique and then crossing?
# 
# sla_ldmc %<>%
#   dplyr::mutate(Fam_uniq = paste0(Population, "_", Family))
# 
# # this works: p = 0.19
# car::Anova(update(ldmc_gr_city_m1,
#                   . ~ (1|Population:Fam_uniq) + City_dist ,
#                   data = sla_ldmc %>%
#                     dplyr::filter(LDMC < 1)))
# 
# # gives same result (p = 0.19)
# car::Anova(update(ldmc_gr_city_m1,
#                              . ~ (1|Population:Family) + City_dist ,
#                              data = sla_ldmc %>%
#                                dplyr::filter(LDMC < 1)))
# 
# # this doesn't work though
# car::Anova(update(ldmc_gr_city_m1,
#                              . ~ (1|Population/Fam_uniq) + City_dist ,
#                              data = sla_ldmc %>%
#                                dplyr::filter(LDMC < 1)))
# 
# # this DOES work and gives same result as previous models
# car::Anova(update(ldmc_gr_city_m1,
#                              . ~ (1|Fam_uniq) + City_dist ,
#                              data = sla_ldmc %>%
#                                dplyr::filter(LDMC < 1)))


# going to use this as final model
ldmc_gr_city_m1 <- glmmTMB(LDMC ~ (1|Population:Fam_uniq) + City_dist,
                             data = sla_ldmc %>%
                               dplyr::filter(LDMC < 1))

# car::Anova(ldmc_gr_city_m1)
# performance::check_model(ldmc_gr_city_m1)


## ------------------------------------------------------------
# ldmc_gr_usc_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population/Family) + Urb_score,
#                         data = sla_ldmc,
#                         REML = F)
# 
# performance::check_model(ldmc_gr_usc_m1) # remove same outlier
# 
# performance::check_model(update(ldmc_gr_usc_m1, . ~ . ,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(LDMC < 1)))
# # works fine
# 
# car::Anova(update(ldmc_gr_usc_m1, . ~ . ,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(LDMC < 1)))
# 
# # I took out block: result barely changed. Subbed in Fam_uniq, result didn't change.
# 

ldmc_gr_usc_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population/Family) + Urb_score,
                        data = sla_ldmc %>%
                                  dplyr::filter(LDMC < 1),
                        REML = F)


## ------------------------------------------------------------
# ldmc_urbsubs_city_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population/Family) + City_dist * Transect_ID,
#                         data = sla_ldmc %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# not converging. Try changing family

# performance::check_model(update(ldmc_urbsubs_city_m1, . ~ (1|Block) + (1|Population:Family) + City_dist * Transect_ID,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural") %>%
#                                   dplyr::filter(LDMC < 1.5)))
# # says CIty_dist and Transect ID are collinear. Otherwise looks fine. Same diagnostics as (1|Fam_uniq) and (1|Population:Fam_uniq)
# 
# car::Anova(update(ldmc_urbsubs_city_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + City_dist * Transect_ID,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural") %>%
#                                   dplyr::filter(LDMC < 1.5)),
#            type = "III") # interaxn not sig so will use type II SS
# 
# car::Anova(update(ldmc_urbsubs_city_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + City_dist * Transect_ID,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural") %>%
#                                   dplyr::filter(LDMC < 1.5)),
#            type = "II") # interaxn not sig
# Final model
ldmc_urbsubs_city_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population:Fam_uniq) + City_dist * Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)



# MAIN EFFECTS MODEL
ldmc_urbsubs_city_m2 <- glmmTMB(LDMC ~ (1|Block) + (1|Population:Fam_uniq) + City_dist + Transect_ID,
                        data = sla_ldmc %>%
                                  dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5),
                        REML = F)

# car::Anova(ldmc_urbsubs_city_m2)
# 
# # COMPARE AIC
# AIC(ldmc_urbsubs_city_m1, ldmc_urbsubs_city_m2) # m2 lower but not by more than 2 AIC


## ------------------------------------------------------------
# ldmc_urbsubs_usc_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population/Family) + Urb_score * Transect_ID,
#                         data = sla_ldmc %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)

# performance::check_model(ldmc_urbsubs_usc_m1) # not converging. Remove outlier and try again
# 
# 
# performance::check_model(update(ldmc_urbsubs_usc_m1, . ~ .,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5))) # still not converging. try using Fam_uniq
# 
# # this works. As does using (1|Fam_uniq)
# performance::check_model(update(ldmc_urbsubs_usc_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5)))
# 
# # gives same result as (1|Fam_uniq)
# car::Anova(update(ldmc_urbsubs_usc_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5)),
#            type = "III") # interaxn not sig so try type II SS
# 
# 
# car::Anova(update(ldmc_urbsubs_usc_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID,
#                                 data = sla_ldmc %>%
#                                   dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5)),
#            type = "II")


ldmc_urbsubs_usc_m1 <- glmmTMB(LDMC ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID,
                                data = sla_ldmc %>%
                                  dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5))


# MAIN EFFECTS MODEL
ldmc_urbsubs_usc_m2 <- glmmTMB(LDMC ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score + Transect_ID,
                                data = sla_ldmc %>%
                                  dplyr::filter(Transect_ID != "Rural" & LDMC < 1.5))

# car::Anova(ldmc_urbsubs_usc_m2)
# 
# AIC(ldmc_urbsubs_usc_m1, ldmc_urbsubs_usc_m2) # ME model best model but not <2 AIC away


## ------------------------------------------------------------
# sla_gr_city_m1 <- glmmTMB(SLA ~ (1|Block) + (1|Population/Family) + City_dist,
#                         data = sla_ldmc,
#                         REML = F)
# 
# performance::check_model(sla_gr_city_m1) 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(sla_gr_city_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + City_dist)) 
# 
# 
# 
# # Try IDing outlier residuals, removing from df, and rerunning model
# LargeResiduals <- resid(sla_gr_city_m1) > 50
# # 101, 170, 220, 304, 410, 444, 787
# 
# lg_resids <- sla_ldmc %>%
#   dplyr::slice(101, 170, 220, 304, 410, 444, 787)
# 
# sla_without_lgresids <- anti_join(sla_ldmc, lg_resids)
# 
# 
# sla_gr_city_m1_no_lg_resids <- glmmTMB(SLA ~ (1|Block) + (1|Population/Family) + City_dist,
#                         data = sla_without_lgresids,
#                         REML = F)
# 
# performance::check_model(sla_gr_city_m1_no_lg_resids) 
# 
# 
# car::Anova(sla_gr_city_m1) # same result as third model
# car::Anova(sla_gr_city_m1_no_lg_resids) # still qualitatively same result as other two models (p >> 0.1)
# car::Anova(update(sla_gr_city_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + City_dist)) # same result as first model
# 

sla_gr_city_m1 <- glmmTMB(SLA ~ (1|Block) + (1|Population:Fam_uniq) + City_dist,
                        data = sla_ldmc,
                        REML = F)



## ------------------------------------------------------------
# sla_gr_usc_m1 <- glmmTMB(SLA ~ (1|Block) + (1|Population/Family) + Urb_score,
#                         data = sla_ldmc,
#                         REML = F)
# 
# performance::check_model(sla_gr_usc_m1) # try sqrt transformation
# 
# performance::check_model(update(sla_gr_usc_m1, sqrt(SLA) ~ .)) # looks better
# 
# # try with fam_uniq- looks same as above
# performance::check_model(update(sla_gr_usc_m1, sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score))
# 
# car::Anova(sla_gr_usc_m1) # lowest p-val but still >>0.1 so qualitatively same as other models
# car::Anova(update(sla_gr_usc_m1, sqrt(SLA) ~ .)) # same as third model
# car::Anova(update(sla_gr_usc_m1, sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score)) # same as second model

sla_gr_usc_m1 <- glmmTMB(sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score,
                        data = sla_ldmc,
                        REML = F)


## ------------------------------------------------------------
# sla_urbsubs_city_m1 <- glmmTMB(SLA ~ (1|Block) + (1|Population/Family) + City_dist * Transect_ID,
#                         data = sla_ldmc %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# 
# performance::check_model(sla_urbsubs_city_m1) # seems ok but try sqrt
# 
# performance::check_model(update(sla_urbsubs_city_m1, sqrt(SLA) ~ .)) # looks a bit better
# 
# # No interaxns sig for either so use type II SS
# car::Anova(sla_urbsubs_city_m1, type = "III")
# car::Anova(update(sla_urbsubs_city_m1, sqrt(SLA) ~ .), type = "III")
# car::Anova(update(sla_urbsubs_city_m1, sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + City_dist * Transect_ID), type = "III")
# 
# # Qualitatively identical again
# car::Anova(sla_urbsubs_city_m1, type = "II")
# car::Anova(update(sla_urbsubs_city_m1, sqrt(SLA) ~ .), type = "II")
# car::Anova(update(sla_urbsubs_city_m1, sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + City_dist * Transect_ID), type = "II")


sla_urbsubs_city_m1 <- glmmTMB(sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + City_dist * Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# MAIN EFFECTS
sla_urbsubs_city_m2 <- glmmTMB(sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + City_dist + Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# AIC(sla_urbsubs_city_m1, sla_urbsubs_city_m2) # qualitatively identical but m2 best model


## ------------------------------------------------------------
# sla_urbsubs_usc_m1 <- glmmTMB(SLA ~ (1|Block) + (1|Population/Family) + Urb_score * Transect_ID,
#                         data = sla_ldmc %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# 
# performance::check_model(sla_urbsubs_usc_m1) # seems ok but try sqrt
# 
# performance::check_model(update(sla_urbsubs_usc_m1, sqrt(SLA) ~ .)) # looks a bit better
# 
# # these are identical
# car::Anova(update(sla_urbsubs_usc_m1, sqrt(SLA) ~ .), type = "III") # interaxn not sig
# car::Anova(update(sla_urbsubs_usc_m1, sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID), type = "III") # interaxn not sig
# 
# 
# 
# # Qualitatively identical again
# car::Anova(update(sla_urbsubs_usc_m1, sqrt(SLA) ~ .), type = "II")
# car::Anova(update(sla_urbsubs_usc_m1, sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID), type = "II")


sla_urbsubs_usc_m1 <- glmmTMB(sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score * Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# MAIN EFFECTS
sla_urbsubs_usc_m2 <- glmmTMB(sqrt(SLA) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score + Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# AIC(sla_urbsubs_usc_m1, sla_urbsubs_usc_m2) # qualitatively identical but m2 best model


## ------------------------------------------------------------
ltx_gr_city_m1 <- glmmTMB(Latex_weight_mg ~ (1|Block) + (1|Population/Family) + City_dist,
                        data = latex,
                        REML = F)
# 
# performance::check_model(ltx_gr_city_m1) 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(ltx_gr_city_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + City_dist)) 
# 
# # there is a diff (qualitatively identical though). So use Pop/Fam
# car::Anova(ltx_gr_city_m1)
# car::Anova(update(ltx_gr_city_m1, . ~ (1|Block) + (1|Fam_uniq) + City_dist))



## ------------------------------------------------------------
ltx_gr_usc_m1 <- glmmTMB(Latex_weight_mg ~ (1|Block) + (1|Population/Family) + Urb_score,
                        data = latex,
                        REML = F)

# performance::check_model(ltx_gr_usc_m1) 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(ltx_gr_usc_m1, . ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score)) 
# 
# # there is a diff (qualitatively identical though). So use Pop/Fam. P < 0.01 in both cases
# car::Anova(ltx_gr_usc_m1)
# car::Anova(update(ltx_gr_usc_m1, . ~ (1|Block) + (1|Fam_uniq) + Urb_score))



## ------------------------------------------------------------
# ltx_urbsubs_city_m1 <- glmmTMB(Latex_weight_mg ~ (1|Block) + (1|Population/Family) + City_dist * Transect_ID,
#                         data = latex %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# 
# performance::check_model(ltx_urbsubs_city_m1) # try sqrt
# 
# performance::check_model(update(ltx_urbsubs_city_m1, Latex_weight_mg^(1/2) ~ .)) # looks better
# 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(ltx_urbsubs_city_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population:Fam_uniq) + City_dist* Transect_ID)) 
# 
# # there is a diff (qualitatively identical though). So use Pop/Fam
# ## interaxn not sig, so use type II SS
# car::Anova(update(ltx_urbsubs_city_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + City_dist* Transect_ID), type = "III")
# car::Anova(update(ltx_urbsubs_city_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Fam_uniq) + City_dist* Transect_ID), type = "III")
# 
# ## interaxn not sig, so use type II SS
# car::Anova(update(ltx_urbsubs_city_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + City_dist* Transect_ID), type = "II")
# car::Anova(update(ltx_urbsubs_city_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Fam_uniq) + City_dist* Transect_ID), type = "II")

# final model
ltx_urbsubs_city_m1 <- glmmTMB(Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + City_dist * Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)



# MAIN EFFECTS MODEL
# ltx_urbsubs_city_m2 <- glmmTMB(Latex_weight_mg ~ (1|Block) + (1|Population/Family) + City_dist + Transect_ID,
#                         data = latex %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# 
# performance::check_model(ltx_urbsubs_city_m2) # try sqrt again
# 
# performance::check_model(update(ltx_urbsubs_city_m2, Latex_weight_mg^(1/2) ~ .)) # looks better
# 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(ltx_urbsubs_city_m2, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population:Fam_uniq) + City_dist + Transect_ID)) 
# 
# # there is a diff (qualitatively identical though). So use Pop/Fam
# car::Anova(update(ltx_urbsubs_city_m2, Latex_weight_mg^(1/2) ~ .), type = "II")
# car::Anova(update(ltx_urbsubs_city_m2, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Fam_uniq) + City_dist + Transect_ID), type = "II")


# final ME model
ltx_urbsubs_city_m2 <- glmmTMB(Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + City_dist + Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# AIC(ltx_urbsubs_city_m1, ltx_urbsubs_city_m2) # m2 best but <2 AIC away


## ------------------------------------------------------------
# ltx_urbsubs_usc_m1 <- glmmTMB(Latex_weight_mg ~ (1|Block) + (1|Population/Family) + Urb_score * Transect_ID,
#                         data = latex %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# 
# performance::check_model(ltx_urbsubs_usc_m1) # try sqrt
# 
# performance::check_model(update(ltx_urbsubs_usc_m1, Latex_weight_mg^(1/2) ~ .)) # looks better
# 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(ltx_urbsubs_usc_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score* Transect_ID)) 
# 
# # there is a diff (qualitatively identical though). So use Pop/Fam
# ## interaxn sig for both
# car::Anova(update(ltx_urbsubs_usc_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + Urb_score* Transect_ID), type = "III")
# car::Anova(update(ltx_urbsubs_usc_m1, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Fam_uniq) + Urb_score* Transect_ID), type = "III")


# final model
ltx_urbsubs_usc_m1 <- glmmTMB(Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + Urb_score * Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)



# MAIN EFFECTS MODEL
# ltx_urbsubs_usc_m2 <- glmmTMB(Latex_weight_mg ~ (1|Block) + (1|Population/Family) + Urb_score + Transect_ID,
#                         data = latex %>%
#                           dplyr::filter(Transect_ID != "Rural"),
#                         REML = F)
# 
# performance::check_model(ltx_urbsubs_usc_m2) # try sqrt again
# 
# performance::check_model(update(ltx_urbsubs_usc_m2, Latex_weight_mg^(1/2) ~ .)) # looks better
# 
# 
# # use fam_uniq, see if anything changes
# performance::check_model(update(ltx_urbsubs_usc_m2, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population:Fam_uniq) + Urb_score + Transect_ID)) 
# 
# # there is a diff (qualitatively identical though). So use Pop/Fam
# car::Anova(update(ltx_urbsubs_usc_m2, Latex_weight_mg^(1/2) ~ .), type = "II")
# car::Anova(update(ltx_urbsubs_usc_m2, Latex_weight_mg^(1/2) ~ (1|Block) + (1|Fam_uniq) + Urb_score + Transect_ID), type = "II")


# final ME model
ltx_urbsubs_usc_m2 <- glmmTMB(Latex_weight_mg^(1/2) ~ (1|Block) + (1|Population/Family) + Urb_score + Transect_ID,
                        data = latex %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# AIC(ltx_urbsubs_usc_m1, ltx_urbsubs_usc_m2) # m1 best


## ------------------------------------------------------------
# herbiv_e_gr_dist_m1 <- glmmTMB(Herbivory_mean_early ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = herbivory,
#                               REML = F) # convergence issue
# 
# performance::check_model(herbiv_e_gr_dist_m1) # very right-skewed. try sqrt transformation
# 
# performance::check_model(update(herbiv_e_gr_dist_m1, sqrt(Herbivory_mean_early) ~ .)) # better but still very right-skewed. try cube root transformation
# 
# performance::check_model(update(herbiv_e_gr_dist_m1, (Herbivory_mean_early)^(1/3) ~ .)) # better but still skewed. Is this good enough?
# 
# 
# hist(herbivory$Herbivory_mean_early, breaks = 50) # zero-inflated?
# 
# 
# performance::check_model(
#   glmmTMB(Herbivory_mean_early^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = herbivory,
#           ziformula = ~1,
#                               REML = F)
# ) # still some heavy tails but looks better
# 
# # doesn't work
# car::Anova(
#   glmmTMB(Herbivory_mean_early^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = herbivory,
#           ziformula = ~1,
#                               REML = F)
# ) 
# 
# performance::check_model(
#   glmmTMB(Herbivory_mean_early^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = herbivory %>%
#             dplyr::filter(Year != "2021"),
#           ziformula = ~1,
#                               REML = F)
# ) # 2020 looks better than 2021
# 
# 
# # just curious- take out 0s
# performance::check_model(
#   glmmTMB(Herbivory_mean_early^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = herbivory %>%
#             dplyr::filter(Year != "2021" & Herbivory_mean_early > 0),
#           ziformula = ~1,
#                               REML = F)
# ) # looks better...
# 
# 
# # create bins for herbivory: Low, medium, high
# herbivory %<>%
#   dplyr::mutate(early_quantiles = cut(Herbivory_mean_early,
#                                breaks=c(0, 0.05, 0.2, 0.4, 1),
#                               labels=c("very_low", "low","middle","high")))
# 
# performance::check_model(
#   glmmTMB(early_levels ~ (1|Year) + (1|Block) + (1|Population/Family) + City_dist,
#                               data = herbivory,
#                                REML = F)) # very bimodal residuals
# 
# 
# 
# 
# # TRY BETA FAMILY
herbivory$Herbivory_mean_early_recode <- herbivory$Herbivory_mean_early
herbivory$Herbivory_mean_early_recode[herbivory$Herbivory_mean_early_recode == 1] <- 0.999999
herbivory$Herbivory_mean_early_recode[herbivory$Herbivory_mean_early_recode == 0] <- 0.000001

# test1 <- glmmTMB(Herbivory_mean_early_recode  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                                 data = herbivory,
#                                 family = beta_family(link="logit"),
#                                REML = F)
# performance::check_model(test1) # heavy right tail. Try sqrt transformation
# 
# performance::check_model(update(test1, sqrt(Herbivory_mean_early_recode) ~ .)) # better. Try cube root transformation
# 
# performance::check_model(update(test1, (Herbivory_mean_early_recode)^(1/3) ~ .)) # better. Try log transformation
# 
# performance::check_model(update(test1, log(Herbivory_mean_early_recode + 1) ~ .)) # worse
# 
# # still not converging though. Make Pop/Fam Fam_uniq
# performance::check_model(update(test1, (Herbivory_mean_early_recode)^(1/3) ~ (1|Block) + (1|Year) + (1|Population:Fam_uniq) + City_dist)) # not converging. take out block

# performance::check_model(update(test1, (Herbivory_mean_early_recode)^(1/3) ~  (1|Year) + (1|Population/Family) + City_dist)) # looks good enough

herbiv_e_gr_dist_m1 <- glmmTMB(Herbivory_mean_early_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + City_dist,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# car::Anova(herbiv_e_gr_dist_m1)


## ------------------------------------------------------------
# herbiv_e_gr_usc_m1 <- glmmTMB(Herbivory_mean_early ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
#                               data = herbivory,
#                               REML = F) 
# 
# performance::check_model(herbiv_e_gr_usc_m1) # very right-skewed. try sqrt transformation
# 
# performance::check_model(update(herbiv_e_gr_usc_m1, sqrt(Herbivory_mean_early) ~ .)) # better but still very right-skewed. try cube root transformation
# 
# performance::check_model(update(herbiv_e_gr_usc_m1, (Herbivory_mean_early)^(1/3) ~ .)) # better but still skewed. Is this good enough?
# 
# 
# performance::check_model(
#   glmmTMB(Herbivory_mean_early^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
#                               data = herbivory,
#           ziformula = ~1,
#                               REML = F)
# ) # still some heavy tails but looks better
# 
# 
# 
# # try bins
# performance::check_model(
#   glmmTMB(early_levels ~ (1|Year) + (1|Block) + (1|Population/Family) + Urb_score,
#                               data = herbivory,
#                                REML = F)) # very bimodal residuals
# 
# 
# 
# 
# # TRY BETA FAMILY
# test1 <- glmmTMB(Herbivory_mean_early_recode  ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
#                                 data = herbivory,
#                                 family = beta_family(link="logit"),
#                                REML = F)
# performance::check_model(test1) # heavy right tail. Try sqrt transformation
# 
# performance::check_model(update(test1, sqrt(Herbivory_mean_early_recode) ~ .)) # better. Try cube root transformation
# 
# performance::check_model(update(test1, (Herbivory_mean_early_recode)^(1/3) ~ .)) # better. But not converging. Try log transformation
# 
# performance::check_model(update(test1, log(Herbivory_mean_early_recode + 1) ~ .)) # worse
# 
# # still not converging though. Make Pop/Fam Fam_uniq
# performance::check_model(update(test1, (Herbivory_mean_early_recode)^(1/3) ~ (1|Block) + (1|Year) + (1|Population:Fam_uniq) + Urb_score)) # looks good enough

herbiv_e_gr_usc_m1 <- glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population:Fam_uniq) + Urb_score,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# car::Anova(herbiv_e_gr_usc_m1)


## ------------------------------------------------------------
# herbiv_e_urbsubs_dist_m1 <- glmmTMB(Herbivory_mean_early ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural"),
#                               REML = F) # convergence issue. Try taking out block
# 
# 
# performance::check_model(glmmTMB(Herbivory_mean_early ~  (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural")),
#                               REML = F) # very right-skewed. try sqrt transformation
# 
# 
# performance::check_model(glmmTMB(Herbivory_mean_early^(1/2) ~  (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural")),
#                               REML = F) # better but still very right-skewed. try cube root transformation
# 
# 
# performance::check_model(glmmTMB(Herbivory_mean_early^(1/3) ~  (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural")),
#                               REML = F) # better. Put back block, see if it runs
# 
# performance::check_model(glmmTMB(Herbivory_mean_early^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural")),
#                               REML = F) # doesn't run
# 
# 
# # Compare w/beta distribution
# performance::check_model(glmmTMB(Herbivory_mean_early_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                                 data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural"),
#                                 family = beta_family(link="logit"),
#                                REML = F)) # looks about the same. Add back block
# 
# performance::check_model(glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                                 data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural"),
#                                 family = beta_family(link="logit"),
#                                REML = F)) # looks better. Make pop/fam Pop:Fam_uniq
# 
# performance::check_model(glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population:Fam_uniq) + City_dist * Transect_ID,
#                                 data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural"),
#                                 family = beta_family(link="logit"),
#                                REML = F))
# 
# 
# 
# # compare both
# car::Anova(glmmTMB(Herbivory_mean_early^(1/3) ~  (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural"),
#                               REML = F),
#            type = "III")
# 
# # going to use this one because it includes block and beta distribution is better than gaussian I think
# car::Anova(
#   glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                                 data = herbivory %>%
#                                 dplyr::filter(Transect_ID != "Rural"),
#                                 family = beta_family(link="logit"),
#                                REML = F),
#            type = "III") # interaxn marg sig, so will keep type III SS

# final model
herbiv_e_urbsubs_dist_m1 <- glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
                                data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                                family = beta_family(link="logit"),
                                REML = F)





# MAIN EFFECTS MODEL
herbiv_e_urbsubs_dist_m2 <- glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist + Transect_ID,
                                data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                                family = beta_family(link="logit"),
                                REML = F)

# performance::check_model(herbiv_e_urbsubs_dist_m2) # looks fine
# 
# AIC(herbiv_e_urbsubs_dist_m1, herbiv_e_urbsubs_dist_m2) # m1 better but <2 AIC apart


## ------------------------------------------------------------
herbiv_e_urbsubs_usc_m1 <- glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score * Transect_ID,
                                data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                                family = beta_family(link="logit"),
                                REML = F)

# performance::check_model(herbiv_e_urbsubs_usc_m1) # looks good



# MAIN EFFECTS MODEL
herbiv_e_urbsubs_usc_m2 <- glmmTMB(Herbivory_mean_early_recode^(1/3)  ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score + Transect_ID,
                                data = herbivory %>%
                                dplyr::filter(Transect_ID != "Rural"),
                                family = beta_family(link="logit"),
                                REML = F)

# performance::check_model(herbiv_e_urbsubs_usc_m2) # looks good
# 
# 
# AIC(herbiv_e_urbsubs_usc_m1, herbiv_e_urbsubs_usc_m2) # m2 better but <2 AIC from m1


## ------------------------------------------------------------
# herbiv_l_gr_dist_m1 <- glmmTMB(Herbivory_mean_late ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = herbivory,
#                               REML = F) # convergence issue
# 
# performance::check_model(herbiv_l_gr_dist_m1) # very right-skewed. try cube root transformation
# 
# performance::check_model(update(herbiv_l_gr_dist_m1, Herbivory_mean_late^(1/3) ~ .)) # still not converging and I think beta is better choice
# 

# BETA FAMILY
herbivory$Herbivory_mean_late_recode <- herbivory$Herbivory_mean_late
herbivory$Herbivory_mean_late_recode[herbivory$Herbivory_mean_late_recode == 1] <- 0.999999
herbivory$Herbivory_mean_late_recode[herbivory$Herbivory_mean_late_recode == 0] <- 0.000001
# 
# test1 <- glmmTMB(Herbivory_mean_late_recode  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                                 data = herbivory,
#                                 family = beta_family(link="logit"),
#                                REML = F)
# performance::check_model(test1) # heavy right tail. Try sqrt transformation
# 
# performance::check_model(update(test1, sqrt(Herbivory_mean_late_recode) ~ .)) # better. Try cube root transformation
# 
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~ .)) # better. Try log transformation
# 
# performance::check_model(update(test1, log(Herbivory_mean_late_recode + 1) ~ .)) # worse
# 
# # still not converging though. Make Pop/Fam Fam_uniq
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~ (1|Block) + (1|Year) + (1|Population:Fam_uniq) + City_dist))  # works, but try taking out block
# 
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~  (1|Year) + (1|Population/Family) + City_dist)) # looks good enough

herbiv_l_gr_dist_m1 <- glmmTMB(Herbivory_mean_late_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + City_dist,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# car::Anova(herbiv_l_gr_dist_m1)



## ------------------------------------------------------------
# test1 <- glmmTMB(Herbivory_mean_late_recode  ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
#                                 data = herbivory,
#                                 family = beta_family(link="logit"),
#                                 REML = F)
# performance::check_model(test1) # heavy right tail. Try sqrt transformation
# 
# performance::check_model(update(test1, sqrt(Herbivory_mean_late_recode) ~ .)) # better. Try cube root transformation
# 
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~ .)) # better. Try log transformation
# 
# performance::check_model(update(test1, log(Herbivory_mean_late_recode + 1) ~ .)) # worse
# 
# # try Pop/Fam --> Fam_uniq
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~ (1|Block) + (1|Year) + (1|Population:Fam_uniq) + Urb_score))  # works
# 

# this is good enough
herbiv_l_gr_usc_m1 <- glmmTMB(Herbivory_mean_late_recode^(1/3)  ~  (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

car::Anova(herbiv_l_gr_usc_m1)


## ------------------------------------------------------------
# BETA FAMILY
# test1 <- glmmTMB(Herbivory_mean_late_recode  ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                                 data = herbivory %>%
#                    dplyr::filter(Transect_ID != "Rural"),
#                                 family = beta_family(link="logit"),
#                  REML = F) # not converging. Try taking out block
# 
# 
# performance::check_model(update(test1, . ~ . -(1|Block))) # heavy right tail. Try sqrt transformation and put block back in
# 
# performance::check_model(update(test1, sqrt(Herbivory_mean_late_recode) ~ .)) # better. Try cube root transformation
# 
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~ .)) 
# # looks good enough. Go with this one

herbiv_l_urbsubs_dist_m1 <- glmmTMB(Herbivory_mean_late_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# interaxn marg sig, so keep type III SS
car::Anova(herbiv_l_urbsubs_dist_m1, type = "III")



# MAIN EFFECTS MODEL
herbiv_l_urbsubs_dist_m2 <- glmmTMB(Herbivory_mean_late_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + City_dist + Transect_ID,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# performance::check_model(herbiv_l_urbsubs_dist_m2) # looks fine
# 
# car::Anova(herbiv_l_urbsubs_dist_m2)
# 
# AIC(herbiv_l_urbsubs_dist_m1,
#     herbiv_l_urbsubs_dist_m2) # m1 better but <2AIC away


## ------------------------------------------------------------
# BETA FAMILY
test1 <- glmmTMB(Herbivory_mean_late_recode  ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score * Transect_ID,
                                data = herbivory %>%
                   dplyr::filter(Transect_ID != "Rural"),
                                family = beta_family(link="logit"),
                 REML = F)

# performance::check_model(test1) # heavy right tail. Try sqrt transformation 
# 
# performance::check_model(update(test1, sqrt(Herbivory_mean_late_recode) ~ .)) # better. Try cube root transformation
# 
# performance::check_model(update(test1, (Herbivory_mean_late_recode)^(1/3) ~ .)) 
# # looks good enough. Go with this one

herbiv_l_urbsubs_usc_m1 <- glmmTMB(Herbivory_mean_late_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + Urb_score * Transect_ID,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# # interaxn not sig, so use type II SS
# car::Anova(herbiv_l_urbsubs_usc_m1, type = "III")
# car::Anova(herbiv_l_urbsubs_usc_m1, type = "II")


# MAIN EFFECTS MODEL
herbiv_l_urbsubs_usc_m2 <- glmmTMB(Herbivory_mean_late_recode^(1/3)  ~  (1|Year) + (1|Population/Family) + Urb_score + Transect_ID,
                                data = herbivory,
                                family = beta_family(link="logit"),
                                REML = F)

# performance::check_model(herbiv_l_urbsubs_usc_m2) # looks fine
# 
# car::Anova(herbiv_l_urbsubs_usc_m2)
# 
# AIC(herbiv_l_urbsubs_usc_m1,
#     herbiv_l_urbsubs_usc_m2) # m2 better model


## ------------------------------------------------------------
# BINARY MODEL-----
# create binary column
weevil$Scar_binary <- weevil$Scar_length_cm
weevil$Scar_binary[weevil$Scar_binary > 0] <- 1


weev_gr_dist_m1_bin <- glmmTMB(Scar_binary ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
                              data = weevil,
                              family = "binomial"(link = "logit"),
                              REML = F)

performance::check_model(weev_gr_dist_m1_bin)
plot(DHARMa::simulateResiduals(weev_gr_dist_m1_bin)) # looks good



# QUANTITATIVE MODEL-----
# weev_gr_dist_m1_quant <- glmmTMB(Scar_length_cm ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
#                               data = weevil %>%
#                                 dplyr::filter(Scar_length_cm > 0),
#                               REML = F)
# 
# performance::check_model(weev_gr_dist_m1_quant)
# plot(DHARMa::simulateResiduals(weev_gr_dist_m1_quant)) # right-skew. try sqrt
# 
# performance::check_model(update(weev_gr_dist_m1_quant, sqrt(Scar_length_cm) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_gr_dist_m1_quant, sqrt(Scar_length_cm) ~ .))) # better. Try cube root
# 
# 
# performance::check_model(update(weev_gr_dist_m1_quant, (Scar_length_cm)^(1/3) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_gr_dist_m1_quant, (Scar_length_cm)^(1/3) ~ .))) # looks great


# final model
weev_gr_dist_m1_quant <- glmmTMB(Scar_length_cm^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0),
                              REML = F)

weev_gr_dist_m1_quant %>% car::Anova()


## ------------------------------------------------------------
# BINARY MODEL-----
weev_gr_usc_m1_bin <- glmmTMB(Scar_binary ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
                              data = weevil,
                              family = "binomial"(link = "logit"),
                              REML = F)

performance::check_model(weev_gr_usc_m1_bin)
plot(DHARMa::simulateResiduals(weev_gr_usc_m1_bin)) # looks good



# QUANTITATIVE MODEL-----
# weev_gr_usc_m1_quant <- glmmTMB(Scar_length_cm ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
#                               data = weevil %>%
#                                 dplyr::filter(Scar_length_cm > 0),
#                               REML = F)
# 
# performance::check_model(weev_gr_usc_m1_quant)
# plot(DHARMa::simulateResiduals(weev_gr_usc_m1_quant)) # right-skew. try sqrt
# 
# performance::check_model(update(weev_gr_usc_m1_quant, sqrt(Scar_length_cm) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_gr_usc_m1_quant, sqrt(Scar_length_cm) ~ .))) # better. Try cube root
# 
# 
# performance::check_model(update(weev_gr_usc_m1_quant, (Scar_length_cm)^(1/3) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_gr_usc_m1_quant, (Scar_length_cm)^(1/3) ~ .))) # looks great


# final model
weev_gr_usc_m1_quant <- glmmTMB(Scar_length_cm^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0),
                              REML = F)

weev_gr_usc_m1_quant %>% car::Anova()


## ------------------------------------------------------------
# BINARY MODEL-----
weev_urbsubs_dist_m1_bin <- glmmTMB(Scar_binary ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_urbsubs_dist_m1_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m1_bin)) # looks good

# MAIN EFFECTS
weev_urbsubs_dist_m2_bin <- glmmTMB(Scar_binary ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)

# performance::check_model(weev_urbsubs_dist_m2_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m2_bin)) # looks good
# 
# AIC(weev_urbsubs_dist_m1_bin, weev_urbsubs_dist_m2_bin) # m2 better but <2AIC away




# QUANTITATIVE MODEL-----
# weev_urbsubs_dist_m1_quant <- glmmTMB(Scar_length_cm ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
#                               data = weevil %>%
#                                 dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
#                               REML = F)
# 
# performance::check_model(weev_urbsubs_dist_m1_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m1_quant)) # right-skew. try sqrt
# 
# performance::check_model(update(weev_urbsubs_dist_m1_quant, sqrt(Scar_length_cm) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_urbsubs_dist_m1_quant, sqrt(Scar_length_cm) ~ .))) # better. Try cube root
# 
# 
# performance::check_model(update(weev_urbsubs_dist_m1_quant, (Scar_length_cm)^(1/3) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_urbsubs_dist_m1_quant, (Scar_length_cm)^(1/3) ~ .))) # looks great


# final model
weev_urbsubs_dist_m1_quant <- glmmTMB(Scar_length_cm^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# No interaxns sig, so use type II SS
car::Anova(weev_urbsubs_dist_m1_quant, type = "III")
car::Anova(weev_urbsubs_dist_m1_quant, type = "II")



# MAIN EFFECTS MODEL
weev_urbsubs_dist_m2_quant <- glmmTMB(Scar_length_cm^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + City_dist + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)
# 
# performance::check_model(weev_urbsubs_dist_m2_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_dist_m2_quant)) # looks great
# 
# 
# AIC(weev_urbsubs_dist_m1_quant, weev_urbsubs_dist_m2_quant) # m2 best but <2 AIC away


## ------------------------------------------------------------
# BINARY MODEL-----
weev_urbsubs_usc_m1_bin <- glmmTMB(Scar_binary ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)
# 
# performance::check_model(weev_urbsubs_usc_m1_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m1_bin)) # looks good

# MAIN EFFECTS
weev_urbsubs_usc_m2_bin <- glmmTMB(Scar_binary ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Transect_ID != "Rural"),
                              family = "binomial"(link = "logit"),
                              REML = F)
# 
# performance::check_model(weev_urbsubs_usc_m2_bin)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m2_bin)) # looks good
# 
# AIC(weev_urbsubs_usc_m1_bin, weev_urbsubs_usc_m2_bin) # m2 better but <2AIC away




# QUANTITATIVE MODEL-----
# weev_urbsubs_usc_m1_quant <- glmmTMB(Scar_length_cm ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score * Transect_ID,
#                               data = weevil %>%
#                                 dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
#                               REML = F)
# 
# performance::check_model(weev_urbsubs_usc_m1_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m1_quant)) # right-skew. try sqrt
# 
# performance::check_model(update(weev_urbsubs_usc_m1_quant, sqrt(Scar_length_cm) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_urbsubs_usc_m1_quant, sqrt(Scar_length_cm) ~ .))) # better. Try cube root
# 
# 
# performance::check_model(update(weev_urbsubs_usc_m1_quant, (Scar_length_cm)^(1/3) ~ .))
# plot(DHARMa::simulateResiduals(update(weev_urbsubs_usc_m1_quant, (Scar_length_cm)^(1/3) ~ .))) # looks great


# final model
weev_urbsubs_usc_m1_quant <- glmmTMB(Scar_length_cm^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score * Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# No interaxns sig, so use type II SS
# car::Anova(weev_urbsubs_usc_m1_quant, type = "III")
# car::Anova(weev_urbsubs_usc_m1_quant, type = "II")



# MAIN EFFECTS MODEL
weev_urbsubs_usc_m2_quant <- glmmTMB(Scar_length_cm^(1/3) ~ (1|Block) + (1|Year) + (1|Population/Family) + Urb_score + Transect_ID,
                              data = weevil %>%
                                dplyr::filter(Scar_length_cm > 0 & Transect_ID != "Rural"),
                              REML = F)

# performance::check_model(weev_urbsubs_usc_m2_quant)
# plot(DHARMa::simulateResiduals(weev_urbsubs_usc_m2_quant)) # looks great
# 
# 
# AIC(weev_urbsubs_usc_m1_quant, weev_urbsubs_usc_m2_quant) # m2 best but <2 AIC away



## ------------------------------------------------------------
ldmc_mods <- list(

## City_dist / gradient
ldmc_gr_city_m1 ,

## Urbanization score / gradient
ldmc_gr_usc_m1 ,

## City_dist / urban subtransects
ldmc_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
ldmc_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
ldmc_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
ldmc_urbsubs_usc_m2 # Best model

)


names(ldmc_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## ------------------------------------------------------------
sla_mods <- list(

## City_dist / gradient
sla_gr_city_m1 ,

## Urbanization score / gradient
sla_gr_usc_m1,

## City_dist / urban subtransects
sla_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
sla_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
sla_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
sla_urbsubs_usc_m2 # Best model

)


names(sla_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## ------------------------------------------------------------
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


## ------------------------------------------------------------
herb_early_mods <- list(

## City_dist / gradient
herbiv_e_gr_dist_m1 ,

## Urbanization score / gradient
herbiv_e_gr_usc_m1,

## City_dist / urban subtransects
herbiv_e_urbsubs_dist_m1, # Best model
herbiv_e_urbsubs_dist_m2, # Qualitatively identical model (<2 AIC away)

## Urbanization score  / urban subtransects
herbiv_e_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
herbiv_e_urbsubs_usc_m2 # Best model
)

names(herb_early_mods) <- c("City_gr",
                            "Usc_gr",
                            "City_urbsubs_best",
                            "City_urbsubs_alt",
                            "Usc_urbsubs_alt",
                            "Usc_urbsubs_best")


## ------------------------------------------------------------
herb_late_mods <- list(

## City_dist / gradient
herbiv_l_gr_dist_m1 ,

## Urbanization score / gradient
herbiv_l_gr_usc_m1,

## City_dist / urban subtransects
herbiv_l_urbsubs_dist_m1, # Best model
herbiv_l_urbsubs_dist_m2, # Qualitatively identical model (<2 AIC away)

## Urbanization score  / urban subtransects
herbiv_l_urbsubs_usc_m2 # Best model
)

names(herb_late_mods) <- c("City_gr",
                            "Usc_gr",
                            "City_urbsubs_best",
                            "City_urbsubs_alt",
                            "Usc_urbsubs")


## ------------------------------------------------------------
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

