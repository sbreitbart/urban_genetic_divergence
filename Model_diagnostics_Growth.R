## -----------------------------------------------------------------------------
source("libraries.R")
source("functions.R")


## -----------------------------------------------------------------------------
# SLA & LDMC-----
sla_ldmc <- read.csv(here::here("./CommonGardenExperiment_2020Data/clean_data/2020_sla_ldmc_clean.csv")) %>%
  dplyr::select(., -c(1:2)) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block")), as.factor) %>%
  dplyr::mutate(Fam_uniq = paste0(Population, "_", Family))


heights <- read.csv(here::here("./Joined_annual_data/heights.csv"), na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -1) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.factor) %>%
  dplyr::mutate_at(vars(c("rel_growth_rate")), as.numeric) %>%
  dplyr::mutate(Fam_uniq = paste0(Population, "_", Family))

str(heights)



survival <- read.csv(here::here("./Joined_annual_data/survival.csv"), na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -1) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Transect_ID", "Urb_Rur")), as.factor)%>%
  dplyr::mutate(Fam_uniq = as.factor(paste0(Population, "_", Family)))

str(survival)



## -----------------------------------------------------------------------------
# # Basic data exploration
# plot(LDMC ~ City_dist, data = sla_ldmc) # outlier > 1.5? It's biologically possible though
# plot(LDMC ~ City_dist, data = sla_ldmc %>%
#           dplyr::filter(LDMC < 1))
# 
# boxplot(LDMC ~ Block, data = sla_ldmc)
# boxplot(LDMC ~ Block, data = sla_ldmc %>%
#           dplyr::filter(LDMC < 1))

# doesn't converge if log or cube root transformed
ldmc_gr_city_m1 <- glmmTMB(sqrt(LDMC) ~
                             Block +
                             (1|Population/Family) +
                             City_dist,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1), # next highest LMDC is 0.7, so removing the one more than double that
                        REML = F)

# DHARMa::simulateResiduals(ldmc_gr_city_m1) %>%
#   plot
# performance::check_model(ldmc_gr_city_m1)



# test for gxe intxns
gxe_test_LDMC1 <- lm(log(LDMC) ~
                             Block*Population ,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1))


plot(gxe_test_LDMC1)
car::Anova(gxe_test_LDMC1) # p >> 0.1


gxe_test_LDMC2 <- lm(log(LDMC) ~
                             Block*Fam_uniq ,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1))


plot(gxe_test_LDMC2)
car::Anova(gxe_test_LDMC2) # p >> 0.1


## -----------------------------------------------------------------------------
ldmc_gr_usc_m1 <- glmmTMB(sqrt(LDMC) ~
                             Block +
                             (1|Population/Family) +
                             Urb_score,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1), # next highest LMDC is 0.7, so removing the one more than double that
                        REML = F)

# DHARMa::simulateResiduals(ldmc_gr_usc_m1) %>%
#   plot
# performance::check_model(ldmc_gr_usc_m1)


## -----------------------------------------------------------------------------
# doesn't converge unless I remove block
ldmc_urbsubs_city_m1 <- glmmTMB(log(LDMC) ~
                             # Block +
                             (1|Population/Family) +
                             City_dist * Transect_ID,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1 &
                                             Transect_ID != "Rural"), # next highest LMDC is 0.7, so removing the one more than double that
                        REML = F)

# DHARMa::simulateResiduals(ldmc_urbsubs_city_m1) %>%
#   plot
# performance::check_model(ldmc_urbsubs_city_m1)



# MAIN EFFECTS MODEL
ldmc_urbsubs_city_m2 <- glmmTMB(log(LDMC) ~
                             # Block +
                             (1|Population/Family) +
                             City_dist + Transect_ID,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1 &
                                             Transect_ID != "Rural"), # next highest LMDC is 0.7, so removing the one more than double that
                        REML = F)
# DHARMa::simulateResiduals(ldmc_urbsubs_city_m2) %>%
#   plot
# 
# performance::check_model(ldmc_urbsubs_city_m2)
# car::Anova(ldmc_urbsubs_city_m2)
# 
# # COMPARE AIC
# AIC(ldmc_urbsubs_city_m1, ldmc_urbsubs_city_m2) # m2 lower but not by more than 2 AIC


## -----------------------------------------------------------------------------
ldmc_urbsubs_usc_m1 <- glmmTMB(log(LDMC) ~
                             Block +
                             (1|Population/Family) +
                             Urb_score * Transect_ID,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1 &
                                             Transect_ID != "Rural"), # next highest LMDC is 0.7, so removing the one more than double that
                        REML = F)

# DHARMa::simulateResiduals(ldmc_urbsubs_usc_m1) %>%
#   plot
# performance::check_model(ldmc_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
ldmc_urbsubs_usc_m2 <- glmmTMB(log(LDMC) ~
                             Block +
                             (1|Population/Family) +
                             Urb_score + Transect_ID,
                        data = sla_ldmc %>%
                             dplyr::filter(LDMC < 1 &
                                             Transect_ID != "Rural"), # next highest LMDC is 0.7, so removing the one more than double that
                        REML = F)
# DHARMa::simulateResiduals(ldmc_urbsubs_usc_m2) %>%
#   plot
# performance::check_model(ldmc_urbsubs_usc_m2)
# AIC(ldmc_urbsubs_usc_m1, ldmc_urbsubs_usc_m2) # m2 lower but not by more than 2 AIC


## -----------------------------------------------------------------------------
# Basic data exploration
plot(SLA ~ City_dist, data = sla_ldmc) 

boxplot(SLA ~ Block, data = sla_ldmc)

# doesn't converge if I transform SLA to the extent that diagnostics look spotless (only if I remove block). I can also remove most outliers, but then sometimes it doesn't converge either. But if I remove some outliers (only use SLA < 40), anova results still qualitatitvley identical to other situations noted above. So I'll remove Block and transform SLA so I can compare with Urb_score model.
sla_gr_city_m1 <- glmmTMB(SLA^(1/3) ~
                            # Block +
                            (1|Population/Family) +
                            City_dist,
                        data = sla_ldmc,
                        REML = F)

# performance::check_model(sla_gr_city_m1)
# plot(DHARMa::simulateResiduals(sla_gr_city_m1))
# car::Anova(sla_gr_city_m1)


## -----------------------------------------------------------------------------
sla_gr_usc_m1 <- glmmTMB(SLA^(1/3) ~
                           # Block +
                            (1|Population/Family) +
                            Urb_score,
                        data = sla_ldmc,
                        REML = F)

# performance::check_model(sla_gr_usc_m1)
# plot(DHARMa::simulateResiduals(sla_gr_usc_m1))
# car::Anova(sla_gr_usc_m1)


## -----------------------------------------------------------------------------
sla_urbsubs_city_m1 <- glmmTMB(SLA^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(sla_urbsubs_city_m1)
# plot(DHARMa::simulateResiduals(sla_urbsubs_city_m1))
# car::Anova(sla_urbsubs_city_m1)


# MAIN EFFECTS
sla_urbsubs_city_m2 <- glmmTMB(SLA^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(sla_urbsubs_city_m2)
# plot(DHARMa::simulateResiduals(sla_urbsubs_city_m2))
# car::Anova(sla_urbsubs_city_m2)
# AIC(sla_urbsubs_city_m1, sla_urbsubs_city_m2) # qualitatively identical but m2 best model


## -----------------------------------------------------------------------------
sla_urbsubs_usc_m1 <- glmmTMB(SLA^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(sla_urbsubs_usc_m1)
# plot(DHARMa::simulateResiduals(sla_urbsubs_usc_m1))
# car::Anova(sla_urbsubs_usc_m1)


# MAIN EFFECTS
sla_urbsubs_usc_m2 <- glmmTMB(SLA^(1/3) ~
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = sla_ldmc %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# performance::check_model(sla_urbsubs_usc_m2)
# plot(DHARMa::simulateResiduals(sla_urbsubs_usc_m2))
# car::Anova(sla_urbsubs_usc_m2)
# AIC(sla_urbsubs_usc_m1, sla_urbsubs_usc_m2) # qualitatively identical but m2 best model


## -----------------------------------------------------------------------------
# hist(heights$Total_Height_early, breaks = 80) # zero-inflated?
# 
# boxplot(Total_Height_early ~ Year, data = heights)


height_e_gr_city_m1 <- glmmTMB(Total_Height_early^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = heights,
                        REML = F)


# plot(DHARMa::simulateResiduals(height_e_gr_city_m1))
# performance::check_model(height_e_gr_city_m1)
# car::Anova(height_e_gr_city_m1)


## -----------------------------------------------------------------------------
height_e_gr_usc_m1 <- glmmTMB(Total_Height_early^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = heights,
                        REML = F)


# plot(DHARMa::simulateResiduals(height_e_gr_usc_m1))
# performance::check_model(height_e_gr_usc_m1)
# car::Anova(height_e_gr_usc_m1)


## -----------------------------------------------------------------------------
# year is adding a lot of variation and complicates model fit. When I treat it as random, model fits MUCH better as per diagnostics, yet anova shows nearly identical results for city_dist and transect_ID so I'll keep it fixed.
height_e_urbsubs_city_m1 <- glmmTMB(Total_Height_early^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_e_urbsubs_city_m1))
# performance::check_model(height_e_urbsubs_city_m1)
# car::Anova(height_e_urbsubs_city_m1)


# MAIN EFFECTS MODEL
height_e_urbsubs_city_m2 <- glmmTMB(Total_Height_early^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_e_urbsubs_city_m2)) 
# performance::check_model(height_e_urbsubs_city_m2)
# car::Anova(height_e_urbsubs_city_m2)
# AIC(height_e_urbsubs_city_m1, height_e_urbsubs_city_m2) # m2 better but <2 AIC from full


## -----------------------------------------------------------------------------
height_e_urbsubs_usc_m1 <- glmmTMB(Total_Height_early^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)


# plot(DHARMa::simulateResiduals(height_e_urbsubs_usc_m1)) 
# performance::check_model(height_e_urbsubs_usc_m1)
# car::Anova(height_e_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
height_e_urbsubs_usc_m2 <- glmmTMB(Total_Height_early^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_e_urbsubs_usc_m2)) 
# performance::check_model(height_e_urbsubs_usc_m2)
# AIC(height_e_urbsubs_usc_m1, height_e_urbsubs_usc_m2) # m2 better but <2 AIC from full


## -----------------------------------------------------------------------------
height_l_gr_city_m1 <- glmmTMB(Total_Height_late^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = heights,
                        REML = F)

# plot(DHARMa::simulateResiduals(height_l_gr_city_m1)) 
# performance::check_model(height_l_gr_city_m1)
# car::Anova(height_l_gr_city_m1)


## -----------------------------------------------------------------------------
height_l_gr_usc_m1 <- glmmTMB(Total_Height_late^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = heights,
                        REML = F)

# plot(DHARMa::simulateResiduals(height_l_gr_usc_m1)) 
# performance::check_model(height_l_gr_usc_m1)



## -----------------------------------------------------------------------------
# again, model fit isn't perfect but it's due to year being so variable. When it's taken out/turned into random effect, diagnostics looks fine and anova result for city_dist nearly identical.
height_l_urbsubs_city_m1 <- glmmTMB(Total_Height_late^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_l_urbsubs_city_m1)) 
# performance::check_model(height_l_urbsubs_city_m1)
# car::Anova(height_l_urbsubs_city_m1)


# MAIN EFFECTS MODEL
height_l_urbsubs_city_m2 <- glmmTMB(Total_Height_late^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_l_urbsubs_city_m2)) 
# performance::check_model(height_l_urbsubs_city_m2)
# AIC(height_l_urbsubs_city_m1, height_l_urbsubs_city_m2) # m2 better but <2 AIC from full


## -----------------------------------------------------------------------------
height_l_urbsubs_usc_m1 <- glmmTMB(Total_Height_late^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_l_urbsubs_usc_m1)) 
# performance::check_model(height_l_urbsubs_usc_m1)



# MAIN EFFECTS MODEL
height_l_urbsubs_usc_m2 <- glmmTMB(Total_Height_late^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)

# plot(DHARMa::simulateResiduals(height_l_urbsubs_usc_m2)) 
# performance::check_model(height_l_urbsubs_usc_m2)
# AIC(height_l_urbsubs_usc_m1, height_l_urbsubs_usc_m2) # m2 better but <2 AIC from full


## -----------------------------------------------------------------------------
# hist(heights$rel_growth_rate, breaks = 80) 
# boxplot(rel_growth_rate ~ Year, data = heights)

rgr_gr_city_m1 <- glmmTMB(rel_growth_rate^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = heights,
                        REML = F)


# plot(DHARMa::simulateResiduals(rgr_gr_city_m1))
# performance::check_model(rgr_gr_city_m1)
# car::Anova(rgr_gr_city_m1)


## -----------------------------------------------------------------------------
rgr_gr_usc_m1 <- glmmTMB(rel_growth_rate^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = heights,
                        REML = F)


# plot(DHARMa::simulateResiduals(rgr_gr_usc_m1)) 
# performance::check_model(rgr_gr_usc_m1)
# car::Anova(rgr_gr_usc_m1)


## -----------------------------------------------------------------------------
rgr_urbsubs_city_m1 <- glmmTMB(rel_growth_rate^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)


# plot(DHARMa::simulateResiduals(rgr_urbsubs_city_m1))
# performance::check_model(rgr_urbsubs_city_m1)
# car::Anova(rgr_urbsubs_city_m1)


# MAIN EFFECTS MODEL
rgr_urbsubs_city_m2 <- glmmTMB(rel_growth_rate^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)


# plot(DHARMa::simulateResiduals(rgr_urbsubs_city_m2)) # ???
# performance::check_model(rgr_urbsubs_city_m2)
# AIC(rgr_urbsubs_city_m1, rgr_urbsubs_city_m2) #m2 best but <2 AIC from full


## -----------------------------------------------------------------------------
rgr_urbsubs_usc_m1 <- glmmTMB(rel_growth_rate^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)


# plot(DHARMa::simulateResiduals(rgr_urbsubs_usc_m1))
# performance::check_model(rgr_urbsubs_usc_m1)
# car::Anova(rgr_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
rgr_urbsubs_usc_m2 <- glmmTMB(rel_growth_rate^(1/3) ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        REML = F)


# plot(DHARMa::simulateResiduals(rgr_urbsubs_usc_m2))
# performance::check_model(rgr_urbsubs_usc_m2)
# AIC(rgr_urbsubs_usc_m1, rgr_urbsubs_usc_m2) # m2 best but <2 AIC from m2


## -----------------------------------------------------------------------------
ramets_e_gr_city_m1 <- glmmTMB(Ramets_early ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = heights,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_e_gr_city_m1)) 
# performance::check_model(ramets_e_gr_city_m1)
# car::Anova(ramets_e_gr_city_m1)


## -----------------------------------------------------------------------------
ramets_e_gr_usc_m1 <- glmmTMB(Ramets_early ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = heights,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_e_gr_usc_m1)) 
# performance::check_model(ramets_e_gr_usc_m1)
# car::Anova(ramets_e_gr_usc_m1)


## -----------------------------------------------------------------------------
ramets_e_urbsubs_city_m1 <- glmmTMB(Ramets_early ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_e_urbsubs_city_m1)) 
# performance::check_model(ramets_e_urbsubs_city_m1)


# MAIN EFFECTS MODEL
ramets_e_urbsubs_city_m2 <- glmmTMB(Ramets_early ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)

# plot(DHARMa::simulateResiduals(ramets_e_urbsubs_city_m2)) 
# performance::check_model(ramets_e_urbsubs_city_m2)
# AIC(ramets_e_urbsubs_city_m1, ramets_e_urbsubs_city_m2) # m2 best but <2 AIC from full


## -----------------------------------------------------------------------------
ramets_e_urbsubs_usc_m1 <- glmmTMB(Ramets_early ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_e_urbsubs_usc_m1))
# performance::check_model(ramets_e_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
ramets_e_urbsubs_usc_m2 <- glmmTMB(Ramets_early ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_e_urbsubs_usc_m2)) 
# performance::check_model(ramets_e_urbsubs_usc_m2)
# AIC(ramets_e_urbsubs_usc_m1, ramets_e_urbsubs_usc_m2) # m2 best but <2 AIC from full


## -----------------------------------------------------------------------------
ramets_l_gr_city_m1 <- glmmTMB(Ramets_late ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = heights,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_l_gr_city_m1)) 
# performance::check_model(ramets_l_gr_city_m1)



## -----------------------------------------------------------------------------
ramets_l_gr_usc_m1 <- glmmTMB(Ramets_late ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = heights,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_l_gr_usc_m1)) 
# performance::check_model(ramets_l_gr_usc_m1)



## -----------------------------------------------------------------------------
ramets_l_urbsubs_city_m1 <- glmmTMB(Ramets_late ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_l_urbsubs_city_m1)) 
# performance::check_model(ramets_l_urbsubs_city_m1)


# MAIN EFFECTS MODEL
ramets_l_urbsubs_city_m2 <- glmmTMB(Ramets_late ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_l_urbsubs_city_m2)) 
# performance::check_model(ramets_l_urbsubs_city_m2)
# AIC(ramets_l_urbsubs_city_m1, ramets_l_urbsubs_city_m2) # m2 best but <2 AIC from full


## -----------------------------------------------------------------------------
ramets_l_urbsubs_usc_m1 <- glmmTMB(Ramets_late ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_l_urbsubs_usc_m1))
# performance::check_model(ramets_l_urbsubs_usc_m1)


# MAIN EFFECTS MODEL
ramets_l_urbsubs_usc_m2 <- glmmTMB(Ramets_late ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = heights %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(ramets_l_urbsubs_usc_m2)) 
# performance::check_model(ramets_l_urbsubs_usc_m2)
# AIC(ramets_l_urbsubs_usc_m1, ramets_l_urbsubs_usc_m2) # m2 best but <2 AIC from full


## -----------------------------------------------------------------------------
# Response: number of seasons alive (1-4)-----
# seasons survived until death
survival_seasons <- survival %>%
  mutate(seasons = case_when(
    Dead == 1 & Year == 2019 ~ 1,
    Dead == 1 & Year == 2020 ~ 2,
    Dead == 1 & Year == 2021 ~ 3,
    TRUE ~ 4)) %>%
  group_by(Row, Column, Population, Block, Family, Replicate, Transect_ID, City_dist, Urb_score, Fam_uniq) %>%
  summarise(seasons = first(seasons)) %>%
  mutate(dead = case_when(
    seasons == 4 ~ 0,
    TRUE ~ 1
  ))

# are diagnostics ok?
surv_gr_city_m1_seasons <- glmmTMB(seasons ~ 
                                 Block +
                                 (1|Population/Family) +
                                 City_dist,
                        data = survival_seasons ,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_city_m1_seasons))
# performance::check_model(surv_gr_city_m1_seasons)
# car::Anova(surv_gr_city_m1_seasons)

# Response: 0 or 1, as dead/alive-----
# 2019
surv_gr_city_m1_19 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = survival %>%
                          dplyr::filter(Year == "2019"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_city_m1_19))
# performance::check_model(surv_gr_city_m1_19)
# car::Anova(surv_gr_city_m1_19)


# 2020
surv_gr_city_m1_20 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = survival %>%
                          dplyr::filter(Year == "2020"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_city_m1_20))
# performance::check_model(surv_gr_city_m1_20)
# car::Anova(surv_gr_city_m1_20)


# 2021
surv_gr_city_m1_21 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = survival %>%
                          dplyr::filter(Year == "2021"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_city_m1_21))
# performance::check_model(surv_gr_city_m1_21)
# car::Anova(surv_gr_city_m1_21)


# 2019-2021
surv_gr_city_m1_all <- glmmTMB(Dead ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist,
                        data = survival ,
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_city_m1_all))
# performance::check_model(surv_gr_city_m1_all)
# car::Anova(surv_gr_city_m1_all)


# seasons survived until death
survival_seasons <- survival %>%
  mutate(seasons = case_when(
    Dead == 1 & Year == 2019 ~ 1,
    Dead == 1 & Year == 2020 ~ 2,
    Dead == 1 & Year == 2021 ~ 3,
    TRUE ~ 4)) %>%
  group_by(Row, Column, Population, Block, Family, Replicate, Transect_ID, City_dist, Urb_score, Fam_uniq) %>%
  summarise(seasons = first(seasons)) %>%
  mutate(dead = case_when(
    seasons == 4 ~ 0,
    TRUE ~ 1
  ))

# how to fix diagnostics?
surv_gr_city_m1_seasons <- glmmTMB(seasons ~ 
                                 Block +
                                 (1|Population/Family) +
                                 City_dist,
                        data = survival_seasons ,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_city_m1_seasons))
# performance::check_model(surv_gr_city_m1_seasons)
# car::Anova(surv_gr_city_m1_seasons)



## -----------------------------------------------------------------------------
# Response: number of seasons alive (1-4)-----
# seasons survived until death
# are diagnostics ok?
surv_gr_usc_m1_seasons <- glmmTMB(seasons ~ 
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = survival_seasons ,
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_usc_m1_seasons))
# performance::check_model(surv_gr_usc_m1_seasons)
# car::Anova(surv_gr_usc_m1_seasons)
# Response: 0 or 1, as dead/alive-----
# 2019
surv_gr_usc_m1_19 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = survival %>%
                          dplyr::filter(Year == "2019"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_usc_m1_19))
# performance::check_model(surv_gr_usc_m1_19)
# car::Anova(surv_gr_usc_m1_19)


# 2020
surv_gr_usc_m1_20 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = survival %>%
                          dplyr::filter(Year == "2020"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_usc_m1_20))
# performance::check_model(surv_gr_usc_m1_20)
# car::Anova(surv_gr_usc_m1_20)


# 2021
surv_gr_usc_m1_21 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = survival %>%
                          dplyr::filter(Year == "2021"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_usc_m1_21))
# performance::check_model(surv_gr_usc_m1_21)
# car::Anova(surv_gr_usc_m1_21)


# 2019-2021
surv_gr_usc_m1_all <- glmmTMB(Dead ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score,
                        data = survival ,
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_gr_usc_m1_all))
# performance::check_model(surv_gr_usc_m1_all)
# car::Anova(surv_gr_usc_m1_all)





## -----------------------------------------------------------------------------
# Response: number of seasons alive (1-4)-----
# seasons survived until death
# are diagnostics ok?
## full model
surv_urbsubs_city_m1_seasons <- glmmTMB(seasons ~ 
                             #    Block +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = survival_seasons %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m1_seasons))
# performance::check_model(surv_urbsubs_city_m1_seasons)
# car::Anova(surv_urbsubs_city_m1_seasons)

## ME model
surv_urbsubs_city_m2_seasons <- glmmTMB(seasons ~ 
                            #     Block +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = survival_seasons %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m2_seasons))
# performance::check_model(surv_urbsubs_city_m2_seasons)
# car::Anova(surv_urbsubs_city_m2_seasons)
# AIC(surv_urbsubs_city_m1_seasons,
#     surv_urbsubs_city_m2_seasons) # even though full mod can run with block included (and ME mod can't converge with it included), ME mod is still best model although <2 AIC from full.


# Response: 0 or 1, as dead/alive-----
## 2019
## Full model
surv_urbsubs_city_m1_19 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2019" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m1_19))
# performance::check_model(surv_urbsubs_city_m1_19)
# car::Anova(surv_urbsubs_city_m1_19)

## Main effects model
surv_urbsubs_city_m2_19 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2019" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m2_19))
# performance::check_model(surv_urbsubs_city_m2_19)
# car::Anova(surv_urbsubs_city_m2_19)
# AIC(surv_urbsubs_city_m1_19, surv_urbsubs_city_m2_19) # m1 best but <2 AIC from m2

## 2020
## Full model
surv_urbsubs_city_m1_20 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2020" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m1_20))
# performance::check_model(surv_urbsubs_city_m1_20)
# car::Anova(surv_urbsubs_city_m1_20)

## Main effects model
surv_urbsubs_city_m2_20 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2020" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m2_20))
# performance::check_model(surv_urbsubs_city_m2_20)
# car::Anova(surv_urbsubs_city_m2_20)
# AIC(surv_urbsubs_city_m1_20, surv_urbsubs_city_m2_20) # m1 best but <2 AIC from m2
## 2021
## Full model
surv_urbsubs_city_m1_21 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2021" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m1_21))
# car::Anova(surv_urbsubs_city_m1_21)

## Main effects model
surv_urbsubs_city_m2_21 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2021" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m2_21))
# car::Anova(surv_urbsubs_city_m2_21)
# AIC(surv_urbsubs_city_m1_21, surv_urbsubs_city_m2_21) # m2 best but <2 AIC from m2

## all years
## Full model
surv_urbsubs_city_m1_all <- glmmTMB(Dead ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m1_all))
# car::Anova(surv_urbsubs_city_m1_all)

## Main effects model
surv_urbsubs_city_m2_all <- glmmTMB(Dead ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 City_dist + Transect_ID,
                        data = survival %>%
                          dplyr::filter( Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_city_m2_all))
# car::Anova(surv_urbsubs_city_m2_all)
# AIC(surv_urbsubs_city_m1_all, surv_urbsubs_city_m2_all) # m1 best but <2 AIC from m2




## -----------------------------------------------------------------------------
# Response: number of seasons alive (1-4)-----
# seasons survived until death
# are diagnostics ok?
## full model
surv_urbsubs_usc_m1_seasons <- glmmTMB(seasons ~ 
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = survival_seasons %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m1_seasons))
# performance::check_model(surv_urbsubs_usc_m1_seasons)
# car::Anova(surv_urbsubs_usc_m1_seasons)

## ME model
surv_urbsubs_usc_m2_seasons <- glmmTMB(seasons ~ 
                                 Block +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = survival_seasons %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = poisson,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m2_seasons))
# performance::check_model(surv_urbsubs_usc_m2_seasons)
# car::Anova(surv_urbsubs_usc_m2_seasons)
# AIC(surv_urbsubs_usc_m1_seasons,
#     surv_urbsubs_usc_m2_seasons) # ME mod is best model although <2 AIC from full


# Response: 0 or 1, as dead/alive-----
# 2019
## Full model
surv_urbsubs_usc_m1_19 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2019" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m1_19))
# performance::check_model(surv_urbsubs_usc_m1_19)
# car::Anova(surv_urbsubs_usc_m1_19)

## Main effects model
surv_urbsubs_usc_m2_19 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2019" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m2_19))
# performance::check_model(surv_urbsubs_usc_m2_19)
# car::Anova(surv_urbsubs_usc_m2_19)
# AIC(surv_urbsubs_usc_m1_19, surv_urbsubs_usc_m2_19) # m2 best but <2 AIC from m1

# 2020
## Full model
surv_urbsubs_usc_m1_20 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2020" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m1_20))
# performance::check_model(surv_urbsubs_usc_m1_20)
# car::Anova(surv_urbsubs_usc_m1_20)

## Main effects model
surv_urbsubs_usc_m2_20 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2020" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m2_20))
# performance::check_model(surv_urbsubs_usc_m2_20)
# car::Anova(surv_urbsubs_usc_m2_20)
# AIC(surv_urbsubs_usc_m1_20, surv_urbsubs_usc_m2_20) # m1 best but <2 AIC from m2

# 2021
## Full model
surv_urbsubs_usc_m1_21 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2021" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m1_21))
# car::Anova(surv_urbsubs_usc_m1_21)

## Main effects model
surv_urbsubs_usc_m2_21 <- glmmTMB(Dead ~ 
                                 Block +
                            #     Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = survival %>%
                          dplyr::filter(Year == "2021" & Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m2_21))
# car::Anova(surv_urbsubs_usc_m2_21)
# AIC(surv_urbsubs_usc_m1_21, surv_urbsubs_usc_m2_21) # m2 best but <2 AIC from m2

# all years
## Full model
surv_urbsubs_usc_m1_all <- glmmTMB(Dead ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score * Transect_ID,
                        data = survival %>%
                          dplyr::filter(Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m1_all))
# car::Anova(surv_urbsubs_usc_m1_all)

## Main effects model
surv_urbsubs_usc_m2_all <- glmmTMB(Dead ~ 
                                 Block +
                                 Year +
                                 (1|Population/Family) +
                                 Urb_score + Transect_ID,
                        data = survival %>%
                          dplyr::filter( Transect_ID != "Rural"),
                        family = binomial,
                        REML = F)


# plot(DHARMa::simulateResiduals(surv_urbsubs_usc_m2_all))
# car::Anova(surv_urbsubs_usc_m2_all)
# AIC(surv_urbsubs_usc_m1_all, surv_urbsubs_usc_m2_all) # m1 best but <2 AIC from m2


## -----------------------------------------------------------------------------
# # install.packages("survival")
# library(survival)
# coxph(Surv(seasons, dead) ~
#             #    (1|Population/Family) +
#                 Block + City_dist,
#       data = survival_seasons) %T>% 
#   summary() # doesn't work with random effect

mortality_mod1 <- coxme(Surv(seasons, dead) ~
                (1|Population/Family) +
                Block + City_dist,
      data = survival_seasons) 

# diagnostics
# cox.zph(coxme(Surv(seasons, dead) ~
#                 (1|Population/Family) +
#                 Block + City_dist,
#       data = survival_seasons)) # keep getting this error message: Error in solve.default(imat, u) : Lapack routine dgesv: system is exactly singular: U[5,5] = 0

# sjPlot::plot_model(mortality_mod1)


## -----------------------------------------------------------------------------
# coxme(Surv(seasons, dead) ~
#                 (1|Population/Family) +
#                 Block + Urb_score,
#       data = survival_seasons)


## -----------------------------------------------------------------------------
surv_transects <- droplevels(
  subset(
    survival_seasons,
    !Transect_ID %in% "Rural"))

mortality_mod3 <- coxme(Surv(seasons, dead) ~
        (1|Population/Family) +
        Block +
        City_dist * Transect_ID,
      data = surv_transects) 


# sjPlot::plot_model(mortality_mod3,
#                     vline.color = "grey",
#                    sort.est = TRUE,
#                    show.values = TRUE,
#                    value.offset = .3,
#                    rm.terms = c("Block [3]",
#                                 "Block [2]",
#                                 "Block [4]"),
#                     transform = "plogis")

pop_mort_estimates <- ranef(mortality_mod3)$Population %>%
  as.data.frame()%>%
  tibble::rownames_to_column("Population") %>%
  dplyr::rename(estimate = 2) %>%
  left_join(., surv_transects %>% # add city_dist and other variables
              dplyr::select(c(3, 7:9)) %>%
              dplyr::group_by(Population) %>%
              dplyr::summarise(Population = first(Population),
                               Transect_ID = first(Transect_ID),
                               City_dist = first(City_dist),
                               Urb_score = first(Urb_score)))

# ggplot(pop_mort_estimates) +
#        aes(City_dist, estimate,
#            color = Transect_ID,
#            fill = Transect_ID) +
#          geom_point() +
#   geom_smooth(method = "lm") +
#   theme_minimal()


## -----------------------------------------------------------------------------
mortality_mod4 <- coxme(Surv(seasons, dead) ~
        (1|Population/Family) +
        Block +
        Urb_score * Transect_ID,
      data = surv_transects) 


# sjPlot::plot_model(mortality_mod4,
#                     vline.color = "grey",
#                    sort.est = TRUE,
#                    show.values = TRUE,
#                    value.offset = .3,
#                    rm.terms = c("Block [3]",
#                                 "Block [2]",
#                                 "Block [4]"),
#                     transform = "plogis")


# ggplot(pop_mort_estimates) +
#        aes(Urb_score, estimate,
#            color = Transect_ID,
#            fill = Transect_ID) +
#          geom_point() +
#   geom_smooth(method = "lm") +
#   xlim(4, -2) +
#   theme_minimal()


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
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


## -----------------------------------------------------------------------------
heights_early_mods <- list(

## City_dist / gradient
height_e_gr_city_m1 ,

## Urbanization score / gradient
height_e_gr_usc_m1 ,

## City_dist / urban subtransects
height_e_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
height_e_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
height_e_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
height_e_urbsubs_usc_m2 # Best model

)


names(heights_early_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## -----------------------------------------------------------------------------
heights_late_mods <- list(

## City_dist / gradient
height_l_gr_city_m1 ,

## Urbanization score / gradient
height_l_gr_usc_m1 ,

## City_dist / urban subtransects
height_l_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
height_l_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
height_l_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
height_l_urbsubs_usc_m2 # Best model

)


names(heights_late_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## -----------------------------------------------------------------------------
rgr_mods <- list(

## City_dist / gradient
rgr_gr_city_m1 ,

## Urbanization score / gradient
rgr_gr_usc_m1 ,

## City_dist / urban subtransects
rgr_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
rgr_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
rgr_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
rgr_urbsubs_usc_m2 # Best model
)


names(rgr_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## -----------------------------------------------------------------------------
ramets_early_mods <- list(

## City_dist / gradient
ramets_e_gr_city_m1 ,

## Urbanization score / gradient
ramets_e_gr_usc_m1,

## City_dist / urban subtransects
ramets_e_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
ramets_e_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
ramets_e_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
ramets_e_urbsubs_usc_m2 # Best model

)


names(ramets_early_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## -----------------------------------------------------------------------------
ramets_late_mods <- list(

## City_dist / gradient
ramets_l_gr_city_m1 ,

## Urbanization score / gradient
ramets_l_gr_usc_m1 ,

## City_dist / urban subtransects
ramets_l_urbsubs_city_m1, # Qualitatively identical model (<2 AIC away)
ramets_l_urbsubs_city_m2, # Best model

## Urbanization score  / urban subtransects
ramets_l_urbsubs_usc_m1, # Qualitatively identical model (<2 AIC away)
ramets_l_urbsubs_usc_m2 # Best model

)


names(ramets_late_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## -----------------------------------------------------------------------------
mortality_mods <- list(

## City_dist / gradient
 surv_gr_city_m1_seasons,

## Urbanization score / gradient
 surv_gr_usc_m1_seasons,

## City_dist / urban subtransects
surv_urbsubs_city_m1_seasons, # Qualitatively identical model (<2 AIC away)
surv_urbsubs_city_m2_seasons, # Best model

## Urbanization score  / urban subtransects
surv_urbsubs_usc_m1_seasons, # Qualitatively identical model (<2 AIC away)
surv_urbsubs_usc_m2_seasons # Best model

)


names(mortality_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## -----------------------------------------------------------------------------

all_models <- list(
ldmc_mods,
sla_mods,
heights_early_mods,
heights_late_mods,
rgr_mods,
ramets_early_mods,
ramets_late_mods,
mortality_mods 
)

names(all_models) <- c(
"ldmc_mods",
"sla_mods",
"heights_early_mods",
"heights_late_mods",
"rgr_mods",
"ramets_early_mods",
"ramets_late_mods" ,
"mortality_mods" 
  )


## -----------------------------------------------------------------------------
## Best
### City_dist
ldmc_mods_best_c <- ldmc_mods[c(1,4)]

### Urb_score
ldmc_mods_best_u <- ldmc_mods[c(2,6)]

## Alt
### City_dist
ldmc_mods_alt_c <- ldmc_mods[3]

### Urb_score
ldmc_mods_alt_u <- ldmc_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
sla_mods_best_c <- sla_mods[c(1,4)]

### Urb_score
sla_mods_best_u <- sla_mods[c(2,6)]

## Alt
### City_dist
sla_mods_alt_c <- sla_mods[3]

### Urb_score
sla_mods_alt_u <- sla_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
heights_early_mods_best_c <- heights_early_mods[c(1,4)]

### Urb_score
heights_early_mods_best_u <- heights_early_mods[c(2,6)]

## Alt
### City_dist
heights_early_mods_alt_c <- heights_early_mods[3]

### Urb_score
heights_early_mods_alt_u <- heights_early_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
heights_late_mods_best_c <- heights_late_mods[c(1,4)]

### Urb_score
heights_late_mods_best_u <- heights_late_mods[c(2,6)]

## Alt
### City_dist
heights_late_mods_alt_c <- heights_late_mods[3]

### Urb_score
heights_late_mods_alt_u <- heights_late_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
rgr_mods_best_c <- rgr_mods[c(1,4)]

### Urb_score
rgr_mods_best_u <- rgr_mods[c(2,6)]

## Alt
### City_dist
rgr_mods_alt_c <- rgr_mods[3]

### Urb_score
rgr_mods_alt_u <- rgr_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
ramets_early_mods_best_c <- ramets_early_mods[c(1,4)]

### Urb_score
ramets_early_mods_best_u <- ramets_early_mods[c(2,6)]

## Alt
### City_dist
ramets_early_mods_alt_c <- ramets_early_mods[3]

### Urb_score
ramets_early_mods_alt_u <- ramets_early_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
ramets_late_mods_best_c <- ramets_late_mods[c(1,4)]

### Urb_score
ramets_late_mods_best_u <- ramets_late_mods[c(2,6)]

## Alt
### City_dist
ramets_late_mods_alt_c <- ramets_late_mods[3]

### Urb_score
ramets_late_mods_alt_u <- ramets_late_mods[5]


## -----------------------------------------------------------------------------
## Best
### City_dist
mortality_mods_best_c <- mortality_mods[c(1,4)]

### Urb_score
mortality_mods_best_u <- mortality_mods[c(2,6)]

## Alt
### City_dist
mortality_mods_alt_c <- mortality_mods[3]

### Urb_score
mortality_mods_alt_u <- mortality_mods[5]

