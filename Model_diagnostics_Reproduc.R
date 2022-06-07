## ---------------------------------------------------------
source("libraries.R")
source("functions.R")


## ---------------------------------------------------------
reproduc <- read.csv(here::here("./Joined_annual_data/reproductive.csv"), na.strings=c("NO PLANT", "none", "NA"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -1)%>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block")), as.factor)

flowering <- read.csv(here::here("./Joined_annual_data/flowering.csv"), na.strings=c("NO PLANT", "none", "NA"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -1)%>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year")), as.character) %>%
    dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year")), as.factor) %>%
    dplyr::mutate_at(vars(c(Date_oldest_inflor_flower,
                            Date_youngest_inflor_flower,
                            Date_first_follicle_2in)),
                     as.Date,
                     format = '%m/%d/%Y') %>%
  dplyr::mutate(flowering_time = Date_youngest_inflor_flower-Date_oldest_inflor_flower + 1) %>% # if I don't add 1, then it will say some plants flowered for 0 days
  relocate(flowering_time,
           .before = Date_first_follicle_2in) %>%
  dplyr::mutate(Julian_oldest_inflor = as.numeric(format(Date_oldest_inflor_flower, "%j")))



## ---------------------------------------------------------
flsucc_gr_city1 <- glmmTMB(Flowered ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             City_dist,
  data = reproduc,
  family = binomial(link = "logit"))


# plot(DHARMa::simulateResiduals(flsucc_gr_city1))


## ---------------------------------------------------------
flsucc_gr_usc1 <- glmmTMB(Flowered ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             Urb_score,
  data = reproduc,
  family = binomial(link = "logit"))


# plot(DHARMa::simulateResiduals(flsucc_gr_usc1))


## ---------------------------------------------------------
flsucc_urbsubs_city1 <- glmmTMB(Flowered ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             City_dist * Transect_ID,
  data = reproduc %>%
    dplyr::filter(Transect_ID != "Rural"),
  family = binomial(link = "logit"))

# plot(DHARMa::simulateResiduals(flsucc_urbsubs_city1))


## MAIN EFFECTS
flsucc_urbsubs_city1_ME <- glmmTMB(Flowered ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             City_dist + Transect_ID,
  data = reproduc %>%
    dplyr::filter(Transect_ID != "Rural"),
  family = binomial(link = "logit"))


# plot(DHARMa::simulateResiduals(flsucc_urbsubs_city1_ME)) 
# AIC(flsucc_urbsubs_city1, flsucc_urbsubs_city1_ME) # ME model best


## ---------------------------------------------------------
flsucc_urbsubs_usc1 <- glmmTMB(Flowered ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             Urb_score * Transect_ID,
  data = reproduc %>%
    dplyr::filter(Transect_ID != "Rural"),
  family = binomial(link = "logit"))

# plot(DHARMa::simulateResiduals(flsucc_urbsubs_usc1))


## MAIN EFFECTS
flsucc_urbsubs_usc1_ME <- glmmTMB(Flowered ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             Urb_score + Transect_ID,
  data = reproduc %>%
    dplyr::filter(Transect_ID != "Rural"),
  family = binomial(link = "logit"))


# plot(DHARMa::simulateResiduals(flsucc_urbsubs_usc1_ME))
# AIC(flsucc_urbsubs_usc1, flsucc_urbsubs_usc1_ME) # ME model best


## ---------------------------------------------------------
## Basic data exploration
# plot(total_flower_count ~ City_dist, data = flowering)
# 
# boxplot(total_flower_count ~ Block, data = flowering)
# 
# boxplot(total_flower_count ~ Year, data = flowering) 

# 
flowers_gr_city1 <- glmmTMB(total_flower_count ~
                                          Block +
                                          Year +
                                          (1|Population/Family) +
                                          City_dist, 
                                        data = flowering,
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(flowers_gr_city1)) 



## ---------------------------------------------------------
flowers_gr_usc1 <- glmmTMB(total_flower_count ~
                                          Block +
                                          Year +
                                          (1|Population/Family) +
                                          Urb_score, 
                                        data = flowering,
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(flowers_gr_usc1))



## ---------------------------------------------------------
flowers_urbsubs_city1 <- glmmTMB(total_flower_count ~
                                         Block +
                                          Year +
                                          (1|Population/Family) +
                                          City_dist * Transect_ID, 
                                        data = flowering %>%
                                   dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2) 
# plot(DHARMa::simulateResiduals(flowers_urbsubs_city1))

## MAIN EFFECTS
flowers_urbsubs_city1_ME <- glmmTMB(total_flower_count ~
                                          Block +
                                          Year +
                                          (1|Population/Family) +
                                          City_dist + Transect_ID, 
                                        data = flowering %>%
                                   dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(flowers_urbsubs_city1_ME))
# AIC(flowers_urbsubs_city1, flowers_urbsubs_city1_ME) # ME model better but <2 AIC from full


## ---------------------------------------------------------
flowers_urbsubs_usc1 <- glmmTMB(total_flower_count ~
                                          Block +
                                          Year +
                                          (1|Population/Family) +
                                          Urb_score * Transect_ID, 
                                        data = flowering %>%
                                   dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2) 

# plot(DHARMa::simulateResiduals(flowers_urbsubs_usc1))


## MAIN EFFECTS
flowers_urbsubs_usc1_ME <- glmmTMB(total_flower_count ~
                                          Block +
                                          Year +
                                          (1|Population/Family) +
                                          Urb_score + Transect_ID, 
                                        data = flowering %>%
                                   dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2) 

# plot(DHARMa::simulateResiduals(flowers_urbsubs_usc1_ME))
# AIC(flowers_urbsubs_usc1, flowers_urbsubs_usc1_ME) # ME model better but <2 AIC from full


## ---------------------------------------------------------
## Basic data exploration
# plot(Overall_mean ~ City_dist, data = flowering)
# 
# boxplot(Overall_mean ~ Block, data = flowering)
# 
# boxplot(Overall_mean ~ Year, data = flowering)

flsize_gr_city1 <- glmmTMB(Overall_mean ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             City_dist, 
                           data = flowering)

# plot(DHARMa::simulateResiduals(flsize_gr_city1))
# performance::check_model(flsize_gr_city1)


## ---------------------------------------------------------
flsize_gr_usc1 <- glmmTMB(Overall_mean ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             Urb_score, 
                           data = flowering,
                          REML = F)

# plot(DHARMa::simulateResiduals(flsize_gr_usc1))
# performance::check_model(flsize_gr_usc1)


## ---------------------------------------------------------
flsize_urbsubs_city1 <- glmmTMB(Overall_mean ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             City_dist * Transect_ID, 
                           data = flowering %>%
                             dplyr::filter(Transect_ID != "Rural"))

# plot(DHARMa::simulateResiduals(flsize_urbsubs_city1))
# performance::check_model(flsize_urbsubs_city1)


## MAIN EFFECTS
flsize_urbsubs_city1_ME <- glmmTMB(Overall_mean ~
                             Block +
                             Year +
                             (1|Population/Family) +
                             City_dist + Transect_ID, 
                           data = flowering %>%
                             dplyr::filter(Transect_ID != "Rural")) 

# plot(DHARMa::simulateResiduals(flsize_urbsubs_city1_ME))
# performance::check_model(flsize_urbsubs_city1_ME)
# AIC(flsize_urbsubs_city1, flsize_urbsubs_city1_ME) # full model best but <2 AIC from ME


## ---------------------------------------------------------
# doesn't converge unless block removed
flsize_urbsubs_usc1 <- glmmTMB(Overall_mean ~
                            # Block +
                             Year +
                             (1|Population/Family) +
                             Urb_score * Transect_ID, 
                           data = flowering %>%
                             dplyr::filter(Transect_ID != "Rural")) 

plot(DHARMa::simulateResiduals(flsize_urbsubs_usc1))
performance::check_model(flsize_urbsubs_usc1)
car::Anova(flsize_urbsubs_usc1)

## MAIN EFFECTS
flsize_urbsubs_usc1_ME <- glmmTMB(Overall_mean ~
                            # Block +
                             Year +
                             (1|Population/Family) +
                             Urb_score + Transect_ID,
                           data = flowering %>%
                             dplyr::filter(Transect_ID != "Rural"))

# plot(DHARMa::simulateResiduals(flsize_urbsubs_usc1_ME))
# performance::check_model(flsize_urbsubs_usc1_ME)
# AIC(flsize_urbsubs_usc1, flsize_urbsubs_usc1_ME) # full model best


## ---------------------------------------------------------
flowering$flowering_time_num <- as.numeric(flowering$flowering_time)

fl_time_gr_city1 <- glmmTMB(flowering_time_num ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          City_dist, 
                                        data = flowering,
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(fl_time_gr_city1))
# performance::check_model(fl_time_gr_city1)
# car::Anova(fl_time_gr_city1)


## ---------------------------------------------------------
fl_time_gr_usc1 <- glmmTMB(flowering_time_num ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          Urb_score, 
                                        data = flowering,
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(fl_time_gr_usc1))
# performance::check_model(fl_time_gr_usc1)
# car::Anova(fl_time_gr_usc1)


## ---------------------------------------------------------
# full model
fl_time_urbsubs_city1 <- glmmTMB(flowering_time_num ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          City_dist * Transect_ID, 
                                        data = flowering %>%
                               dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(fl_time_urbsubs_city1))
# performance::check_model(fl_time_urbsubs_city1)
# car::Anova(fl_time_urbsubs_city1)


# reduced model
fl_time_urbsubs_city1_ME <- glmmTMB(flowering_time_num ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          City_dist + Transect_ID, 
                                        data = flowering %>%
                               dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(fl_time_urbsubs_city1_ME))
# performance::check_model(fl_time_urbsubs_city1_ME)
# car::Anova(fl_time_urbsubs_city1_ME)
# AIC(fl_time_urbsubs_city1, fl_time_urbsubs_city1_ME) # ME model best but <2 AIC from full


## ---------------------------------------------------------
# full model
fl_time_urbsubs_usc1 <- glmmTMB(flowering_time_num ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          Urb_score * Transect_ID, 
                                        data = flowering %>%
                               dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(fl_time_urbsubs_usc1))
# performance::check_model(fl_time_urbsubs_usc1)
# car::Anova(fl_time_urbsubs_usc1)


# reduced model
fl_time_urbsubs_usc1_ME <- glmmTMB(flowering_time_num ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          Urb_score + Transect_ID, 
                                        data = flowering %>%
                               dplyr::filter(Transect_ID != "Rural"),
                            family = nbinom2)

# plot(DHARMa::simulateResiduals(fl_time_urbsubs_usc1_ME))
# performance::check_model(fl_time_urbsubs_usc1_ME)
# car::Anova(fl_time_urbsubs_usc1_ME)
# AIC(fl_time_urbsubs_usc1, fl_time_urbsubs_usc1_ME) # ME model best but <2 AIC from full


## ---------------------------------------------------------
fl_start_gr_city1 <- glmmTMB(Julian_oldest_inflor ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          City_dist, 
                                        data = flowering,
                             family = poisson)

plot(DHARMa::simulateResiduals(fl_start_gr_city1))
performance::check_model(fl_start_gr_city1)
car::Anova(fl_start_gr_city1)


## ---------------------------------------------------------
fl_start_gr_usc1 <- glmmTMB(Julian_oldest_inflor ~
                                         #  Block +
                                           Year +
                                          (1|Population/Family) +
                                          Urb_score, 
                                        data = flowering,
                             family = poisson)

plot(DHARMa::simulateResiduals(fl_start_gr_usc1))
performance::check_model(fl_start_gr_usc1)
car::Anova(fl_start_gr_usc1)


## ---------------------------------------------------------
fl_start_urbsubs_city1 <- glmmTMB(Julian_oldest_inflor ~
                                           Block +
                                      #     Year +
                                          (1|Population/Family) +
                                          City_dist * Transect_ID, 
                                        data = flowering %>%
                                    dplyr::filter(Transect_ID != "Rural"),
                             family = poisson)

plot(DHARMa::simulateResiduals(fl_start_urbsubs_city1))
performance::check_model(fl_start_urbsubs_city1)
car::Anova(fl_start_urbsubs_city1)
summary(fl_start_urbsubs_city1)

# Main effects model
fl_start_urbsubs_city1_ME <- glmmTMB(Julian_oldest_inflor ~
                                           Block +
                                      #     Year +
                                          (1|Population/Family) +
                                          City_dist + Transect_ID, 
                                        data = flowering %>%
                                    dplyr::filter(Transect_ID != "Rural"),
                             family = poisson)

plot(DHARMa::simulateResiduals(fl_start_urbsubs_city1_ME))
performance::check_model(fl_start_urbsubs_city1_ME)
car::Anova(fl_start_urbsubs_city1_ME)
summary(fl_start_urbsubs_city1_ME)

AIC(fl_start_urbsubs_city1, fl_start_urbsubs_city1_ME) # ME best model but <2 AIC from full


## ---------------------------------------------------------
# not converging
# fl_start_urbsubs_usc1 <- glmmTMB(Julian_oldest_inflor ~
#                                           # Block +
#                                       #     Year +
#                                           (1|Population/Family) +
#                                           Urb_score * Transect_ID, 
#                                         data = flowering %>%
#                                     dplyr::filter(Transect_ID != "Rural"),
#                              family = poisson)
# 
# plot(DHARMa::simulateResiduals(fl_start_urbsubs_usc1))
# performance::check_model(fl_start_urbsubs_usc1)
# car::Anova(fl_start_urbsubs_usc1)
# summary(fl_start_urbsubs_usc1)



# Main effects model
# not converging, so I'll take out urb_score and test it separately
# TEST TRANSECT
fl_start_urbsubs_usc1_m1 <- glmmTMB(Julian_oldest_inflor ~
                                          # Block +
                                           Year +
                                          (1|Population/Family) +
                                      #    Urb_score +
                                      Transect_ID, 
                                        data = flowering %>%
                                    dplyr::filter(Transect_ID != "Rural"),
                             family = poisson)

plot(DHARMa::simulateResiduals(fl_start_urbsubs_usc1_m1))
performance::check_model(fl_start_urbsubs_usc1_m1)
car::Anova(fl_start_urbsubs_usc1_m1)
summary(fl_start_urbsubs_usc1_m1)


# TEST URB_SCORE
fl_start_urbsubs_usc1_m2 <- glmmTMB(Julian_oldest_inflor ~
                                           Block +
                                           Year +
                                          (1|Population/Family) +
                                          Urb_score, #+ Transect_ID, 
                                        data = flowering %>%
                                    dplyr::filter(Transect_ID != "Rural"),
                             family = poisson)

plot(DHARMa::simulateResiduals(fl_start_urbsubs_usc1_m2))
performance::check_model(fl_start_urbsubs_usc1_m2)
car::Anova(fl_start_urbsubs_usc1_m2)
summary(fl_start_urbsubs_usc1_m2)





# AIC(fl_start_urbsubs_usc1, fl_start_urbsubs_usc1_ME) # ME best model but <2 AIC from full


## ---------------------------------------------------------
pods_gr_city1 <- glmmTMB(Pods ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           City_dist,
                         data = flowering,
                         family = poisson)

plot(DHARMa::simulateResiduals(pods_gr_city1))
performance::check_model(pods_gr_city1)
car::Anova(pods_gr_city1)



## ---------------------------------------------------------
pods_gr_usc1 <- glmmTMB(Pods ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           Urb_score,
                         data = flowering,
                         family = poisson)

plot(DHARMa::simulateResiduals(pods_gr_usc1))
performance::check_model(pods_gr_usc1)
car::Anova(pods_gr_usc1)



## ---------------------------------------------------------
pods_urbsubs_city1 <- glmmTMB(Pods ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           City_dist * Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = poisson)

plot(DHARMa::simulateResiduals(pods_urbsubs_city1))
performance::check_model(pods_urbsubs_city1)
car::Anova(pods_urbsubs_city1)


# MAIN EFFECTS MODEL
pods_urbsubs_city1_ME <- glmmTMB(Pods ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           City_dist + Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = poisson)

plot(DHARMa::simulateResiduals(pods_urbsubs_city1_ME)) # looks fine
performance::check_model(pods_urbsubs_city1_ME)
car::Anova(pods_urbsubs_city1_ME)

AIC(pods_urbsubs_city1, pods_urbsubs_city1_ME) # ME model lower AIC but <2 away



## ---------------------------------------------------------
pods_urbsubs_usc1 <- glmmTMB(Pods ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           Urb_score * Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = poisson)

plot(DHARMa::simulateResiduals(pods_urbsubs_usc1))
performance::check_model(pods_urbsubs_usc1)
car::Anova(pods_urbsubs_usc1)


# MAIN EFFECTS MODEL
pods_urbsubs_usc1_ME <- glmmTMB(Pods ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           Urb_score + Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = poisson)

plot(DHARMa::simulateResiduals(pods_urbsubs_usc1_ME)) # looks fine
performance::check_model(pods_urbsubs_usc1_ME)
car::Anova(pods_urbsubs_usc1_ME)

AIC(pods_urbsubs_usc1, pods_urbsubs_usc1_ME) # full model lower AIC but <2 away


## ---------------------------------------------------------
hist(flowering$Peduncles) # very right-skewed. Looks like there might be a few outliers

peduncles_gr_city1 <- glmmTMB(Peduncles^3 ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           City_dist,
                         data = flowering,
                         family = nbinom2)

plot(DHARMa::simulateResiduals(peduncles_gr_city1)) 
performance::check_model(peduncles_gr_city1)
performance::check_outliers(peduncles_gr_city1) # no outliers
car::Anova(peduncles_gr_city1)


## ---------------------------------------------------------
peduncles_gr_usc1 <- glmmTMB(Peduncles^3 ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           Urb_score,
                         data = flowering,
                         family = nbinom2)

plot(DHARMa::simulateResiduals(peduncles_gr_usc1)) 
performance::check_model(peduncles_gr_usc1)
car::Anova(peduncles_gr_usc1)


## ---------------------------------------------------------
peduncles_urbsubs_city1 <- glmmTMB(Peduncles^2 ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           City_dist * Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = nbinom2)

plot(DHARMa::simulateResiduals(peduncles_urbsubs_city1))


# MAIN EFFECTS MODEL
peduncles_urbsubs_city1_ME <- glmmTMB(Peduncles^2 ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           City_dist + Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = nbinom2)

plot(DHARMa::simulateResiduals(peduncles_urbsubs_city1_ME))

AIC(peduncles_urbsubs_city1, peduncles_urbsubs_city1_ME) # ME model best but <2 AIC from full

car::Anova(peduncles_urbsubs_city1_ME)


## ---------------------------------------------------------
# neg binomial- nbinom2 has best results though residual vs predicted plot still has issues
peduncles_urbsubs_usc1 <- glmmTMB(Peduncles ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           Urb_score * Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = nbinom2)

plot(DHARMa::simulateResiduals(peduncles_urbsubs_usc1))
DHARMa::testOutliers(peduncles_urbsubs_usc1) # no outliers
performance::check_model(peduncles_urbsubs_usc1)


# MAIN EFFECTS MODEL
peduncles_urbsubs_usc1_ME <- glmmTMB(Peduncles ~
                           Block +
                           Year +
                           (1|Population/Family) +
                           Urb_score + Transect_ID,
                         data = flowering %>%
                           dplyr::filter(Transect_ID != "Rural"),
                         family = nbinom2)

plot(DHARMa::simulateResiduals(peduncles_urbsubs_usc1_ME)) # looks good

performance::check_model(peduncles_urbsubs_usc1_ME)

AIC(peduncles_urbsubs_usc1, peduncles_urbsubs_usc1_ME) #full model best model


## ---------------------------------------------------------
flsucc_mods <- list(

## City_dist / gradient
flsucc_gr_city1 ,

## Urbanization score / gradient
flsucc_gr_usc1 ,

## City_dist / urban subtransects
flsucc_urbsubs_city1_ME, # Best model

## Urbanization score  / urban subtransects
flsucc_urbsubs_usc1_ME # Best model

)


names(flsucc_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_best",
                     "Usc_urbsubs_best")


## ---------------------------------------------------------
flowers_mods <- list(

## City_dist / gradient
flowers_gr_city1 ,

## Urbanization score / gradient
flowers_gr_usc1 ,

## City_dist / urban subtransects
flowers_urbsubs_city1, # Qualitatively identical model (<2 AIC away)
flowers_urbsubs_city1_ME, # Best model

## Urbanization score  / urban subtransects
# flowers_urbsubs_usc1, 
flowers_urbsubs_usc1_ME # Best model

)


names(flowers_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                   #  "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## ---------------------------------------------------------
flsize_mods <- list(

## City_dist / gradient
flsize_gr_city1 ,

## Urbanization score / gradient
flsize_gr_usc1 ,

## City_dist / urban subtransects
flsize_urbsubs_city1, # Best model
flsize_urbsubs_city1_ME, # Qualitatively identical model (<2 AIC away)

## Urbanization score  / urban subtransects
flsize_urbsubs_usc1 # Best model
#flsize_urbsubs_usc1_ME  # >2 AIC from best model

)


names(flsize_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_best",
                     "City_urbsubs_alt",
                     "Usc_urbsubs_best")


## ---------------------------------------------------------
fltime_mods <- list(

## City_dist / gradient
fl_time_gr_city1 ,

## Urbanization score / gradient
fl_time_gr_usc1 ,

## City_dist / urban subtransects
fl_time_urbsubs_city1,   # Qualitatively identical model (<2 AIC away)
fl_time_urbsubs_city1_ME, # Best model

## Urbanization score  / urban subtransects
fl_time_urbsubs_city1, # Qualitatively identical model (<2 AIC away)
fl_time_urbsubs_city1_ME  # Best model

)

names(fltime_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best")


## ---------------------------------------------------------
flstart_mods <- list(

## City_dist / gradient
fl_start_gr_city1 ,

## Urbanization score / gradient
fl_start_gr_usc1 ,

## City_dist / urban subtransects
fl_start_urbsubs_city1, # Qualitatively identical model (<2 AIC away)
fl_start_urbsubs_city1_ME, # Best model

## Urbanization score  / urban subtransects
fl_start_urbsubs_usc1_m1, # THIS MODEL TESTS TRANSECT ONLY
fl_start_urbsubs_usc1_m2 # THIS MODEL TESTS URBSCORE ONLY

)

names(flstart_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_transectonly",
                     "Usc_urbsubs_urbscoreonly")


## ---------------------------------------------------------
pods_mods <- list(

## City_dist / gradient
pods_gr_city1 ,

## Urbanization score / gradient
pods_gr_usc1 ,

## City_dist / urban subtransects
pods_urbsubs_city1, # Best model
pods_urbsubs_city1_ME, # Qualitatively identical model (<2 AIC away)

## Urbanization score  / urban subtransects
pods_urbsubs_usc1, # Qualitatively identical model (<2 AIC away)
pods_urbsubs_usc1_ME # Best model

)


names(pods_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_best",
                     "City_urbsubs_alt",
                     "Usc_urbsubs_alt",
                     "Usc_urbsubs_best"
                     )


## ---------------------------------------------------------
peduncles_mods <- list(

## City_dist / gradient
peduncles_gr_city1 ,

## Urbanization score / gradient
peduncles_gr_usc1 ,

## City_dist / urban subtransects
peduncles_urbsubs_city1, # Qualitatively identical model (<2 AIC away)
peduncles_urbsubs_city1_ME, # Best model

## Urbanization score  / urban subtransects
peduncles_urbsubs_usc1 #, # Best model
# peduncles_urbsubs_usc1_ME 

)


names(peduncles_mods) <- c("City_gr",
                     "Usc_gr",
                     "City_urbsubs_alt",
                     "City_urbsubs_best",
                     "Usc_urbsubs_best")


## ---------------------------------------------------------
all_models <- list(
flsucc_mods      , # flowering success
flowers_mods     , # total flowers/plant
flsize_mods      , # mean flower size
fltime_mods      , # flowering time
flstart_mods     , # flowering start
pods_mods        , # pods (follicles)
peduncles_mods   ) # peduncles (inflorescences)

names(all_models) <- c(
"flsucc_mods"          ,
"flowers_mods"         ,
"flsize_mods"          ,
"fltime_mods",
"flstart_mods",
"pods_mods"            ,
"peduncles_mods")


## ---------------------------------------------------------
## Best
### City_dist
flsucc_mods_best_c <- flsucc_mods[c(1,4)]

### Urb_score
flsucc_mods_best_u <- flsucc_mods[c(2,6)]

## Alt
### City_dist
flsucc_mods_alt_c <- flsucc_mods[3]

### Urb_score
flsucc_mods_alt_u <- flsucc_mods[5]


## ---------------------------------------------------------
## Best
### City_dist
flowers_mods_best_c <- flowers_mods[c(1,4)]

### Urb_score
flowers_mods_best_u <- flowers_mods[c(2,6)]

## Alt
### City_dist
flowers_mods_alt_c <- flowers_mods[3]

### Urb_score
flowers_mods_alt_u <- flowers_mods[5]



## ---------------------------------------------------------
## Best
### City_dist
flsize_mods_best_c <- flsize_mods[c(1,4)]

### Urb_score
flsize_mods_best_u <- flsize_mods[c(2,5)]

## Alt
### City_dist
flsize_mods_alt_c <- flsize_mods[3]

### Urb_score- NONE


## ---------------------------------------------------------
## Best
### City_dist
fltime_mods_best_c <- fltime_mods[c(1,4)]

### Urb_score
fltime_mods_best_u <- fltime_mods[c(2,6)]

## Alt
### City_dist
fltime_mods_alt_c <- fltime_mods[3]

### Urb_score
fltime_mods_alt_u <- fltime_mods[5]



## ---------------------------------------------------------
## Best
### City_dist
flstart_mods_best_c <- flstart_mods[c(1,4)]

### Urb_score
flstart_mods_best_u <- flstart_mods[c(2,5,6)] # includes transect-only and urbscore-only mods

## Alt
### City_dist
flstart_mods_alt_c <- flstart_mods[3]

### Urb_score
# NONE



## ---------------------------------------------------------
## Best
### City_dist
pods_mods_best_c <- pods_mods[c(1,4)]

### Urb_score
pods_mods_best_u <- pods_mods[c(2,5)]

## Alt
### City_dist
pods_mods_alt_c <- pods_mods[3]

### Urb_score
pods_mods_alt_u <- pods_mods[6]


## ---------------------------------------------------------
## Best
### City_dist
peduncles_mods_best_c <- peduncles_mods[c(1,4)]

### Urb_score
peduncles_mods_best_u <- peduncles_mods[c(2,5)]

## Alt
### City_dist
peduncles_mods_alt_c <- peduncles_mods[3]

### Urb_score- NONE

