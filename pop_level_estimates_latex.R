# Generating population-level estimates
## Model: Latex, across entire gradient, urbanization = city_dist

### Linear mixed model (original)-----
ltx_gr_city_m1 <- glmmTMB(Latex_weight_mg ~
                            (1|Block) + 
                            (1|Population/Family) +
                            City_dist,
                          data = latex,
                          REML = F)

summary(ltx_gr_city_m1)
car::Anova(ltx_gr_city_m1) # p = 0.37
r.squaredGLMM(ltx_gr_city_m1) # very small R-squared: R2m = 0.002
AIC(ltx_gr_city_m1)



### Population-level model-----
#### Best Linear Unbiased Prediction (BLUP) of Random Effect (Pop)

latex_mod_without_citydist <- update(
  ltx_gr_city_m1,
  .~. - City_dist
)

blup_fams <- ranef(
  latex_mod_without_citydist
  )$cond[['Family:Population']] %>%
  tibble::rownames_to_column("Family_Pop") %>%
  dplyr::rename(estimate = 2)
  

blup_pops <- ranef(
  latex_mod_without_citydist
)$cond$Population %>%
  tibble::rownames_to_column("Population") %>%
  dplyr::rename(estimate = 2) %>%
  left_join(., latex %>% # add city_dist and other variables
              dplyr::select(c(2,18,20,22)) %>%
              dplyr::group_by(Population) %>%
              dplyr::summarise(Population = first(Population),
                               Transect_ID = first(Transect_ID),
                               City_dist = first(City_dist),
                               Urb_score = first(Urb_score)))

# simpler linear model
ltx_gr_city_m2 <- glmmTMB(estimate ~ 
                            City_dist,
                          data = blup_pops)

performance::check_model(ltx_gr_city_m2) # looks ok
summary(ltx_gr_city_m2)
car::Anova(ltx_gr_city_m2) # p = 0.39
r.squaredGLMM(ltx_gr_city_m2) # still small R-squared though about
# 10x larger than original MLM: R2m = 0.015
AIC(ltx_gr_city_m2) # ~7 AIC lower than plant-level model too

ggpredict(ltx_gr_city_m2,
          se=TRUE) %>%
  plot()

ggpredict(ltx_gr_city_m1,
          se=TRUE) %>%
  plot()


##### CONCLUSION #####
# p-values extremely similar yet marginal R-square value of population-
# level model is 10x higher than plant-level model (still small, though-
# R2m = 0.015).
# This suggests that my experimental design is powerful enough to detect
# an effect of genetic divergence due to urbanization.
# Also suggests that random effects adding a lot of noise to the model.
