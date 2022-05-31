# Functions

############################################################
######################## ANOVA #############################
############################################################

## Create tidy anova tables-----
### GOALS:
### 1. Perform car::Anova() on each model in list
###   -if it's got an interaction, do type III first
###     -if interaxn p-val < 0.1, retain that result
###     -else, move on to type II
### 2. Save anova table w/appropriate SS type as new item in trait's sublist

### function that IDs if there is >= 1 sig interaction in
### the type III anova
ID_intxn <- function(model){
  model.df <- broom.mixed::tidy(car::Anova(model, type = "III"))
  intxn.df <- model.df[grep(":", model.df$term), "p.value"]
  sig.intxns <- sum(intxn.df <= 0.1)
  return(sig.intxns)
}

### Tidy up anova into table
tidy_anova <- function(model){
  type.choice <- ifelse(ID_intxn(model) > 0,
                        3,
                        2)
  return(
    car::Anova(model, type = type.choice) %>%
      broom.mixed::tidy() %>%
      as.data.frame() %>%
      dplyr::mutate(Response = as.character(formula(model)[2]),
                    .before = term) %>%
      # dplyr::mutate(Model = paste(n(model)),
      #               .before = term) %>%
      dplyr::mutate(Sites = case_when(
        str_detect(as.character(formula(model)[3]), "Transect_ID") == TRUE ~ "Urban Only",
        str_detect(as.character(formula(model)[3]), "Transect_ID") == FALSE ~ "All"),
        .before = term) %>%
      dplyr::mutate(Significance = case_when(p.value < 0.001 ~ "***",
                                             p.value < 0.01 ~ "**",
                                             p.value <= 0.05 ~ "*")) %>%
      # dplyr::mutate(., AIC = as.numeric(AIC(model))) %>%
      mutate_if(is.numeric, round, 3)      %>%
      dplyr::mutate(p.value = replace(p.value, Significance == "***", "<0.001"))  %>%
      dplyr::select(., -df) %>%
      dplyr::rename(., Chi_sq = statistic,
                    Predictor = term,
                    p = p.value) %>%
      unite("p", p:Significance, sep = "", na.rm = TRUE) %>%
      dplyr::mutate_if(.,
                       is.character,
                       str_replace_all,
                       pattern = c("City_dist"),
                       replacement = c("Distance to City Center")) %>%
      dplyr::mutate_if(.,
                       is.character,
                       str_replace_all,
                       pattern = c("Urb_score"),
                       replacement = c("Urbanization Score")) %>%
      dplyr::mutate_if(.,
                       is.character,
                       str_replace_all,
                       pattern = c("Transect_ID"),
                       replacement = c("Subtransect")) %>%
      dplyr::mutate_if(.,
                       is.character,
                       str_replace_all,
                       pattern = c(":"),
                       replacement = c(" x ")) %>%
      dplyr::mutate_if(.,
                       is.character,
                       str_replace_all,
                       pattern = c("Year x Urbanization Score"),
                       replacement = c("Urbanization Score x Year"))
  )}

### Next, create best and alt models for individual lists
### of traits
make_all_anovas_tidy <- function(anova_list){
  return(
    lapply(anova_list, tidy_anova) %>%
      lapply(., as.data.frame) %>%
      purrr::reduce(full_join) %>%
      replace(., is.na(.), "-") %>%
      dplyr::filter(Predictor != "(Intercept)") %>%
      flextable::flextable() %>% 
      merge_v(j = "Response") %>% 
      merge_v(j = "Sites") %>% 
      valign(valign = "top")%>%
      flextable::autofit() %>%
      align(j = c(1, 3), align = "left", part = "all") %>%
      align(j = c(2,4,5), align = "center", part = "all") %>%
      align(j = c(2,4,5), align = "center", part = "header") %>%
      flextable::compose(i = 1, j = 4, part = "header",
                         value = as_paragraph("χ", as_sup("2"))) %>%
      flextable::compose(i = 1, j = 5, part = "header",
                         value = as_paragraph(as_i("p"))) %>%
      fontsize(size = 12, part = "header") %>%
      fontsize(size = 12, part = "body") %>%
      #theme_booktabs()%>%
      flextable::hline(i = 1, part = "body",
                       border = officer::fp_border(color = "#666666",
                                                   width = 1.5) )
    %>% fix_border_issues()
    
  )
}

### For only one model (e.g., alt models), use this slightly
### altered function that doesn't include horizontal bar
### *Can also be used for models that include other fixed effects
### like year, block, sample
make_all_anovas_tidy_altmods <- function(anova_list){
  return(
    lapply(anova_list, tidy_anova) %>%
      lapply(., as.data.frame) %>%
      purrr::reduce(full_join) %>%
      replace(., is.na(.), "-") %>%
      dplyr::filter(Predictor != "(Intercept)") %>%
      flextable::flextable() %>% 
      merge_v(j = "Response") %>% 
      merge_v(j = "Sites") %>% 
      valign(valign = "top")%>%
      flextable::autofit() %>%
      align(j = c(1, 3), align = "left", part = "all") %>%
      align(j = c(2,4,5), align = "center", part = "all") %>%
      align(j = c(2,4,5), align = "center", part = "header") %>%
      flextable::compose(i = 1, j = 4, part = "header",
                         value = as_paragraph("χ", as_sup("2"))) %>%
      flextable::compose(i = 1, j = 5, part = "header",
                         value = as_paragraph(as_i("p"))) %>%
      fontsize(size = 12, part = "header") %>%
      fontsize(size = 12, part = "body") %>%
      #theme_booktabs()%>%
      fix_border_issues()
    
  )
}



############################################################
########## Do linear regression on multiple models #########
############# (for cardenolide analysis)####################
############################################################

DoLinearReg <- function(response_var, predictor_var, input_data){
  
  # first, make the formula as a string
  formula_str <- paste(
    response_var, "~",
    paste(predictor_var,
          collapse = " + ")   )
  print(paste0("Model formula: ", formula_str))
  
  # input string into lm
  my_model <- lm(formula_str, data = input_data) 
  performance::check_model(my_model) %T>%
    print()
  performance::model_performance(my_model) %T>%
    print()
  my_anova <- car::Anova(my_model)
  my_list <<- list(my_model, my_anova)
}



############################################################
##################### RANOVAS ##############################
############################################################

## Create tidy ranova table-----
### assessing if variation among pops/fams varies with
### distance from city center
#### for regular g/lmer models:
CreateRanovaOutput <- function(lmer_model, var_name){
  
  m1 <- ranova(lmer_model)
  
  m1.ranova <- as.data.frame(m1) %>%
    dplyr::mutate(Group = row.names(.)) %>%
    remove_rownames %>%
    dplyr::select(c(Group, 1:6)) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "<none>",
                                  "Residual")) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "(1 | Family:Population)",
                                  "Family:Population")) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "(1 | Population)",
                                  "Population")) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "(1 | Block)",
                                  "Block")) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "(1 | Year)",
                                  "Year"))
  
  
  variances1 <- print(lme4::VarCorr(lmer_model),
                      comp="Variance") %>%
    as.data.frame() %>%
    dplyr::mutate(., PVE = round((vcov*100 / sum(vcov)),3)) %>%
    dplyr::select(., c(grp, vcov, PVE)) %>%
    dplyr::rename(., Group = grp,
                  Variance = vcov) %>%
    as.data.frame()
  
  tab1 <- full_join(m1.ranova, variances1) %>%
    as.data.frame() %>%
    dplyr::arrange(Group) %>%
    dplyr::select(c(1,8,9,7)) %>%
    dplyr::mutate_at(vars(c(2,4)), round, 3) %>%
    dplyr::rename(., p = 4) %>%
    dplyr::mutate(p = replace(p,
                              p < 0.001,
                              "<0.001")) %>%
    dplyr::mutate(Variable = noquote(var_name)) %>%
    dplyr::select(c(5, 1:4)) %>%
    dplyr::filter(Group != "Block") %>%
    dplyr::filter(Group != "Year") %>%
    flextable() %>%
    merge_at(j = 1) %>%
    fix_border_issues() %>%
    bold(i = ~ p <= 0.05, j = 5) %>%
    autofit()
  
  return(tab1)
}

#### for boostrapping models:
CreateRanovaOutput_bootstrap <- function(ranova_fam,
                                         ranova_pop,
                                         var_name){
  
  ranova.fam <- tidy(ranova_fam) %>%
    dplyr::filter(type == "LRT") %>%
    dplyr::rename(Group = type) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "LRT",
                                  "Family")) %>%
    dplyr::select(-c(2:3))
  
  ranova.pop <- tidy(ranova_pop) %>%
    dplyr::filter(type == "LRT") %>%
    dplyr::rename(Group = type) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "LRT",
                                  "Population")) %>%
    dplyr::select(-c(2:3))
  
  tab1 <- full_join(ranova.fam, ranova.pop) %>%
    as.data.frame() %>%
    dplyr::mutate_at(vars(c(p.value)), round, 3) %>%
    dplyr::rename(., p = 2) %>%
    dplyr::mutate(p = replace(p,
                              p < 0.001,
                              "<0.001")) %>%
    dplyr::mutate(Variable = noquote(var_name)) %>%
    dplyr::select(c(3,1,2)) %>%
    flextable() %>%
    merge_at(j = 1) %>%
    fix_border_issues() %>%
    bold(i = ~ p <= 0.05, j = 3) %>%
    autofit()
  
  return(tab1)
}

### assessing if variation among pops/fams varies by transect
CreateRanovaOutput_Q2 <- function(lmer_model, var_name){

  tab1 <- car::Anova(lmer_model) %>%
    tidy() %>%
    as.data.frame() %>%
    dplyr::select(-df) %>%
    dplyr::mutate_at(vars(c(2,3)), round, 3) %>%
    dplyr::rename(.,
                  p = 3,
                  Variable = 1) %>%
    dplyr::filter(grepl('Transect_ID', Variable)) %>%
    dplyr::mutate(p = replace(p,
                              p < 0.001,
                              "<0.001")) %>%
    dplyr::mutate(Variable = noquote(var_name)) %>%
    flextable() %>%
    flextable::compose(i = 1, j = 2,
                       part = "header",
                       value = as_paragraph("χ", as_sup("2"))) %>%
    bold(i = ~ p <= 0.05, j = 3) %>%
    align(j = c(2,3),
          align = "center",
          part = "header") %>%
    align(j = c(2,3),
          align = "center",
          part = "body") %>%
    autofit()
  
  return(tab1)
}



############################################################
##################### HERITABILITY & QST ###################
############################################################

## Calculate heritability
Calc_narrow_sense_h <- function(fam_var, pop_var, resid_var){
  add_var <- 2*(fam_var^2)
  total_wp_var <- (fam_var^2) + # family
    (pop_var^2) + # population
    (resid_var^2) # residual
  h2 <- add_var/total_wp_var
  return(h2)
}

## Calculate Qst
Calc_qst <- function(fam_var, pop_var){
  num_qst <- pop_var^2
  dem_qst <- pop_var^2 + (2*(fam_var^2))
  qst <- num_qst/dem_qst
  return(qst)
}

## Calculate h2 and Qst in same function
Calc_h2_qst <- function(fam_var,
                                               pop_var,
                                               resid_var){
  add_var <- 2*(fam_var^2)
  total_wp_var <- (fam_var^2) + # family
    (pop_var^2) + # population
    (resid_var^2) # residual
  h2 <- add_var/total_wp_var
  
  num_qst <- pop_var^2
  dem_qst <- pop_var^2 + (2*(fam_var^2))
  qst <- num_qst/dem_qst
  
  print(paste0("Narrow-sense heritability: ", h2))
  print(paste0("Qst: ", qst))
}

CVA <- function(fam_var,
                pop_var,
                resid_var,
                trait_mean){
  add_var <- 2*(fam_var^2)
  total_wp_var <- (fam_var^2) + # family
    (pop_var^2) + # population
    (resid_var^2) # residual
  
  cva <- sqrt(add_var) / trait_mean
  
  return(cva)
}

calc_quantgenvars <-  function(fam_var,
                               pop_var,
                               resid_var,
                               trait_mean,
                               variable_name){
  add_var <- 2*(fam_var^2)
  total_wp_var <- (fam_var^2) + # family
    (pop_var^2) + # population
    (resid_var^2) # residual
  h2 <- add_var/total_wp_var
  
  num_qst <- pop_var^2
  dem_qst <- pop_var^2 + (2*(fam_var^2))
  qst <- num_qst/dem_qst
  
  cva <- sqrt(add_var) / trait_mean
  
  print(paste0("Narrow-sense heritability: ", round(h2, 3)))
  print(paste0("Qst: ", round(qst, 3)))
  print(paste0("CVA: ", round(cva, 3)))

return(data.frame(trait = variable_name,
                  h2  = h2,
                  qst = qst,
                  cva = cva))
}

###########################################################
##################### FIGURES #############################
###########################################################

## Create theme for figures
theme_1 <- function(){ 
  
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box = 'horizontal',
    legend.box.just = "right",
    legend.margin = margin(5,5,5,5),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.key = element_rect(fill = NA),
    axis.text = element_text(size = 12),
    text = element_text(size = 14)
  )
}

## Create variable associated with replicated aesthetics
# Gradient figs
rep_geoms <- c(geom_smooth(aes(x = x,
                               y = predicted),
                           color = "#66a182",
                           size = 1,
                           se = F),
               geom_ribbon(aes(x = x,
                               ymin = predicted - std.error,
                               ymax = predicted + std.error),
                           fill = "#66a182",
                           alpha = 0.3))

# Transect figs
rep_geoms2 <- c(geom_smooth(
  aes(
    x = x,
    y = predicted,
    color = group),
  size = 1,
  se = F),
  geom_ribbon(aes(
    x = x,
    ymin = predicted - std.error,
    ymax = predicted + std.error,
    fill = group),
    alpha = 0.3),
  scale_colour_brewer(labels = c("Corridor",
                                 "Non-Corridor"),
                      palette = "Set2"),
  scale_fill_brewer(labels = c("Corridor",
                               "Non-Corridor"),
                    palette = "Set2"))



###########################################################
################## PERCENT CHANGE #########################
###########################################################
### Gradient/city_dist models
Calc_percent_change <- function(ggpredict_object){
  baseline_numbers <- ggpredict_object %>%
    dplyr::filter(., x == 0 | x == 70) %>%
    dplyr::select(c(1,2))
  
  urb_mean <- baseline_numbers[baseline_numbers$x == min(baseline_numbers$x),]$predicted
  
  rur_mean <- baseline_numbers[baseline_numbers$x == max(baseline_numbers$x),]$predicted
  
  percent_change <- ((urb_mean - rur_mean) / rur_mean) * 100
  print(baseline_numbers)
  return(paste0("Percent change, from rural to urban terminus: ", round(percent_change, 1), "%"))
}

### Urban subtransects/city_dist models
Calc_percent_change_urbsubs <- function(ggpredict_object){
  baseline_numbers <- ggpredict_object %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(mean = mean(predicted)) %>%
    dplyr::filter(., x == max(x) | x == min(x)) 
  
  urb_mean <- baseline_numbers[baseline_numbers$x == min(baseline_numbers$x),]$mean
  
  rur_mean <- baseline_numbers[baseline_numbers$x == max(baseline_numbers$x),]$mean
  
  percent_change <- ((urb_mean - rur_mean) / rur_mean) * 100
  print(baseline_numbers)
  return(paste0("Percent change, from suburban to urban terminus: ",
                round(percent_change, 1), "%"))
  
}
Calc_percent_change_urbsubs_intxn <- function(ggpredict_object){
  baseline <- ggpredict_object %>%
    dplyr::filter(., x == max(x) | x == min(x)) %>%
    dplyr::select(c(1,2,6))
  
  # percent change from suburban to urban terminus
  ## Corridor subtransect
  suburban_corridor_mean <- baseline %>%
    dplyr::filter(x == max(x) & group == "South") %>%
    dplyr::select("predicted") %>%
    as.numeric()
  
  urban_corridor_mean <- baseline %>%
    dplyr::filter(x == min(x) & group == "South") %>%
    dplyr::select("predicted") %>%
    as.numeric()
  
  PC_corr_suburbanTOurban <- round(
    (((urban_corridor_mean - suburban_corridor_mean) /
        suburban_corridor_mean) * 100),
    1)
  
  
  ## Non-corridor subtransect
  suburban_noncorridor_mean <- baseline %>%
    dplyr::filter(x == max(x) & group == "North") %>%
    dplyr::select("predicted") %>%
    as.numeric()
  
  urban_noncorridor_mean <- baseline %>%
    dplyr::filter(x == min(x) & group == "North") %>%
    dplyr::select("predicted") %>%
    as.numeric()
  
  PC_noncorr_suburbanTOurban <- round(
    (((urban_noncorridor_mean - suburban_noncorridor_mean) /
        suburban_noncorridor_mean) * 100),
    1)
  
  print(baseline)
  print(paste0("Percent change from suburban to urban terminus along corridor subtransect: ", PC_corr_suburbanTOurban, "%"))
  print(paste0("Percent change from suburban to urban terminus along non-corridor subtransect: ", PC_noncorr_suburbanTOurban, "%"))
}
Calc_percent_change_transects <- function(ggpredict_object){
  baseline <- ggpredict_object %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean = mean(predicted))
  
  corridor_mean <- baseline %>%
    dplyr::filter(group == "South") %>%
    dplyr::select("mean") %>%
    as.numeric()
  
  noncorridor_mean <- baseline %>%
    dplyr::filter(group == "North") %>%
    dplyr::select("mean") %>%
    as.numeric()
  
  PC_noncorrTOcorridor <- round(
    (((corridor_mean - noncorridor_mean) /
        noncorridor_mean) * 100),
    1)
  
  print(baseline)
  print(paste0("Percent change from non-corridor to corridor subtransect: ",
               PC_noncorrTOcorridor, "%"))
  
}
