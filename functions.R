# Functions

# test for genotype x environment interactions
test_gxe <- function(regular_mod, gxe_mod){
  
  plot(DHARMa::simulateResiduals(gxe_mod))
  AIC(regular_mod, gxe_mod)
  
}



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
### assessing if variation among pops/fams varies with urbanization
#### for regular g/lmer models:
# filepath should look something like this:
# "./Defense_trait_analyses/Tables/Ranova/latex.html"
ranova_1step <- function(lmer_model, var_name){
  
  m1.ranova <- ranova(lmer_model) %>%
    as.data.frame() %>%
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
                                  "Block")) 
  
  
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
    dplyr::mutate(p = p/2) %>% # divide by 2 b/c 1-sided test
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
ranova_2step <- function(lmer_model1, var_name, filepath){
  
  # make table 1
  table1 <- ranova_1step(lmer_model1, var_name)
  
  # make table 2
  lmer_model2 <- update(lmer_model1, . ~ . + City_dist)
  table2 <- ranova_1step(lmer_model2, var_name)
  
  # make table 3
  lmer_model2_anova <- update(lmer_model2, . ~ ., REML = F)
  table3 <- tidy_anova(lmer_model2_anova) %>%
    dplyr::rename(Variable = 1) %>%
    dplyr::mutate(Variable = var_name) %>%
    dplyr::select(-Sites) %>%
    flextable() %>%
    merge_v(j = "Variable") %>% 
    flextable::compose(i = 1, j = 3, part = "header",
                       value = as_paragraph("χ", as_sup("2"))) %>%
    bold(i = ~ p <= 0.05, j = 4) %>%
    autofit()
  
  # make table 4
  lmer_model3 <- update(lmer_model1, . ~ . + Urb_score)
  table4 <- ranova_1step(lmer_model3, var_name)
  
  # make table 5
  lmer_model3_anova <- update(lmer_model3, . ~ ., REML = F)
  table5 <- tidy_anova(lmer_model3_anova) %>%
    dplyr::rename(Variable = 1) %>%
    dplyr::mutate(Variable = var_name) %>%
    dplyr::select(-Sites) %>%
    flextable() %>%
    merge_v(j = "Variable") %>% 
    flextable::compose(i = 1, j = 3, part = "header",
                       value = as_paragraph("χ", as_sup("2"))) %>%
    bold(i = ~ p <= 0.05, j = 4) %>%
    autofit()
  
  mod_formula1 <- paste(deparse(formula(lmer_model1)), collapse = "") %>%
    unlist() 
  
  mod_formula2 <- paste(deparse(formula(lmer_model2)), collapse = "") %>%
    unlist()
  
  mod_formula3 <- paste(deparse(formula(lmer_model3)), collapse = "") %>%
    unlist()
  
  # Export
  word_export <- read_docx()
  
  body_add_par(word_export, value = "Table 1: Test for variance among families and populations")
  body_add_par(word_export, value = paste("Model:", mod_formula1))
  body_add_flextable(word_export, table1)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 2: Assess how much variance is explained by urbanization")
  body_add_par(word_export, value = "Urbanization = Distance to the City Center")
  body_add_par(word_export, value = paste("Model:", mod_formula2))
  body_add_flextable(word_export, table2)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 3: Quantify variance explained by urbanization")
  body_add_flextable(word_export, table3)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 4: Assess how much variance is explained by urbanization")
  body_add_par(word_export, value = "Urbanization = Urbanization Score")
  body_add_par(word_export, value = paste("Model:", mod_formula3))
  body_add_flextable(word_export, table4)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 5: Quantify variance explained by urbanization")
  body_add_flextable(word_export, table5)
  
  print(word_export, here::here(filepath))
  
}
ranova_transect <- function(lmer_model_without_urb, var_name, filepath){
  
  # make table 1
  lmer_model2 <- update(lmer_model_without_urb, . ~ . + Transect_ID*City_dist)
  table2 <- ranova_1step(lmer_model2, var_name)

  
  # make table 2
  lmer_model2_anova <- update(lmer_model2, . ~ ., REML = F)
  table3 <- tidy_anova(lmer_model2_anova) %>%
    dplyr::rename(Variable = 1) %>%
    dplyr::mutate(Variable = var_name) %>%
    dplyr::select(-Sites) %>%
    flextable() %>%
    merge_v(j = "Variable") %>% 
    flextable::compose(i = 1, j = 3, part = "header",
                       value = as_paragraph("χ", as_sup("2"))) %>%
    bold(i = ~ p <= 0.05, j = 4) %>%
    autofit()
  
  # make table 3
  lmer_model3 <- update(lmer_model_without_urb, . ~ . + Transect_ID*Urb_score)
  table4 <- ranova_1step(lmer_model3, var_name)
  
  # make table 4
  lmer_model3_anova <- update(lmer_model3, . ~ ., REML = F)
  table5 <- tidy_anova(lmer_model3_anova) %>%
    dplyr::rename(Variable = 1) %>%
    dplyr::mutate(Variable = var_name) %>%
    dplyr::select(-Sites) %>%
    flextable() %>%
    merge_v(j = "Variable") %>% 
    flextable::compose(i = 1, j = 3, part = "header",
                       value = as_paragraph("χ", as_sup("2"))) %>%
    bold(i = ~ p <= 0.05, j = 4) %>%
    autofit()
  
  
  mod_formula2 <- paste(deparse(formula(lmer_model2)), collapse = "") %>%
    unlist()
  
  mod_formula3 <- paste(deparse(formula(lmer_model3)), collapse = "") %>%
    unlist()
  
  
  # Export
  word_export <- read_docx()
  
  body_add_par(word_export, value = "Table 1: Assess how much variance is explained by transect")
  body_add_par(word_export, value = "Urbanization = Distance to the City Center")
  body_add_par(word_export, value = paste("Model:", mod_formula2))
  body_add_flextable(word_export, table2)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 2: Quantify variance explained by transect")
  body_add_flextable(word_export, table3)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 3: Assess how much variance is explained by transect")
  body_add_par(word_export, value = "Urbanization = Urbanization Score")
  body_add_par(word_export, value = paste("Model:", mod_formula3))
  body_add_flextable(word_export, table4)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 4: Quantify variance explained by transect")
  body_add_flextable(word_export, table5)
  
  print(word_export, here::here(filepath))
}

#### for boostrapping models:
##### all 4 of these intermediary functions needed for pb_ranova_2step
CreateRanovaOutput_bootstrap <- function(ranova_fam,
                                         ranova_pop,
                                         var_name ,
                                         PVE_pop,
                                         PVE_fam,
                                         variance_pop,
                                         variance_fam
                                         ){
  
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
    dplyr::mutate(p = p/2) %>%
    dplyr::mutate(p = replace(p,
                              p < 0.001,
                              "<0.001")) %>%
    dplyr::mutate(Variable = noquote(var_name)) %>% 
    dplyr::mutate(Variance = c(variance_fam,
                               variance_pop)) %>%
    dplyr::mutate(PVE = c(PVE_fam, PVE_pop)) %>%
    dplyr::select(c(3,1,4,5,2)) %>%
    flextable() %>%
    merge_at(j = 1) %>%
    fix_border_issues() %>%
    bold(i = ~ p <= 0.05, j = 5) %>%
    autofit()
  
  return(tab1)
  }

PVE_testing <- function(ranova_obj){
  if (isTRUE(performance::check_singularity(ranova_obj)) |
      isTRUE(as.numeric(length(get_pve(ranova_obj))) == 0)) {
    return("NA")
  }
  else {
    return((get_pve(ranova_obj)))}
}

get_variance <- function(ranova_obj){
  if (isTRUE(performance::check_singularity(ranova_obj)) |
      isTRUE(as.numeric(length(get_pve(ranova_obj))) == 0)) {
    return("NA")
  }
  else {
    return(as.numeric(insight::get_variance_random(ranova_obj)) %>%
              round(digits = 3))
           }
}

pb_ranova_1step <- function(full_model_forstep, trait_name){
  
  test_pop <- update(full_model_forstep, .~. - (1|Population))
  test_fam <- update(full_model_forstep, .~. - (1|Population:Fam_uniq))
  
  ranova.pop <- PBmodcomp(full_model_forstep,
                          test_pop,
                          nsim = 1000, 
                          cl = makeCluster(6)) 
  
  ranova.fam <- PBmodcomp(full_model_forstep,
                          test_fam,
                          nsim = 1000, 
                          cl = makeCluster(6)) 
  
  
  PVE_pop <- PVE_testing(test_pop)
  PVE_fam <- PVE_testing(test_fam)
  
  variance_pop <- get_variance(test_pop)
  variance_fam <- get_variance(test_fam)
  
  # merge Pop and Fam ranovas
  table1 <- CreateRanovaOutput_bootstrap(ranova.fam,
                                         ranova.pop,
                                         trait_name,
                                         PVE_pop,
                                         PVE_fam,
                                         variance_pop,
                                         variance_fam)
  
  return(table1)
  
}
make_tables3_5 <- function(glmer_mod, trait_name){
  
  table3_5 <- tidy_anova(glmer_mod) %>%
    dplyr::rename(Variable = 1) %>%
    dplyr::mutate(Variable = trait_name) %>%
    dplyr::select(-Sites) %>%
    flextable() %>%
    merge_v(j = "Variable") %>% 
    flextable::compose(i = 1, j = 3, part = "header",
                       value = as_paragraph("χ", as_sup("2"))) %>%
    bold(i = ~ p <= 0.05, j = 4) %>%
    autofit()
  
  return(table3_5)
}

####### get percent variance explained for bootstrapped models
####### calculating it as random / random + resid var
get_pve <- function(full_model){
  random_var <- as.numeric(insight::get_variance_random(full_model))
  resid_var <- as.numeric(insight::get_variance_residual(full_model))
  PVE <- round(
    (random_var*100 /
       ( random_var + resid_var)),
    3)
  return(PVE)
}

##### perform ranova for Q1 (all sites included)
pb_ranova_2step <- function(full_model, trait_name, filepath){
  
  # CREATE TABLE 1
  table1 <- pb_ranova_1step(full_model, trait_name)
  mod_formula1 <- paste(deparse(formula(full_model)), collapse = "") %>%
    unlist()
  
  
  # CREATE TABLE 2
  glmer_model2 <- update(full_model, . ~ . + City_dist)
  
  table2 <- pb_ranova_1step(glmer_model2, trait_name)
  
  mod_formula2 <- paste(deparse(formula(glmer_model2)), collapse = "") %>%
    unlist()
  
  # CREATE TABLE 3
  table3 <- make_tables3_5(glmer_model2, trait_name)
  
  # CREATE TABLE 4
  glmer_model3 <- update(full_model, . ~ . + Urb_score)
  
  table4 <- pb_ranova_1step(glmer_model3, trait_name)
  
  mod_formula3 <- paste(deparse(formula(glmer_model3)), collapse = "") %>%
    unlist()
  

  # MAKE TABLE 5
  table5 <- make_tables3_5(glmer_model3, trait_name)
  
  
  # Export
  word_export <- read_docx()
  
  body_add_par(word_export, value = "Table 1: Test for variance among families and populations")
  body_add_par(word_export, value = paste("Model:", mod_formula1))
  body_add_flextable(word_export, table1)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 2: Assess how much variance is explained by urbanization")
  body_add_par(word_export, value = "Urbanization = Distance to the City Center")
  body_add_par(word_export, value = paste("Model:", mod_formula2))
  body_add_flextable(word_export, table2)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 3: Quantify variance explained by urbanization")
  body_add_flextable(word_export, table3)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 4: Assess how much variance is explained by urbanization")
  body_add_par(word_export, value = "Urbanization = Urbanization Score")
  body_add_par(word_export, value = paste("Model:", mod_formula3))
  body_add_flextable(word_export, table4)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 5: Quantify variance explained by urbanization")
  body_add_flextable(word_export, table5)
  
  print(word_export, here::here(filepath))
}

##### perform ranova for Q2 (only urban sites included)
pb_ranova_transects <- function(glmer_model_without_urb, trait_name, filepath){
  
  # CREATE TABLE 1
  glmer_model2 <- update(glmer_model_without_urb, . ~ . + Transect_ID*City_dist)
  table2 <- pb_ranova_1step(glmer_model2, trait_name)
  
  mod_formula2 <- paste(deparse(formula(glmer_model2)), collapse = "") %>%
    unlist()
  
  # CREATE TABLE 2
  table3 <- make_tables3_5(glmer_model2, trait_name)
  
  # CREATE TABLE 3
  glmer_model3 <- update(glmer_model_without_urb, . ~ . + Transect_ID*Urb_score)
  table4 <- pb_ranova_1step(glmer_model3, trait_name)
  
  mod_formula3 <- paste(deparse(formula(glmer_model3)), collapse = "") %>%
    unlist()
  
  
  # MAKE TABLE 4
  table5 <- make_tables3_5(glmer_model3, trait_name)
  
  
  # Export
  word_export <- read_docx()
  
  body_add_par(word_export, value = "Table 1: Assess how much variance is explained by transect")
  body_add_par(word_export, value = "Urbanization = Distance to the City Center")
  body_add_par(word_export, value = paste("Model:", mod_formula2))
  body_add_flextable(word_export, table2)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 2: Quantify variance explained by transect")
  body_add_flextable(word_export, table3)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 3: Assess how much variance is explained by transect")
  body_add_par(word_export, value = "Urbanization = Urbanization Score")
  body_add_par(word_export, value = paste("Model:", mod_formula3))
  body_add_flextable(word_export, table4)
  
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  
  body_add_par(word_export, value = "Table 4: Quantify variance explained by transect")
  body_add_flextable(word_export, table5)
  
  print(word_export, here::here(filepath))
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
