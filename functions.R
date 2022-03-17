# Functions for project


##### functions to create tidy anova tables #####
# GOALS:
# 1. Perform car::Anova() on each model in list
#   -if it's got an interaction, do type III first
#     -if interaxn p-val < 0.1, retain that result
#     -else, move on to type II
# 2. Save anova table w/appropriate SS type as new item in trait's sublist

# function that IDs if there is >= 1 sig interaction in the type III anova
ID_intxn <- function(model){
  model.df <- broom.mixed::tidy(car::Anova(model, type = "III"))
  intxn.df <- model.df[grep(":", model.df$term), "p.value"]
  sig.intxns <- sum(intxn.df <= 0.1)
  return(sig.intxns)
}

# Next, create best and alt models for individual lists of traits, then do this for the city_dist models, then urb_score models... put underlines in to separate Qs... export!
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



##### function for doing linear regression on multiple models
##### (used this for cardenolide functions)
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



##### function to create tidy ranova table #####
# for regular g/lmer models
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

# for boostrapping models
CreateRanovaOutput_bootstrap <- function(ranova_fam,
                                         ranova_pop,
                                         var_name){
  
  ranova.fam <- tidy(ranova_fam) %>%
    dplyr::filter(type == "PBtest") %>%
    dplyr::rename(Group = type) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "PBtest",
                                  "Family")) %>%
    dplyr::select(-c(2:3))
  
  ranova.pop <- tidy(ranova_pop) %>%
    dplyr::filter(type == "PBtest") %>%
    dplyr::rename(Group = type) %>%
    dplyr::mutate(Group = replace(Group,
                                  Group == "PBtest",
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


##### theme for figures #####
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



##### Export ANOVAs ##### 
anova_table_flx <- function(mod1) {
  
  flx_1 <-  car::Anova(mod1) %>% 
    tidy() %>%
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
    rename_at(1, ~ "Effect") %>%
    rename_at(2, ~ "Chi Square" ) %>%
    rename_at(4, ~ "P" ) %>%
    # dplyr::select(-3) %>%
    flextable()  %>%
    colformat_num(j = c(2,4), digits = 3) %>%
    flextable::bold(., ~ P  == "< 0.001" | P <= 0.05, ~ P, bold = TRUE) %>%
    flextable::italic(., ~ P > 0.05 & P <= 0.1, ~ P, italic = TRUE) %>%
    # set_caption(., caption = "Does Urbanization Affect Year 2 Flowering?") %>%
    # align(., i = 1, part = "header", align = "center") %>%
    # add_footer_lines(., paste("Model formula: ", paste0(toString(formula(mod1)[3])))) %>%
    # italic(., italic = TRUE, part = "footer") %>%
    align_text_col(., align = "left") %>%
    align_nottext_col(., align = "center") %>%
    autofit(., add_w = 0.1, add_h = 0.1) 
  
  
  # make filename a string (from symbol)
  # filename1 <- toString(formula(mod1)[[2]])
  
  # can't figure out how to remove space from filename so it's staying for now...
  path_out = here::here("./Figures_Tables/ANOVA_tables_images/ ")
  
  save_as_image(flx_1,
                gsub(" ", "",
                     paste(path_out,
                           # filename1,
                           print(substitute(mod1)),
                           "_ANOVA.png")))
  
}

# use _transf function if response var has been transformed
# use _binomial or _quantitative when I had to fit 2 extra models per var:
#  binomial and then, with positive values, quantitative
AIC_compare.city_dist <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         formula(full_model)[[2]],
                         "_city",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}
AIC_compare.city_dist_transf <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]][2]),
                         "_city",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}
AIC_compare.city_dist_transf_binomial <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]][2]),
                         "_city_binomial",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}
AIC_compare.city_dist_transf_quantitative <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]][2]),
                         "_city_quantitative",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}

AIC_compare.urb_score <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]]),
                         "_urbscore",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}
AIC_compare.urb_score_transf <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]][2]),
                         "_urbscore",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}
AIC_compare.urb_score_transf_binomial <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]][2]),
                         "_urbscore_binomial",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}
AIC_compare.urb_score_transf_quantitative <- function(full_model, ME_model) {
  
  data.frame(Model = c("Full model",
                       "Main effects only"),
             AIC = c(AIC(full_model),
                     AIC(ME_model))) %>%
    write.csv(gsub(" ", "",
                   paste(here::here("./Figures_Tables/ANOVA_tables_images/ "),
                         "AIC_comparison_",
                         toString(formula(full_model)[[2]][2]),
                         "_urbscore_quantitative",
                         # deparse(substitute(full_model)),
                         ".csv")))
  
}


# get name of models so I can later export all model names with their formulas-----
qq <- function(...) sapply(substitute({ ... })[-1], deparse)
