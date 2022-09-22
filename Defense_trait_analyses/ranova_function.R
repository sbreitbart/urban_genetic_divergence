pb_ranova <- function(full_model, trait_name, filepath){
  
  test_pop <- update(full_model, .~. - (1|Population))
  test_fam <- update(full_model, .~. - (1|Population/Family))
  
  ranova.pop <- PBmodcomp(full,
                          test_pop,
                          nsim = 1, 
                          cl = makeCluster(6)) 
  
  ranova.fam <- PBmodcomp(full,
                          test_fam,
                          nsim = 1, 
                          cl = makeCluster(6)) 
  

  # PVE_testing <- function(ranova_obj){      
  #   if (performance::check_singularity((ranova_obj)) == TRUE |
  #       str(get_pve(ranova_obj)) == "num(0)") {
  #     return("NA")
  #   } else {
  #     return((get_pve(ranova_obj)))}
  # }
  # 
  # PVE_pop <- PVE_testing(test_pop)
  # PVE_fam <- PVE_testing(test_fam)
  
  PVE_pop <- "NA_for_now"
  PVE_fam <- "NA_for_now"
  
  # merge Pop and Fam ranovas
  CreateRanovaOutput_bootstrap(ranova.fam,
                               ranova.pop,
                               trait_name,
                               PVE_pop,
                               PVE_fam) %T>%
    flextable::save_as_docx(path = here::here(filepath))
  
  
}

# pb_ranova(full,
#           "herbivory test",
#           "./Defense_trait_analyses/Tables/Ranova/TEST.docx")
# 
# # I cannot figure out how to distinguish the PVEs w/singular mods from non-sing mods

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
      dplyr::mutate(p = p/2) %>%
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
    # 
    # flextable::save_as_docx(tab1,
    #                         tab2,
    #                         path = here::here(filepath))
    

}

ranova_2step <- function(lmer_model1, var_name, filepath){
  
  table1 <- ranova_1step(lmer_model1, var_name)
  lmer_model2 <- update(lmer_model1, . ~ . + City_dist)
  table2 <- ranova_1step(lmer_model2, var_name)
  
  
  word_export <- read_docx()
  body_add_par(word_export, value = "Table 1: Without urbanization")
  body_add_par(word_export, value = paste("Model:", deparse(formula(lmer_model1))))
  body_add_flextable(word_export, table1)
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "")
  body_add_par(word_export, value = "Table 2: With urbanization")
  body_add_par(word_export, value = paste("Model:", deparse(formula(lmer_model2))))
  body_add_flextable(word_export, table2)
  print(word_export, here::here(filepath))
  
}



# latex trial
# latex_ranova_mod1 <- lmer(Latex_weight_mg^(1/3) ~
#                             Block +
#                             (1|Population/Family),
#                           data = latex,
#                           REML = T)
# 
# performance::check_model(latex_ranova_mod1)
# 
# mod2 <- update(latex_ranova_mod1, .~. + City_dist, REML = F)
# car::Anova(mod2)




ranova_2step(latex_ranova_mod1,
             "Latex",
             "./Defense_trait_analyses/Tables/Ranova/latextest.docx")

# herbivory trial

herbiv_e_ranova_mod1 <- lmer(Herbivory_mean_early ~
                                     Block +
                                     (1|Population/Family),
                                   data = herbivory %>%
                                     dplyr::filter(
                                       Herbivory_mean_early_binary == 1 &
                                         Year == 2021),
                                   REML = T)

performance::check_model(herbiv_e_ranova_mod1)

ranova_2step(herbiv_e_quant_ranova_mod1,
             "Herbivory, before flowering, quantitative: 2021",
             "./Defense_trait_analyses/Tables/Ranova/herbivtest.docx")

