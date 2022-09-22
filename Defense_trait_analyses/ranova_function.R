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

