# Functions for project

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
