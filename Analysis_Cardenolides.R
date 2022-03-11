## -----------------------------------------------------------------
source("libraries.R")
source("functions.R")


## -----------------------------------------------------------------
# cardenolides per population (pooled)
cards <-  read.csv(here::here("./CommonGardenExperiment_2020Data/clean_data/2020_cardenolides_clean.csv")) %>%
  dplyr::select(., -1) 


## -----------------------------------------------------------------
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


# get names of response variables
cards_vars <- cards %>%
  dplyr::select(3,5,7,10) %>%
  names() %>%
  as.list()


## -----------------------------------------------------------------
city_dist_list <- lapply(cards_vars,
       DoLinearReg,
       predictor_var = "City_dist",
       input_data = cards)
names(city_dist_list) <- cards_vars
# Num 3 doesn't look normal... try transformation. Residuals look bimodal



## -----------------------------------------------------------------
DoLinearReg("X17.6_main", "City_dist", cards)
DoLinearReg("log(X17.6_main)", "City_dist", cards)
car::Anova(lm(log(X17.6_main) ~ City_dist, cards)) # still isn't near p = 0.05 so maybe not worth worrying about


## -----------------------------------------------------------------
urb_score_list <- lapply(cards_vars,
       DoLinearReg,
       predictor_var = "Urb_score",
       input_data = cards)
names(urb_score_list) <- cards_vars
# Num 3 doesn't look normal... try transformation. Looks bimodal


## -----------------------------------------------------------------
DoLinearReg("X17.6_main", "Urb_score", cards)
DoLinearReg("log(X17.6_main)", "Urb_score", cards)
car::Anova(lm(log(X17.6_main) ~ Urb_score, cards)) # now it's near p = 0.05 (it's 0.068)... so investigate further

