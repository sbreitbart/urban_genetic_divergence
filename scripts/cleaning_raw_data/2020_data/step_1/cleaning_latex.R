#-------------------
# Import data
##-------------------
latex <- read.csv(here::here("./data/CommonGardenExperiment_2020Data/raw_data/Latex_Cardenolides/Latex.csv"),
                  header=T, na.strings=c("NO PLANT", "none", ""),
                  blank.lines.skip=TRUE) %>%
  as.data.frame()


##-------------------
# Clean data
#-------------------

str(latex)

# Remove rows without latex weight values
latex %<>%
  dplyr::rename("Row" = 1) %>%
  dplyr::select(-Dead) %>%
  dplyr::filter(!is.na(Filter_paper_and_tube_weight_mg)) %>%
  dplyr::filter(is.na(Latex_not_collected)) %>%
  dplyr::filter(Latex_weight_mg != "NA") %>%
  dplyr::filter(Latex_and_filter_paper_and_tube_weight_mg != "NA") %>%
  dplyr::mutate_at(vars(Latex_and_filter_paper_and_tube_weight_mg, Latex_weight_mg), as.numeric) %>%
  dplyr::mutate_at(vars(3:6, 8:10), as.factor)

str(latex)

# Remove repeats as is done in Joining_annual_datasets script

repeats <- data.frame(Population  = factor(c(19, 23, 35, 35, 41, 41, 41)),
                      Family =      factor(c(5, 1, 4, 3, 1, 1, 1)),
                      Replicate =   factor(c(3, 5, 2, 1, 2, 3, 4))
)

str(repeats)

latex %<>%
  anti_join(., repeats)

#-------------------
# Export to new csv
#-------------------
write.csv(latex,
          here::here("./data/CommonGardenExperiment_2020Data/partially_cleaned_data/2020_latex_partialclean.csv"))
