# load libraries


librarian::shelf(broom       ,
                broom.mixed ,
                car         ,
              #  coxme       ,
                devtools    ,
                # DHARMa      ,
                flextable   ,
                # forcats     ,
                # geosphere   ,
                ggeffects   ,
                # ggpubr      ,
                glmmTMB     ,
                # grid        ,
                # gt          ,
                # here        ,
                Hmisc       ,
                knitr       ,
                lme4        ,
                lmerTest    ,
                lsmeans     ,
                MASS        ,
                magrittr    ,
                multcomp    ,
                MuMIn       ,
                nlme        ,
                officer     ,
                pbkrtest    ,
                plyr        ,
                # png         ,
                # purrr       ,
                scales      ,
                # stringi     ,
                # survival    ,
                tidyverse  # ,
                # vctrs       ,
                # vegan
)

# load ranova functions
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



# read in data
flowering <- read.csv(here::here("./Joined_annual_data/flowering.csv"), na.strings=c("NO PLANT", "none", "NA"), blank.lines.skip=TRUE, header=TRUE, sep=",") %>%
  dplyr::select(., -1)%>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Pop_ID", "Patch_ID", "Transect_ID", "Urb_Rur")), as.character) %>%
  dplyr::mutate_at(vars(c("Population", "Family", "Replicate", "Block", "Year", "Pop_ID", "Patch_ID", "Transect_ID", "Urb_Rur")), as.factor) %>%
  dplyr::mutate_at(vars(c(Date_oldest_inflor_flower,
                          Date_youngest_inflor_flower,
                          Date_first_follicle_2in)),
                   as.Date,
                   format = '%m/%d/%Y') %>%
  dplyr::mutate(flowering_time = Date_youngest_inflor_flower-Date_oldest_inflor_flower + 1) %>% # if I don't add 1, then it will say some plants flowered for 0 days
  relocate(flowering_time,
           .before = Date_first_follicle_2in) %>%
  dplyr::mutate(Julian_oldest_inflor = as.numeric(format(Date_oldest_inflor_flower, "%j")),
                Julian_first_follicle = as.numeric(format(Date_first_follicle_2in, "%j"))) %>%
  dplyr::mutate(Fam_uniq = as.factor(paste0(Population, "_", Family)))

# RANOVA
# Flowering success-----

# take out fam
full <- glmer(Flowered ~
                Block +
                Year +
                (1|Population) +
                City_dist,
              data = reproduc,
              family = binomial(link = "logit"),
              na.action = na.exclude # THIS LINE IS IMPORTANT
              # ,control=glmerControl(optimizer="bobyqa",
              #                         optCtrl=list(maxfun=2e5))
)

reduced <- update(full, .~. - City_dist)

flsucc_ranova.pop <- PBmodcomp(full, reduced,
                               nsim = 1000,
                               cl = makeCluster(rep("localhost", 8)))



# take out pop
full <- glmer(Flowered ~
                Block +
                Year +
                (1|Fam_uniq) +
                City_dist,
              data = reproduc,
              family = binomial(link = "logit"),
              nAGQ = 0, # model won't converge otherwise. I tested the difference nAGQ = 0 vs the default (>0) makes in the model above and the difference was negligible: the p-value changed by 0.003 so I think it's ok to use here, since the p-values are nowhere near 0.1 or lower.
              na.action = na.exclude # THIS LINE IS IMPORTANT
              ,control=glmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5))
)

reduced <- update(full, .~. - City_dist)

flsize_ranova.fam <- PBmodcomp(full, reduced,
                               nsim = 1000,
                               cl = makeCluster(rep("localhost", 8)))


# merge Pop and Fam ranovas
flsize_ranova1 <- CreateRanovaOutput_bootstrap(
  flsize_ranova.fam, 
  flsize_ranova.pop,
  "Flower size") %T>%
  flextable::save_as_html("flower_size.html")