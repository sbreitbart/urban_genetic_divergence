# Import data-----
transf <- read.csv(here::here(
  "./scripts/data_transf_families/data_transformations.csv")) %>%
  dplyr::rename("Trait" = 1)

fams <- read.csv(here::here(
  "./scripts/data_transf_families/data_families.csv")) %>%
  dplyr::rename("Trait" = 1)

# Load libraries-----
# source("libraries.R")

# Create data transformations table-----
transf %>%
  flextable() %>%
  ftExtra::separate_header() %>%
  align(j = 2:5, align = "center", part = "body") %>%
  merge_h(i = 1, part = "header") %>%
  align(i = 2, align = "center", part = "header") %>%
  hline(border = NULL, part = "body") %>%
  vline(j = 3, part = "all") %>%
  flextable::compose(i = 1, j = 2, part = "header",
                     value = as_paragraph("Distance")) %>%
  flextable::compose(i = 1, j = 4, part = "header",
                     value = as_paragraph("Urbanization Score")) %>%
  flextable::compose(i = 1, j = 1, part = "header",
                     value = as_paragraph(" ")) %>%
  flextable::compose(i = 2, j = c(2,4), part = "header",
                     value = as_paragraph("All Populations")) %>%
  flextable::compose(i = 2, j = c(3,5), part = "header",
                     value = as_paragraph("Urban Populations")) %>%
  flextable::compose(i = c(1,2,7,16), j = c(2:5), part = "body",
                     value = as_paragraph("x", as_sup("1/3"))) %>%
  flextable::compose(i = c(3, 27), j = c(2:5), part = "body",
                     value = as_paragraph("x", as_sup("1/2"))) %>%
  flextable::compose(i = c(25), j = c(4), part = "body",
                     value = as_paragraph("x", as_sup("2"))) %>%
#  padding(padding = 3) %>%
  fix_border_issues(part = "all") %>%
  autofit() %>%
  save_as_docx(path = here::here("./Figures_Tables/data_tr_fams/data_distributions.docx"))


# Create model families table-----
fams %>%
  flextable() %>%
  ftExtra::separate_header() %>%
  align(j = 2:5, align = "center", part = "body") %>%
  merge_h(i = 1, part = "header") %>%
  align(i = 2, align = "center", part = "header") %>%
  hline(border = NULL, part = "body") %>%
  vline(j = 3, part = "all") %>%
  flextable::compose(i = 1, j = 2, part = "header",
                     value = as_paragraph("Distance")) %>%
  flextable::compose(i = 1, j = 4, part = "header",
                     value = as_paragraph("Urbanization Score")) %>%
  flextable::compose(i = 1, j = 1, part = "header",
                     value = as_paragraph(" ")) %>%
  flextable::compose(i = 2, j = c(2,4), part = "header",
                     value = as_paragraph("All Populations")) %>%
  flextable::compose(i = 2, j = c(3,5), part = "header",
                     value = as_paragraph("Urban Populations")) %>%
  fix_border_issues(part = "all") %>%
  autofit() %>%
  save_as_docx(path = here::here("./Figures_Tables/data_tr_fams/data_fams.docx"))
