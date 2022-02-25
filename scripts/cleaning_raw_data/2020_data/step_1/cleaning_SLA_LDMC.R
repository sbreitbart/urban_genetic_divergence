
# -------------------
# Load packages
#-------------------
library(dplyr)
library(here)



#-------------------
# Import data
#-------------------
# at start of season (Data Collection 1)- BINARY
sla_ldmc <- read.csv(
  here::here("./CommonGardenExperiment_2020Data/raw_data/SLA_DLMC/2020_Datacollection5_Leaves_SLA_DLMC_20210603.csv"), header=T, na.strings=c("NO PLANT", "none"), blank.lines.skip=TRUE) %>%
  as.data.frame()



# -------------------
# Clean data
#-------------------
# Remove rows without plants, empty columns-----
# Though 46 holes were dug per 21 rows of the field plot, each hole was not filled with a plant- ex. Row 1 has empty holes until plot column 13. Removing those empty spreadsheet rows as well as empty spreadsheet columns.
  
# weevil_1-----
  c <- is.na(sla_ldmc$Block) == TRUE
  c2 <- c(1:nrow(sla_ldmc))[c]
  sla_ldmc <- sla_ldmc[-c2,]
  names(sla_ldmc)[1]<-"Row"
  sla_ldmc <- sla_ldmc[!is.na(sla_ldmc$Population), ]
  
rm(c, c2)



# Make certain columns factors / check classes are correct-----
str(sla_ldmc)

# remove unimportant columns
sla_ldmc <- sla_ldmc[,-11]

# make these columns into factors
factor_cols <- c("Block", "Population", "Family", "Replicate", "Comment")
sla_ldmc[factor_cols] <- lapply(sla_ldmc[factor_cols], factor)
str(sla_ldmc)

# Make these cols into numeric
numeric_cols <- c(12, 15:18, 22)
sla_ldmc[numeric_cols] <- lapply(sla_ldmc[numeric_cols], as.character)
sla_ldmc[numeric_cols] <- lapply(sla_ldmc[numeric_cols], as.numeric)
str(sla_ldmc)


# Remove repeats as is done in Joining_annual_datasets script

repeats <- data.frame(Population  = factor(c(19, 23, 35, 35, 41, 41, 41)),
                      Family =      factor(c(5, 1, 4, 3, 1, 1, 1)),
                      Replicate =   factor(c(3, 5, 2, 1, 2, 3, 4))
)

str(repeats)

sla_ldmc %<>%
  anti_join(., repeats)

#-------------------
# Export to new csv
#-------------------
write.csv(sla_ldmc,
          here::here("./CommonGardenExperiment_2020Data/partially_cleaned_data/2020_sla_ldmc_partialclean.csv"))
