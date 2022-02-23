library(tidyverse)
library(readxl)

# Read in Glasgow data
glasgow_path <- "D:/share/scamp/Data analysis/Glasgow Data/Data v1.2/Biomarker_PDS_Merged_2020_11_04.csv"
glasgow <- read.csv(glasgow_path) %>%
  filter(!is.na(GLASGOW_ID)) %>%
  .[-140,] # manually remove duplicate

# Read in UIDs
uid_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/SX050_Scamp_Data_Extract.csv"
key_table <- read.csv(uid_path)

# Read in AS Psytools main data (MP exposure, MH and covariate information)
source("./scripts/2_variable_processing.R")
scamp_f1 <- psytools %>%
  filter(!is.na(PSYTOOLS_UID_F1)) %>%
  filter(!is.na(phq_sum) & !is.na(gad_sum)) %>%
  as.data.frame()

## READ IN PROCESSED PDS
pds<- read.csv("./data/pds_processed_2022_02_20.csv")

## First link SCAMP data with Glasgow UID (and biozone and Psytools UIDs for Qual Check)
scamp_glasgow_keys <- key_table %>%
  select(SCAMP_ID, GLASGOW_ID, BIOZONE_ID_BL, BIOZONE_ID_FU, PM_STUDY_ID_1, PM_STUDY_ID_2, PSYTOOLS_UUID_BL, PSYTOOLS_UUID_F1) %>%
  distinct() # remove duplicated rooms

## LINK EVERYTHING to scamp_f1 to create scamp_puberty
# Left join scamp_data with scamp_glasgow_keys matching sunique_id with SCAMP_ID
scamp_puberty <- scamp_f1 %>%
  left_join(scamp_glasgow_keys, by = c("SCAMP_ID"), na_matches = "never")

# Now left-join Glasgow Data BIOZONE data ONLY
scamp_puberty <- scamp_puberty %>%
  left_join(glasgow, by = "GLASGOW_ID", na_matches = "never") %>%
  select(-c(PDS_1:PDS_4)) %>%
  select(-c(Survey_Date_1:Q44_4)) %>%
  # LEFT-JOIN PDS data by SCAMP_ID
  left_join(pds, by = c("SCAMP_ID" = "Scamp_ID"), na_matches = "never")

#write.csv(scamp_puberty, "./data/SCAMP_puberty_22_02_20.csv", row.names = FALSE)




### Estimate sample sizes
# scamp_puberty %>%
#   group_by(sex_bio, saliva_assay_result_BL, saliva_assay_result_F1) %>%
#   summarise(n = n()) %>%
#   as.data.frame() %>%
#   replace(is.na(.), "no biozone data")
# 
# scamp_puberty %>%
#   group_by(sex_bio, urine_assay_result_BL, urine_assay_result_F1) %>%
#   summarise(n = n()) %>%
#   as.data.frame() %>%
#   replace(is.na(.), "no biozone data")
# 
# scamp_puberty %>%
#   group_by(PDS_SEX, PDS_1) %>%
#   summarise(n = n()) %>%
#   as.data.frame() %>%
#   replace(is.na(.), "no pds data")
