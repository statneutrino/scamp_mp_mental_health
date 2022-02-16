library(tidyverse)
library(readxl)

###### IMPORT ########

### Load Psytools data files
psytools_bl_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/Psytools Baseline Wide Response 20220209.csv"
psytools_f1_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/Psytools F1 Wide Response 20220209.csv"

psytools_bl <- read.csv(psytools_bl_path)
psytools_f1 <- read.csv(psytools_f1_path)

# Read in scamp_extract_42 key table first two columns (Psytools UID ONLY)
extract_42_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/SX042_SCAMP_DATA_EXTRACT.csv"
sx_42 <- read.csv(extract_42_path) %>%
  mutate(SX_LINK = case_when(
    !is.na(PSYTOOLS_ID_BL) ~ PSYTOOLS_ID_BL,
    !is.na(PSYTOOLS_ID_F1) ~ PSYTOOLS_ID_F1,
    TRUE ~ NA_character_
  ))

# Read in IMD data (scamp)
imd_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/SX040 Scamp Data Extract 20210521.csv"
imd <- read.csv(imd_path)

### Load analysis-relevant code book
codebook <- read_excel("./docs/codebook.xlsx")

###### LINKAGE #######

### Select relevant column names for baseline
psytool_vars <- codebook %>%
  filter(derived == FALSE & followup == TRUE) %>%
  pull(variable_key) %>%
  toupper()

psytools_bl <- psytools_bl %>% 
  select(SCAMP_ID, PSYTOOLS_UID, BATTERYVERSION, all_of(psytool_vars)) %>%
  rename_with(~ paste(., "BL", sep = "_"))

### Select relevant column names for follow-up
psytool_vars <- codebook %>%
  filter(derived == FALSE) %>%
  pull(variable_key) %>%
  toupper()

psytools_f1 <- psytools_f1 %>% 
  select(SCAMP_ID,PSYTOOLS_UID, BATTERYVERSION, all_of(psytool_vars)) %>%
  rename_with(~ paste(., "F1", sep = "_"))

### Check for duplicates
psytools_bl$PSYTOOLS_UID_BL %>% duplicated() %>% sum()
psytools_f1$PSYTOOLS_UID_F1 %>% duplicated() %>% sum()

### Merge all psytools into wideformat using SCAMP_ID
psytools <- psytools_bl %>%
  full_join(
    y = psytools_f1,
    by = c("SCAMP_ID_BL" = "SCAMP_ID_F1"),
    na_matches = "never") %>%
  rename(SCAMP_ID = SCAMP_ID_BL) %>%
  mutate(SX_LINK = case_when(
    !is.na(PSYTOOLS_UID_BL) ~ PSYTOOLS_UID_BL,
    !is.na(PSYTOOLS_UID_F1) ~ PSYTOOLS_UID_F1,
    TRUE ~ NA_character_
  )) %>%
  select(SCAMP_ID, PSYTOOLS_UID_BL, BATTERYVERSION_BL, PSYTOOLS_UID_F1, 
         BATTERYVERSION_F1, SX_LINK, everything())


# psytools_bl <- psytools_bl %>%
#   left_join(
#     y = sx_42 %>% select(PSYTOOLS_ID_BL, SX_42_UUID), 
#     by = c("PSYTOOLS_UID_BL" = "PSYTOOLS_ID_BL"),
#     na_matches = "never")
# 
# psytools_f1 <- psytools_f1 %>%
#   left_join(
#     y = sx_42 %>% select(PSYTOOLS_ID_F1, SX_42_UUID), 
#     by = c("PSYTOOLS_UID_F1" = "PSYTOOLS_ID_F1"),
#     na_matches = "never")

### Now join SX_42 relevant categories
sx_42_link <- sx_42 %>% 
  select(SX_LINK, SCHOOL_ID_BL, SCHOOL_ID_F1, POSTCODE_BL,  POSTCODE_F1,
 ETHNICITY, SEX, AGE_BL, AGE_F1, NSSEC_BL, NSSEC_F1, MU_BL, MU_F1, FU_BL, FU_F1, FL_BL, FL_F1 , 
 ETS_BL,  ETS_F1,  smoking_BL,   smoking_F1,   cannabis_BL,  cannabis_F1,
 smoke,   Religion_F1,  height_BL, height_F1,  weight_BL,  weight_F1)
psytools <- psytools %>%
  left_join(sx_42_link, by="SX_LINK", na_matches = "never")

### Link IMD and schools data
imd_link <- imd %>%
  select(SCAMP_ID, GENDER, SCHOOL_IND_F1, IMD_DECILE_F1, ETHNICITY_F1)
psytools <- psytools %>%
  left_join(imd_link, by="SCAMP_ID", na_matches = "never")

write.csv(psytools, "./data/psytools_school.csv", row.names=FALSE)
  
