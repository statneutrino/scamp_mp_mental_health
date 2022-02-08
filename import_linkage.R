library(tidyverse)
library(readxl)

###### IMPORT ########

### Load Psytools data files
psytools_bl_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/Psytools Baseline Wide Response 20210216.xlsx"
psytools_f1_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/Psytools F1 Wide Response 20210216.xlsx"

psytools_bl <- read_excel(psytools_bl_path)
psytools_f1 <- read_excel(psytools_f1_path)

# Read in scamp_extract_42 key table first two columns (Psytools UID ONLY)
extract_42_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/SX042_SCAMP_DATA_EXTRACT.csv"
sx_42 <- read.csv(extract_42_path)
N <- nrow(sx_42)
sx_42$SX_42_UUID <- uuid::UUIDgenerate(use.time=NA, n=N)


### Load analysis-relevant code book
codebook <- read_excel("./docs/codebook.xlsx")

###### LINKAGE #######

### Select relevant column names for baseline
psytool_vars <- codebook %>%
  filter(derived == FALSE & followup == TRUE) %>%
  pull(variable_key) %>%
  toupper()

psytools_bl <- psytools_bl %>% 
  select(PSYTOOLS_UID, BATTERYVERSION, psytool_vars) %>%
  rename_with(~ paste(., "BL", sep = "_"))

### Select relevant column names for follow-up
psytool_vars <- codebook %>%
  filter(derived == FALSE) %>%
  pull(variable_key) %>%
  toupper()

psytools_f1 <- psytools_f1 %>% 
  select(PSYTOOLS_UID, BATTERYVERSION, psytool_vars) %>%
  rename_with(~ paste(., "F1", sep = "_"))

### Check for duplicates
psytools_bl$PSYTOOLS_UID_BL %>% duplicated() %>% sum()
psytools_f1$PSYTOOLS_UID_F1 %>% duplicated() %>% sum()

### Merge all psytools into wideformat using sx_42
psytools_bl <- psytools_bl %>%
  left_join(
    y = sx_42 %>% select(PSYTOOLS_ID_BL, SX_42_UUID), 
    by = c("PSYTOOLS_UID_BL" = "PSYTOOLS_ID_BL"),
    na_matches = "never")

psytools_f1 <- psytools_f1 %>%
  left_join(
    y = sx_42 %>% select(PSYTOOLS_ID_F1, SX_42_UUID), 
    by = c("PSYTOOLS_UID_F1" = "PSYTOOLS_ID_F1"),
    na_matches = "never")

psytools <- full_join(psytools_bl, psytools_f1, by="SX_42_UUID", na_matches = "never") %>%
  select(SX_42_UUID, PSYTOOLS_UID_BL, BATTERYVERSION_BL, PSYTOOLS_UID_F1, BATTERYVERSION_F1, everything())

### Now join SX_42 relevant categories
sx_42_link <- sx_42 %>% 
  select(SX_42_UUID, SCHOOL_ID_BL, SCHOOL_ID_F1, POSTCODE_BL,  POSTCODE_F1,
 ETHNICITY, SEX, AGE_BL, AGE_F1, NSSEC_BL, NSSEC_F1, MU_BL, MU_F1, FU_BL, FU_F1, FL_BL, FL_F1 , 
 ETS_BL,  ETS_F1,  smoking_BL,   smoking_F1,   cannabis_BL,  cannabis_F1,
 smoke,   Religion_F1,  height_BL, height_F1,  weight_BL,  weight_F1,  
 BMI.BL,  BMI.F1)
psytools <- psytools %>%
  left_join(sx_42_link, by = "SX_42_UUID", na_matches = "never")

write.csv(psytools, "./data/psytools_school.csv", row.names=FALSE)
  
