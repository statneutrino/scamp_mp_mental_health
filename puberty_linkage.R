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

# Read in exposure and cognitive data
scamp_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/exposures and outcomes data.csv"
scamp_df <- read.csv(scamp_path)

#Import Home & Parent Questionnaire
home_quests_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/SX012 Scamp Data Extract V2 20200921.csv"
home_quests <- read.csv(home_quests_path, fileEncoding = "UTF-8-BOM")

###### Clean home questionnaire dataset before linkage

# Function to convert Age.Month Format
convert_age <- function(age.months){
  for (i in seq_along(age.months)){
    if (!grepl("\\.", as.character(age.months[i])) & !is.na(age.months[i])){
      age.months[i] <- as.character(paste0(age.months[i],".0"))
    }
  }
  years <- as.numeric(sub("\\..*","",as.character(age.months)))
  months <- as.numeric(sub(".*\\.","",as.character(age.months)))
  return(years * 12 + months)
}

pds <- home_quests %>%
  # Convert age to age decimal
  mutate(AGE_PDS_DECIMAL = convert_age(Age.Years.Months) / 12) %>%
  # Only include responses where at least one question relating to puberty on PDS scale
  filter(!is.na(Q32) | !is.na(Q33) | !is.na(Q34) | !is.na(Q35) | !is.na(Q36) | !is.na(Q37) | !is.na(Q38) |
         !is.na(Q39) | !is.na(Q40) | !is.na(Q41) | !is.na(Q42) | !is.na(Q43) | !is.na(Q44)) %>%
  # Create wideformat PDS Data
  select(Scamp_ID:Q44, AGE_PDS_DECIMAL) %>% select(-Age.Years.Months) %>%
  rename(sex_pds = Sex) %>%
  mutate(Survey_Date = as.Date(Survey_Date, "%d-%b-%y")) %>%
  arrange(Scamp_ID, Survey_Date) 

#Create iteration data for wide format
iteration <- rep(NA, dim(pds)[1])
for (i in 1:dim(pds)[1]){
  if(i == 1){
    counter <- 1
    iteration[i] <- 1
  } else {
    if(pds$Scamp_ID [i] != pds$Scamp_ID [i-1]){
      counter <- 1
    } else {
      counter <- counter + 1
    }
    iteration[i] <- counter
  }
}
pds <- pds %>%
  mutate(iteration = iteration) %>%
  pivot_wider(
    names_from = iteration,
    names_sep = "_",
    values_from = c(Q32, Q33, Q34, Q35, Q36, Q37, Q38, Q39, Q40, Q41, Q42, Q43, Q44, Survey_Date, AGE_PDS_DECIMAL)
  ) %>%
  as.data.frame() %>%
  select(Scamp_ID, sex_pds, 
         Survey_Date_1, AGE_PDS_DECIMAL_1, Q32_1, Q33_1, Q34_1, Q35_1, Q36_1, Q37_1, Q38_1, Q39_1, Q40_1, Q41_1, Q42_1, Q43_1, Q44_1,
         Survey_Date_2, AGE_PDS_DECIMAL_2, Q32_2, Q33_2, Q34_2, Q35_2, Q36_2, Q37_2, Q38_2, Q39_2, Q40_2, Q41_2, Q42_2, Q43_2, Q44_2,
         Survey_Date_3, AGE_PDS_DECIMAL_3, Q32_3, Q33_3, Q34_3, Q35_3, Q36_3, Q37_3, Q38_3, Q39_3, Q40_3, Q41_3, Q42_3, Q43_3, Q44_3,
         Survey_Date_4, AGE_PDS_DECIMAL_4, Q32_4, Q33_4, Q34_4, Q35_4, Q36_4, Q37_4, Q38_4, Q39_4, Q40_4, Q41_4, Q42_4, Q43_4, Q44_4, everything()
  ) %>% 
  mutate(PDS_1 = !is.na(Survey_Date_1)) %>%
  mutate(PDS_2 = !is.na(Survey_Date_2)) %>%
  mutate(PDS_3 = !is.na(Survey_Date_3)) %>%
  select(Scamp_ID, sex_pds, PDS_1, PDS_2, PDS_3, everything())

###### Linkage

# First link SCAMP data with Glasgow UID (and biozone and Psytools UIDs for Qual Check)
scamp_glasgow_keys <- key_table %>%
  select(SCAMP_ID, GLASGOW_ID, BIOZONE_ID_BL, BIOZONE_ID_FU, PM_STUDY_ID_1, PM_STUDY_ID_2, PSYTOOLS_UUID_BL, PSYTOOLS_UUID_F1) %>%
  distinct() # remove duplicated rooms
  
#Check dimensions before linkage
dim(scamp_df)
dim(scamp_glasgow_keys)

# Left join scamp_data with scamp_glasgow_keys matching sunique_id with SCAMP_ID
scamp_merged <- scamp_df %>%
  left_join(scamp_glasgow_keys, by = c("sunique_id" = "SCAMP_ID"))

# Now left-join Glasgow Data BIOZONE data ONLY
scamp_merged <- scamp_merged %>%
  left_join(glasgow, by = "GLASGOW_ID", na_matches = "never") %>%
  select(-c(PDS_1:PDS_4)) %>%
  select(-c(Survey_Date_1:Q44_4)) %>%
  # LEFT-JOIN PDS data by SCAMP_ID
  left_join(pds, by = c("sunique_id" = "Scamp_ID"), na_matches = "never") %>%
  select(-c(Survey_Date_4:Q44_4))

write.csv(scamp_merged, "SCAMP_Biozone_PDS_merged_2022_01_30.csv", row.names = FALSE)

##### LINKAGE COMPLETE ######

##### ESTIMATE SAMPLE SIZES ######

## Saliva sample numbers
saliva_n <- scamp_merged %>%
  group_by(sex_bio, saliva_assay_result_BL, saliva_assay_result_F1) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  replace(is.na(.), "no biozone data")
write.csv(saliva_n, "./table_outputs/saliva_n.csv", row.names = FALSE)

## Urine sample numbers
urine_n <- scamp_merged %>%
  group_by(sex_bio, urine_assay_result_BL, urine_assay_result_F1) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  replace(is.na(.), "no biozone data")
write.csv(urine_n, "./table_outputs/urine_n.csv", row.names = FALSE)

## PDS numbers
pds_n <- scamp_merged %>%
  group_by(sex_pds, PDS_1) %>%
  summarise(n = n()) %>%
  as.data.frame() %>%
  replace(is.na(.), "no pds data")
write.csv(pds_n, "./table_outputs/pds_n.csv", row.names = FALSE)

