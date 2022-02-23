library(tidyverse)
library(readxl)

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
  filter(!is.na(Q32) & !is.na(Q33) | !is.na(Q34) | !is.na(Q35) | !is.na(Q36) | !is.na(Q37) | 
           !is.na(Q39) | !is.na(Q40) | !is.na(Q41) | !is.na(Q42) | !is.na(Q43) | !is.na(Q44)) %>%
  # Rename boys PDS variables
  rename(PDS_GROWTH_BOYS = Q39) %>%
  rename(PDS_AXILLARY_BOYS = Q40) %>%
  rename(PDS_FACIAL = Q41) %>%
  rename(PDS_PUBIC_BOYS = Q42) %>%
  rename(PDS_SKIN_BOYS = Q43) %>%
  rename(PDS_VOICE = Q44) %>%
  # Rename girls PDS variables
  rename(PDS_GROWTH_GIRLS = Q32) %>%
  rename(PDS_AXILLARY_GIRLS = Q33) %>%
  rename(PDS_BREASTS = Q36) %>%
  rename(PDS_PUBIC_GIRLS = Q34) %>%
  rename(PDS_SKIN_GIRLS = Q35) %>%
  rename(PDS_MENARCHE = Q37) %>%
  rename(PDS_MENARCHE_AGE = Q38) %>%
  mutate(PDS_MENARCHE_TRANSFORMED = 1+PDS_MENARCHE*3) %>%
  rowwise() %>%
  mutate(PDS_MEAN_BOYS = mean(
    c(PDS_GROWTH_BOYS, PDS_AXILLARY_BOYS, PDS_FACIAL, PDS_PUBIC_BOYS, PDS_SKIN_BOYS, PDS_VOICE)
  )) %>% 
  mutate(PDS_MEAN_GIRLS = mean(
    c(PDS_GROWTH_GIRLS, PDS_AXILLARY_GIRLS, PDS_PUBIC_GIRLS, PDS_SKIN_GIRLS, PDS_BREASTS, PDS_MENARCHE_TRANSFORMED)
  )) %>%
  mutate(PDS_BODY_HAIR_BOYS = round(mean(c(PDS_PUBIC_BOYS, PDS_AXILLARY_BOYS)))) %>%
  mutate(PDS_BODY_HAIR_GIRLS = round(mean(c(PDS_AXILLARY_GIRLS, PDS_PUBIC_GIRLS)))) %>%
  ungroup() %>%
  mutate(
    PDS_MATURATION_BOYS = case_when(
      PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL == 3 ~ "pre",
      (PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL == 4 | PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL == 5) & PDS_BODY_HAIR_BOYS < 3 & PDS_VOICE < 3 & PDS_FACIAL < 3 ~ "early",
      (PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL == 4 | PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL == 5) & (PDS_BODY_HAIR_BOYS == 3 | PDS_VOICE == 3 | PDS_FACIAL == 3) ~ "mid",
      (PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL >= 6 & PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL <= 8) & PDS_BODY_HAIR_BOYS < 4 & PDS_VOICE < 4 & PDS_FACIAL < 4 ~ "mid",
      (PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL >= 6 & PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL <= 8) & (PDS_BODY_HAIR_BOYS == 4 | PDS_VOICE == 4 | PDS_FACIAL == 4) ~ "late",
      (PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL >= 9 & PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL <= 11) ~ "late",
      PDS_BODY_HAIR_BOYS + PDS_VOICE + PDS_FACIAL == 12 ~ "post",
      Sex == 2 ~ NA_character_,
      is.na(PDS_BODY_HAIR_BOYS) | is.na(PDS_FACIAL) | is.na(PDS_VOICE) ~ "info.missing",
      TRUE ~ NA_character_) ) %>% 
  as.data.frame() %>%
  mutate(PDS_MATURATION_SCORE_BOYS = case_when(
    PDS_MATURATION_BOYS == "pre" ~ 1,
    PDS_MATURATION_BOYS == "early" ~ 2,
    PDS_MATURATION_BOYS == "mid" ~ 3,
    PDS_MATURATION_BOYS == "late" ~ 4,
    PDS_MATURATION_BOYS == "post" ~ 5,
    TRUE ~ NA_real_
  )) %>%
  mutate(
    PDS_MATURATION_GIRLS= case_when(
      PDS_BODY_HAIR_GIRLS + PDS_BREASTS == 2 & PDS_MENARCHE_TRANSFORMED == 1 ~ "pre",
      PDS_BODY_HAIR_GIRLS + PDS_BREASTS == 3 & PDS_MENARCHE_TRANSFORMED == 1 ~ "early",
      PDS_BODY_HAIR_GIRLS + PDS_BREASTS >= 3 & PDS_MENARCHE_TRANSFORMED == 1 ~ "mid",
      PDS_BODY_HAIR_GIRLS + PDS_BREASTS  <= 7 & PDS_MENARCHE_TRANSFORMED == 4 ~ "late",
      PDS_BODY_HAIR_GIRLS + PDS_BREASTS == 8 & PDS_MENARCHE_TRANSFORMED == 4 ~ "post",
      Sex == 1 ~ NA_character_,
      is.na(PDS_BODY_HAIR_GIRLS) | is.na(PDS_BREASTS) | is.na(PDS_MENARCHE_TRANSFORMED) ~ "info.missing",
      TRUE ~ NA_character_
    )
  ) %>% mutate(PDS_MATURATION_SCORE_GIRLS = case_when(
    PDS_MATURATION_GIRLS == "pre" ~ 1,
    PDS_MATURATION_GIRLS == "early" ~ 2,
    PDS_MATURATION_GIRLS == "mid" ~ 3,
    PDS_MATURATION_GIRLS == "late" ~ 4,
    PDS_MATURATION_GIRLS == "post" ~ 5,
    TRUE ~ NA_real_
  )) %>% as.data.frame() %>%
  # Create wideformat PDS Data
  select(Scamp_ID:PDS_VOICE, AGE_PDS_DECIMAL:PDS_MATURATION_SCORE_GIRLS) %>% select(-Age.Years.Months) %>%
  rename(PDS_SEX = Sex) %>%
  mutate(Survey_Date = as.Date(Survey_Date, "%d-%b-%y")) %>%
  arrange(Scamp_ID, Survey_Date) %>%
  select(Scamp_ID, PDS_SEX, Survey_Date, everything())

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
  mutate(iteration = iteration) 

## REMOVE ALL DUPLICATES LESS THAN 1<year APART
pds$keep_survey <- rep(NA)
for (i in 1:nrow(pds)){
  if (pds$iteration[i] == 1) {
    pds$keep_survey[i] <- TRUE
  } else {
    indiv_surveys <- pds[pds$Scamp_ID == pds$Scamp_ID[i],]
    last_date <- indiv_surveys$Survey_Date[indiv_surveys$iteration == max(indiv_surveys$iteration[indiv_surveys$keep_survey == TRUE & !is.na(indiv_surveys$keep_survey)])]
    current_date <- pds$Survey_Date[i]
    days_between <- difftime(
      as.POSIXct(current_date,tz="UTC"), as.POSIXct(last_date,tz="UTC")
    ) %>% as.numeric()
    pds$keep_survey[i] <- ifelse(days_between >= 365, TRUE, FALSE)
  }
}
pds <- pds %>% filter(keep_survey == TRUE) %>%
  select(-keep_survey)
           
##SUBSET BOYS AND GIRLS FOR REGRESSION
pds_boys <- pds %>% filter(PDS_SEX == 1)
pds_girls <- pds %>% filter(PDS_SEX == 2)

girls_lm <- lm(PDS_MEAN_GIRLS ~ AGE_PDS_DECIMAL, data=pds_girls)
boys_lm <- lm(PDS_MEAN_BOYS ~ AGE_PDS_DECIMAL, data=pds_boys)
# KROENKE METHOD 
# ## Calculate quantiles
# library(splines)
# library(quantreg)
# deg.free<-4
# X_girls <- model.matrix(PDS_MEAN_GIRLS ~ bs(AGE_PDS_DECIMAL, df=deg.free),data=pds_girls)
# 
# 
# tau_vector <- c(0.025, 0.16, 0.50, 0.84, 0.975)
# 
# AGE_PDS_DECIMAL = seq(11,16,length.out=100)
# plot(pds_girls$AGE_PDS_DECIMAL, pds_girls$PDS_MEAN_GIRLS)
# for(tau in tau_vector){
#   fit <- rq(PDS_MEAN_GIRLS ~ bs(AGE_PDS_DECIMAL, df=deg.free), tau=tau, data=pds_girls)
#   fitted.values <- predict(fit, newdata = list(AGE_PDS_DECIMAL))
#   lines(AGE_PDS_DECIMAL, fitted.values)
# }

## PDS_MEAN automatic bandwidth for girls and boys
bw_girls <- np::npcdistbw(formula=PDS_MEAN_GIRLS~AGE_PDS_DECIMAL, data = pds_girls)
fhat_girls <- np::npcdist(bws=bw_girls)

bw_boys <- np::npcdistbw(formula=PDS_MEAN_BOYS~AGE_PDS_DECIMAL, data = pds_boys)
fhat_boys <- np::npcdist(bws=bw_boys)

## Predict quantile and create z-scores based on 
pds$PDS_QUANTILE = NA
for (i in 1:nrow(pds)){
  if(pds$PDS_SEX[i] == 1 & !is.na(pds$PDS_MEAN_BOYS[i]) & !is.na(pds$AGE_PDS_DECIMAL[i]) & !is.na(pds$PDS_SEX[i]) ){
    newdata = data.frame(
      AGE_PDS_DECIMAL = pds$AGE_PDS_DECIMAL[i], 
      PDS_MEAN_BOYS = pds$PDS_MEAN_BOYS[i])
    pds$PDS_QUANTILE[i] <- predict(fhat_boys, newdata=newdata)
  } else if(pds$PDS_SEX[i] == 2 & !is.na(pds$PDS_MEAN_GIRLS[i]) & !is.na(pds$AGE_PDS_DECIMAL[i]) & !is.na(pds$PDS_SEX[i])){
    newdata = data.frame(
      AGE_PDS_DECIMAL = pds$AGE_PDS_DECIMAL[i], 
      PDS_MEAN_GIRLS = pds$PDS_MEAN_GIRLS[i])
    pds$PDS_QUANTILE[i] <- predict(fhat_girls, newdata=newdata)
  }
}
pds <- pds %>%
  mutate(PDS_Z_AGE = qnorm(PDS_QUANTILE))

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
pds$iteration <- iteration

### WIDEN DATA
pds_wide <- pds %>%
  select(Scamp_ID, PDS_SEX, iteration, everything()) %>%
  pivot_wider(
    names_from = iteration,
    names_sep = "_",
    values_from = c(Survey_Date:PDS_Z_AGE)
  ) %>%
  as.data.frame() %>%
  select(Scamp_ID, 
         Survey_Date_1, AGE_PDS_DECIMAL_1, PDS_GROWTH_GIRLS_1, PDS_AXILLARY_GIRLS_1, PDS_PUBIC_GIRLS_1, 
         PDS_SKIN_GIRLS_1, PDS_BREASTS_1, PDS_MENARCHE_1, PDS_MENARCHE_AGE_1, PDS_GROWTH_BOYS_1, 
         PDS_AXILLARY_BOYS_1, PDS_FACIAL_1, PDS_PUBIC_BOYS_1, PDS_SKIN_BOYS_1, PDS_VOICE_1, 
         PDS_MENARCHE_TRANSFORMED_1, PDS_MEAN_BOYS_1, PDS_MEAN_GIRLS_1, PDS_BODY_HAIR_BOYS_1, 
         PDS_BODY_HAIR_GIRLS_1, PDS_MATURATION_BOYS_1, PDS_MATURATION_SCORE_BOYS_1, 
         PDS_MATURATION_GIRLS_1, PDS_MATURATION_SCORE_GIRLS_1, PDS_QUANTILE_1, PDS_Z_AGE_1,
         Survey_Date_2, AGE_PDS_DECIMAL_2, PDS_GROWTH_GIRLS_2, PDS_AXILLARY_GIRLS_2, PDS_PUBIC_GIRLS_2, 
         PDS_SKIN_GIRLS_2, PDS_BREASTS_2, PDS_MENARCHE_2, PDS_MENARCHE_AGE_2, PDS_GROWTH_BOYS_2, 
         PDS_AXILLARY_BOYS_2, PDS_FACIAL_2, PDS_PUBIC_BOYS_2, PDS_SKIN_BOYS_2, PDS_VOICE_2, 
         PDS_MENARCHE_TRANSFORMED_2, PDS_MEAN_BOYS_2, PDS_MEAN_GIRLS_2, PDS_BODY_HAIR_BOYS_2, 
         PDS_BODY_HAIR_GIRLS_2, PDS_MATURATION_BOYS_2, PDS_MATURATION_SCORE_BOYS_2, 
         PDS_MATURATION_GIRLS_2, PDS_MATURATION_SCORE_GIRLS_2,PDS_QUANTILE_2, PDS_Z_AGE_2,
         Survey_Date_3, AGE_PDS_DECIMAL_3, PDS_GROWTH_GIRLS_3, PDS_AXILLARY_GIRLS_3, PDS_PUBIC_GIRLS_3, 
         PDS_SKIN_GIRLS_3, PDS_BREASTS_3, PDS_MENARCHE_3, PDS_MENARCHE_AGE_3, PDS_GROWTH_BOYS_3, 
         PDS_AXILLARY_BOYS_3, PDS_FACIAL_3, PDS_PUBIC_BOYS_3, PDS_SKIN_BOYS_3, PDS_VOICE_3, 
         PDS_MENARCHE_TRANSFORMED_3, PDS_MEAN_BOYS_3, PDS_MEAN_GIRLS_3, PDS_BODY_HAIR_BOYS_3, 
         PDS_BODY_HAIR_GIRLS_3, PDS_MATURATION_BOYS_3, PDS_MATURATION_SCORE_BOYS_3, 
         PDS_MATURATION_GIRLS_3, PDS_MATURATION_SCORE_GIRLS_3,PDS_QUANTILE_3, PDS_Z_AGE_3,
         everything()
  ) %>% 
  mutate(PDS_1 = !is.na(Survey_Date_1)) %>%
  mutate(PDS_2 = !is.na(Survey_Date_2)) %>%
  mutate(PDS_3 = !is.na(Survey_Date_3)) %>%
  # Reorder columns
  select(Scamp_ID, PDS_SEX, PDS_1, PDS_2, PDS_3, everything())

write.csv(pds_wide, "./data/pds_processed_2022_02_20.csv")

