library(tidyverse)
library(readxl)

source("./scripts/7_puberty_linkage.R")

## GIRLS - Fit model with puberty alone
puberty_alone <- lm(
  formula = gad_sum ~ PDS_Z_AGE_1,
  data=scamp_puberty %>% filter(SEX == "F")
)

puberty_adjusted_demog <- lm(
  formula = gad_sum ~ PDS_Z_AGE_1+ AGE_F1 + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 + 
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

### CROSS-SECTIONAL MODELS
cross_SNS_demog_only <- lm(
  formula = phq_sum ~ sns_mp_avg_F1 + AGE_F1 + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 + 
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

cross_SNS_demog_puberty <- lm(
  formula = phq_sum ~ sns_mp_avg_F1*PDS_Z_AGE_1 + AGE_F1 + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 + 
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

models <- list(
  "Puberty Only" = puberty_alone,
  "Pubety adjusted" = puberty_adjusted_demog,
  "SNS adjusted" = cross_SNS_demog_only,
  "SNSxPuberty adjusted" = cross_SNS_demog_puberty
)

cm <- c(
  "PDS_Z_AGE_1" = "Puberty Z",
  "sns_mp_avg_F1" = "SNS",
  "sns_mp_avg_F1:PDS_Z_AGE_1" = "Puberty Z x SNS"
)


modelsummary::modelsummary(
  models,
  #exponentiate = TRUE,
  stars = TRUE,
  statistic = "conf.int",
  fmt=2,
  coef_map = cm,
  output = paste0('./tables/SNS_puberty_F1.docx'),
)

### LONGITUDINAL MODELS
long_SNS_demog_only <- lm(
  formula = phq_sum ~ sns_mp_avg_BL + AGE_F1 + AGE_BL + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

long_SNS_demog_puberty <- lm(
  formula = phq_sum ~ sns_mp_avg_BL*PDS_Z_AGE_1 + AGE_F1 + AGE_BL + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

long_SNS_SDQ_only <- lm(
  formula = phq_sum ~ sns_mp_avg_BL + AGE_F1 + AGE_BL + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 + semotion_BL + 
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

long_SNS_SDQ_puberty <- lm(
  formula = phq_sum ~ sns_mp_avg_BL*PDS_Z_AGE_1 + AGE_F1 + AGE_BL + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 + semotion_BL + 
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)

models <- list(
  "SNS adj" = long_SNS_demog_only,
  "SNS + SDQ_BL adj" = long_SNS_SDQ_only,
  "SNSxPuberty adj" = long_SNS_demog_puberty,
  "SNSxPuberty +SDQ_BL adj" = long_SNS_SDQ_puberty
)

cm_long <- c(
  "PDS_Z_AGE_1" = "Puberty Z",
  "sns_mp_avg_BL" = "SNS",
  "sns_mp_avg_BL:PDS_Z_AGE_1" = "Puberty Z x SNS"
)

modelsummary::modelsummary(
  models,
  #exponentiate = TRUE,
  stars = TRUE,
  statistic = "conf.int",
  fmt=2,
  coef_map = cm_long,
  output = paste0('./tables/SNS_puberty_BL.docx'),
)


##INTERACT_PLOT
interactions::interact_plot(cross_SNS_demog_puberty, 
                            pred="sns_mp_avg_F1", 
                            modx="PDS_Z_AGE_1", 
                            interval=TRUE, 
                            legend.main="Puberty Z",
                            int.width=0.8,
                            x.label = "SNS Usage (hrs/day)",
                            y.label = "PHQ9 Score")

interactions::interact_plot(long_SNS_demog_puberty, 
                            pred="sns_mp_avg_BL", 
                            modx="PDS_Z_AGE_1", 
                            interval=TRUE, 
                            legend.main="Puberty Z",
                            int.width=0.8,
                            x.label = "SNS Usage (hrs/day)",
                            y.label = "PHQ9 Score", plot.points = TRUE)

interactions::interact_plot(long_SNS_SDQ_puberty, 
                            pred="sns_mp_avg_BL", 
                            modx="PDS_Z_AGE_1", 
                            interval=TRUE, 
                            legend.main="Puberty Z",
                            int.width=0.8,
                            x.label = "SNS Usage (hrs/day)",
                            y.label = "PHQ9 Score")


scamp_puberty <- scamp_puberty %>%
  mutate(PDS_PSYTOOLS_MATCH_BL = case_when(
    abs(AGE_BL - AGE_PDS_DECIMAL_1) <= 0.5 ~ 1,
    abs(AGE_BL - AGE_PDS_DECIMAL_2) <= 0.5 ~ 2,
    abs(AGE_BL - AGE_PDS_DECIMAL_3) <= 0.5 ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(PDS_PSYTOOLS_MATCH_F1 = case_when(
    abs(AGE_F1 - AGE_PDS_DECIMAL_1) <= 0.5 ~ 1,
    abs(AGE_F1 - AGE_PDS_DECIMAL_2) <= 0.5 ~ 2,
    abs(AGE_F1 - AGE_PDS_DECIMAL_3) <= 0.5 ~ 3,
    TRUE ~ NA_real_
  )) %>%
  mutate(PCS_SCORE_BL = case_when(
    .$SEX == "M" & .$PDS_PSYTOOLS_MATCH_BL == 1 ~ PDS_MATURATION_SCORE_BOYS_1,
    .$SEX == "F" & .$PDS_PSYTOOLS_MATCH_BL == 1 ~ PDS_MATURATION_SCORE_GIRLS_1,
    .$SEX == "M" & .$PDS_PSYTOOLS_MATCH_BL == 2 ~ PDS_MATURATION_SCORE_BOYS_2,
    .$SEX == "F" & .$PDS_PSYTOOLS_MATCH_BL == 2 ~ PDS_MATURATION_SCORE_GIRLS_2,
    TRUE ~ NA_integer_
  )) %>%
  mutate(PCS_SCORE_CAT_BL = case_when(
    PCS_SCORE_BL == 1 ~ "early",
    PCS_SCORE_BL == 2 ~ "mid",
    PCS_SCORE_BL == 3 ~ "late",
    TRUE ~ NA_character_
  ) %>% as.factor()) %>%
  mutate(PCS_SCORE_CAT_BL = fct_relevel(PCS_SCORE_CAT_BL, "mid"))

## Cross-sectional SNS with PDS ZSCORE
sns_demog_cross_F <- lm(
  formula = phq_sum ~ sns_mp_avg_F1*PDS_Z_AGE_1 + AGE_F1 + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)
summary(sns_demog_cross_F)

sns_demog_cross_M <- lm(
  formula = phq_sum ~ sns_mp_avg_F1*PDS_Z_AGE_1 + AGE_F1 + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "M")
)
summary(sns_demog_cross_M)

## Cross-sectional SNS with CATEGORY
sns_demog_long_F <- lm(
  formula = phq_sum ~ sns_mp_avg_BL*PCS_SCORE_CAT_BL + AGE_F1 + AGE_BL + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)
summary(sns_demog_long_F)
  

interactions::interact_plot(sns_demog_cross_F, pred="sns_mp_avg_BL", modx="PDS_Z_AGE_1", interval=TRUE, int.width=0.8)

## Cross-sectional INTERNET
sns_demog_cross_F <- lm(
  formula = phq_sum ~ im_avg_F1*PDS_Z_AGE_1 + AGE_F1 + ETHNICITY_DESC + 
    parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +
    smoking_cat + alcohol_cat + cannabis_cat,
  data=scamp_puberty %>% filter(SEX == "F")
)
summary(sns_demog_cross_F)

interactions::interact_plot(sns_demog_cross_F, pred="im_avg_F1", modx="PDS_Z_AGE_1", interval=TRUE, int.width=0.8)



