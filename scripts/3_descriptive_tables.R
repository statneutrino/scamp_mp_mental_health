library(arsenal)
library(tidyverse)
library(jtools)

# psytools <- read.csv("./data/psytools_cleaned.csv")
source("./scripts/2_variable_processing.R")

std_controls <- tableby.control(
  numeric.stats = c("medianq1q3", "Nmiss"),
  cat.stats = c("countpct", "Nmiss"),
  digits = 2 
)

mean_controls <- tableby.control(
  numeric.stats = c("meansd", "Nmiss"),
  cat.stats = c("countpct", "Nmiss"),
  digits = 2 
)

scamp <- psytools %>%
  select(
    SCAMP_ID, PSYTOOLS_UID_BL, BATTERYVERSION_BL, PSYTOOLS_UID_F1, BATTERYVERSION_F1, ETHNICITY_DESC,
    SCHOOL_ID_BL:AGE_F1, SCHOOL_IND_F1:IMD_QUINTILE
  )

## Subset sample by have taken follow-up psytools
scamp_f1 <- scamp %>% 
  filter(!is.na(PSYTOOLS_UID_F1)) %>%
  filter(!is.na(phq_sum))

# ## Transform to long form  
# scamp_f1_long <- scamp_f1  %>%
#   mutate(across(
#     all_of(ends_with(c("_BL", "_F1"))), 
#     as.character)) %>%
#   pivot_longer(
#     cols = all_of(ends_with(c("_BL", "_F1"))),
#     names_to = c("variable", "timepoint"),
#     names_pattern = "(.*)_(..$)"
#   ) %>%
#   pivot_wider(
#     names_from = "variable"
#   ) %>%
#   mutate(AGE = as.numeric(AGE))

## Create demographic labels
labels(scamp_f1) <- c(
  AGE_BL = "Age at baseline",
  AGE_F1 = "Age at follow-up",
)


## Create table for demographics

demographics <- tableby(
  list(SEX) ~ AGE_BL + AGE_F1 + ETHNICITY_F1 + parent_occ + as.factor(IMD_QUINTILE) +
    as.factor(SCHOOL_IND_F1),
  data = scamp_f1, control = std_controls
)
summary(demographics)

## Confounder
confounders <- tableby(
  list(SEX) ~ sleep_dtn_weekday_BL + sleep_dtn_weekend_BL + sleep_dtn_weekday_F1 + sleep_dtn_weekend_F1 + 
    sleep_diff_BL + sleep_diff_F1 + physical_activity,
  data = scamp_f1, control = mean_controls
)
summary(confounders)


## Calculate table for exposure
exposure <- tableby(
  list(SEX) ~ call_own_weekday_BL + call_own_weekday_F1 + call_own_weekend_BL + call_own_weekend_F1 +
    call_others_weekday_BL + call_others_weekday_F1 + call_others_weekend_BL + call_others_weekend_F1 +
    sns_mp_weekday_BL + sns_mp_weekday_F1 + sns_mp_weekend_BL + sns_mp_weekend_F1 +
    internet_weekday_BL + internet_weekend_BL + internet_weekend_BL + internet_weekend_BL +
    text_weekday_BL + text_weekend_BL +  text_weekday_F1 + text_weekend_F1 + 
    im_weekday_BL + im_weekend_BL +  im_weekday_F1 + im_weekend_F1,
  data = scamp_f1, control = std_controls
)
summary(exposure)

## Create table for mental health
mh <- tableby(
  list(SEX) ~ phq_sum + gad_sum + sebdtot_BL + semotion_BL + sconduct_BL+shyper_BL+
    speer_BL + sprosoc_BL + sebdtot_F1 + semotion_F1 + sconduct_F1 + shyper_F1 + 
    speer_F1 + sprosoc_F1,
  data = scamp_f1, control = std_controls
)
summary(mh)




mh_mean <- tableby(
  list(SEX) ~ phq_sum + gad_sum + sebdtot_BL + semotion_BL + sconduct_BL+shyper_BL+
    speer_BL + sprosoc_BL + sebdtot_F1 + semotion_F1 + sconduct_F1 + shyper_F1 + 
    speer_F1 + sprosoc_F1,
  data = scamp_f1, control = mean_controls
)
summary(mh_mean)

## Mental Health Distribution Plots





