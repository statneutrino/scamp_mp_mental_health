# library(tidyverse)
# library(glue)
# num_cpu <- parallel::detectCores(logical=FALSE)
# ## Proportional odds models
# 
# source("./scripts/0_helper_functions.R")
# source("./scripts/2_variable_processing.R")
# scamp_f1 <- psytools %>%
#   filter(!is.na(PSYTOOLS_UID_F1)) %>%
#   filter(!is.na(phq_sum) & !is.na(gad_sum)) %>%
#   as.data.frame()
# 
# ## Transform PHQ data to categories
# scamp_f1 <- scamp_f1 %>%
#   mutate(phq_cat = case_when(
#     phq_sum <= 9 ~ "None/mild",
#     phq_sum <= 14 ~ "Moderate",
#     phq_sum <= 19 ~ "Moderately severe",
#     phq_sum <= 27 ~ "Severe",
#     TRUE ~ NA_character_
#   )) %>%
#   mutate(phq_cat = factor(
#     phq_cat,
#     ordered = TRUE,
#     levels=c("None/mild","Moderate","Moderately severe","Severe"))) %>%
#   mutate(phq_cat_num = as.numeric(phq_cat)-1)


## Create function to fit linear PHQ models at BASELINE
fit_phq_polr_BL <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_BL", "ownership_binary_BL")
  
  phq <- list()
  
  ## DEPRESSION MODELS
  
  # Fit univariate model without random effects
  phq[["model_uni"]] <- MASS::polr(glue("phq_cat ~ {exposure_var}"), data=df, Hess=TRUE)
  phq[["model_uni_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_BL + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat "
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  phq[["model_demo"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} "), data=df, Hess=TRUE)
  phq[["model_demo_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with all demographic confounders, drugs and school effects AND semotion_BL
  phq[["model_sdq"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + semotion_BL "), data=df, Hess=TRUE)
  phq[["model_sdq_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  phq[["model_phys"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + {activity} + semotion_BL "), data=df, Hess=TRUE)
  phq[["model_phys_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_BL"
  phq[["model_sleep"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + {sleep} +  semotion_BL "), data=df, Hess=TRUE)
  phq[["model_sleep_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  phq[["model_all"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + {sleep} + {activity} + semotion_BL "), data=df, Hess=TRUE)
  phq[["model_all_N"]] <- length(phq[["model_uni"]]$residuals)
  
  return(phq)
}


## Create function to fit linear PHQ models at BASELINE
fit_gad_polr_BL <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_BL", "ownership_binary_BL")
  
  gad <- list()
  
  ## ANXIETY MODELS
  
  # Fit univariate model without random effects
  gad[["model_uni"]] <- MASS::polr(glue("gad_cat ~ {exposure_var}"), data=df, Hess=TRUE)
  gad[["model_uni_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_BL + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat "
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  gad[["model_demo"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} "), data=df, Hess=TRUE)
  gad[["model_demo_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with all demographic confounders, drugs and school effects AND semotion_BL
  gad[["model_sdq"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + semotion_BL "), data=df, Hess=TRUE)
  gad[["model_sdq_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  gad[["model_phys"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + {activity} + semotion_BL "), data=df, Hess=TRUE)
  gad[["model_phys_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_BL"
  gad[["model_sleep"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + {sleep} +  semotion_BL "), data=df, Hess=TRUE)
  gad[["model_sleep_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  gad[["model_all"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + {sleep} + {activity} + semotion_BL "), data=df, Hess=TRUE)
  gad[["model_all_N"]] <- length(gad[["model_uni"]]$residuals)
  
  return(gad)
}

##### LONGITUDINAL MODELS

## Create function to fit linear PHQ models at BASELINE
fit_phq_polr_F1 <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_F1", "ownership_binary_F1")
  
  phq <- list()
  
  ## DEPRESSION MODELS
  
  # Fit univariate model without random effects
  phq[["model_uni"]] <- MASS::polr(glue("phq_cat ~ {exposure_var}"), data=df, Hess=TRUE)
  phq[["model_uni_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat "
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  phq[["model_demo"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog}"), data=df, Hess=TRUE)
  phq[["model_demo_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  phq[["model_phys"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + {activity}"), data=df, Hess=TRUE)
  phq[["model_phys_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_F1"
  phq[["model_sleep"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + {sleep}"), data=df, Hess=TRUE)
  phq[["model_sleep_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  phq[["model_all"]] <- MASS::polr(glue("phq_cat ~ {exposure_var} + {demog} + {sleep} + {activity}"), data=df, Hess=TRUE)
  phq[["model_all_N"]] <- length(phq[["model_uni"]]$residuals)
  
  return(phq)
}


## Create function to fit linear PHQ models at BASELINE
fit_gad_polr_F1 <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_F1", "ownership_binary_F1")
  
  gad <- list()
  
  ## ANXIETY MODELS
  
  # Fit univariate model without random effects
  gad[["model_uni"]] <- MASS::polr(glue("gad_cat ~ {exposure_var}"), data=df, Hess=TRUE)
  gad[["model_uni_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat "
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  gad[["model_demo"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog}"), data=df, Hess=TRUE)
  gad[["model_demo_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  gad[["model_phys"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + {activity}"), data=df, Hess=TRUE)
  gad[["model_phys_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_F1"
  gad[["model_sleep"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + {sleep}"), data=df, Hess=TRUE)
  gad[["model_sleep_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  gad[["model_all"]] <- MASS::polr(glue("gad_cat ~ {exposure_var} + {demog} + {sleep} + {activity}"), data=df, Hess=TRUE)
  gad[["model_all_N"]] <- length(gad[["model_uni"]]$residuals)
  
  return(gad)
}



tidy_custom.polr <- function(x,...){
  s <- lmtest::coeftest(x)
  out <- data.frame(
    term = row.names(s),
    p.value = s[,"Pr(>|t|)"]
  )
  out
}

    
    