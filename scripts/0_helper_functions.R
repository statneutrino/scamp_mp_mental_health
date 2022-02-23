## Create function to fit linear PHQ models at BASELINE
fit_phq_lm_BL <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_BL", "ownership_binary_BL")
  
  phq <- list()
  
  ## DEPRESSION MODELS
  
  # Fit univariate model without random effects
  phq[["model_uni"]] <- lm(glue("phq_sum ~ {exposure_var}"), data=df)
  phq[["model_uni_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit univariate model with school effects
  phq[["model_uni_RE"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_uni_RE_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_BL + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat + cannabis_cat"
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  phq[["model_demo"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_demo_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with all demographic confounders, drugs and school effects AND semotion_BL
  phq[["model_sdq"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + semotion_BL + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_sdq_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  phq[["model_phys"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + {activity} + semotion_BL + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_phys_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_BL"
  phq[["model_sleep"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + {sleep} +  semotion_BL + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_sleep_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  phq[["model_all"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + {sleep} + {activity} + semotion_BL + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_all_N"]] <- length(phq[["model_uni"]]$residuals)
  
  phq[["model_all_ranova"]] <- lmerTest::ranova(phq$model_all)
  
  return(phq)
}

## Create function to fit linear GAD models at BASELINE
fit_gad_lm_BL <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_BL", "ownership_binary_BL")
  
  gad <- list()
  
  ## DEPRESSION MODELS
  
  # Fit univariate model without random effects
  gad[["model_uni"]] <- lm(glue("gad_sum ~ {exposure_var}"), data=df)
  gad[["model_uni_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit univariate model with school effects
  gad[["model_uni_RE"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_uni_RE_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, and condition on phone ownership and school effects
  demog <- "SEX + AGE_BL + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat + cannabis_cat"
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  gad[["model_demo"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_demo_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with all demographic confounders, drugs and school effects AND semotion_BL

  gad[["model_sdq"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + semotion_BL + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_sdq_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  gad[["model_phys"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + {activity} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_phys_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_BL"
  gad[["model_sleep"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + {sleep} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_sleep_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  gad[["model_all"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + {sleep} + {activity} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_all_N"]] <- length(gad[["model_uni"]]$residuals)
  
  gad[["model_all_ranova"]] <- lmerTest::ranova(gad$model_all)
  
  return(gad)
}



#### FOLLOW-UP ####
## Create function to fit linear PHQ models at FOLLOW UP
fit_phq_lm_F1 <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_BL", "ownership_binary_BL")
  
  phq <- list()
  
  ## DEPRESSION MODELS

  # Fit univariate model without random effects
  phq[["model_uni"]] <- lm(glue("phq_sum ~ {exposure_var}"), data=df)
  phq[["model_uni_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit univariate model with school effects
  phq[["model_uni_RE"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_uni_RE_N"]] <- length(phq[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat + cannabis_cat"
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  phq[["model_demo"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_demo_N"]] <- length(phq[["model_uni"]]$residuals)

  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  phq[["model_phys"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + {activity} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_phys_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_F1"
  phq[["model_sleep"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + {sleep} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_sleep_N"]] <- length(phq[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  phq[["model_all"]] <- lmerTest::lmer(glue("phq_sum ~ {exposure_var} + {demog} + {sleep} + {activity} + (1 | SCHOOL_ID_F1)"), data=df)
  phq[["model_all_N"]] <- length(phq[["model_uni"]]$residuals)
  
  phq[["model_all_ranova"]] <- lmerTest::ranova(phq$model_all)
  
  return(phq)
}


#### FOLLOW-UP ####
## Create function to fit linear GAD models at FOLLOW UP
fit_gad_lm_F1 <- function(exposure_var, df, internet=FALSE, device_adjust=FALSE){
  
  device <- ifelse(internet==TRUE, "internet_BL", "ownership_binary_BL")
  
  gad <- list()
  
  ## ANXIETY MODELS
  
  # Fit univariate model without random effects
  gad[["model_uni"]] <- lm(glue("gad_sum ~ {exposure_var}"), data=df)
  gad[["model_uni_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit univariate model with school effects
  gad[["model_uni_RE"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_uni_RE_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit model with all demographic confounders, drugs, ownership/internet and school effects
  demog <- "SEX + AGE_F1 + ETHNICITY_DESC + parent_occ + IMD_DECILE_F1 + SCHOOL_IND_F1 +  smoking_cat + alcohol_cat + cannabis_cat"
  
  demog <- ifelse(device_adjust == TRUE, paste(demog, glue(" + {device}")), demog)
  
  gad[["model_demo"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_demo_N"]] <- length(gad[["model_uni"]]$residuals)
  
  
  # Fit model with demographic conf AND physical activity adjusted
  activity <- "physical_activity_quintile"
  gad[["model_phys"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + {activity} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_phys_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep adjusted
  sleep <- "sleep_dtn_weekday_F1"
  gad[["model_sleep"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + {sleep} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_sleep_N"]] <- length(gad[["model_uni"]]$residuals)
  
  # Fit model with demographic conf AND BASELINE sleep AND PHYSICAL ACTIVITY! adjusted
  gad[["model_all"]] <- lmerTest::lmer(glue("gad_sum ~ {exposure_var} + {demog} + {sleep} + {activity} + (1 | SCHOOL_ID_F1)"), data=df)
  gad[["model_all_N"]] <- length(gad[["model_uni"]]$residuals)
  
  gad[["model_all_ranova"]] <- lmerTest::ranova(gad$model_all)
  
  return(gad)
}



