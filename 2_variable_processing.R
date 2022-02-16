library(tidyverse)
library(readxl)

#### Import Psytools Data
psytools <- read.csv("data/psytools_school.csv")

#### Import Codebook
codebook <- read_excel("docs/codebook.xlsx") %>%
  mutate(variable_key= toupper(variable_key))

#### Create function to find code answers
codebook_responses <- function(variable){
  question_data <- codebook[codebook$variable_key == variable,] %>%
    select(starts_with("answer_")) %>%
    select(where( ~!is.na(.) ))
  
  question_data <- sapply(question_data, function(x) str_split(x, "%%", simplify = TRUE)) %>%
    t() %>%
    as.data.frame()
  
  colnames(question_data) <- c("code", "full_name")
  rownames(question_data) <- NULL
  return(question_data)
}

#### Mobile phone variable processing

# Function to code missing values
replace_missing_codes <- function(column_vector){
  return(
    case_when(
      column_vector == 9999 ~ NA_integer_,
      column_vector == 8888 ~ NA_integer_,
      TRUE ~ column_vector
    )
  )
}

replace_dont_know <- function(column_vector){
  return(
    case_when(
      column_vector == 7777 ~ NA_integer_,
      TRUE ~ column_vector
    )
  )
}


replace_6666 <- function(column_vector){
  return(
    case_when(
      column_vector == 6666 ~ NA_integer_,
      TRUE ~ column_vector
    )
  )
}



replace_with_codebook <- function(x, question){
  question <- gsub("_BL$", "", question)
  question <- gsub("_F1$", "", question)
  lookup <- codebook_responses(question)
  level_key <- lookup[,2]
  names(level_key) <- lookup[,1]
  result <- recode(x, !!!level_key) %>% fct_relevel(., level_key)
  return(result)
}

get_labels <- function(question){
  question <- deparse(substitute(question))
  question <- gsub("_BL$", "", question)
  question <- gsub("_F1$", "", question)
  return(codebook_responses(question)[,2])
}



psytools <- psytools %>%
  mutate(
    across(c(
      Q11_1_BL, Q11_2_BL, Q11_1_F1, Q11_2_F1, # own call duration
      Q2_1_BL, Q2_2_BL, Q2_3_BL, Q2_4_BL, Q2_5_BL, # whose mp do you use?
      Q2_1_F1, Q2_2_F1, Q2_3_F1, Q2_4_F1, Q2_5_F1,
      Q26_1_BL, Q26_2_BL, Q26_1_F1, Q26_2_F1, # relative call duration
      Q20_1_BL, Q20_2_BL, Q20_1_F1, Q20_2_F1, # internet usage
      Q49_1_BL, Q49_2_BL, Q49_1_F1, Q49_2_F1, # social media phone
      Q49_3_BL, Q49_4_BL, Q49_3_F1, Q49_4_F1, # social media other devices
      Q16_1_BL, Q16_2_BL, Q16_1_F1, Q16_2_F1, # text messages
      Q17_1_BL, Q17_2_BL, Q17_1_F1, Q17_2_F1, # instant messages
      Q1_1_BL, Q1_1_F1,
    ), ~ replace_missing_codes(.x)
  )) %>%
  ### OWN PHONE CALL DURATION ###
  mutate(call_own_weekday_BL = replace_with_codebook(Q11_1_BL, "Q11_1_BL") %>% as.character()) %>%
  mutate(call_own_weekday_F1 = replace_with_codebook(Q11_1_F1, "Q11_1_F1") %>% as.character()) %>%
  mutate(call_own_weekend_BL = replace_with_codebook(Q11_2_BL, "Q11_2_BL") %>% as.character()) %>%
  mutate(call_own_weekend_F1 = replace_with_codebook(Q11_2_F1, "Q11_2_F1") %>% as.character()) %>%
  mutate(call_own_weekday_BL = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    TRUE ~ call_own_weekday_BL
  ) %>% fct_relevel(., get_labels(Q11_1_BL))) %>%
  mutate(call_own_weekday_F1 = case_when(
    Q1_1_F1 == 0 ~ "Do not own",
    Q3_1_F1 == 0 ~ "Do not own",
    TRUE ~ call_own_weekday_F1
  ) %>% fct_relevel(., get_labels(Q11_1_F1)) ) %>%
  mutate(call_own_weekend_BL = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    TRUE ~ call_own_weekend_BL
  ) %>% fct_relevel(., get_labels(Q11_2_BL)) ) %>%
  mutate(call_own_weekend_F1 = case_when(
    Q1_1_F1 == 0 ~ "Do not own",
    Q3_1_F1 == 0 ~ "Do not own",
    TRUE ~ call_own_weekend_F1
  ) %>% fct_relevel(., get_labels(Q11_2_F1)) ) %>%
  mutate(calls_own_avg_BL = (5 * case_when(
      call_own_weekday_BL == "Do not own" ~ 0,
      Q11_1_BL == 0 ~ 0,
      Q11_1_BL == 1 ~ 0.05,
      Q11_1_BL == 2 ~ 0.175,
      Q11_1_BL == 3 ~ 0.383,
      Q11_1_BL == 4 ~ 0.75,
      Q11_1_BL == 5 ~ 1.5,
      Q11_1_BL == 6 ~ 3,
      TRUE ~ NA_real_
    ) + 2 * case_when(
      call_own_weekend_BL == "Do not own" ~ 0,
      Q11_2_BL == 0 ~ 0,
      Q11_2_BL == 1 ~ 0.05,
      Q11_2_BL == 2 ~ 0.175,
      Q11_2_BL == 3 ~ 0.383,
      Q11_2_BL == 4 ~ 0.75,
      Q11_2_BL == 5 ~ 1.5,
      Q11_2_BL == 6 ~ 3,  
      TRUE ~ NA_real_
    )) / 7
  ) %>%
  mutate(calls_own_avg_F1 = (5 * case_when(
    call_own_weekday_F1 == "Do not own" ~ 0,
    Q11_1_F1 == 0 ~ 0,
    Q11_1_F1 == 1 ~ 0.05,
    Q11_1_F1 == 2 ~ 0.175,
    Q11_1_F1 == 3 ~ 0.383,
    Q11_1_F1 == 4 ~ 0.75,
    Q11_1_F1 == 5 ~ 1.5,
    Q11_1_F1 == 6 ~ 3,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    call_own_weekend_F1 == "Do not own" ~ 0,
    Q11_2_F1 == 0 ~ 0,
    Q11_2_F1 == 1 ~ 0.05,
    Q11_2_F1 == 2 ~ 0.175,
    Q11_2_F1 == 3 ~ 0.383,
    Q11_2_F1 == 4 ~ 0.75,
    Q11_2_F1 == 5 ~ 1.5,
    Q11_2_F1 == 6 ~ 3,  
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  ## RELATIVE PHONE CALL DURATION
  mutate(call_others_weekday_BL = replace_with_codebook(Q26_1_BL, "Q26_1_BL") %>% as.character()) %>%
  mutate(call_others_weekday_F1 = replace_with_codebook(Q26_1_F1, "Q26_1_F1") %>% as.character()) %>%
  mutate(call_others_weekend_BL = replace_with_codebook(Q26_2_BL, "Q26_2_BL") %>% as.character()) %>%
  mutate(call_others_weekend_F1 = replace_with_codebook(Q26_2_F1, "Q26_2_F1") %>% as.character()) %>%
  mutate(call_others_weekday_BL = case_when(
    Q1_1_BL == 0 ~ "Never used",
    Q3_1_BL == 0 ~ "Never used",
    Q2_1_BL == 0 & Q2_2_BL == 0 & Q2_3_BL == 0 & Q2_4_BL == 1 & Q2_5_BL == 0 ~ "Use own MP only",
    TRUE ~ call_others_weekday_BL
  ) %>% fct_relevel(., get_labels(Q26_1_BL)) ) %>%
  mutate(call_others_weekday_F1 = case_when(
    Q1_1_F1 == 0 ~ "Never used",
    Q3_1_F1 == 0 ~ "Never used",
    Q2_1_F1 == 0 & Q2_2_F1 == 0 & Q2_3_F1 == 0 & Q2_4_F1 == 1 & Q2_5_F1 == 0 ~ "Use own MP only",
    TRUE ~ call_others_weekday_F1
  ) %>% fct_relevel(., get_labels(Q26_1_F1)) ) %>%
  mutate(call_others_weekend_BL = case_when(
    Q1_1_BL == 0 ~ "Never used",
    Q3_1_BL == 0 ~ "Never used",
    Q2_1_BL == 0 & Q2_2_BL == 0 & Q2_3_BL == 0 & Q2_4_BL == 1 & Q2_5_BL == 0 ~ "Use own MP only",
    TRUE ~ call_others_weekend_BL
  ) %>% fct_relevel(., get_labels(Q26_2_BL)) ) %>%
  mutate(call_others_weekend_F1 = case_when(
    Q1_1_F1 == 0 ~ "Never used",
    Q3_1_F1 == 0 ~ "Never used",
    Q2_1_F1 == 0 & Q2_2_F1 == 0 & Q2_3_F1 == 0 & Q2_4_F1 == 1 & Q2_5_F1 == 0 ~ "Use own MP only",
    TRUE ~ call_others_weekend_F1
  ) %>% fct_relevel(., get_labels(Q26_2_F1)) ) %>%
  mutate(calls_others_avg_BL = (5 * case_when(
    call_others_weekday_BL == "Never used" ~ 0,
    Q26_1_BL == 0 ~ 0,
    Q26_1_BL == 1 ~ 0.05,
    Q26_1_BL == 2 ~ 0.175,
    Q26_1_BL == 3 ~ 0.383,
    Q26_1_BL == 4 ~ 0.75,
    Q26_1_BL == 5 ~ 1.5,
    Q26_1_BL == 6 ~ 3,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    call_others_weekend_BL == "Never used" ~ 0,
    Q26_2_BL == 0 ~ 0,
    Q26_2_BL == 1 ~ 0.05,
    Q26_2_BL == 2 ~ 0.175,
    Q26_2_BL == 3 ~ 0.383,
    Q26_2_BL == 4 ~ 0.75,
    Q26_2_BL == 5 ~ 1.5,
    Q26_2_BL == 6 ~ 3,  
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(calls_others_avg_F1 = (5 * case_when(
    call_others_weekday_F1 == "Never used" ~ 0,
    Q26_1_F1 == 0 ~ 0,
    Q26_1_F1 == 1 ~ 0.05,
    Q26_1_F1 == 2 ~ 0.175,
    Q26_1_F1 == 3 ~ 0.383,
    Q26_1_F1 == 4 ~ 0.75,
    Q26_1_F1 == 5 ~ 1.5,
    Q26_1_F1 == 6 ~ 3,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    call_others_weekend_F1 == "Never used" ~ 0,
    Q26_2_F1 == 0 ~ 0,
    Q26_2_F1 == 1 ~ 0.05,
    Q26_2_F1 == 2 ~ 0.175,
    Q26_2_F1 == 3 ~ 0.383,
    Q26_2_F1 == 4 ~ 0.75,
    Q26_2_F1 == 5 ~ 1.5,
    Q26_2_F1 == 6 ~ 3,  
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(calls_avg_BL = calls_own_avg_BL + calls_others_avg_BL) %>%
  mutate(calls_avg_F1 = calls_own_avg_F1 + calls_others_avg_F1) %>%
  ## SOCIAL MEDIA PHONE 
  mutate(sns_mp_weekday_BL = replace_with_codebook(Q49_1_BL, "Q49_1_BL") %>% as.character()) %>%
  mutate(sns_mp_weekday_F1 = replace_with_codebook(Q49_1_F1, "Q49_1_F1") %>% as.character()) %>%
  mutate(sns_mp_weekend_BL = replace_with_codebook(Q49_2_BL, "Q49_2_BL") %>% as.character()) %>%
  mutate(sns_mp_weekend_F1 = replace_with_codebook(Q49_2_F1, "Q49_2_F1") %>% as.character()) %>%
  mutate(sns_od_weekday_BL = replace_with_codebook(Q49_3_BL, "Q49_3_BL") %>% as.character()) %>%
  mutate(sns_od_weekday_F1 = replace_with_codebook(Q49_3_F1, "Q49_3_F1") %>% as.character()) %>%
  mutate(sns_od_weekend_BL = replace_with_codebook(Q49_4_BL, "Q49_4_BL") %>% as.character()) %>%
  mutate(sns_od_weekend_F1 = replace_with_codebook(Q49_4_F1, "Q49_4_F1") %>% as.character()) %>%
  
  mutate(sns_mp_weekday_BL = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ sns_mp_weekday_BL
  ) %>% fct_relevel(c(get_labels(Q49_1_BL), "Do not own", "No internet"))) %>%
  
  mutate(sns_mp_weekday_F1 = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ sns_mp_weekday_F1
  ) %>% fct_relevel(c(get_labels(Q49_1_F1), "Do not own", "No internet"))) %>%
  
  mutate(sns_mp_weekend_BL = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ sns_mp_weekend_BL
  ) %>% fct_relevel(c(get_labels(Q49_2_BL), "Do not own", "No internet"))) %>%
  
  mutate(sns_mp_weekend_F1 = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ sns_mp_weekend_F1
  ) %>% fct_relevel(c(get_labels(Q49_2_F1), "Do not own", "No internet"))) %>%
  
  mutate(sns_mp_avg_BL = (5 * case_when(
    sns_mp_weekday_BL == "Do not own" ~ 0,
    sns_mp_weekday_BL == "No internet" ~ 0,
    Q49_1_BL == 0 ~ 0,
    Q49_1_BL == 1 ~ 0.092,
    Q49_1_BL == 2 ~ 0.342,
    Q49_1_BL == 3 ~ 0.75,
    Q49_1_BL == 4 ~ 1.5,
    Q49_1_BL == 5 ~ 3.5,
    Q49_1_BL == 6 ~ 5,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    sns_mp_weekend_BL == "Do not own" ~ 0,
    sns_mp_weekend_BL == "No internet" ~ 0,
    Q49_2_BL == 0 ~ 0,
    Q49_2_BL == 1 ~ 0.092,
    Q49_2_BL == 2 ~ 0.342,
    Q49_2_BL == 3 ~ 0.75,
    Q49_2_BL == 4 ~ 1.5,
    Q49_2_BL == 5 ~ 3.5,
    Q49_2_BL == 6 ~ 5, 
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(sns_od_avg_BL = (5 * case_when(
    Q49_3_BL == 0 ~ 0,
    Q49_3_BL == 1 ~ 0.092,
    Q49_3_BL == 2 ~ 0.342,
    Q49_3_BL == 3 ~ 0.75,
    Q49_3_BL == 4 ~ 1.5,
    Q49_3_BL == 5 ~ 3.5,
    Q49_3_BL == 6 ~ 5.5,
    Q49_3_BL == 7 ~ 7,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q49_4_BL == 0 ~ 0,
    Q49_4_BL == 1 ~ 0.092,
    Q49_4_BL == 2 ~ 0.342,
    Q49_4_BL == 3 ~ 0.75,
    Q49_4_BL == 4 ~ 1.5,
    Q49_4_BL == 5 ~ 3.5,
    Q49_4_BL == 6 ~ 5.5,
    Q49_4_BL == 7 ~ 7, 
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(sns_mp_avg_F1 = (5 * case_when(
    sns_mp_weekday_F1 == "Do not own" ~ 0,
    sns_mp_weekday_F1 == "No internet" ~ 0,
    Q49_1_F1 == 0 ~ 0,
    Q49_1_F1 == 1 ~ 0.092,
    Q49_1_F1 == 2 ~ 0.342,
    Q49_1_F1 == 3 ~ 0.75,
    Q49_1_F1 == 4 ~ 1.5,
    Q49_1_F1 == 5 ~ 3.5,
    Q49_1_F1 == 6 ~ 5,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    sns_mp_weekend_F1 == "Do not own" ~ 0,
    sns_mp_weekend_F1 == "No internet" ~ 0,
    Q49_2_F1 == 0 ~ 0,
    Q49_2_F1 == 1 ~ 0.092,
    Q49_2_F1 == 2 ~ 0.342,
    Q49_2_F1 == 3 ~ 0.75,
    Q49_2_F1 == 4 ~ 1.5,
    Q49_2_F1 == 5 ~ 3.5,
    Q49_2_F1 == 6 ~ 5, 
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(sns_od_avg_F1 = (5 * case_when(
    Q49_3_F1 == 0 ~ 0,
    Q49_3_F1 == 1 ~ 0.092,
    Q49_3_F1 == 2 ~ 0.342,
    Q49_3_F1 == 3 ~ 0.75,
    Q49_3_F1 == 4 ~ 1.5,
    Q49_3_F1 == 5 ~ 3.5,
    Q49_3_F1 == 6 ~ 5.5,
    Q49_3_F1 == 7 ~ 7,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q49_4_F1 == 0 ~ 0,
    Q49_4_F1 == 1 ~ 0.092,
    Q49_4_F1 == 2 ~ 0.342,
    Q49_4_F1 == 3 ~ 0.75,
    Q49_4_F1 == 4 ~ 1.5,
    Q49_4_F1 == 5 ~ 3.5,
    Q49_4_F1 == 6 ~ 5.5,
    Q49_4_F1 == 7 ~ 7, 
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  ### INTERNET ON PHONE
  mutate(internet_weekday_BL = replace_with_codebook(Q20_1_BL, "Q20_1_BL") %>% as.character()) %>%
  mutate(internet_weekend_BL = replace_with_codebook(Q20_2_BL, "Q20_2_BL") %>% as.character()) %>%
  mutate(internet_weekday_F1 = replace_with_codebook(Q20_1_F1, "Q20_1_F1") %>% as.character()) %>%
  mutate(internet_weekend_F1 = replace_with_codebook(Q20_2_F1, "Q20_2_F1") %>% as.character()) %>%
  
  mutate(internet_weekday_BL = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ internet_weekday_BL
  ) %>% fct_relevel(c(get_labels(Q20_1_BL), "Do not own", "No internet"))) %>%
  
  mutate(internet_weekday_F1 = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ internet_weekday_F1
  ) %>% fct_relevel(c(get_labels(Q20_1_F1), "Do not own", "No internet"))) %>%
  
  mutate(internet_weekend_BL = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ internet_weekend_BL
  ) %>% fct_relevel(c(get_labels(Q20_2_BL), "Do not own", "No internet"))) %>%
  
  mutate(internet_weekend_F1 = case_when(
    Q1_1_BL == 0 ~ "Do not own",
    Q3_1_BL == 0 ~ "Do not own",
    Q6_1_BL == 0 ~ "No internet",
    Q6_1_BL == 1 ~ "No internet",
    TRUE ~ internet_weekend_F1
  ) %>% fct_relevel(c(get_labels(Q20_2_F1), "Do not own", "No internet"))) %>%
  
  mutate(internet_avg_BL = (5 * case_when(
    Q20_1_BL == 0 ~ 0,
    Q20_1_BL == 1 ~ 0.092,
    Q20_1_BL == 2 ~ 0.342,
    Q20_1_BL == 3 ~ 0.75,
    Q20_1_BL == 4 ~ 1.5,
    Q20_1_BL == 5 ~ 3.5,
    Q20_1_BL == 6 ~ 5.5,
    Q20_1_BL == 7 ~ 7,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q20_2_BL == 0 ~ 0,
    Q20_2_BL == 1 ~ 0.092,
    Q20_2_BL == 2 ~ 0.342,
    Q20_2_BL == 3 ~ 0.75,
    Q20_2_BL == 4 ~ 1.5,
    Q20_2_BL == 5 ~ 3.5,
    Q20_2_BL == 6 ~ 5.5, 
    Q20_2_BL == 7 ~ 7,
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(internet_avg_F1 = (5 * case_when(
    Q20_1_F1 == 0 ~ 0,
    Q20_1_F1 == 1 ~ 0.092,
    Q20_1_F1 == 2 ~ 0.342,
    Q20_1_F1 == 3 ~ 0.75,
    Q20_1_F1 == 4 ~ 1.5,
    Q20_1_F1 == 5 ~ 3.5,
    Q20_1_F1 == 6 ~ 5.5,
    Q20_1_F1 == 7 ~ 7,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q20_2_F1 == 0 ~ 0,
    Q20_2_F1 == 1 ~ 0.092,
    Q20_2_F1 == 2 ~ 0.342,
    Q20_2_F1 == 3 ~ 0.75,
    Q20_2_F1 == 4 ~ 1.5,
    Q20_2_F1 == 5 ~ 3.5,
    Q20_2_F1 == 6 ~ 5.5, 
    Q20_1_F1 == 7 ~ 7,
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  ## Number of instant messages per week
  mutate(im_weekday_BL = replace_with_codebook(Q17_1_BL, "Q17_1_BL")) %>%
  mutate(im_weekend_BL = replace_with_codebook(Q17_2_BL, "Q17_2_BL")) %>%
  mutate(im_weekday_F1 = replace_with_codebook(Q17_1_F1, "Q17_1_F1")) %>%
  mutate(im_weekend_F1 = replace_with_codebook(Q17_2_F1, "Q17_2_F1")) %>%
  mutate(im_avg_BL = (5 * case_when(
    Q17_1_BL == 0 ~ 0,
    Q17_1_BL == 1 ~ (1+5)/2,
    Q17_1_BL == 2 ~ (6+10)/2,
    Q17_1_BL == 3 ~ (11+40)/2,
    Q17_1_BL == 4 ~ (41+70)/2,
    Q17_1_BL == 5 ~ (71+100)/2,
    Q17_1_BL == 6 ~ 101,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q17_2_BL == 0 ~ 0,
    Q17_2_BL == 1 ~ (1+5)/2,
    Q17_2_BL == 2 ~ (6+10)/2,
    Q17_2_BL == 3 ~ (11+40)/2,
    Q17_2_BL == 4 ~ (41+70)/2,
    Q17_2_BL == 5 ~ (71+100)/2,
    Q17_2_BL == 6 ~ 101,
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(im_avg_F1 = (5 * case_when(
    Q17_1_F1 == 0 ~ 0,
    Q17_1_F1 == 1 ~ (1+5)/2,
    Q17_1_F1 == 2 ~ (6+10)/2,
    Q17_1_F1 == 3 ~ (11+40)/2,
    Q17_1_F1 == 4 ~ (41+70)/2,
    Q17_1_F1 == 5 ~ (71+100)/2,
    Q17_1_F1 == 6 ~ 101,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q17_2_F1 == 0 ~ 0,
    Q17_2_F1 == 1 ~ (1+5)/2,
    Q17_2_F1 == 2 ~ (6+10)/2,
    Q17_2_F1 == 3 ~ (11+40)/2,
    Q17_2_F1 == 4 ~ (41+70)/2,
    Q17_2_F1 == 5 ~ (71+100)/2,
    Q17_2_F1 == 6 ~ 101,
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  ### Number of text messages
  mutate(text_weekday_BL = replace_with_codebook(Q16_1_BL, "Q16_1_BL")) %>%
  mutate(text_weekend_BL = replace_with_codebook(Q16_2_BL, "Q16_2_BL")) %>%
  mutate(text_weekday_F1 = replace_with_codebook(Q16_1_F1, "Q16_1_F1")) %>%
  mutate(text_weekend_F1 = replace_with_codebook(Q16_2_F1, "Q16_2_F1")) %>%
  mutate(text_avg_BL = (5 * case_when(
    Q16_1_BL == 0 ~ 0,
    Q16_1_BL == 1 ~ (1+5)/2,
    Q16_1_BL == 2 ~ (6+10)/2,
    Q16_1_BL == 3 ~ (11+40)/2,
    Q16_1_BL == 4 ~ (41+70)/2,
    Q16_1_BL == 5 ~ (71+100)/2,
    Q16_1_BL == 6 ~ 101,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q16_2_BL == 0 ~ 0,
    Q16_2_BL == 1 ~ (1+5)/2,
    Q16_2_BL == 2 ~ (6+10)/2,
    Q16_2_BL == 3 ~ (11+40)/2,
    Q16_2_BL == 4 ~ (41+70)/2,
    Q16_2_BL == 5 ~ (71+100)/2,
    Q16_2_BL == 6 ~ 101,
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  mutate(text_avg_F1 = (5 * case_when(
    Q16_1_F1 == 0 ~ 0,
    Q16_1_F1 == 1 ~ (1+5)/2,
    Q16_1_F1 == 2 ~ (6+10)/2,
    Q16_1_F1 == 3 ~ (11+40)/2,
    Q16_1_F1 == 4 ~ (41+70)/2,
    Q16_1_F1 == 5 ~ (71+100)/2,
    Q16_1_F1 == 6 ~ 101,
    TRUE ~ NA_real_
  ) + 2 * case_when(
    Q16_2_F1 == 0 ~ 0,
    Q16_2_F1 == 1 ~ (1+5)/2,
    Q16_2_F1 == 2 ~ (6+10)/2,
    Q16_2_F1 == 3 ~ (11+40)/2,
    Q16_2_F1 == 4 ~ (41+70)/2,
    Q16_2_F1 == 5 ~ (71+100)/2,
    Q16_2_F1 == 6 ~ 101,
    TRUE ~ NA_real_
  )) / 7
  ) %>%
  ### Add exposure covariate factors for FOLLOWUP
  # Parental control
  mutate(parental_control_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_used_phone",
    Q27_1_F1 == 9999 ~ NA_character_,
    Q27_1_F1 == 1 ~ "yes",
    Q27_1_F1 == 0 ~ "no",
    TRUE~ NA_character_
  ) %>% as.factor()) %>% 
  mutate(ever_owned_F1 = as.factor(Q1_1_F1)) %>% # Ever owned
  # Do you currently own? 
  mutate(current_own_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_owned",
    Q3_1_F1 == 9999 ~ NA_character_,
    Q3_1_F1 == 0 ~ "never_owned",
    Q3_1_F1 == 1 ~ "used_to_own",
    Q3_1_F1 == 2 ~ "own_one",
    Q3_1_F1 == 3 ~ "own_two_plus",
    TRUE~ NA_character_
  )) %>% 
  # What year did you get a phone?
  mutate(first_phone_year_F1 = case_when(
    Q4_1_F1 == 7777 | Q4_1_F1 == 8888 | Q4_1_F1 == 9999 ~ NA_integer_,
    TRUE ~ Q4_1_F1
  )) %>% 
  # Internet enabled?
  mutate(int_enabled_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_owned",
    current_own_F1 == "never_owned" ~ "never_owned",
    current_own_F1 == "used_to_own" ~ "used_to_own",
    Q6_1_F1 == 0 ~ "no",
    Q6_1_F1 == 1 ~ "no",
    Q6_1_F1 == 2 ~ "network_wifi",
    Q6_1_F1 == 3 ~ "wifi_only",
    Q6_1_F1 == 4 ~ "network_only",
    TRUE ~ NA_character_
  )) %>% # Is it internet enabled?
  # is it a smartphone?
  mutate(smartphone_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_owned",
    current_own_F1 == "never_owned" ~ "never_owned",
    current_own_F1 == "used_to_own" ~ "used_to_own",
    Q7_1_F1 == 0 ~ "no",
    Q7_1_F1 == 1 ~ "yes",
    TRUE~ NA_character_
  )) %>%
  # First year smartphone
  mutate(smartphone_year_status_F1 = case_when(
    Q8_1_F1 == 8888 ~ "no_smartphone", 
    Q8_1_F1 == 9999 ~ NA_character_, 
    Q8_1_F1 == 7777 ~ "don't_know", 
    TRUE ~ as.character(Q8_1_F1)
  )) %>%
  mutate(smartphone_year_value_F1 = case_when(
    Q8_1_F1 == 8888  | Q8_1_F1 == 9999 | Q8_1_F1 == 7777 ~ NA_integer_,
    TRUE ~ Q8_1_F1
  )) %>%
  ### Add exposure covariate factors for BASELINE
  # Parental control
  mutate(parental_control_BL = case_when(
    Q1_1_BL == 0 ~ "never_used_phone",
    Q27_1_BL == 9999 ~ NA_character_,
    Q27_1_BL == 1 ~ "yes",
    Q27_1_BL == 0 ~ "no",
    TRUE~ NA_character_
  ) %>% as.factor()) %>% 
  mutate(ever_owned_BL = replace_with_codebook(Q1_1_BL, "Q1_1_BL")) %>% # Ever owned
  # Do you currently own? 
  mutate(current_own_BL = case_when(
    Q1_1_BL == 0 ~ "never_owned",
    Q3_1_BL == 9999 ~ NA_character_,
    Q3_1_BL == 0 ~ "never_owned",
    Q3_1_BL == 1 ~ "used_to_own",
    Q3_1_BL == 2 ~ "own_one",
    Q3_1_BL == 3 ~ "own_two_plus",
    TRUE~ NA_character_
  )) %>% 
  # What year did you get a phone?
  mutate(first_phone_year_BL = case_when(
    Q4_1_BL == 7777 | Q4_1_BL == 8888 | Q4_1_BL == 9999 ~ NA_integer_,
    TRUE ~ Q4_1_BL
  )) %>% 
  # Internet enabled?
  mutate(int_enabled_BL = case_when(
    Q1_1_BL == 0 ~ "never_owned",
    current_own_BL == "never_owned" ~ "never_owned",
    current_own_BL == "used_to_own" ~ "used_to_own",
    Q6_1_BL == 0 ~ "no",
    Q6_1_BL == 1 ~ "no",
    Q6_1_BL == 2 ~ "network_wifi",
    Q6_1_BL == 3 ~ "wifi_only",
    Q6_1_BL == 4 ~ "network_only",
    TRUE ~ NA_character_
  )) %>% # Is it internet enabled?
  # is it a smartphone?
  mutate(smartphone_BL = case_when(
    Q1_1_BL == 0 ~ "never_owned",
    current_own_BL == "never_owned" ~ "never_owned",
    current_own_BL == "used_to_own" ~ "used_to_own",
    Q7_1_BL == 0 ~ "no",
    Q7_1_BL == 1 ~ "yes",
    TRUE~ NA_character_
  )) %>%
  # First year smartphone
  mutate(smartphone_year_status_BL = case_when(
    Q8_1_BL == 8888 ~ "no_smartphone", 
    Q8_1_BL == 9999 ~ NA_character_, 
    Q8_1_BL == 7777 ~ "don't_know", 
    TRUE ~ as.character(Q8_1_BL)
  )) %>%
  mutate(smartphone_year_value_BL = case_when(
    Q8_1_BL == 8888  | Q8_1_BL == 9999 | Q8_1_BL == 7777 ~ NA_integer_,
    TRUE ~ Q8_1_BL
  )) %>%

  ### Add exposure covariate factors for FOLLOWUP
  # Parental control
  mutate(parental_control_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_used_phone",
    Q27_1_F1 == 9999 ~ NA_character_,
    Q27_1_F1 == 1 ~ "yes",
    Q27_1_F1 == 0 ~ "no",
    TRUE~ NA_character_
  ) %>% as.factor()) %>% 
  mutate(ever_owned_F1 = as.factor(Q1_1_F1)) %>% # Ever owned
  # Do you currently own? 
  mutate(current_own_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_owned",
    Q3_1_F1 == 9999 ~ NA_character_,
    Q3_1_F1 == 0 ~ "never_owned",
    Q3_1_F1 == 1 ~ "used_to_own",
    Q3_1_F1 == 2 ~ "own_one",
    Q3_1_F1 == 3 ~ "own_two_plus",
    TRUE~ NA_character_
  )) %>% 
  # What year did you get a phone?
  mutate(first_phone_year_F1 = case_when(
    Q4_1_F1 == 7777 | Q4_1_F1 == 8888 | Q4_1_F1 == 9999 ~ NA_integer_,
    TRUE ~ Q4_1_F1
  )) %>% 
  # Internet enabled?
  mutate(int_enabled_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_owned",
    current_own_F1 == "never_owned" ~ "never_owned",
    current_own_F1 == "used_to_own" ~ "used_to_own",
    Q6_1_F1 == 0 ~ "no",
    Q6_1_F1 == 1 ~ "no",
    Q6_1_F1 == 2 ~ "network_wifi",
    Q6_1_F1 == 3 ~ "wifi_only",
    Q6_1_F1 == 4 ~ "network_only",
    TRUE ~ NA_character_
  )) %>% # Is it internet enabled?
  # is it a smartphone?
  mutate(smartphone_F1 = case_when(
    Q1_1_F1 == 0 ~ "never_owned",
    current_own_F1 == "never_owned" ~ "never_owned",
    current_own_F1 == "used_to_own" ~ "used_to_own",
    Q7_1_F1 == 0 ~ "no",
    Q7_1_F1 == 1 ~ "yes",
    TRUE~ NA_character_
  )) %>%
  # First year smartphone
  mutate(smartphone_year_status_F1 = case_when(
    Q8_1_F1 == 8888 ~ "no_smartphone", 
    Q8_1_F1 == 9999 ~ NA_character_, 
    Q8_1_F1 == 7777 ~ "don't_know", 
    TRUE ~ as.character(Q8_1_F1)
  )) %>%
  mutate(smartphone_year_value_F1 = case_when(
    Q8_1_F1 == 8888  | Q8_1_F1 == 9999 | Q8_1_F1 == 7777 ~ NA_integer_,
    TRUE ~ Q8_1_F1
  )) %>%
  # Condition phone ownership variables
  mutate(ownership_BL = case_when(
    current_own_BL == "own_one" | current_own_BL == "own_two_plus" ~ "yes",
    TRUE ~ current_own_BL
  )) %>%
  mutate(ownership_BL = fct_relevel(factor(ownership_BL), "yes")) %>%
  mutate(ownership_F1 = case_when(
    current_own_F1 == "own_one" | current_own_F1 == "own_two_plus" ~ "yes",
    TRUE ~ current_own_F1
  )) %>%
  mutate(ownership_F1 = fct_relevel(factor(ownership_F1), "yes")) %>%
  
  # Condition internet-enabled variables
  mutate(internet_BL = case_when(
    int_enabled_BL == "never_owned"  ~ "do not own",
    int_enabled_BL == "used_to_own"  ~ "do not own",
    int_enabled_BL == "no" ~ "phone without internet",
    !is.na(int_enabled_BL) ~ "yes",
    TRUE ~ NA_character_
  )) %>%
  mutate(internet_BL = fct_relevel(factor(internet_BL), "yes")) %>%
  mutate(internet_F1 = case_when(
    int_enabled_F1 == "never_owned"  ~ "do not own",
    int_enabled_F1 == "used_to_own"  ~ "do not own",
    int_enabled_F1 == "no" ~ "phone without internet",
    !is.na(int_enabled_F1) ~ "yes",
    TRUE ~ NA_character_
  )) %>%
  mutate(internet_F1 = fct_relevel(factor(internet_F1), "yes"))


#### Mental Health variable processing
psytools <- psytools %>%
  mutate(
    across(c(
      Q114_1_F1, Q114_2_F1, Q114_3_F1, Q114_4_F1, Q114_5_F1, Q114_6_F1,
      Q114_7_F1, Q114_8_F1, Q114_9_F1, Q114_10_F1, Q114_11_F1, Q114_12_F1, 
      Q114_13_F1, Q114_14_F1, Q114_15_F1, Q114_16_F1
    ), ~ replace_missing_codes(.x)
    )) %>%
  rowwise() %>%
  mutate(gad_sum = sum(
    Q114_1_F1, Q114_2_F1, Q114_3_F1, Q114_4_F1, Q114_5_F1, Q114_6_F1, Q114_7_F1
  )) %>% 
  mutate(phq_sum = sum(
    Q114_8_F1, Q114_9_F1, Q114_10_F1, Q114_11_F1, Q114_12_F1, 
    Q114_3_F1, Q114_14_F1, Q114_15_F1, Q114_16_F1
  )) %>% 
  ungroup()

##Process SDQ data
sdq_doc <- read_excel("./docs/sdq_docs.xlsx", col_names = FALSE) %>%
  select(1,3) %>%
  .[1:25,] %>%
  as.data.frame()

colnames(sdq_doc) <- c("sdq_item", "sdq_item_desc")
sdq_doc <- sdq_doc %>%
  mutate(sdq_item = paste0("s", sdq_item))

scamp_sdq_varname <- rep(NA, 25)
for (i in 1:25){
  scamp_sdq_varname[i] <- paste0("Q79_", i)
}
 
newnames_bl <- paste0(sdq_doc$sdq_item, "_BL")
newnames_f1 <- paste0(sdq_doc$sdq_item, "_F1")
oldnames_bl <- paste0(scamp_sdq_varname, "_BL")
oldnames_f1 <- paste0(scamp_sdq_varname, "_F1")

# Rename Psytools based on sdq.com varnames
psytools <- psytools %>%
  rename_with(~ newnames_bl[which(oldnames_bl == .x)], .cols=all_of(oldnames_bl)) %>%
  rename_with(~ newnames_f1[which(oldnames_f1 == .x)], .cols=all_of(oldnames_f1))


# Recoding variables and then scoring the youth self-report SDQ scores
psytools <- psytools %>%
  mutate(
    across(all_of(c(newnames_bl, newnames_f1)), ~ replace_missing_codes(.x))
    ) %>%
  mutate(
    across(all_of(c(newnames_bl, newnames_f1)), ~ .x - 1)
    )
# Calculate baseline SDQ summaries
attach(psytools)
robeys_BL <- recode(sobeys_BL, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rreflect_BL <- recode(sreflect_BL, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rattends_BL <- recode(sattends_BL, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rfriend_BL <- recode(sfriend_BL, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rpopular_BL <- recode(spopular_BL, `0`=2, `1`=1, `2`=0, .default=NA_real_)

df.semotion_BL <- data.frame(ssomatic_BL, sworries_BL, sunhappy_BL, sclingy_BL, safraid_BL)
snemotion_BL <- apply(df.semotion_BL, 1, function(x) sum(is.na(x)))
semotion_BL <- ifelse(snemotion_BL<3, rowMeans(df.semotion_BL, na.rm=TRUE), NA)
semotion_BL <- as.numeric(semotion_BL) * 5
semotion_BL <- floor(0.5 + semotion_BL)

df.sconduct_BL <- data.frame(stantrum_BL, robeys_BL, sfights_BL, slies_BL, ssteals_BL)
snconduct_BL <- apply(df.sconduct_BL, 1, function(x) sum(is.na(x)))
sconduct_BL <- ifelse(snconduct_BL<3, rowMeans(df.sconduct_BL, na.rm=TRUE), NA)
sconduct_BL <- as.numeric(sconduct_BL) * 5
sconduct_BL <- floor(0.5 + sconduct_BL)

df.shyper_BL <- data.frame(srestles_BL, sfidgety_BL, sdistrac_BL, rreflect_BL, rattends_BL)
snhyper_BL <- apply(df.shyper_BL, 1, function(x) sum(is.na(x)))
shyper_BL <- ifelse(snhyper_BL<3, rowMeans(df.shyper_BL, na.rm=TRUE), NA)
shyper_BL <- as.numeric(shyper_BL) * 5
shyper_BL <- floor(0.5 + shyper_BL)

df.speer_BL <- data.frame(sloner_BL, rfriend_BL, rpopular_BL, sbullied_BL, soldbest_BL)
snpeer_BL <- apply(df.speer_BL, 1, function(x) sum(is.na(x)))
speer_BL <- ifelse(snpeer_BL<3, rowMeans(df.speer_BL, na.rm=TRUE), NA)
speer_BL <- as.numeric(speer_BL) * 5
speer_BL <- floor(0.5 + speer_BL)

df.sprosoc_BL <- data.frame(sconsid_BL, sshares_BL, scaring_BL, skind_BL, shelpout_BL)
snprosoc_BL <- apply(df.sprosoc_BL, 1, function(x) sum(is.na(x)))
sprosoc_BL <- ifelse(snprosoc_BL<3, rowMeans(df.sprosoc_BL, na.rm=TRUE), NA)
sprosoc_BL <- as.numeric(sprosoc_BL) * 5
sprosoc_BL <- floor(0.5 + sprosoc_BL)


psytools$sebdtot_BL <- semotion_BL+sconduct_BL+shyper_BL+speer_BL
psytools$semotion_BL <- semotion_BL
psytools$sconduct_BL <- sconduct_BL
psytools$shyper_BL <- shyper_BL
psytools$speer_BL <- speer_BL
psytools$sprosoc_BL <- sprosoc_BL

detach(psytools)

rm(robeys_BL, rattends_BL, rfriend_BL, rpopular_BL,  snemotion_BL, snconduct_BL, 
   snhyper_BL, snpeer_BL, snprosoc_BL,  df.semotion_BL, df.sconduct_BL, 
   df.shyper_BL, df.speer_BL, df.sprosoc_BL,
   semotion_BL, sconduct_BL, shyper_BL, speer_BL, sprosoc_BL)

# Calculate F1 SDQ summaries
attach(psytools)
robeys_F1 <- recode(sobeys_F1, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rreflect_F1 <- recode(sreflect_F1, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rattends_F1 <- recode(sattends_F1, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rfriend_F1 <- recode(sfriend_F1, `0`=2, `1`=1, `2`=0, .default=NA_real_)
rpopular_F1 <- recode(spopular_F1, `0`=2, `1`=1, `2`=0, .default=NA_real_)

df.semotion_F1 <- data.frame(ssomatic_F1, sworries_F1, sunhappy_F1, sclingy_F1, safraid_F1)
snemotion_F1 <- apply(df.semotion_F1, 1, function(x) sum(is.na(x)))
semotion_F1 <- ifelse(snemotion_F1<3, rowMeans(df.semotion_F1, na.rm=TRUE), NA)
semotion_F1 <- as.numeric(semotion_F1) * 5
semotion_F1 <- floor(0.5 + semotion_F1)

df.sconduct_F1 <- data.frame(stantrum_F1, robeys_F1, sfights_F1, slies_F1, ssteals_F1)
snconduct_F1 <- apply(df.sconduct_F1, 1, function(x) sum(is.na(x)))
sconduct_F1 <- ifelse(snconduct_F1<3, rowMeans(df.sconduct_F1, na.rm=TRUE), NA)
sconduct_F1 <- as.numeric(sconduct_F1) * 5
sconduct_F1 <- floor(0.5 + sconduct_F1)

df.shyper_F1 <- data.frame(srestles_F1, sfidgety_F1, sdistrac_F1, rreflect_F1, rattends_F1)
snhyper_F1 <- apply(df.shyper_F1, 1, function(x) sum(is.na(x)))
shyper_F1 <- ifelse(snhyper_F1<3, rowMeans(df.shyper_F1, na.rm=TRUE), NA)
shyper_F1 <- as.numeric(shyper_F1) * 5
shyper_F1 <- floor(0.5 + shyper_F1)

df.speer_F1 <- data.frame(sloner_F1, rfriend_F1, rpopular_F1, sbullied_F1, soldbest_F1)
snpeer_F1 <- apply(df.speer_F1, 1, function(x) sum(is.na(x)))
speer_F1 <- ifelse(snpeer_F1<3, rowMeans(df.speer_F1, na.rm=TRUE), NA)
speer_F1 <- as.numeric(speer_F1) * 5
speer_F1 <- floor(0.5 + speer_F1)

df.sprosoc_F1 <- data.frame(sconsid_F1, sshares_F1, scaring_F1, skind_F1, shelpout_F1)
snprosoc_F1 <- apply(df.sprosoc_F1, 1, function(x) sum(is.na(x)))
sprosoc_F1 <- ifelse(snprosoc_F1<3, rowMeans(df.sprosoc_F1, na.rm=TRUE), NA)
sprosoc_F1 <- as.numeric(sprosoc_F1) * 5
sprosoc_F1 <- floor(0.5 + sprosoc_F1)


psytools$sebdtot_F1 <- semotion_F1+sconduct_F1+shyper_F1+speer_F1
psytools$semotion_F1 <- semotion_F1
psytools$sconduct_F1 <- sconduct_F1
psytools$shyper_F1 <- shyper_F1
psytools$speer_F1 <- speer_F1
psytools$sprosoc_F1 <- sprosoc_F1

detach(psytools)

rm(robeys_F1, rattends_F1, rfriend_F1, rpopular_F1,  snemotion_F1, snconduct_F1, 
   snhyper_F1, snpeer_F1, snprosoc_F1,  df.semotion_F1, df.sconduct_F1, 
   df.shyper_F1, df.speer_F1, df.sprosoc_F1,
   semotion_F1, sconduct_F1, shyper_F1, speer_F1, sprosoc_F1)

## Processing confounding variables and covariates
## SLeep duration
psytools <- psytools %>%
  mutate(waketime_weekday_BL = case_when(
    Q50_1_BL == 1 ~ 6,
    Q50_1_BL == 18 ~ 14,
    Q50_1_BL == 9999 ~ NA_real_,
    TRUE ~ 5.75 + (Q50_1_BL-1)*0.5
  )) %>%
  mutate(waketime_weekend_BL = case_when(
    Q50_2_BL == 1 ~ 6,
    Q50_2_BL == 18 ~ 14,
    Q50_2_BL == 9999 ~ NA_real_,
    TRUE ~ 5.75 + (Q50_2_BL-1)*0.5
  )) %>%
  mutate(bedtime_weekday_BL = case_when(
    Q51_1_BL == 1 ~ 8.5,
    Q51_1_BL == 15 ~ 15,
    Q51_1_BL == 9999 ~ NA_real_,
    TRUE ~ 8.25 + (Q51_1_BL-1)*0.5
  )) %>%
  mutate(bedtime_weekend_BL = case_when(
    Q51_2_BL == 1 ~ 8.5,
    Q51_2_BL == 15 ~ 15,
    Q51_2_BL == 9999 ~ NA_real_,
    TRUE ~ 8.25 + (Q51_2_BL-1)*0.5
  )) %>%
  mutate(waketime_weekday_F1 = case_when(
    Q50_1_F1 == 1 ~ 6,
    Q50_1_F1 == 18 ~ 14,
    Q50_1_F1 == 9999 ~ NA_real_,
    TRUE ~ 5.75 + (Q50_1_F1-1)*0.5
  )) %>%
  mutate(waketime_weekend_F1 = case_when(
    Q50_2_F1 == 1 ~ 6,
    Q50_2_F1 == 18 ~ 14,
    Q50_2_F1 == 9999 ~ NA_real_,
    TRUE ~ 5.75 + (Q50_2_F1-1)*0.5
  )) %>%
  mutate(bedtime_weekday_F1 = case_when(
    Q51_1_F1 == 1 ~ 8.5,
    Q51_1_F1 == 15 ~ 15,
    Q51_1_F1 == 9999 ~ NA_real_,
    TRUE ~ 8.25 + (Q51_1_F1-1)*0.5
  )) %>%
  mutate(bedtime_weekend_F1 = case_when(
    Q51_2_F1 == 1 ~ 8.5,
    Q51_2_F1 == 15 ~ 15,
    Q51_2_F1 == 9999 ~ NA_real_,
    TRUE ~ 8.25 + (Q51_2_F1-1)*0.5
  )) %>%
  mutate(sleep_dtn_weekday_BL = waketime_weekday_BL - bedtime_weekday_BL + 12) %>%
  mutate(sleep_dtn_weekend_BL = waketime_weekend_BL - bedtime_weekend_BL + 12) %>%
  mutate(sleep_dtn_weekday_F1 = waketime_weekday_F1 - bedtime_weekday_F1 + 12) %>%
  mutate(sleep_dtn_weekend_F1 = waketime_weekend_F1 - bedtime_weekend_F1 + 12) %>%
  ## SLeep difficulties
  mutate(
    across(
      c(Q54_1_BL, Q54_2_BL, Q54_3_BL, Q54_4_BL, Q54_1_F1, Q54_2_F1, Q54_3_F1, Q54_4_F1), 
      ~ replace_missing_codes(.x)
    )
  ) %>%
  mutate(sleep_diff_BL = Q54_1_BL + Q54_2_BL + Q54_3_BL + Q54_4_BL) %>%
  mutate(sleep_diff_F1 = Q54_1_F1 + Q54_2_F1 + Q54_3_F1 + Q54_4_F1) %>%
  # Parental Occupation
  mutate(NSSEC = if_else(is.na(NSSEC_BL), NSSEC_F1, NSSEC_BL)) %>%
  mutate(parent_occ = case_when(
    NSSEC < 2.5 ~ "Managerial/professional",
    NSSEC < 4.5 ~ "Intermediate",
    NSSEC > 4.5 ~ "Routine and manual",
    TRUE ~ "Missing/Not interpretable"
  )) %>%
  # Parental HE
  mutate(across(c(Q70_1_BL, Q71_1_BL, Q70_1_F1, Q71_1_F1), ~ replace_missing_codes(.x))) %>%
  mutate(across(c(Q70_1_BL, Q71_1_BL, Q70_1_F1, Q71_1_F1), ~ replace_dont_know(.x))) %>%
  mutate(Q70_1 = if_else(is.na(Q70_1_BL), Q70_1_F1, Q70_1_BL)) %>%
  mutate(Q71_1 = if_else(is.na(Q71_1_BL), Q71_1_F1, Q71_1_BL)) %>%
  mutate(parent_he = case_when(
    Q70_1==1 ~ 0,
    Q71_1==1 ~ 0,
    Q70_1==0 & Q71_1==0 ~ 1,
    TRUE ~ as.numeric(Q70_1)
  )) %>%
  mutate(across(c(Q96_1_BL, Q96_2_BL, Q96_3_BL, Q96_1_F1, Q96_2_F1, Q96_3_F1), ~ replace_missing_codes(.x))) %>%
  mutate(across(c(Q96_1_BL, Q96_2_BL, Q96_3_BL, Q96_1_F1, Q96_2_F1, Q96_3_F1), ~ replace_dont_know(.x))) %>%
  mutate(across(c(Q96_1_BL, Q96_2_BL, Q96_3_BL, Q96_1_F1, Q96_2_F1, Q96_3_F1), ~ replace_6666(.x))) %>%
  mutate(alcohol = case_when(
    !is.na(Q96_1_BL) & is.na(Q96_1_F1) ~ Q96_1_BL,
    is.na(Q96_1_BL) & !is.na(Q96_1_F1) ~ Q96_1_F1,
    !is.na(Q96_1_BL) & !is.na(Q96_1_F1) ~ if_else(Q96_1_BL >= Q96_1_F1, Q96_1_BL, Q96_1_F1),
    TRUE ~ NA_integer_
  )) %>%
  mutate(alcohol_cat = factor(case_when(
    alcohol == 0 ~ "Never",
    alcohol <= 2 ~ "Once / A few times",
    alcohol <=5 ~ "Once a week or more",
    TRUE ~ NA_character_
    ), levels=c("Never", "Once / A few times", "Once a week or more"))) %>%
  mutate(smoking = case_when(
    !is.na(Q96_2_BL) & is.na(Q96_2_F1) ~ Q96_2_BL,
    is.na(Q96_2_BL) & !is.na(Q96_2_F1) ~ Q96_2_F1,
    !is.na(Q96_2_BL) & !is.na(Q96_2_F1) ~ if_else(Q96_2_BL >= Q96_2_F1, Q96_2_BL, Q96_2_F1),
    TRUE ~ NA_integer_
  )) %>%
  mutate(smoking_cat = factor(case_when(
    smoking == 0 ~ "Never",
    smoking <= 2 ~ "Once / A few times",
    smoking <=5 ~ "Once a week or more",
    TRUE ~ NA_character_
  ), levels=c("Never", "Once / A few times", "Once a week or more"))) %>%
  mutate(cannabis = case_when(
    !is.na(Q96_3_BL) & is.na(Q96_3_F1) ~ Q96_2_BL,
    is.na(Q96_3_BL) & !is.na(Q96_3_F1) ~ Q96_2_F1,
    !is.na(Q96_3_BL) & !is.na(Q96_3_F1) ~ if_else(Q96_3_BL >= Q96_3_F1, Q96_3_BL, Q96_3_F1),
    TRUE ~ NA_integer_
  )) %>%
  mutate(cannabis_cat = factor(case_when(
    cannabis == 0 ~ "Never",
    cannabis <= 2 ~ "Once / A few times",
    cannabis <=5 ~ "Once a week or more",
    TRUE ~ NA_character_
  ), levels=c("Never", "Once / A few times", "Once a week or more"))) %>%
  ## Physical Activity
  mutate(across(c(Q109_1_F1, Q109_2_F1, Q110_1_F1), ~ replace_missing_codes(.x))) %>%
  mutate(sport_school = case_when(
    Q109_1_F1 == 0 ~ 0,
    Q109_1_F1 == 1 ~ 0.5,
    Q109_1_F1 == 2 ~ 1,
    Q109_1_F1 == 3 ~ 2,
    Q109_1_F1 == 4 ~ 3,
    Q109_1_F1 == 5 ~ 4,
    Q109_1_F1 == 6 ~ 5,
    Q109_1_F1 == 7 ~ 6,
    Q109_1_F1 == 8 ~ 7,
    Q109_1_F1 == 9 ~ 8,
    Q109_1_F1 == 10 ~ 9,
    TRUE ~ NA_real_
  )) %>%
  mutate(sport_outside = case_when(
    Q109_2_F1 == 0 ~ 0,
    Q109_2_F1 == 1 ~ 0.5,
    Q109_2_F1 == 2 ~ 1,
    Q109_2_F1 == 3 ~ 2,
    Q109_2_F1 == 4 ~ 3,
    Q109_2_F1 == 5 ~ 4,
    Q109_2_F1 == 6 ~ 5,
    Q109_2_F1 == 7 ~ 6,
    Q109_2_F1 == 8 ~ 7,
    Q109_2_F1 == 9 ~ 8,
    Q109_2_F1 == 10 ~ 9,
    TRUE ~ NA_real_
  )) %>%
  mutate(sport_sparetime = case_when(
    Q110_1_F1 == 0 ~ 0,
    Q110_1_F1 == 1 ~ 0.5,
    Q110_1_F1 == 2 ~ 1,
    Q110_1_F1 == 3 ~ 2,
    Q110_1_F1 == 4 ~ 3,
    Q110_1_F1 == 5 ~ 4,
    Q110_1_F1 == 6 ~ 5,
    Q110_1_F1 == 7 ~ 6,
    Q110_1_F1 == 8 ~ 7,
    Q110_1_F1 == 9 ~ 8,
    Q110_1_F1 == 10 ~ 9,
    TRUE ~ NA_real_
  )) %>%
  mutate(physical_activity =  sport_school + sport_outside + sport_sparetime) %>%
  mutate(IMD_QUINTILE = case_when(
    IMD_DECILE_F1 <= 2 ~ 1,
    IMD_DECILE_F1 <= 4 ~ 2,
    IMD_DECILE_F1 <= 6 ~ 3,
    IMD_DECILE_F1 <= 8 ~ 4,
    IMD_DECILE_F1 <= 10 ~ 5,
    TRUE ~ NA_real_
  )) %>%
  mutate(ETHNICITY_F1 = if_else(ETHNICITY_F1 == "",NA_character_, ETHNICITY_F1)) %>%
  
  mutate(SCHOOL_ID_F1 = as.factor(SCHOOL_ID_F1)) %>%
  mutate(ETHNICITY_DESC = case_when(
    ETHNICITY == 1 ~ "White",
    ETHNICITY == 2 ~ "Black",
    ETHNICITY == 3 ~ "Asian",
    ETHNICITY == 4 ~ "Mixed",
    ETHNICITY == 5 ~ "Other",
    TRUE ~ NA_character_
  )) %>%
  mutate(ETHNICITY_DESC = fct_relevel(ETHNICITY_DESC, "White")) %>%
  mutate(SEX = case_when(
    SEX == 1 ~ "M",
    SEX == 2 ~ "F",
    TRUE ~ NA_character_
  )) %>%
  mutate(SEX = fct_relevel(SEX, "M")) %>%
  mutate(physical_activity_quintile = cut(physical_activity, breaks=5)) %>%
  mutate(physical_activity_quintile = fct_relevel(
    physical_activity_quintile, 
    levels(cut(physical_activity, breaks=5))[3]
    ))

# write.csv(psytools, "./data/psytools_cleaned.csv", row.names = FALSE)


