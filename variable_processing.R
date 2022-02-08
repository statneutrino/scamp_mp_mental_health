library(tidyverse)
library(readxl)

#### Import Psytools Data
psytools = read.csv("data/psytools_school.csv")

#### Import Codebook
codebook = read_excel("docs/codebook.xlsx") %>%
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

psytools <- psytools %>%
  mutate(
    across(c(
      Q11_1_BL, Q11_2_BL, Q11_1_F1, Q11_2_F1, # own call duration
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
  mutate(call_own_weekday_BL = as.factor(Q11_1_BL)) %>%
  mutate(call_own_weekday_F1 = as.factor(Q11_1_F1)) %>%
  mutate(call_own_weekend_BL = as.factor(Q11_2_BL)) %>%
  mutate(call_own_weekend_F1 = as.factor(Q11_2_F1)) %>%
  mutate(calls_own_avg_BL = (5 * case_when(
      Q11_1_BL == 0 ~ 0,
      Q11_1_BL == 1 ~ 0.05,
      Q11_1_BL == 2 ~ 0.175,
      Q11_1_BL == 3 ~ 0.383,
      Q11_1_BL == 4 ~ 0.75,
      Q11_1_BL == 5 ~ 1.5,
      Q11_1_BL == 6 ~ 3,
      TRUE ~ NA_real_
    ) + 2 * case_when(
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
    Q11_1_F1 == 0 ~ 0,
    Q11_1_F1 == 1 ~ 0.05,
    Q11_1_F1 == 2 ~ 0.175,
    Q11_1_F1 == 3 ~ 0.383,
    Q11_1_F1 == 4 ~ 0.75,
    Q11_1_F1 == 5 ~ 1.5,
    Q11_1_F1 == 6 ~ 3,
    TRUE ~ NA_real_
  ) + 2 * case_when(
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
  mutate(call_others_weekday_BL = as.factor(Q26_1_BL)) %>%
  mutate(call_others_weekday_F1 = as.factor(Q26_1_F1)) %>%
  mutate(call_others_weekend_BL = as.factor(Q26_2_BL)) %>%
  mutate(call_others_weekend_F1 = as.factor(Q26_2_F1)) %>%
  mutate(calls_others_avg_BL = (5 * case_when(
    Q26_1_BL == 0 ~ 0,
    Q26_1_BL == 1 ~ 0.05,
    Q26_1_BL == 2 ~ 0.175,
    Q26_1_BL == 3 ~ 0.383,
    Q26_1_BL == 4 ~ 0.75,
    Q26_1_BL == 5 ~ 1.5,
    Q26_1_BL == 6 ~ 3,
    TRUE ~ NA_real_
  ) + 2 * case_when(
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
    Q26_1_F1 == 0 ~ 0,
    Q26_1_F1 == 1 ~ 0.05,
    Q26_1_F1 == 2 ~ 0.175,
    Q26_1_F1 == 3 ~ 0.383,
    Q26_1_F1 == 4 ~ 0.75,
    Q26_1_F1 == 5 ~ 1.5,
    Q26_1_F1 == 6 ~ 3,
    TRUE ~ NA_real_
  ) + 2 * case_when(
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
  ## SOCIAL MEDIA PHONE 
  mutate(sns_mp_weekday_BL = as.factor(Q49_1_BL)) %>%
  mutate(sns_mp_weekday_F1 = as.factor(Q49_1_F1)) %>%
  mutate(sns_mp_weekend_BL = as.factor(Q49_2_BL)) %>%
  mutate(sns_mp_weekend_F1 = as.factor(Q49_2_F1)) %>%
  mutate(sns_od_weekday_BL = as.factor(Q49_3_BL)) %>%
  mutate(sns_od_weekday_F1 = as.factor(Q49_3_F1)) %>%
  mutate(sns_od_weekend_BL = as.factor(Q49_4_BL)) %>%
  mutate(sns_od_weekend_F1 = as.factor(Q49_4_F1)) %>%
  mutate(sns_mp_avg_BL = (5 * case_when(
    Q49_1_BL == 0 ~ 0,
    Q49_1_BL == 1 ~ 0.092,
    Q49_1_BL == 2 ~ 0.342,
    Q49_1_BL == 3 ~ 0.75,
    Q49_1_BL == 4 ~ 1.5,
    Q49_1_BL == 5 ~ 3.5,
    Q49_1_BL == 6 ~ 5,
    TRUE ~ NA_real_
  ) + 2 * case_when(
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
    Q49_1_F1 == 0 ~ 0,
    Q49_1_F1 == 1 ~ 0.092,
    Q49_1_F1 == 2 ~ 0.342,
    Q49_1_F1 == 3 ~ 0.75,
    Q49_1_F1 == 4 ~ 1.5,
    Q49_1_F1 == 5 ~ 3.5,
    Q49_1_F1 == 6 ~ 5,
    TRUE ~ NA_real_
  ) + 2 * case_when(
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
  mutate(internet_weekday_BL = as.factor(Q20_1_BL)) %>%
  mutate(internet_weekend_BL = as.factor(Q20_2_BL)) %>%
  mutate(internet_weekday_F1 = as.factor(Q20_1_F1)) %>%
  mutate(internet_weekend_F1 = as.factor(Q20_2_F1)) %>%
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
  mutate(im_weekday_BL = as.factor(Q17_1_BL)) %>%
  mutate(im_weekend_BL = as.factor(Q17_2_BL)) %>%
  mutate(im_weekday_F1 = as.factor(Q17_1_F1)) %>%
  mutate(im_weekday_F1 = as.factor(Q17_2_F1)) %>%
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
  mutate(text_weekday_BL = as.factor(Q16_1_BL)) %>%
  mutate(text_weekend_BL = as.factor(Q16_2_BL)) %>%
  mutate(text_weekday_F1 = as.factor(Q16_1_F1)) %>%
  mutate(text_weekday_F1 = as.factor(Q16_2_F1)) %>%
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
  ) %>% as.factor()) %>% 
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
  ) %>% as.factor()) %>% 
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
  ))

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
