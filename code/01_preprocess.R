library(tidyverse)
library(janitor)
library(here)

marp = read_csv(here("data/MARP_data.csv"))


# Variables age, ses, education will be standardized by subtracting their mean and divinding them by their standard deviation.

marp2 = marp %>% 
  filter(attention_check == 1) %>% 
  mutate(religiosity_index =  (rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9)/9,
         denomination = ifelse(is.na(denomination), "None", denomination)) %>% 
  select(
    subject,
    country,
    religiosity_index,
    cnorm_1,
    cnorm_2, 
    age, 
    gender, ses, education, denomination, gdp_scaled,
    sample_type, compensation,
    wb_overall_mean,
    ) %>% 
  filter(complete.cases(.)) %>% 
  mutate(
    age = (age - mean(age))/sd(age),
    ses = (ses - mean(ses))/sd(ses),
    education = (education - mean(education)/sd(education))
  ) 


denomination_names = tibble(denomination = unique(marp2$denomination)) %>% 
  mutate(denomination_new = make_clean_names(denomination))


sample_type_names = tibble(sample_type = unique(marp2$sample_type)) %>% 
  mutate(sample_type_new = make_clean_names(sample_type))


compensation_names = tibble(compensation = unique(marp2$compensation)) %>% 
  mutate(compensation_new = make_clean_names(compensation))


marp3 = marp2 %>% 
  left_join(denomination_names) %>% 
  left_join(sample_type_names) %>% 
  left_join(compensation_names) %>% 
  select(-denomination,
         -sample_type,
         -compensation) %>% 
    rename(denomination = denomination_new,
           sample_type = sample_type_new,
           compensation = compensation_new)

write_csv(marp3, here("out/marp_data_processed.csv"))




