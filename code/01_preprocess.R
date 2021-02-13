library(tidyverse)
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

write_csv(marp2, here("out/marp_data_processed.csv"))
