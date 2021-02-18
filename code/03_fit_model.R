library(tidyverse)
library(here)
library(rethinking)

marp = read.csv(
  here("out/marp_data_processed.csv"), 
  stringsAsFactors = T) 

marp_stan = marp %>% 
  mutate(country = as.integer(country)) %>% 
  select(
    country,
    wb_overall_mean,
    religiosity_index,
    cnorm_1,
    cnorm_2,
    age,
    ses,
    education) %>% 
  as.list() %>% 
  append(
    model.matrix( ~ -1 + gender, data = marp) %>% 
      as.data.frame() %>% 
      select(-genderwoman) %>% # Default level
      as.list()
  )




marp_stan$denomination = as.integer(marp_stan$denomination)
marp_stan$sample_type = as.integer(marp_stan$sample_type)
marp_stan$compensation = as.integer(marp_stan$compensation)

marp_stan$n_obs = nrow(marp)
marp_stan$n_countries = length(unique(marp$country))
marp_stan$n_pars = 7L



marp_fit <- sampling(marp_model, data = marp_stan)



