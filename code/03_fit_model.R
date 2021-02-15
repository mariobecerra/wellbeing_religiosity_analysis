library(tidyverse)
library(here)
library(rethinking)

marp = read.csv(
  here("out/marp_data_processed.csv"), 
  stringsAsFactors = T) 

marp_stan = as.list(marp)

marp_stan$country = as.integer(marp_stan$country)
marp_stan$gender = as.integer(marp_stan$gender)
marp_stan$denomination = as.integer(marp_stan$denomination)
marp_stan$sample_type = as.integer(marp_stan$sample_type)
marp_stan$compensation = as.integer(marp_stan$compensation)

marp_stan$n_obs = nrow(marp)
marp_stan$n_countries = length(unique(marp$country))
marp_stan$n_pars = 7L



marp_fit <- sampling(marp_model, data = marp_stan)



