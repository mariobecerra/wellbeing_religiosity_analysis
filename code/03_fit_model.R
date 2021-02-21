library(tidyverse)
library(rstan)
library(here)

covariable_names = c("religiosity_index", "cnorm_1", "cnorm_2", "age", "ses", "education", "genderman", "genderother", "denominationsome")

marp = read.csv(
  here("out/marp_data_processed.csv"), 
  stringsAsFactors = F) %>% 
  mutate(denomination = ifelse(denomination != "none", "some", denomination))

marp_stan = marp %>% 
  mutate(country = as.integer(as.factor(country))) %>% 
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
  ) %>% 
  append(
    model.matrix( ~ -1 + denomination, data = marp) %>% 
      as.data.frame() %>% 
      select(-denominationnone) %>% # Default level
      as.list()
  )


marp_stan$n_obs = nrow(marp)
marp_stan$n_countries = length(unique(marp$country))
marp_stan$n_pars = length(covariable_names) + 1



marp_model <- stan_model(file = here("code/model.stan"), verbose = TRUE)


n_iter = 3000
# 18 minutes for 100 iterations
(t1 = Sys.time())
marp_fit <- sampling(
  marp_model, 
  data = marp_stan,
  chains = 4,
  iter = n_iter,
  cores = parallel::detectCores(),
  control = list(max_treedepth = 15)
)
(t2= Sys.time())
t2 - t1

saveRDS(marp_fit, here(paste0("out/marp_fit_", n_iter, "_iter.rds")))


