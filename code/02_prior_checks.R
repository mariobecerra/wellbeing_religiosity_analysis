library(tidyverse)
library(here)
library(rethinking)

# Original data
# Very few of these lines are actually used
marp = read.csv(
  here("out/marp_data_processed.csv"), 
  stringsAsFactors = T) 

marp_stan = as.list(marp)

marp_stan$n_obs = nrow(marp)
marp_stan$n_countries = length(unique(marp$country))
marp_stan$n_pars = 7L


## Compile the model
marp_model <- stan_model(file = here("code/model.stan"), verbose = TRUE)



## Prior simulation
# Inspire by https://willhipson.netlify.app/post/stan-random-slopes/varying_effects_stan/
set.seed(2021)

N <- marp_stan$n_countries # number of countries
n_obs_per_country <- 100 # number of observations per country
total_obs <- N * n_obs_per_country

beta = rnorm(marp_stan$n_pars, mean = 0, sd = 5) # alpha and betas
sigma = rexp(1, 1) # population error
sigma_country = rexp(marp_stan$n_pars, 1)
Omega <- rethinking::rlkjcorr(n = 1, K = marp_stan$n_pars, eta = 2) # from McElreath's rethinking package
Sigma <- diag(sigma_country) %*% Omega %*% diag(sigma_country)
beta_p <- rmvnorm(N, mean = beta, sigma = Sigma)

x <- matrix(
  c(rep(1, N * n_obs_per_country), 
    rnorm((marp_stan$n_pars-1) * N * n_obs_per_country, mean = 0, sd = 1)), 
  ncol = marp_stan$n_pars)

subject <- rep(1:N, each = n_obs_per_country)

sim_dat = lapply(1:N, function(i){
  X = matrix(
    c(rep(1, n_obs_per_country), 
    rnorm((marp_stan$n_pars-1) * n_obs_per_country, mean = 0, sd = 1)), 
  ncol = marp_stan$n_pars)
  
  mu_i = X %*% t(t(beta_p[i,]))
  
  covariables = as_tibble(X)
  covariables[,1] = NULL
  names(covariables) = c("religiosity_index", "cnorm_1", "cnorm_2", "age", "ses", "education")
  
  out = tibble(country = rep(i, n_obs_per_country)) %>% 
    mutate(mu = as.numeric(mu_i)) %>% 
    bind_cols(covariables)
  
}) %>% 
  bind_rows() %>% 
  mutate(wb_overall_mean = rnorm(nrow(.), mu, sigma))


sim_dat %>% 
  ggplot() +
  geom_histogram(aes(wb_overall_mean)) +
  facet_wrap(~country) +
  theme_bw()


sim_dat_stan = as.list(sim_dat)
sim_dat_stan$n_obs = nrow(sim_dat)
sim_dat_stan$n_countries = length(unique(sim_dat_stan$country))
sim_dat_stan$n_pars = marp_stan$n_pars

sims_fit <- sampling(
  marp_model, 
  data = sim_dat_stan,
  chains = 4,
  iter = 2000,
  cores = parallel::detectCores()
  )

aaa = summary(sims_fit)


fit_summary = aaa$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  set_names(make.names(names(.)))

fit_summary %>% 
  filter(grepl("hyper", rowname)) %>% 
  select(rowname, mean, X2.5., X97.5.) %>% 
  mutate(real_value = beta)







marp_fit <- sampling(marp_model, data = marp_stan)







# fit <- stan(file = 'schools.stan', data = schools_dat)
# 
# marp_model_1 <- ulam(
#   alist(
#     wb_overall_mean ~ normal( mu , sigma ),
#     mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
#     c(a_cafe, b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
#     a ~ normal(5,2),
#     b ~ normal(-1,0.5),
#     sigma_cafe ~ exponential(1),
#     sigma ~ exponential(1),
#     Rho ~ lkj_corr(2)
#   ) , data=d , chains=4 , cores=4 )
