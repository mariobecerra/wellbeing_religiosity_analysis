library(tidyverse)
library(here)
library(rethinking)

covariable_names = c("religiosity_index", "cnorm_1", "cnorm_2", "age", "ses", "education")

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

beta = rnorm(marp_stan$n_pars, mean = 0, sd = 1) # alpha and betas
sigma = rexp(1, 1) # population error
sigma_country = rexp(marp_stan$n_pars, 1)
# Omega <- rethinking::rlkjcorr(n = 1, K = marp_stan$n_pars, eta = 2) # from McElreath's rethinking package
Omega <- diag(rep(1.0, marp_stan$n_pars)) # Independent parameters
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
  names(covariables) = covariable_names
  
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

# 7.5 minutes
(t1 = Sys.time())
sims_fit <- sampling(
  marp_model, 
  data = sim_dat_stan,
  chains = 4,
  iter = 2000,
  cores = parallel::detectCores()
  )
(t2= Sys.time())
t2 - t1



fit_summary = summary(sims_fit)$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  set_names(make.names(names(.)))




### Hyperpriors

fit_summary_hyperpriors = fit_summary %>% 
  filter(grepl("hyper", rowname)) %>% 
  select(rowname, mean, X2.5., X97.5., sd) %>% 
  mutate(real_value = beta)

fit_summary_hyperpriors %>% 
  select(rowname, mean, real_value) %>% 
  pivot_longer(cols = c(real_value, mean)) %>% 
  ggplot() +
  geom_point(aes(x = rowname, y = value, color = name),
             position = position_dodge2(w = 0.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle("Hyperpriors") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))









### Sigmas

fit_summary_sigmas = fit_summary %>% 
  filter(grepl("sigma", rowname, fixed = T)) %>% 
  select(rowname, mean) %>% 
  mutate(real_value = c(sigma_country, sigma))

fit_summary_sigmas


fit_summary_sigmas %>% 
  select(rowname, mean, real_value) %>% 
  pivot_longer(cols = c(real_value, mean)) %>% 
  ggplot() +
  geom_point(aes(x = rowname, y = value, color = name),
             position = position_dodge2(w = 0.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle("Sigmas") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




### Slopes and intercepts

fit_summary_slopes_intercepts = beta_p %>% 
  as_tibble() %>% 
  set_names(c("intercept", covariable_names)) %>% 
  mutate(country = 1:nrow(.)) %>% 
  pivot_longer(cols = -country, names_to = "parameter", values_to = "real_value") %>% 
  group_by(country) %>% 
  mutate(id = 1:n()) %>% 
  ungroup() %>% 
  arrange(id, country) %>% 
  select(-id) %>% 
  bind_cols(
    fit_summary %>% 
      filter(!grepl("hyper", rowname)) %>% 
      filter(!grepl("raw", rowname)) %>% 
      filter(substr(rowname, 1, 2) == "a_" | substr(rowname, 1, 2) == "b_") %>% 
      select(rowname, mean)
  )

fit_summary_slopes_intercepts


fit_summary_slopes_intercepts %>% 
  select(parameter, country, real_value, mean) %>% 
  pivot_longer(cols = c(real_value, mean)) %>% 
  ggplot() +
  geom_point(aes(x = parameter, y = value, color = name),
             position = position_dodge2(w = 0.5)) +
  facet_wrap(~country) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle("Intercepts and slopes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


fit_summary_slopes_intercepts %>% 
  mutate(error = real_value - mean) %>% 
  ggplot() +
  geom_point(aes(x = parameter, y = error)) +
  facet_wrap(~country) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




