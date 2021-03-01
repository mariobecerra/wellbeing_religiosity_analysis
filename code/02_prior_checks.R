# Creates a fake dataset with the assumptions from the model and then fits the model using Stan. This step helps find possible errors in the model code and see if the model recovers the parameters from the simulated data.
library(tidyverse)
library(mvtnorm)
library(rstan)
library(here)

covariable_names = c("religiosity_index", "cnorm_1", "cnorm_2", "age", "ses", "education", "genderman", "genderother", "denominationafrican_religions", "denominationbuddhist", "denominationchristian", "denominationchristian_orthodox_russian_greek_etc", "denominationchristian_protestant", "denominationchristian_roman_catholic", "denominationdruze", "denominationevangelical", "denominationhindu", "denominationjain", "denominationjewish", "denominationmuslim", "denominationmuslim_alevi", "denominationmuslim_azhari", "denominationmuslim_non_sectarian", "denominationmuslim_sunni", "denominationother", "denominationshinto")

# Original data
marp = read.csv(
  here("out/marp_data_processed.csv"), 
  stringsAsFactors = F) 

marp_stan = as.list(marp)

marp_stan$n_obs = nrow(marp)
marp_stan$n_countries = length(unique(marp$country))
marp_stan$n_pars = length(covariable_names) + 1


## Compile the model
marp_model <- stan_model(file = here("code/model.stan"), verbose = TRUE)




## Prior simulation
# Inspired by https://willhipson.netlify.app/post/stan-random-slopes/varying_effects_stan/
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





sim_dat = lapply(1:N, function(i){
  X = matrix(
    c(rep(1, n_obs_per_country), 
    rnorm((6) * n_obs_per_country, mean = 0, sd = 1)), 
  ncol = 7) %>% 
    cbind(., 
          model.matrix( ~ -1 + gender, 
                        data = data.frame(gender = as.factor(sample.int(3, n_obs_per_country, 
                                                                        replace = T)))) %>% 
            as.data.frame() %>% 
            select(1:2) %>% 
            # set_names("genderman", "genderother") %>% 
            as.matrix()
    ) %>% 
    cbind(., 
          model.matrix( ~ -1 + denomination, 
                        data = data.frame(denomination = as.factor(sample.int(19, n_obs_per_country, 
                                                                        replace = T)))) %>% 
            as.data.frame() %>% 
            select(-1) %>% 
            as.matrix()
    )
  
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




(t1 = Sys.time())
# 20 minutes
sims_fit <- sampling(
  marp_model, 
  data = sim_dat_stan,
  chains = 4,
  # iter = 2000,
  iter = 600,
  cores = parallel::detectCores(),
  control = list(max_treedepth = 15)
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

fit_summary_slopes_intercepts = fit_summary %>% 
  filter(!grepl("hyper", rowname)) %>% 
  filter(!grepl("raw", rowname)) %>% 
  filter(substr(rowname, 1, 2) == "a_" | substr(rowname, 1, 2) == "b_") %>% 
  select(rowname, mean) %>% 
  bind_cols(
    beta_p %>% 
      as_tibble() %>% 
      set_names(c("intercept", covariable_names)) %>% 
      mutate(country = 1:nrow(.)) %>% 
      pivot_longer(cols = -country, names_to = "parameter", values_to = "real_value") %>% 
      group_by(country) %>% 
      mutate(id = 1:n()) %>% 
      ungroup() %>% 
      arrange(id, country) %>% 
      select(-id)
  )

fit_summary_slopes_intercepts


fit_summary_slopes_intercepts %>% 
  select(parameter, country, real_value, mean) %>% 
  pivot_longer(cols = c(real_value, mean)) %>% 
  ggplot() +
  geom_point(aes(x = parameter, y = value, color = name),
             position = position_dodge2(w = 0.5)) +
  facet_wrap(~country, ncol = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle("Intercepts and slopes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


fit_summary_slopes_intercepts %>% 
  mutate(error = real_value - mean) %>% 
  ggplot() +
  geom_point(aes(x = parameter, y = error)) +
  facet_wrap(~country, ncol = 4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





### Predictions

fit_summary_y_rep = fit_summary %>% 
  filter(substr(rowname, 1, 5) == "y_rep") %>% 
  bind_cols(
    sim_dat %>% 
      select(country, wb_overall_mean)
  )


fit_summary_y_rep %>% 
  ggplot() +
  geom_point(aes(wb_overall_mean, mean)) +
  facet_wrap(~country, ncol = 4) +
  geom_abline(slope = 1, linetype = "dashed") +
  theme_bw() +
  xlab("Real") +
  ylab("Predicted") +
  ggtitle("Real vs. predicted values") 





fit_summary_y_rep %>% 
  ggplot(aes(x = wb_overall_mean, y = mean)) +
  geom_abline(slope = 1, linetype = "dashed", size = 0.4) +
  geom_errorbar(aes(ymin = X25., ymax = X75.), size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), size = 0.2) +
  facet_wrap(~country, ncol = 4) +
  theme_bw() +
  xlab("Real") +
  ylab("Predicted") +
  ggtitle("Real vs. predicted values") 








