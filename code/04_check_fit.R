library(tidyverse)
library(stringr)
library(magrittr)
library(rstan)
library(here)

marp = read.csv(
  here("out/marp_data_processed.csv"), 
  stringsAsFactors = F) %>% 
  mutate(denomination = ifelse(denomination != "none", "some", denomination)) %>% 
  as_tibble()

country_int_mapping =  marp %>% 
  select(country) %>% 
  mutate(country_int = as.integer(as.factor(country))) %>% 
  distinct()


marp_fit = readRDS(here(paste0("out/marp_fit_3000_iter.rds")))



# marp_fit %<>% recover_types(marp_stan)
# 
# marp_fit %>%
#   spread_draws(a_marp_hyperprior, regex = F) %>%
#   head(15)  # just show the first few rows





fit_summary = summary(marp_fit)$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  set_names(make.names(names(.)))





### Predictions

fit_summary_y_rep = fit_summary %>% 
  filter(substr(rowname, 1, 5) == "y_rep") %>% 
  bind_cols(
    marp %>% 
      select(country, wb_overall_mean)
  )


fit_summary_y_rep %>% 
  ggplot() +
  geom_point(aes(wb_overall_mean, mean), size = 0.7) +
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





y_rep_tibble = rstan::extract(
  marp_fit,
  pars =  fit_summary %>% 
    filter(grepl("y_rep", rowname)) %>% pull(rowname)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  set_names(1:ncol(.)) %>% 
  mutate(iter = 1:nrow(.)) %>% 
  sample_n(1000) %>%  # Sample 1000 of the 6000 simulations
  pivot_longer(cols = 1:(ncol(.)-1), names_to = "obs") %>% 
  mutate(obs = as.integer(obs)) %>% 
  left_join(
    tibble(country = marp$country) %>% 
      mutate(obs = 1:nrow(.))
  )

y_rep_tibble %>% 
  ggplot() + 
  geom_density(aes(x = value, group = iter),
               size = 0.1, color = "dark grey") +
  theme_bw() +
  geom_density(data = marp, aes(x = wb_overall_mean), size = 1, color = "black")
  


y_rep_tibble %>% 
  filter(iter %in% sample(unique(y_rep_tibble$iter), 100)) %>% # Use only 100 simulations
  ggplot() + 
  geom_density(aes(x = value, group = iter),
               size = 0.3, color = "dark grey") +
  theme_bw() +
  geom_density(data = marp, aes(x = wb_overall_mean), size = 1, color = "black") +
  facet_wrap(~country, ncol = 6)






### Hyperpriors

fit_summary_hyperpriors = fit_summary %>% 
  filter(grepl("hyper", rowname)) %>% 
  select(rowname, mean, X2.5., X25., X75., X97.5., sd) 



fit_summary_hyperpriors %>% 
  ggplot(aes(x = rowname, y = mean)) +
  geom_errorbar(aes(ymin = X25., ymax = X75.), size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  ggtitle("Hyperpriors") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


fit_summary_hyperpriors %>% 
  ggplot(aes(x = rowname, y = mean)) +
  geom_point() +
  geom_linerange(aes(ymin = X25., ymax = X75.), size = 1) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), size = 0.4) +
  theme_bw() +
  ggtitle("Hyperpriors") +
  facet_wrap(~rowname, scales = "free")



rstan::extract(
  marp_fit,
  pars =  fit_summary %>% 
    filter(grepl("hyper", rowname)) %>% pull(rowname)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  pivot_longer(cols = 1:ncol(.)) %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~name, scales = "free") +
  theme_bw()





### Sigmas

rstan::extract(
  marp_fit,
  pars =  fit_summary %>% 
    filter(grepl("sigma", rowname)) %>% pull(rowname)) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  pivot_longer(cols = 1:ncol(.)) %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  facet_wrap(~name, scales = "free") +
  theme_bw()






### Slopes and intercepts
fit_summary_slopes_intercepts = fit_summary %>% 
  filter(!grepl("hyper", rowname)) %>% 
  filter(!grepl("raw", rowname)) %>% 
  filter(substr(rowname, 1, 2) == "a_" | substr(rowname, 1, 2) == "b_") %>% 
  separate(col = rowname, sep = "\\[", into = c("parameter", "country_int")) %>% 
  mutate(country_int = as.integer(str_extract(country_int, "[0-9]+"))) %>% 
  left_join(country_int_mapping) 



fit_summary_slopes_intercepts %>% 
  mutate(parameter_country = paste(parameter, country)) %>% 
  ggplot(aes(x = parameter_country, y = mean, color = parameter)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 0.7) +
  geom_linerange(aes(ymin = X25., ymax = X75.), size = 0.9) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), size = 0.4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Intercepts and slopes") 
  

fit_summary_slopes_intercepts %>% 
  ggplot(aes(x = country, y = mean)) +
  geom_point(size = 1) +
  geom_linerange(aes(ymin = X25., ymax = X75.), size = 0.8) +
  geom_linerange(aes(ymin = X2.5., ymax = X97.5.), size = 0.2) +
  facet_wrap(~parameter, scales = "free_x", ncol = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() +
  ggtitle("Intercepts and slopes") 



