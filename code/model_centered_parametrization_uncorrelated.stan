data{
  int<lower=0> n_obs; // number of observations
  int<lower=0> n_countries; // number of countries

  vector[n_obs] wb_overall_mean;
  int country[n_obs];
  int n_pars;

  real religiosity_index[n_obs];
  real cnorm_1[n_obs];
  real cnorm_2[n_obs];
  real age[n_obs];
  real ses[n_obs];
  real education[n_obs];

  // real gdp_scaled[n_obs];
  // int gender[n_obs];
  // int denomination[n_obs];
  // int sample_type[n_obs];
  // int compensation[n_obs];

}

parameters{

  vector[n_countries] a_marp;
  vector[n_countries] b_religiosity_index;
  vector[n_countries] b_cnorm_1;
  vector[n_countries] b_cnorm_2;
  vector[n_countries] b_age;
  vector[n_countries] b_ses;
  vector[n_countries] b_education;
  // vector[n_countries] b_gdp_scaled;

  real a_marp_hyperprior;
  real b_religiosity_index_hyperprior;
  real b_cnorm_1_hyperprior;
  real b_cnorm_2_hyperprior;
  real b_age_hyperprior;
  real b_ses_hyperprior;
  real b_education_hyperprior;
  // real b_gdp_scaled_hyperprior;

  vector<lower=0>[n_pars] sigma_country;

  real<lower=0> sigma;
}

model{
  vector[n_obs] mu;
  sigma ~ exponential( 1 );
  sigma_country ~ exponential( 1 );

  a_marp_hyperprior ~ std_normal();
  b_religiosity_index_hyperprior ~ std_normal();
  b_cnorm_1_hyperprior ~ std_normal();
  b_cnorm_2_hyperprior ~ std_normal();
  b_age_hyperprior ~ std_normal();
  b_ses_hyperprior ~ std_normal();
  b_education_hyperprior ~ std_normal();
  // b_gdp_scaled_hyperprior ~ std_normal();

  // for(j in 1:n_countries){
  //   a_marp[j] ~ normal(a_marp_hyperprior, sigma_country);
  //   b_religiosity_index[j] ~ normal(b_religiosity_index_hyperprior, sigma_country);
  //   b_cnorm_1[j] ~ normal(b_cnorm_1_hyperprior, sigma_country);
  //   b_cnorm_2[j] ~ normal(b_cnorm_2_hyperprior, sigma_country);
  //   b_age[j] ~ normal(b_age_hyperprior, sigma_country);
  //   b_ses[j] ~ normal(b_ses_hyperprior, sigma_country);
  //   b_education[j] ~ normal(b_education_hyperprior, sigma_country);
  // }

  // Apparently both the for loop and this work (at least it compiled)
  a_marp ~ normal(a_marp_hyperprior, sigma_country[1]);
  b_religiosity_index ~ normal(b_religiosity_index_hyperprior, sigma_country[2]);
  b_cnorm_1 ~ normal(b_cnorm_1_hyperprior, sigma_country[3]);
  b_cnorm_2 ~ normal(b_cnorm_2_hyperprior, sigma_country[4]);
  b_age ~ normal(b_age_hyperprior, sigma_country[5]);
  b_ses ~ normal(b_ses_hyperprior, sigma_country[6]);
  b_education ~ normal(b_education_hyperprior, sigma_country[7]);

  for(i in 1:n_obs){
      mu[i] = a_marp[country[i]] +
        b_religiosity_index[country[i]] * religiosity_index[i] +
        b_cnorm_1[country[i]] * cnorm_1[i] +
        b_cnorm_2[country[i]] * cnorm_2[i] +
        b_age[country[i]] * age[i] +
        b_ses[country[i]] * ses[i] +
        b_education[country[i]] * education[i];
  }

  wb_overall_mean ~ normal( mu , sigma );

}








