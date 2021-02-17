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
  corr_matrix[n_pars] Rho;
}

model{
  vector[n_obs] mu;
  Rho ~ lkj_corr( 2 );
  sigma ~ exponential( 1 );
  sigma_country ~ exponential( 1 );

  a_marp_hyperprior ~ normal(0, 5);
  b_religiosity_index_hyperprior ~ normal(0, 5);
  b_cnorm_1_hyperprior ~ normal(0, 5);
  b_cnorm_2_hyperprior ~ normal(0, 5);
  b_age_hyperprior ~ normal(0, 5);
  b_ses_hyperprior ~ normal(0, 5);
  b_education_hyperprior ~ normal(0, 5);
  // b_gdp_scaled_hyperprior ~ normal(0, 5);

  {
  vector[n_pars] YY[n_countries];
  vector[n_pars] MU;
  MU = [a_marp_hyperprior, b_religiosity_index_hyperprior, b_cnorm_1_hyperprior, b_cnorm_2_hyperprior, b_age_hyperprior, b_ses_hyperprior, b_education_hyperprior]';

  for(j in 1:n_countries){
    YY[j] = [ a_marp[j],
              b_religiosity_index[j],
              b_cnorm_1[j],
              b_cnorm_2[j],
              b_age[j],
              b_ses[j],
              b_education[j] ]';
  }
    YY ~ multi_normal( MU , quad_form_diag(Rho , sigma_country) );
  }

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








