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
  int genderman[n_obs];
  int genderother[n_obs];

  // real gdp_scaled[n_obs];
  // int gender[n_obs];
  // int denomination[n_obs];
  // int sample_type[n_obs];
  // int compensation[n_obs];

}

parameters{

  vector[n_countries] a_marp_raw;
  vector[n_countries] b_religiosity_index_raw;
  vector[n_countries] b_cnorm_1_raw;
  vector[n_countries] b_cnorm_2_raw;
  vector[n_countries] b_age_raw;
  vector[n_countries] b_ses_raw;
  vector[n_countries] b_education_raw;
  vector[n_countries] b_genderman_raw;
  vector[n_countries] b_genderother_raw;
  // vector[n_countries] b_gdp_scaled;

  real a_marp_hyperprior;
  real b_religiosity_index_hyperprior;
  real b_cnorm_1_hyperprior;
  real b_cnorm_2_hyperprior;
  real b_age_hyperprior;
  real b_ses_hyperprior;
  real b_education_hyperprior;
  real b_genderman_hyperprior;
  real b_genderother_hyperprior;
  // real b_gdp_scaled_hyperprior;

  vector<lower=0>[n_pars] sigma_country;

  real<lower=0> sigma;
}


transformed parameters{
  vector[n_countries] a_marp;
  vector[n_countries] b_religiosity_index;
  vector[n_countries] b_cnorm_1;
  vector[n_countries] b_cnorm_2;
  vector[n_countries] b_age;
  vector[n_countries] b_ses;
  vector[n_countries] b_education;
  vector[n_countries] b_genderman;
  vector[n_countries] b_genderother;
  
  a_marp = a_marp_hyperprior + a_marp_raw*sigma_country[1]; // Implies a_marp ~ normal(a_marp, sigma_country[1])
  b_religiosity_index = b_religiosity_index_hyperprior + b_religiosity_index_raw*sigma_country[2]; // Implies b_religiosity_index ~ normal(b_religiosity_index, sigma_country[2])
  b_cnorm_1 = b_cnorm_1_hyperprior + b_cnorm_1_raw*sigma_country[3]; // Implies b_cnorm_1 ~ normal(b_cnorm_1, sigma_country[3])
  b_cnorm_2 = b_cnorm_2_hyperprior + b_cnorm_2_raw*sigma_country[4]; // Implies b_cnorm_2 ~ normal(b_cnorm_2, sigma_country[4])
  b_age = b_age_hyperprior + b_age_raw*sigma_country[5]; // Implies b_age ~ normal(b_age, sigma_country[5])
  b_ses = b_ses_hyperprior + b_ses_raw*sigma_country[6]; // Implies b_ses ~ normal(b_ses, sigma_country[6])
  b_education = b_education_hyperprior + b_education_raw*sigma_country[7]; // Implies b_education ~ normal(b_education, sigma_country[7])
  b_genderman = b_genderman_hyperprior + b_genderman_raw*sigma_country[8]; // Implies b_genderman ~ normal(b_genderman, sigma_country[8])
  b_genderother = b_genderother_hyperprior + b_genderother_raw*sigma_country[9]; // Implies b_genderother ~ normal(b_genderother, sigma_country[9])
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
  b_genderman_hyperprior ~ std_normal();
  b_genderother_hyperprior ~ std_normal();
  // b_gdp_scaled_hyperprior ~ std_normal();


  a_marp_raw ~ std_normal();
  b_religiosity_index_raw ~ std_normal();
  b_cnorm_1_raw ~ std_normal();
  b_cnorm_2_raw ~ std_normal();
  b_age_raw ~ std_normal();
  b_ses_raw ~ std_normal();
  b_education_raw ~ std_normal();
  b_genderman_raw ~ std_normal();
  b_genderother_raw ~ std_normal();

  for(i in 1:n_obs){
      mu[i] = a_marp[country[i]] +
        b_religiosity_index[country[i]] * religiosity_index[i] +
        b_cnorm_1[country[i]] * cnorm_1[i] +
        b_cnorm_2[country[i]] * cnorm_2[i] +
        b_age[country[i]] * age[i] +
        b_ses[country[i]] * ses[i] +
        b_education[country[i]] * education[i] +
        b_genderman[country[i]] * genderman[i] +
        b_genderother[country[i]] * genderother[i]
        ;
  }

  wb_overall_mean ~ normal( mu , sigma );

}








