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
  int denominationafrican_religions[n_obs];
  int denominationbuddhist[n_obs];
  int denominationchristian[n_obs];
  int denominationchristian_orthodox_russian_greek_etc[n_obs];
  int denominationchristian_protestant[n_obs];
  int denominationchristian_roman_catholic[n_obs];
  int denominationdruze[n_obs];
  int denominationevangelical[n_obs];
  int denominationhindu[n_obs];
  int denominationjain[n_obs];
  int denominationjewish[n_obs];
  int denominationmuslim[n_obs];
  int denominationmuslim_alevi[n_obs];
  int denominationmuslim_azhari[n_obs];
  int denominationmuslim_non_sectarian[n_obs];
  int denominationmuslim_sunni[n_obs];
  int denominationother[n_obs];
  int denominationshinto[n_obs];
  
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
  vector[n_countries] b_denominationafrican_religions_raw;
  vector[n_countries] b_denominationbuddhist_raw;
  vector[n_countries] b_denominationchristian_raw;
  vector[n_countries] b_denominationchristian_orthodox_russian_greek_etc_raw;
  vector[n_countries] b_denominationchristian_protestant_raw;
  vector[n_countries] b_denominationchristian_roman_catholic_raw;
  vector[n_countries] b_denominationdruze_raw;
  vector[n_countries] b_denominationevangelical_raw;
  vector[n_countries] b_denominationhindu_raw;
  vector[n_countries] b_denominationjain_raw;
  vector[n_countries] b_denominationjewish_raw;
  vector[n_countries] b_denominationmuslim_raw;
  vector[n_countries] b_denominationmuslim_alevi_raw;
  vector[n_countries] b_denominationmuslim_azhari_raw;
  vector[n_countries] b_denominationmuslim_non_sectarian_raw;
  vector[n_countries] b_denominationmuslim_sunni_raw;
  vector[n_countries] b_denominationother_raw;
  vector[n_countries] b_denominationshinto_raw;
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
  real b_denominationafrican_religions_hyperprior;
  real b_denominationbuddhist_hyperprior;
  real b_denominationchristian_hyperprior;
  real b_denominationchristian_orthodox_russian_greek_etc_hyperprior;
  real b_denominationchristian_protestant_hyperprior;
  real b_denominationchristian_roman_catholic_hyperprior;
  real b_denominationdruze_hyperprior;
  real b_denominationevangelical_hyperprior;
  real b_denominationhindu_hyperprior;
  real b_denominationjain_hyperprior;
  real b_denominationjewish_hyperprior;
  real b_denominationmuslim_hyperprior;
  real b_denominationmuslim_alevi_hyperprior;
  real b_denominationmuslim_azhari_hyperprior;
  real b_denominationmuslim_non_sectarian_hyperprior;
  real b_denominationmuslim_sunni_hyperprior;
  real b_denominationother_hyperprior;
  real b_denominationshinto_hyperprior;
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
  vector[n_countries] b_denominationafrican_religions;
  vector[n_countries] b_denominationbuddhist;
  vector[n_countries] b_denominationchristian;
  vector[n_countries] b_denominationchristian_orthodox_russian_greek_etc;
  vector[n_countries] b_denominationchristian_protestant;
  vector[n_countries] b_denominationchristian_roman_catholic;
  vector[n_countries] b_denominationdruze;
  vector[n_countries] b_denominationevangelical;
  vector[n_countries] b_denominationhindu;
  vector[n_countries] b_denominationjain;
  vector[n_countries] b_denominationjewish;
  vector[n_countries] b_denominationmuslim;
  vector[n_countries] b_denominationmuslim_alevi;
  vector[n_countries] b_denominationmuslim_azhari;
  vector[n_countries] b_denominationmuslim_non_sectarian;
  vector[n_countries] b_denominationmuslim_sunni;
  vector[n_countries] b_denominationother;
  vector[n_countries] b_denominationshinto;
  
  a_marp = a_marp_hyperprior + a_marp_raw*sigma_country[1]; // Implies a_marp ~ normal(a_marp_hyperprior, sigma_country[1])
  b_religiosity_index = b_religiosity_index_hyperprior + b_religiosity_index_raw*sigma_country[2]; // Implies b_religiosity_index ~ normal(b_religiosity_index_hyperprior, sigma_country[2])
  b_cnorm_1 = b_cnorm_1_hyperprior + b_cnorm_1_raw*sigma_country[3]; // Implies b_cnorm_1 ~ normal(b_cnorm_1_hyperprior, sigma_country[3])
  b_cnorm_2 = b_cnorm_2_hyperprior + b_cnorm_2_raw*sigma_country[4]; // Implies b_cnorm_2 ~ normal(b_cnorm_2_hyperprior, sigma_country[4])
  b_age = b_age_hyperprior + b_age_raw*sigma_country[5]; // Implies b_age ~ normal(b_age_hyperprior, sigma_country[5])
  b_ses = b_ses_hyperprior + b_ses_raw*sigma_country[6]; // Implies b_ses ~ normal(b_ses_hyperprior, sigma_country[6])
  b_education = b_education_hyperprior + b_education_raw*sigma_country[7]; // Implies b_education ~ normal(b_education_hyperprior, sigma_country[7])
  b_genderman = b_genderman_hyperprior + b_genderman_raw*sigma_country[8]; // Implies b_genderman ~ normal(b_genderman_hyperprior, sigma_country[8])
  b_genderother = b_genderother_hyperprior + b_genderother_raw*sigma_country[9]; // Implies b_genderother ~ normal(b_genderother_hyperprior, sigma_country[9])
  b_denominationafrican_religions = b_denominationafrican_religions_hyperprior + b_denominationafrican_religions_raw*sigma_country[10]; // Implies b_denominationafrican_religions ~ normal(b_denominationafrican_religions_hyperprior, sigma_country[10])
  b_denominationbuddhist = b_denominationbuddhist_hyperprior + b_denominationbuddhist_raw*sigma_country[11]; // Implies b_denominationbuddhist ~ normal(b_denominationbuddhist_hyperprior, sigma_country[11])
  b_denominationchristian = b_denominationchristian_hyperprior + b_denominationchristian_raw*sigma_country[12]; // Implies b_denominationchristian ~ normal(b_denominationchristian_hyperprior, sigma_country[12])
  b_denominationchristian_orthodox_russian_greek_etc = b_denominationchristian_orthodox_russian_greek_etc_hyperprior + b_denominationchristian_orthodox_russian_greek_etc_raw*sigma_country[13]; // Implies b_denominationchristian_orthodox_russian_greek_etc ~ normal(b_denominationchristian_orthodox_russian_greek_etc_hyperprior, sigma_country[13])
  b_denominationchristian_protestant = b_denominationchristian_protestant_hyperprior + b_denominationchristian_protestant_raw*sigma_country[14]; // Implies b_denominationchristian_protestant ~ normal(b_denominationchristian_protestant_hyperprior, sigma_country[14])
  b_denominationchristian_roman_catholic = b_denominationchristian_roman_catholic_hyperprior + b_denominationchristian_roman_catholic_raw*sigma_country[15]; // Implies b_denominationchristian_roman_catholic ~ normal(b_denominationchristian_roman_catholic_hyperprior, sigma_country[15])
  b_denominationdruze = b_denominationdruze_hyperprior + b_denominationdruze_raw*sigma_country[16]; // Implies b_denominationdruze ~ normal(b_denominationdruze_hyperprior, sigma_country[16])
  b_denominationevangelical = b_denominationevangelical_hyperprior + b_denominationevangelical_raw*sigma_country[17]; // Implies b_denominationevangelical ~ normal(b_denominationevangelical_hyperprior, sigma_country[17])
  b_denominationhindu = b_denominationhindu_hyperprior + b_denominationhindu_raw*sigma_country[18]; // Implies b_denominationhindu ~ normal(b_denominationhindu_hyperprior, sigma_country[18])
  b_denominationjain = b_denominationjain_hyperprior + b_denominationjain_raw*sigma_country[19]; // Implies b_denominationjain ~ normal(b_denominationjain_hyperprior, sigma_country[19])
  b_denominationjewish = b_denominationjewish_hyperprior + b_denominationjewish_raw*sigma_country[20]; // Implies b_denominationjewish ~ normal(b_denominationjewish_hyperprior, sigma_country[20])
  b_denominationmuslim = b_denominationmuslim_hyperprior + b_denominationmuslim_raw*sigma_country[21]; // Implies b_denominationmuslim ~ normal(b_denominationmuslim_hyperprior, sigma_country[21])
  b_denominationmuslim_alevi = b_denominationmuslim_alevi_hyperprior + b_denominationmuslim_alevi_raw*sigma_country[22]; // Implies b_denominationmuslim_alevi ~ normal(b_denominationmuslim_alevi_hyperprior, sigma_country[22])
  b_denominationmuslim_azhari = b_denominationmuslim_azhari_hyperprior + b_denominationmuslim_azhari_raw*sigma_country[23]; // Implies b_denominationmuslim_azhari ~ normal(b_denominationmuslim_azhari_hyperprior, sigma_country[23])
  b_denominationmuslim_non_sectarian = b_denominationmuslim_non_sectarian_hyperprior + b_denominationmuslim_non_sectarian_raw*sigma_country[24]; // Implies b_denominationmuslim_non_sectarian ~ normal(b_denominationmuslim_non_sectarian_hyperprior, sigma_country[24])
  b_denominationmuslim_sunni = b_denominationmuslim_sunni_hyperprior + b_denominationmuslim_sunni_raw*sigma_country[25]; // Implies b_denominationmuslim_sunni ~ normal(b_denominationmuslim_sunni_hyperprior, sigma_country[25])
  b_denominationother = b_denominationother_hyperprior + b_denominationother_raw*sigma_country[26]; // Implies b_denominationother ~ normal(b_denominationother_hyperprior, sigma_country[26])
  b_denominationshinto = b_denominationshinto_hyperprior + b_denominationshinto_raw*sigma_country[27];   // Implies b_denominationshinto ~ normal(b_denominationshinto_hyperprior, sigma_country[27])
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
  b_denominationafrican_religions_hyperprior ~ std_normal();
  b_denominationbuddhist_hyperprior ~ std_normal();
  b_denominationchristian_hyperprior ~ std_normal();
  b_denominationchristian_orthodox_russian_greek_etc_hyperprior ~ std_normal();
  b_denominationchristian_protestant_hyperprior ~ std_normal();
  b_denominationchristian_roman_catholic_hyperprior ~ std_normal();
  b_denominationdruze_hyperprior ~ std_normal();
  b_denominationevangelical_hyperprior ~ std_normal();
  b_denominationhindu_hyperprior ~ std_normal();
  b_denominationjain_hyperprior ~ std_normal();
  b_denominationjewish_hyperprior ~ std_normal();
  b_denominationmuslim_hyperprior ~ std_normal();
  b_denominationmuslim_alevi_hyperprior ~ std_normal();
  b_denominationmuslim_azhari_hyperprior ~ std_normal();
  b_denominationmuslim_non_sectarian_hyperprior ~ std_normal();
  b_denominationmuslim_sunni_hyperprior ~ std_normal();
  b_denominationother_hyperprior ~ std_normal();
  b_denominationshinto_hyperprior ~ std_normal();
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
  b_denominationafrican_religions_raw ~ std_normal();
  b_denominationbuddhist_raw ~ std_normal();
  b_denominationchristian_raw ~ std_normal();
  b_denominationchristian_orthodox_russian_greek_etc_raw ~ std_normal();
  b_denominationchristian_protestant_raw ~ std_normal();
  b_denominationchristian_roman_catholic_raw ~ std_normal();
  b_denominationdruze_raw ~ std_normal();
  b_denominationevangelical_raw ~ std_normal();
  b_denominationhindu_raw ~ std_normal();
  b_denominationjain_raw ~ std_normal();
  b_denominationjewish_raw ~ std_normal();
  b_denominationmuslim_raw ~ std_normal();
  b_denominationmuslim_alevi_raw ~ std_normal();
  b_denominationmuslim_azhari_raw ~ std_normal();
  b_denominationmuslim_non_sectarian_raw ~ std_normal();
  b_denominationmuslim_sunni_raw ~ std_normal();
  b_denominationother_raw ~ std_normal();
  b_denominationshinto_raw ~ std_normal();
  
  for(i in 1:n_obs){
    mu[i] = a_marp[country[i]] +
    b_religiosity_index[country[i]] * religiosity_index[i] +
    b_cnorm_1[country[i]] * cnorm_1[i] +
    b_cnorm_2[country[i]] * cnorm_2[i] +
    b_age[country[i]] * age[i] +
    b_ses[country[i]] * ses[i] +
    b_education[country[i]] * education[i] +
    b_genderman[country[i]] * genderman[i] +
    b_genderother[country[i]] * genderother[i] +
    b_denominationafrican_religions[country[i]] * denominationafrican_religions[i] +
    b_denominationbuddhist[country[i]] * denominationbuddhist[i] +
    b_denominationchristian[country[i]] * denominationchristian[i] +
    b_denominationchristian_orthodox_russian_greek_etc[country[i]] * denominationchristian_orthodox_russian_greek_etc[i] +
    b_denominationchristian_protestant[country[i]] * denominationchristian_protestant[i] +
    b_denominationchristian_roman_catholic[country[i]] * denominationchristian_roman_catholic[i] +
    b_denominationdruze[country[i]] * denominationdruze[i] +
    b_denominationevangelical[country[i]] * denominationevangelical[i] +
    b_denominationhindu[country[i]] * denominationhindu[i] +
    b_denominationjain[country[i]] * denominationjain[i] +
    b_denominationjewish[country[i]] * denominationjewish[i] +
    b_denominationmuslim[country[i]] * denominationmuslim[i] +
    b_denominationmuslim_alevi[country[i]] * denominationmuslim_alevi[i] +
    b_denominationmuslim_azhari[country[i]] * denominationmuslim_azhari[i] +
    b_denominationmuslim_non_sectarian[country[i]] * denominationmuslim_non_sectarian[i] +
    b_denominationmuslim_sunni[country[i]] * denominationmuslim_sunni[i] +
    b_denominationother[country[i]] * denominationother[i] +
    b_denominationshinto[country[i]] * denominationshinto[i]
    ;
  }
  
  wb_overall_mean ~ normal( mu , sigma );
  
}


generated quantities {
  
  vector[n_obs] y_rep; // replications from posterior predictive distribution
  
  for (i in 1:n_obs) {
    real mu_i = a_marp[country[i]] +
    b_religiosity_index[country[i]] * religiosity_index[i] +
    b_cnorm_1[country[i]] * cnorm_1[i] +
    b_cnorm_2[country[i]] * cnorm_2[i] +
    b_age[country[i]] * age[i] +
    b_ses[country[i]] * ses[i] +
    b_education[country[i]] * education[i] +
    b_genderman[country[i]] * genderman[i] +
    b_genderother[country[i]] * genderother[i] +
    b_denominationbuddhist[country[i]] * denominationbuddhist[i] +
    b_denominationchristian[country[i]] * denominationchristian[i] +
    b_denominationchristian_orthodox_russian_greek_etc[country[i]] * denominationchristian_orthodox_russian_greek_etc[i] +
    b_denominationchristian_protestant[country[i]] * denominationchristian_protestant[i] +
    b_denominationchristian_roman_catholic[country[i]] * denominationchristian_roman_catholic[i] +
    b_denominationdruze[country[i]] * denominationdruze[i] +
    b_denominationevangelical[country[i]] * denominationevangelical[i] +
    b_denominationhindu[country[i]] * denominationhindu[i] +
    b_denominationjain[country[i]] * denominationjain[i] +
    b_denominationjewish[country[i]] * denominationjewish[i] +
    b_denominationmuslim[country[i]] * denominationmuslim[i] +
    b_denominationmuslim_alevi[country[i]] * denominationmuslim_alevi[i] +
    b_denominationmuslim_azhari[country[i]] * denominationmuslim_azhari[i] +
    b_denominationmuslim_non_sectarian[country[i]] * denominationmuslim_non_sectarian[i] +
    b_denominationmuslim_sunni[country[i]] * denominationmuslim_sunni[i] +
    b_denominationother[country[i]] * denominationother[i] +
    b_denominationshinto[country[i]] * denominationshinto[i]
    ;
    
    
    // generate replication values
    y_rep[i] = normal_rng(mu_i, sigma);
    // normal_rng generates random numbers from a normal distribution
  }
  
}







