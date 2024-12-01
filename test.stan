data {
  int<lower=0> num_matches;
  int<lower=0> num_players;
  int<lower=0> num_periods;
  int<lower=0> num_surfaces;
  int<lower=0> num_teams;

  array[num_matches] int<lower=1, upper=num_players> p1;
  array[num_matches] int<lower=1, upper=num_players> p2;
  array[num_matches] int<lower=1, upper=num_players> p3;
  array[num_matches] int<lower=1, upper=num_players> p4;

  array[num_matches] int<lower=1, upper=num_teams> team1;
  array[num_matches] int<lower=1, upper=num_teams> team2;

  array[num_matches] int<lower=1, upper=num_periods> period;
  array[num_matches] int<lower=1, upper=num_surfaces> surface;
  array[num_matches] int<lower=0, upper=1> outcome;
 
 }

parameters {
  real<lower=0> sigma_a0;
  real<lower=0> sigma_a;
  real<lower=0> sigma_surf;
  real<lower=0> sigma_team;

  matrix[num_periods, num_players] eta_a;
  matrix[num_surfaces, num_players] eta_surface;
  vextor[num_teams] eta_team;
}

transformed parameters {
  matrix[num_periods, num_players] alpha;
  matrix[num_surfaces, num_players] alpha_surface;
  vector[num_teams] alpha_team;

  for (i in 1:num_surfaces) {
    alpha_surface[i, :] = sigma_surf * eta_surface[i, :];
  }

  alpha[1] = sigma_a0 * eta_a[1];

  alpha_team[1] = sigma_team * eta_team[1];

  for (p in 2:num_periods) {
    alpha[p] = alpha[p-1] + sigma_a *  eta_a[p];
  }
}

model {
  sigma_a0 ~ normal(0, 1);
  sigma_a ~ normal(0, 1);
  sigma_surf ~ normal(0, 1);
  sigma_team ~ normal(0, 0.1);

  to_vector(eta_a) ~ normal(0, 1);
  to_vector(eta_surface) ~ normal(0, 1);
  to_vector(eta_team) ~ normal(0, 0.1);

  vector[num_matches] pred_logits;

  for (n in 1:num_matches) {
    pred_logits[n] = alpha_team[team1] + (alpha[period[n], p1[n]] + alpha[period[n], p2[n]]) -
                     alpha_team[team2] + (alpha[period[n], p3[n]] + alpha[period[n], p4[n]]) +
                     (alpha_surface[surface[n], p1[n]] + alpha_surface[surface[n], p2[n]]) -
                     (alpha_surface[surface[n], p3[n]] + alpha_surface[surface[n], p4[n]]);
 }

  outcome ~ bernoulli_logit(pred_logits);
}