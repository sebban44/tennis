//Bayesian Bradley-Terry Model for Doubles Tennis

data {
  int<lower=0> N;        // Number of observations (matches)
  int<lower=0> K;        // Number of unique players 
  int<lower=1, upper=K> player1[N];  // Player 1 in each match
  int<lower=1, upper=K> player2[N];  // Player 2 in each match
  int<lower=1, upper=K> player3[N];  // Player 3 in each match
  int<lower=1, upper=K> player4[N];  // Player 4 in each match
  int<lower=0, upper=1> y[N];      // Outcome of each match (0 or 1)
}

parameters {
  vector[K] alpha;    // alpha for players ability
}

model {
  alpha ~ normal(1, 1);  // Rankings should start at 1 for each player
 for (n in 1:N) {
    y[n] ~ bernoulli_logit((alpha[player1[n]] + alpha[player2[n]]) - (alpha[player3[n]] + alpha[player4[n]]));
  }
}

generated quantities {
  real player_ranking[K];
  
  for (k in 1:K) {
    player_ranking[k] = alpha[k];
  }
}
