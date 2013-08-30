data {
  int<lower=1> N; // number of data points
  real y[N]; // observations
}

parameters {
  real<lower=0,upper=1> teta[2]; // locations of mixture components
  real<lower=0,upper=100> mu[2]; // locations of mixture components
  real<lower=0,upper=20> sigma[2]; // scales of mixture components
}

model {
  teta[1] ~ normal(0.5,1);
  teta[2] ~ normal(0.5,1);
  mu[1] ~ normal(40,0.8);
  mu[2] ~ normal(80,0.8);
  sigma[1] ~ normal(6,1);
  sigma[2] ~ normal(6,1);


  lp__ <- lp__ + log_sum_exp(log(teta[1])+normal_log(y,mu[1],sigma[1]),log(teta[2])+normal_log(y,mu[2],sigma[2]));
}
