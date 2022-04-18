data {
  int N;
  vector[N] sales;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  sales ~ normal(mu, sigma);
}
