data {
  int N;
  vector[N] sales;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  for (i in 1:N) {
    sales[i] ~ normal(mu, sigma);
  }
}
