data{
  int r;
  int d[r];
  real exp_d[r];
}

parameters{
  real <lower=0> theta[r];
  real <lower=0> alpha;
  real <lower=0> beta;
}

model {
  for(i in 1:r){
    d[i] ~ poisson(exp_d[i]*theta[i]);
    theta[i] ~ gamma(alpha,beta);
  }
  alpha ~ exponential(1);
  beta ~ gamma(0.1,1);
}

generated quantities {
  real RRmean; 
  real RRvar;
  int  dPred[r];
  RRmean = alpha/beta;
  RRvar = alpha/(beta^2);
  for(i in 1:r){
  dPred[i] = poisson_rng(exp_d[i]*theta[i]);
  }
}
