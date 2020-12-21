//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  real<lower=0> quota;
  int<lower=0> weeks;
  real<lower=0> beta_max; // price premium higher limit (unrealistically high)
  real<lower=0> c_max;  // Illegal price of reference at the port per unit: This comes from my fieldwork 1
  real<lower=0> p_max;  // Illegal price of reference at market per unit: This comes from my fieldwork 2
  real<lower=0> visa;
  real<lower=0> fb;    // fine expected per box, from chilean law
  real<lower=0> T_mean;
  real<lower=0> T_min;
  real<lower=0> T_max;
  real<lower=0> qr;
  real<lower=0> theta_max;
  real<lower=0> Elast;
  
}

transformed data {

  real<lower=0> sigma = quota * (10. / 100) / 3; // solves 3 * sigma = 10% quota since with P(|x-mu|<3*sigma)=99.73
}

// The parameters accepted by the model.
parameters {

  real <lower=0, upper=beta_max> beta; // uniform?

  real <lower=0, upper=theta_max> theta; // would gamma prior be better?
 
  real <lower=T_min, upper=T_max> x_T;
  
}

transformed parameters {
    // Model for one time period
    real C_L=   (c_max *  (1-((-Elast*((qr-x_T)/qr))))) +   visa;   // same but at the market based on legal captures previous year
    real C_I=   (c_max *  (1-((-Elast*((qr-x_T)/qr)))));
    real P_L=   (p_max  * (1-((-Elast*((qr-x_T)/qr))))) + beta;
    real P_I=   (p_max  * (1-((-Elast*((qr-x_T)/qr)))));
    
    real x_l=x_T-((((P_I-C_I-P_L+C_L-(fb*theta))/(8*theta*(P_L)))));
    real  x_i=x_T-x_l;
    real totallegal=x_l*weeks;
    real totalillegal=x_i*weeks;
    real ratio=(totalillegal/(totallegal+totalillegal));
}

// The model to be estimated. 
model {
  quota ~ normal(totallegal, sigma);
}
