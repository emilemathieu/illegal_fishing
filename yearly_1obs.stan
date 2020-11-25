data {

  real<lower=0> quota;
  int<lower=0> daysop;
  real<lower=0> ppmax; // price premium higher limit (unrealistically high)
  real<lower=0> DR;    // This comes from enforcement records data, probability of detection per box
  real<lower=0> wbox;  // Illegal price of reference at the port per unit: This comes from my fieldwork 1
  real<lower=0> pbox;  // Illegal price of reference at market per unit: This comes from my fieldwork 2
  real<lower=0> visa;
  real<lower=0> fb;    // fine expected per box, from chilean law
}

transformed data {
  
  real Dmax = 3 * DR;                        // detectability higher limit
  // Landings prior per day of operation
  real<lower=0> nmin= quota / daysop;       // landings lower limit: 3X the legal quota is from one of our papers
  
  real<lower=0> nmax= (27000000. / 27.) / daysop; // landings higher limit: 27,4K ton is from our paper as well
  real wl= wbox + visa; // Here, I add elasticity of demand at the port for legal, based on previous and current catch
  real wi= wbox;        // Here, I add elasticity of demand at the port for illegal, based on previous and current catch
  real Pi= pbox;        //same but at the market based on illegal captures previous year
  real<lower=0> sigma = quota * (10. / 100) / 3; // solves 3 * sigma = 10% quota since with P(|x-mu|<3*sigma)=99.73
}

// The parameters accepted by the model.
parameters {

  // real totallegal;
  // PRIOR for price premium parameter (value that traders receive for a legal unit)
  // ppI=rtruncnorm(n=iterations,   a=ppmin,  b=ppmax, mean=((ppmin+ppmax)/2),  sd=100000)
  real <lower=0, upper=ppmax> pp; // uniform?
  // PRIOR for detectability (probability of detection per unit)
  //   DI = rtruncnorm(n=iterations, a=Dmin, b=Dmax, mean=((Dmin+Dmax)/2), sd=1)
  // PRIOR range for landings (total units landed each year, considering legal and illegal)
  real <lower=0, upper=Dmax> D; // would gamma prior be better?
  // nRI=rtruncnorm(n=iterations,   a=nmin,  b=nmax, mean=((nmin+nmax)/2),  sd=10000000)
  real <lower=nmin, upper=nmax> nR;
}


transformed parameters {
    // Model for one time period
    real pl= pbox + pp;   // same but at the market based on legal captures previous year
    // Function 6 in draft, calculates the optimal quantity of illegal units. Divided by 100 to convert to ratio
    real x=((((Pi-wi-pl+wl-(fb*D))/(8*D*(pl)))))/100.; // Illegal units
    real l=1-x;  // legal units
    // Calculates the total (for a year)
    real totallegal=(nR*l)*daysop;
    // real totalillegal=(nR*x)*daysop;
}

// The model to be estimated. 
model {
  quota ~ normal(totallegal, sigma);
}
