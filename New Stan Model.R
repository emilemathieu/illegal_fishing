####Stan Model
library("rstan")
library("bayesplot")
library("ggplot2")

####"Data"
weeks=48 ####Weeks in a normal year excluding September
quota=(3200000/27) ### 3200 ton quota in 2018. Divided in 27 because "units" are 27 kg boxes
cost=100 ##Aprox per box
fb=9.2e+05 ## fine expected per box, from chilean law
visa=3000  ##Constant value for the year, except in the end
c_max=15000 ##Max cost per box, then depending on landings and elasticities it goes down
p_max=30000 ##Max cost per box, then depending on landings and elasticities it goes down
beta_max=3000
Elast=0.47

##Landings range for priors
T_min=(quota/weeks)*3     ## landings lower limit: 3X the legal quota is from one of our papers
T_max=(27000000/27)/weeks ##landings higher limit: 27,4K ton is from our paper as well
T_mean=(T_min+T_max)/2

#Dmax: calculates the theta that would result in a x_i/x_T ratio of 0.67...which is the minimum ratio from landings prior
theta_max=(p_max-c_max-(p_max+1500)+(c_max+visa))/(((T_mean*0.67)*(p_max+1500))+fb)
qr=min(T_min)

  
#```{r}
data <- list(quota=quota, weeks=weeks, p_max=p_max, c_max=c_max, visa=visa , fb=9.2e+05, cost=100, T_mean=T_mean,T_min=T_min,T_max=T_max,qr=qr, beta_max=beta_max, theta_max=theta_max, Elast=Elast)
fit <- stan(file='~/OneDrive - Nexus365/Chapter 4/New stanmodel.stan', data=data, iter=15000, warmup=1000, chains=4, seed=0, refresh=11000, control = list(adapt_delta = 0.99))
print(fit)
#```

#```{r}

stan_dens(fit, pars=c("beta","theta","x_T","totallegal", "totalillegal", "ratio"))

stan_dens(fit, pars=c("C_I","C_L","P_I","P_L"))


