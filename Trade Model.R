####Code for trade model Version III gets rid of the trucks and days of operation and does everything in weeks
##First part is ABC Step 1: Here what I do is to obtain the distribution for unknown parameters (landings, price premium and detectability) from uninformed priors
##Second part is ABC Step 2: Using the distribution from Step 1 I run the model to obtain the set of optimal ratio strategies
##Then Ratios figure graph the different ratios distribution

###Packages needed for model####
library(truncnorm)
library(ggplot2)
library(patchwork)
library(performance)
library(see)
library(plotly)
library(plyr)
library(DescTools)

#####ABC Step 1##############################################################################################
####
###########Pre-set parameters
####
#Number of iterations for ABC Step 1
iterations=10000000
###Updates is the number of times it updates the prior from the previous posterior (basically, the number of times it does the whole process)
updates=3

# % difference of quota: The criteria for selection/rejection of the iterations 
cut=10

###Value (in chilean pesos) of the permit that traders pay to fishers per legal unit
visa=3000   
C_i=5000   ### Illegal cost of reference at the port per unit: This comes from my fieldwork 1
P_i=20000  ### Illegal price of reference at market per unit: This comes from my fieldwork 2
fb=9.2e+05 ## fine expected per box, from chilean law
cost=100 ####per box...sort of unknown, but I dont think this matters much because costs are equal for legal and illegal boxes

####PRIORs for three unknown parameters: price premium, detectability and landings
###
####PRIOR range for price premium parameter (value that traders receive for a legal unit)
beta_min=0   ### price premium lower limit
beta_mean=1500
beta_max=3000 ###price premium higher limit, otherwise it goes over visa en then optimal is all legal

####PRIOR range for landings (total units landed each year, considering legal and illegal)
weeks=48 ####Weeks in a normal year excluding September
quota=(3200000/27) ### 3200 ton quota in 2018. Divided in 27 because "units" are 27 kg boxes

###Landings prior per day of operation
T_min=(quota/weeks)*3     ## landings lower limit: 3X the legal quota is from one of our papers
T_max=(27000000/27)/weeks ##landings higher limit: 27,4K ton is from our paper as well
T_mean=(T_min+T_max)/2

####PRIOR range for detectability (probability of detection per unit)
theta_min= 0     ### detectability lower limit, basically no detectability
#theta_min=(pbox-wbox-(pbox+ppmean)+(wbox+visa))/(((nmean*1.2)*(pbox+ppmean))+fb)
theta_max= (P_i-C_i-(P_i+beta_mean)+(C_i+visa))/(((T_mean*0.67)*(P_i+beta_mean))+fb) ###This equation is the same than than the one used to calculate the optimal rate, but solving for D when x is 0.67 (which is the value we use to calculate theta_min and comes from data collected)
###Creates matrix for results
updating=matrix(0,iterations,updates*5)
updating=as.data.frame(updating)

#####Loop function for ABC Step 1
###Here I start the loop for updates, and within that one there is the one with iterations
for (update in 1:updates) ### Start iterations
{
  ###These are functions to obtain a random draw for each parameter based on min and max set before
  beta_I=rtruncnorm(n=iterations,   a=beta_min,  b=beta_max, mean=(beta_mean),  sd=100000)
  theta_I = rtruncnorm(n=iterations, a=theta_min,  b=theta_max, mean=((theta_min+theta_max)/2), sd=1)
  T_I=rtruncnorm(n=iterations,   a=T_min,  b=T_max, mean=((T_mean)),  sd=10000000)
  
  ##If function to tell the model whether to take the uninformed prior, or the anterior posterior (depending on the update round)
beta_prior=if(update<=1) {beta_I} else if (update<=2) {PricePremium} else if (update<=3) {PricePremium}
theta_prior=if(update<=1) {theta_I} else if (update<=2) {Detectability} else if (update<=3) {Detectability}
T_prior=if(update<=1) {T_I} else if (update<=2) {AvLand} else if (update<=3) {AvLand}
  
reality=matrix(0,iterations,9)  ##This creates the matrix for the iterations (sorry for the name!)
  
  ### For loop to start iterations
  for (sim in 1:iterations) 
  {
    ###Take a random draw from the prior
    #
   beta=sample(beta_prior,1) 
   theta=sample(theta_prior,1)
   T=sample(T_prior,1)
    
    ###Model for one time period
    #cost of box to trader at the port for:
    #Legal
     C_l= C_i  + visa 
     
    #price paid to trader at the market for:
    #legal
    P_l= P_i  + beta 
    
    ##Function 6 in draft, calculates the optimal quantity of illegal units. Divided by n to convert to ratio
    x_i=((((P_i-C_i-P_l+C_l-(fb*theta))/(8*theta*(P_l)))))/T ##Illegal units
    
    ####This sets the condition that x_i (illegal ratio) cannot be higher than 1 or less than 0
    X_i=if(x_i<=1) {x_i} else {1} 
    X_i=if(X_i<=0) {0} else {X_i} 
    
    x_l=1-X_i ###legal ratio
    
    ##Calculates the total for a year of legal and illegal units, depending on the ratio
    totallegal=(T*x_l)*weeks
    totalillegal=(T*X_i)*weeks
    
    #Caculates the profit 
    totalprofit=(totalillegal*(P_i-C_i)) + (totallegal*(P_l-C_l))- ((theta* totalillegal)*((4*P_l*totalillegal)+fb)) - ((totalillegal+ totallegal)*cost)
    
    ##Fills the matrix with results
    reality[sim, 1]=((totallegal)-quota)/(quota)*100 ##this calculates the % of deviation of the iteration from the quota
    reality[sim, 2]=beta ##keeps track of what random parameter the iteration used
    reality[sim, 3]=visa ##keeps track of what random parameter the iteration used
    reality[sim, 4]=theta ##keeps track of what random parameter the iteration used
    reality[sim, 5]=T ##keeps track of what random parameter the iteration used
    reality[sim, 6]=totalillegal
    reality[sim, 7]=totallegal
    reality[sim, 8]=totalprofit
    reality[sim, 9]=X_i
    
    ##here the loop of iterations finishes 
  }
  
  ###This filters the data so that only those where legal units are within +- 10% of quota, and profits are higher than 0
  data=data.frame(reality)
  Data=subset(data,data$X8>0)
  Data2=subset(Data,Data$X1<cut)
  DataFinal=subset(Data2,Data2$X1>-cut)
  
  ##Creates the variables from the filtered data
  PricePremium=DataFinal$X2 ##Price premium
  Detectability=DataFinal$X4 ##Detectability
  AvLand=(DataFinal$X5)     ##Landings
  
  ##Build the posterior distribution matrix
  pp=length(AvLand)
  Posteriors=matrix(0,pp,3)
  Posteriors[,1]=Detectability
  Posteriors[,2]=AvLand
  Posteriors[,3]=PricePremium
  
  ###These create vectors with n=iteration of the filtered results, to be used for next prior
  DPP=density(PricePremium, n=iterations,adjust=3)
  DPricePremium=DPP$y
  xPricePremium=DPP$x
  
  DDet=density(Detectability, n=iterations,adjust=3)
  DDetectability=DDet$y
  xDetectability=DDet$x
  
  DLand=density(AvLand, n=iterations,adjust=3)
  DAvLand=DLand$y
  xAvLand=DLand$x
  
  updating[,update]=            DPricePremium
  updating[,update+(updates)]=  xPricePremium
  updating[,update+(updates*2)]=DDetectability
  updating[,update+(updates*3)]=xDetectability
  updating[,update+(updates*4)]=DAvLand
  updating[,update+(updates*5)]=xAvLand
  
  ### here the loop for the updates finish
}

#plot(reality[,9],reality[,8])
plot(DataFinal[,9],DataFinal[,8])

####
##Plot the distributions
#First create density probability distribution from the data, then plot that
###Detectability
DDetIn=density(Detectability, n=iterations, adjust=3)
yDetecIN=DDetIn$y
xDetecIN=DDetIn$x
plot(xDetecIN,yDetecIN, type="l",xlab="Detectability",ylab="Probability Density")

###Landings
DLandIn=density(AvLand, n=iterations,adjust=3,from=T_min, to=T_max)
yLandIN=DLandIn$y
xLandIN=DLandIn$x
Lprior=max(yLandIN)
plot(xLandIN,yLandIN, type="l",xlab="Landings",ylab="Probability Density")

###PricePremium
DPPIn=density(PricePremium, n=iterations,adjust=3)
yPPIN=DPPIn$y
xPPIN=DPPIn$x
Pprior=max(DPPIn$y)
plot(xPPIN,yPPIN, type="l",xlab="PricePremium",ylab="Probability Density")


###Write a csv file to save data from ABC Step 1
write.csv(Posteriors, file="~/OneDrive - Nexus365/Trade Model Chapter/generateddata.csv")

#####ABC Step 2#############################################################################################
#Loads Posterior distribution from step 1
posteriors <- read.csv("~/OneDrive - Nexus365/Trade Model Chapter/generateddata.csv")
T_dist=(posteriors$V2)
Beta=posteriors$V3
Theta=posteriors$V1

###10000 iterations and 4 updates takes 14 minutes, at least on my mac
iterations=10000
updates=5
cut=10

visa=3000   
c_i=5000   ### Illegal price of reference at the port per unit: This comes from my fieldwork 1
p_i=20000  ### Illegal price of reference at market per unit: This comes from my fieldwork 2
fb=9.2e+05 ## fine expected per box, from chilean law
weeks=48
quota=(3200000/27) ### 3200 ton quota in 2018. Divided in 27 because "units" are 27 kg boxes


###
###These are unknown variables (but for now I am treating them as if I knew the value). I only have data for selR, but all the rest are unknowns
pdlR=  -0.47  ### Price elasticity of quantity at market for legal products
pdiR=  -0.47  ### Price elasticity of quantity at market for illegal products
selR=  -0.47  ### Price elasticity of quantity at port for legal products. From my econometric analysis
seiR=  -0.47  ### Price elasticity of quantity at port for illegal products. From my econometric analysis

####
###These are "by how much" things change in the different Seasons. These are completely unknown parameters, but again, I am treating them as if I knew. I only know, from surveys, that these thigs change in each season, but I don't know by how much
ve= -0.6     ###  This is the elasticity of visa for End of year months
le=  0.6     ### This is by how much the legal elasticity is multiplied for in eastern and pre post ban months
he=  1.1     ### This is by how much the enforcement is multiplied for in the eastern months

####Standards!
###These are "by how much" things change in the different Seasons. These are completely unknown parameters, but again, I am treating them as if I knew. I only know, from surveys, that these thigs change in each season, but I don't know by how much
#ve= -0.6     ###  This is the elasticity of visa for End of year months
#le=  0.8     ### This is by how much the legal elasticity is multiplied for in eastern and pre post ban months
#he=  1.2     ### This is by how much the enforcement is multiplied for in the eastern months


cost=100 ####per box...sort of unknown, but I dont think this matters much because costs are equal for legal and illegl boxes

#Qr is the quantity reference for the elasticities. This is just necessary to calculate elasticities
qr=mean(T_dist)

##Create matrix for results
ratios=matrix(0,iterations,7)
policies=matrix(0,iterations,18)
updating=matrix(0,iterations,updates*5)
updating=as.data.frame(updating)

###Track time it takes to run
start_time <- Sys.time()
start_time

###Model#
for (update in 1:updates) ### Start iterations
{
  
  ####Draw random ratio values for each stage of the year
  r1I=  rtruncnorm(n=iterations,a=0,    b=1, mean=0.5,sd=1)
  r2I = rtruncnorm(n=iterations,a=0,    b=1, mean=0.5,sd=1)
  r3I = rtruncnorm(n=iterations,a=0,    b=1, mean=0.5,sd=1)
  r4I = rtruncnorm(n=iterations,a=0,    b=1, mean=0.5,sd=1)
  
  ###Use random prior or previous posterior, depending on number of update
  r1prior=if(update<=1) {r1I} else  {A1}
  r2prior=if(update<=1) {r2I} else  {A2}
  r3prior=if(update<=1) {r3I} else  {A3}
  r4prior=if(update<=1) {r4I} else  {A4}
  
  
  ####For function for the iterations
  for (R in 1:iterations) ###Run time iterations
  {
    
    ##Sample a ratio for each stage
    r1=sample(r1prior,1)
    r2=sample(r2prior,1)
    r3=sample(r3prior,1)
    r4=sample(r4prior,1)
    
    ###These create strings to select what happens in each stage
    ##########################################J1  J2  F1  F2  S1   M1   M2   A1  A2   M1   M2   J1   J2   J1   J2    A1   A2   W1    O1   O1   N1    N2   D1   D2
    erres=                as.data.frame(rep(c(r1, r1, r1, r1, r1,  r2,  r2,  r2, r1,  r1,  r1,  r1,  r1,  r1,  r1,   r3,  r3,  r3,   r3,  r3,  r3,   r3,  r3,  r4), each = 2))
    strategic=            as.data.frame(rep(c( 1,  1,  1,  1,  1,  he,  he,  he,  1,   1,   1,   1,   1,   1,   1,    1,   1,   1,    1,   1,   1,    1,   1,   1), each = 2))
    elasticitiesLEGAL=    as.data.frame(rep(c( 1,  1,  1,  1,  1,  le,  le,  le,  1,   1,   1,   1,   1,   1,   1,   le,  le,  le,   le,  le,  le,   le,  le,   1), each = 2))
    VISA=                 as.data.frame(rep(c( 0,  0,  0,  0,  0,   0,   0,   0,  0,   0,   0,   0,   0,   0,   0,    0,   0,   0,    0,   0,   0,    0,   0,   1), each = 2))
    elasticitiesILLEGAL=  as.data.frame(rep(c( 1,  1,  1,  1,  1,   1,   1,   1,  1,   1,   1,   1,   1,   1,   1,    1,   1,   1,    1,   1,   1,    1,   1,   1), each = 2))
    
    ###Model
    ti=weeks
    Data=matrix(0,ti+1,14)
    
    
    for (t in 1:ti) ###Run time iterations
    {
      r=erres[t,1]
      enfm=strategic[t,1]
      T_week=(sample(T_dist,1))
      VisaR=VISA[t,1]
      
      theta = sample(Theta,1)*enfm
      beta =sample(Beta,1)
      
      pdl=pdlR*elasticitiesLEGAL[t,1]
      pdi=pdiR*elasticitiesILLEGAL[t,1]
      
      sel=selR*elasticitiesLEGAL[t,1]
      sei=seiR*elasticitiesILLEGAL[t,1]
      
      ill=r
      leg=(1-r)
      
      ####Calculate profit and final 
      totallegal=leg*T_week
      totalillegal=ill*T_week
      
      quotaleft=(quota-totallegal-sum(Data[,3]))
      quotaleftR=if(quotaleft>0){quotaleft} else 0
      visaqf=(visa * (1-((-ve*((quota-quotaleftR)/quota)))))
      
      C_L=   (c_i *  (1-((sel*((qr-T_week)/qr))))) + if(VisaR>0.5) {visaqf} else  visa            #Here, I add elasticity of demand at the port for legal, based on previos and current catch
      C_I=   (c_i *  (1-((sei*((qr-T_week)/qr))))) #Here, I add elasticity of demand at the port for illegal, based on previous and current catch
      
      P_L=   (p_i  * (1-((pdl*((qr-T_week)/qr))))) + beta
      P_I=   (p_i  * (1-((pdi*((qr-T_week)/qr))))) #same for illegal 
      
      totalprofit=(totalillegal*P_I-C_I)- ((theta* totalillegal)*(4*P_L*totalillegal+fb)) + (totallegal*(P_L-C_L))- ((totalillegal+ totallegal)*cost)
      
      Data[t,1]=totalprofit
      Data[t,2]=totallegal
      Data[t+1,3]=totallegal
      Data[t+1,4]=totalillegal
      #Data[t,5]=t-3
      Data[t,6]=T_week
      #Data[t,7]=totalillegal
      #Data[t,8]=pi
      #Data[t,9]=pl
      #Data[t,10]=wl
      #Data[t,11]=wi
      #Data[t,12]=totallegal+totalillegal
      
    }
    ratios[R,1]=r1
    ratios[R,2]=r2
    ratios[R,3]=r3
    ratios[R,4]=r4
    ratios[R,5]=((sum(Data[,2])-quota)/quota)*100
    ratios[R,6]=sum(Data[,1])
    ratios[R,7]=sum(Data[,6])
  }
  
  Policies=as.data.frame(ratios)
  
  ####Filter of iterations that results in legal landings in ??5% of quota limit
  A=subset(Policies, Policies$V5>-cut & Policies$V5<cut)
  ####Filter of iterations that results in 5% highest profit
  Cut=1-(cut/100)
  profitcutA=quantile((A$V6), c(Cut))
  
  AF=subset(A,A$V6>profitcutA)
  A1=AF$V1
  A2=AF$V2
  A3=AF$V3
  A4=AF$V4
  
  pp=nrow(AF)
  Posteriors=matrix(0,pp,4)
  Posteriors[,1]=A1
  Posteriors[,2]=A2
  Posteriors[,3]=A3
  Posteriors[,3]=A4
  
}

end_time <- Sys.time()
end_time - start_time

#####Histograms of ratios distribution####
hist(A1)
hist(A2)
hist(A3)
hist(A4)


#####Ratios Figure####
##Create PDFs from results
AFP1=density(A1, n=iterations,adjust=3, from=0, to=1)
AyFP1=AFP1$y
AxFP1=AFP1$x
AFP2=density(A2, n=iterations,adjust=3, from=0, to=1)
AyFP2=AFP2$y
AxFP2=AFP2$x
AFP3=density(A3, n=iterations,adjust=3, from=0, to=1)
AyFP3=AFP3$y
AxFP3=AFP3$x
AFP4=density(A4, n=iterations,adjust=3, from=0, to=1)
AyFP4=AFP4$y
AxFP4=AFP4$x

#Create matrix for Figure
Probabilitites=matrix(0,iterations,18)
Probabilitites[,1]=AyFP1
Probabilitites[,2]=AxFP1
Probabilitites[,3]=AyFP2
Probabilitites[,4]=AxFP2
Probabilitites[,5]=AyFP3
Probabilitites[,6]=AxFP3
Probabilitites[,7]=AyFP4
Probabilitites[,8]=AxFP4
data=as.data.frame(Probabilitites)

###Figure of three stages ratios
fig <- plot_ly(data, x = data$V2, y = data$V1, name = 'Primary strategy', type = 'scatter', mode = 'lines',
               line = list(color = 'black', width = 4), xaxis=c(0,1)) 
fig <- fig %>% add_trace(x =data$V4, y = data$V3, name = 'Eastern month', line = list(color = 'red', width = 4, dash = 'dash')) 
fig <- fig %>% add_trace(x = data$V6, y = data$V5, name = 'Pre/Post ban', line = list(color = 'blue', width = 4, dash = 'dot')) 
fig <- fig %>% add_trace(x = data$V8, y = data$V7, name = 'End of year', line = list(color = 'purple', width = 4, dash = 'dot')) 
fig <- fig %>% layout(xaxis = list(title = "Illegal/total catch ratio"))
fig <- fig %>% layout(yaxis = list(title = "Probability Density"))
fig <- fig %>% layout(legend = list(x =0.1, y = 0.9))
fig



#######Compare with data through time####
####I am not explaining this in detail cause its just to create the graph, no analysis really
Data=matrix(0,weeks,14)
Rounds=length(A1)
NNAS=matrix(0,weeks,Rounds+1)
NNASI=matrix(0,weeks,Rounds+1)
days=weeks
Rmodels=as.data.frame(AF)
ti=weeks

for (R in 1:Rounds) ###Run time simulations
{
  r1=Rmodels[R,1]
  r2=Rmodels[R,2]
  r3=Rmodels[R,3]
  r4=Rmodels[R,4]

  
  erres=                as.data.frame(rep(c(r1, r1, r1, r1, r1,  r2,  r2,  r2, r1,  r1,  r1,  r1,  r1,  r1,  r1,   r3,  r3,  r3,   r3,  r3,  r3,   r3,  r3,  r4), each = 2))
  strategic=            as.data.frame(rep(c( 1,  1,  1,  1,  1,  he,  he,  he,  1,   1,   1,   1,   1,   1,   1,    1,   1,   1,    1,   1,   1,    1,   1,   1), each = 2))
  elasticitiesLEGAL=    as.data.frame(rep(c( 1,  1,  1,  1,  1,  le,  le,  le,  1,   1,   1,   1,   1,   1,   1,   le,  le,  le,   le,  le,  le,   le,  le,   1), each = 2))
  VISA=                 as.data.frame(rep(c( 0,  0,  0,  0,  0,   0,   0,   0,  0,   0,   0,   0,   0,   0,   0,    0,   0,   0,    0,   0,   0,    0,   0,   1), each = 2))
  elasticitiesILLEGAL=  as.data.frame(rep(c( 1,  1,  1,  1,  1,   1,   1,   1,  1,   1,   1,   1,   1,   1,   1,    1,   1,   1,    1,   1,   1,    1,   1,   1), each = 2))
  
  for (t in 1:ti) ###Run time simulations
  {

    r=erres[t,1]
    enfm=strategic[t,1]
    T_week=(sample(T_dist,1))
    VisaR=VISA[t,1]
    
    theta = sample(Theta,1)*enfm
    beta =sample(Beta,1)
    
    pdl=pdlR*elasticitiesLEGAL[t,1]
    pdi=pdiR*elasticitiesILLEGAL[t,1]
    
    sel=selR*elasticitiesLEGAL[t,1]
    sei=seiR*elasticitiesILLEGAL[t,1]
    
    ill=r
    leg=(1-r)
    
    ####Calculate profit and final 
    totallegal=leg*T_week
    totalillegal=ill*T_week
    
    quotaleft=(quota-totallegal-sum(Data[,3]))
    quotaleftR=if(quotaleft>0){quotaleft} else 0
    visaqf=(visa * (1-((-ve*((quota-quotaleftR)/quota)))))
    
    C_L=   (c_i *  (1-((sel*((qr-T_week)/qr))))) + if(VisaR>0.5) {visaqf} else  visa            #Here, I add elasticity of demand at the port for legal, based on previos and current catch
    C_I=   (c_i *  (1-((sei*((qr-T_week)/qr))))) #Here, I add elasticity of demand at the port for illegal, based on previous and current catch
    
    P_L=   (p_i  * (1-((pdl*((qr-T_week)/qr))))) + beta
    P_I=   (p_i  * (1-((pdi*((qr-T_week)/qr))))) #same for illegal 
    
    totalprofit=(totalillegal*P_I-C_I)- ((theta* totalillegal)*(4*P_L*totalillegal+fb)) + (totallegal*(P_L-C_L))- ((totalillegal+ totallegal)*cost)
    
    
    Data[t,1]=totalillegal
    Data[t,2]=totallegal
    
  }
  
  NNASI[,R]=Data[,1]
  NNAS[,R]=Data[,2]
  
  
}

sims=NNAS
simsI=NNASI
###Create data frame with landings data, mean and SD
landingsdata <- read.csv("~/OneDrive - Nexus365/Trade Model Chapter/landingsdata.csv")

ofdata=matrix(0,weeks,18)
ofdata[1:weeks,6]=landingsdata$V1
ofdata[1:weeks,7]=landingsdata$V2
ofdata[1:weeks,8]=landingsdata$V3


###Calculate means and SD for simulations Legal
ofdata[1:weeks,12]=round(rowMeans(sims[,1:Rounds]))
Model=transform(sims[,1:Rounds], SD=apply(sims[,1:Rounds],1, sd, na.rm = TRUE))
ofdata[1:weeks,13]=ofdata[1:weeks,12]+Model$SD
ofdata[1:weeks,14]=ofdata[1:weeks,12]-Model$SD


###Calculate means and SD for simulations Illelgal
ofdata[1:weeks,15]=round(rowMeans(simsI[,1:Rounds]))
Model=transform(simsI[,1:Rounds], SD=apply(simsI[,1:Rounds],1, sd, na.rm = TRUE))
ofdata[1:weeks,16]=ofdata[1:weeks,15]+Model$SD
ofdata[1:weeks,17]=ofdata[1:weeks,15]-Model$SD
Ofdata=as.data.frame((ofdata))
Ofdata[is.na(Ofdata)] = 0 ###remember to change this!!!!!
boxtoton=27/1000

###Graph
fig <- plot_ly(Ofdata, x = ~seq(1:weeks), y = ~Ofdata$V12*boxtoton, type = 'scatter', mode = 'lines',
               line = list(color='rgb(0,100,80)'),
               name = 'Legal Simulation Mean +/-SD') 
fig <- fig %>% add_trace(y = ~Ofdata$V13*boxtoton, type = 'scatter', mode = 'lines',
                         line = list(color = 'transparent'), name = 'High Landings',showlegend = FALSE) 

fig <- fig %>% add_trace(y = ~Ofdata$V14*boxtoton, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                         showlegend = FALSE, name = 'Low Landings')

##Illegal
fig <- fig%>% add_trace(Ofdata, x = ~seq(1:weeks), y = ~Ofdata$V16*boxtoton, type = 'scatter', mode = 'lines',line = list(color = 'transparent'),showlegend = FALSE, name = 'High Landings') 
fig <- fig %>% add_trace(y = ~Ofdata$V17*boxtoton, type = 'scatter', mode = 'lines',fill = 'tonexty', fillcolor='rgba(220,20,60,0.2)', line = list(color = 'transparent'),showlegend = FALSE, name = 'Low Landings')
fig <- fig %>% add_trace(y = ~Ofdata$V15*boxtoton, type = 'scatter', mode = 'lines',line = list(color='red'),name = 'Illegal Simulation Mean +/-SD') 
##Illegal


fig <- fig%>% add_trace(Ofdata, x = ~seq(1:weeks), y = ~Ofdata$V7*boxtoton, type = 'scatter', mode = 'lines',
                        line = list(color = 'transparent'),
                        showlegend = FALSE, name = 'High Landings') 
fig <- fig %>% add_trace(y = ~Ofdata$V8*boxtoton, type = 'scatter', mode = 'lines',
                         fill = 'tonexty', fillcolor='rgba(0,17,157,0.2)', line = list(color = 'transparent'),
                         showlegend = FALSE, name = 'Low Landings')
fig <- fig %>% add_trace(y = ~Ofdata$V6*boxtoton, type = 'scatter', mode = 'lines',
                         line = list(color='blue'),
                         name = 'Landings Mean (2014-2019) +/-SD') 
#fig <- fig %>% add_trace(y = ~Ofdata$V1, line = list(color='black'),name = '2019 Landings') 
fig <- fig %>% layout(showlegend = TRUE)
fig <- fig %>% layout(xaxis = list(range = c(2,weeks)))
fig <- fig %>% layout(yaxis = list(range = c(0,250)))
fig <- fig %>% layout(xaxis = list(title = "Week"))
fig <- fig %>% layout(legend = list(x =0.5, y = 1.1))
fig <- fig %>% layout(yaxis = list(title = "Ton"))
fig

