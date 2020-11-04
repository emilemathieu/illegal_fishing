####Code for trade model 
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
iterations=100000
# % difference of quota: The criteria for selection/rejection of the iterations 
cut=10

###Value (in chilean pesos) of the permit that traders pay to fishers per legal unit
visa=3000    

####PRIORs for three unknown parameters: price premium, detectability and landings
###
####PRIOR range for price premium parameter (value that traders receive for a legal unit)
ppmin=0    ### price premium lower limit
ppmax=6000 ### price premium higher limit (unrealistically high)

####PRIOR range for detectability (probability of detection per unit)
DR=  5e-05   ### This comes from enforcement records data, probability of detection per box
Dmin= 0     ### detectability lower limit
Dmax= DR*3  ### detectability higher limit

####PRIOR range for landings (total units landed each year, considering legal and illegal)
###Since the landing distribution that I want for ABC Step 2 is per day, first I need to calculate how many days fishers operate
daysop=200 ####This comes from government data
quota=(3200000/27) ### 3200 ton quota in 2018. Divided in 27 because "units" are 27 kg boxes

###Landings prior per day of operation
nmin=(quota/daysop)   ### landings lower limit: 3X the legal quota is from one of our papers
nmax=(27000000/27)/daysop ##landings higher limit: 27,4K ton is from our paper as well

##Because nmin and nmax are in units, "boxtoton" transforms back to tons later if needed 
boxtoton=(daysop*27)

###Updates is the number of times it updates the prior from the previous posterior (basically, the number of times it does the whole process)
updates=2

###Creates matrix for results
updating=matrix(0,iterations,updates*5)
updating=as.data.frame(updating)

###Other fix parameters needed for the model

wbox=5000   ### Illegal price of reference at the port per unit: This comes from my fieldwork 1
pbox=17000  ### Illegal price of reference at market per unit: This comes from my fieldwork 2

fb=9.2e+05 ## fine expected per box, from chilean law


#####Loop function for ABC Step 1
###Here I start the loop for updates, and within that one there is the one with iterations
for (update in 1:updates) ### Start iterations
{
  ###These are functions to obtain a random draw for each parameter based on min and max set before
  ppI=rtruncnorm(n=iterations,   a=ppmin,  b=ppmax, mean=((ppmin+ppmax)/2),  sd=100000)
  DI = rtruncnorm(n=iterations, a=Dmin, b=Dmax, mean=((Dmin+Dmax)/2), sd=1)
  nRI=rtruncnorm(n=iterations,   a=nmin,  b=nmax, mean=((nmin+nmax)/2),  sd=10000000)
  
  ##If function to tell the model whether to take the uninformed prior, or the anterior posterior (depending on the update round)
  PPPrior=if(update<=1) {ppI} else if (update<=2) {PricePremium} else if (update<=3) {PricePremium}
  DPrior=if(update<=1) {DI} else if (update<=2) {Detectability} else if (update<=3) {Detectability}
  NPrior=if(update<=1) {nRI} else if (update<=2) {AvLand} else if (update<=3) {AvLand}
  

  reality=matrix(0,iterations,6)  ##This creates the matrix for the iterations (sorry for the name!)
  
  ### For loop to start iterations
  for (sim in 1:iterations) 
  {
    ###Take a random draw from the prior
    pp=sample(PPPrior,1) 
    D=sample(DPrior,1)
    nR=sample(NPrior,1)

    ###Model for one time period
    wl= wbox  + visa #Here, I add elasticity of demand at the port for legal, based on previous and current catch
    wi= wbox  #Here, I add elasticity of demand at the port for illegal, based on previous and current catch
    pl= pbox  + pp #same but at the market based on legal captures previous year
    pi= pbox  #same for illegal 
    
    ##Function 6 in draft, calculates the optimal quantity of illegal units. Divided by 100 to convert to ratio
    x=((((pi-wi-pl+wl-(fb*D))/(8*D*(pl)))))/100 ##Illegal units
    l=1-x ###legal units
    
    ##Calculates the total (for a year)
    totallegal=(nR*l)*daysop
    totalillegal=(nR*x)*daysop
    
    ##Fills the matrix with results
    reality[sim, 1]=((totallegal)-quota)/(quota)*100 ##this calculates the % of deviation of the iteration from the quota
    reality[sim, 2]=pp ##keeps track of what random parameter the iteration used
    reality[sim, 3]=visa ##keeps track of what random parameter the iteration used
    reality[sim, 4]=D ##keeps track of what random parameter the iteration used
    reality[sim, 5]=nR ##keeps track of what random parameter the iteration used
    reality[sim, 6]=totalillegal
   
    ##here the loop of iterations finishes 
  }
  
  ###This filters the data so that only those iteration with legal units within the criteria are kept
  data=data.frame(reality)
  Data=subset(data,data$X1<cut)
  DataFinal=subset(Data,Data$X1>-cut)
  
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
  
  ###THese create vectors with n=iteration of the filtered results, to be used for next prior
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

####
##Plot the distributions
#First create density probability distribution from the data, then plot that
###Detectability
DDetIn=density(Detectability, n=iterations, from=Dmin, to=Dmax,adjust=3)
yDetecIN=DDetIn$y
xDetecIN=DDetIn$x
plot(xDetecIN,yDetecIN, type="l",xlab="Detectability",ylab="Probability Density")

###Landings
DLandIn=density(AvLand, n=iterations,adjust=3,from=nmin, to=nmax)
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

###10000 iterations and 4 updates takes 14 minutes, at least on my mac
iterations=100000
updates=2

#################Fixed Parameters
pdlR=  -0.3  ### Price elasticity of quantity at market for legal products
pdiR=  -0.3  ### Price elasticity of quantity at market for illegal products
selR=  -0.45 ### Price elasticity of quantity at port for legal products. From my econometric analysis
seiR=  -0.45 ### Price elasticity of quantity at port for illegal products. From my econometric analysis

ve= -0.5     ###  This is the elasticity of visa for End of year months
le=  0.4     ### This is by how much the legal elasticity is multiplied for in eastern and pre post ban months
he=  1.5     ### This is by how much the enforcement is multiplied for in the eastern months

cost=100 ####per box

##Create matrix for results
ratios=matrix(0,iterations,7)
policies=matrix(0,iterations,18)

#Qr is the quantity reference for the elasticities
qr=min(AvLand)

####Create TRUCKS characteristics

minlegal=90 #sets whether trucks NEED to take legal units or need
x1n=10  ### Number of type 1 trucks
x2n=10  ### Number of type 2 trucks
x3n=5   ### Number of type 3 trucks

x1cap=60 #max capacity of type 1 trucks
x2cap=150 #max capacity of trucks category 2
x3cap=400 #max capacity of trucks category 3


###Calculate total capacity of truck fleets
capacity=(x1n*x1cap)+(x2n*x2cap)+(x3n*x3cap)
Illcapacity=(x1n*x1IC)+(x2n*x2IC)+(x3n*x3IC)

##Create matrix for results
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
    
    ###These create strings to select what happens in each fortnight (11 months, because there is ban on fishing in September, so there are 22)
    erres=                as.data.frame(rep(c(r1,r1,r1,r1, r2,  r2,  r1, r1,  r1,  r1,  r1,  r1,  r3, r3,   r3,   r1,   r1,  r1,  r4,   r4,  r4,  r1), each = daysop/20))
    strategic=            as.data.frame(rep(c( 1, 1, 1, 1, he,  he,   1,  1,   1,   1,   1,   1,   1,  1,    1,    1,    1,   1,   1,    1,   1,   1), each = daysop/20))
    elasticitiesLEGAL=    as.data.frame(rep(c( 1, 1, 1, 1, le,  le,   1,  1,   1,   1,   1,   1,  le,  le,   le,   1,    1,   1,   1,    1,   1,   1), each = daysop/20))
    VISA=                 as.data.frame(rep(c( 0, 0, 0, 0,  0,   0,   0,  0,   0,   0,   0,   0,   0,   0,    0,    0,   0,   0,   1,    1,   1,   0), each = daysop/20))
    elasticitiesILLEGAL=  as.data.frame(rep(c( 1, 1, 1, 1,  1,   1,   1,  1,   1,   1,   1,   1,   1,   1,    1,   1,    1,   1,   1,    1,   1,   1), each = daysop/20))
    ###Model
    ti=daysop
    Data=matrix(0,ti+1,14)
    

    for (t in 1:ti) ###Run time iterations
    {
      
      r=erres[t,1]
      enfm=strategic[t,1]
      n=(sample(AvLand,1))
      VisaR=VISA[t,1]
      
      enfm=strategic[t,1]
      D =sample(Detectability,1)*enfm
      pp =sample(PricePremium,1)
      
      pdl=pdlR*elasticitiesLEGAL[t,1]
      pdi=pdiR*elasticitiesILLEGAL[t,1]
      
      sel=selR*elasticitiesLEGAL[t,1]
      sei=seiR*elasticitiesILLEGAL[t,1]
      
      ill=r
      leg=(1-r)
      
      x1leg=leg*x1cap
      x1ill=ill*x1cap
      
      x2leg=leg*x2cap
      x2ill=ill*x2cap
      
      x3leg=leg*x3cap
      x3ill=ill*x3cap
      
      totalillegal=((x1n*x1ill)+(x2n*x2ill)+(x3n*x3ill))
      totallegal=((x1n*x1leg)+(x2n*x2leg)+(x3n*x3leg))
      
      #Correction if it goes beyond landings
      over=n-(totalillegal+totallegal)
      factor=(if(over>0) {1} else over= 1+((over)/(totalillegal+totallegal)))
      
      ##Bring new trucks if there's no enough capacity
      diff=n-capacity
      Call=(if(diff>=0) {1} else 0) 
      
      newtrucks=round((diff/x1cap)*Call)
      
      totalillegal=(((x1n+newtrucks)*x1ill)+(x2n*x2ill)+(x3n*x3ill))*factor
      totallegal=(((x1n+newtrucks)*x1leg)+(x2n*x2leg)+(x3n*x3leg))*factor
      
      realx1leg=x1leg*factor
      realx1ill=x1ill*factor
      
      realx2leg=x2leg*factor
      realx2ill=x2ill*factor
      
      realx3leg=x3leg*factor
      realx3ill=x3ill*factor
      
      
      ####Calculate profit and final 
      quotaleft=(quota-totallegal-sum(Data[,3]))
      quotaleftR=if(quotaleft>0){quotaleft} else 0
      visaqf=(visa * (1-((-ve*((quota-quotaleftR)/quota)))))
      
      wl=   (wbox *  (1-((sel*((qr-n)/qr))))) + if(VisaR>0.5) {visaqf} else  visa            #Here, I add elasticity of demand at the port for legal, based on previos and current catch
      wi=   (wbox *  (1-((sei*((qr-n)/qr))))) #Here, I add elasticity of demand at the port for illegal, based on previos and current catch
      
      pi=   (pbox  * (1-((pdi*((qr-n)/qr))))) #same for ilelgal 
      pl=   (pbox  * (1-((pdl*((qr-n)/qr))))) + pp
      
      profitx1=(realx1ill*(pi-wi))-((D*realx1ill)*(4*pl*realx1ill+fb))+(realx1leg*(pl-wl))-((realx1ill+realx1leg)*cost)
      profitx2=(realx2ill*(pi-wi))-((D*realx2ill)*(4*pl*realx2ill+fb))+(realx2leg*(pl-wl))-((realx2ill+realx2leg)*cost)
      profitx3=(realx3ill*(pi-wi))-((D*realx3ill)*(4*pl*realx3ill+fb))+(realx3leg*(pl-wl))-((realx3ill+realx3leg)*cost)
      totalprofit=(profitx1*(x1n+newtrucks))+(profitx2*x2n)+(profitx3*x3n)
      
      Data[t,1]=totalprofit
      Data[t,2]=totallegal
      Data[t+1,3]=totallegal
      Data[t+1,4]=totalillegal
      Data[t,5]=t-3
      Data[t,6]=n
      Data[t,7]=totalillegal
      Data[t,8]=pi
      Data[t,9]=pl
      Data[t,10]=wl
      Data[t,11]=wi
      Data[t,12]=totallegal+totalillegal
      Data[t,13]=newtrucks
      
    }
    ratios[R,1]=r1
    ratios[R,2]=r2
    ratios[R,3]=r3
    ratios[R,4]=r4
    ratios[R,5]=((sum(Data[,2])-quota)/quota)*100
    ratios[R,6]=sum(Data[,1])
    ratios[R,7]=sum(Data[,2])
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

#Histograms of ratios distribution
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


