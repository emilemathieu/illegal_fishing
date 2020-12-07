###Code with other sources of data
###The idea is that you get a per week mean  for landings, enforcement effort and prices at the port
###Packages needed for model###
library(patchwork)
library(performance)
library(see)
library(plotly)
library(plyr)
library(DescTools)
library(dplyr)
library(lubridate)
library(broom)
library(tidyr)
library(car)
library(plotly)
library(zoo)
library(xts)

#####First one is landings#####
####Load data set. I have it in my mac in this location, but in GitHub is called landings
dataland <- read.csv("~/OneDrive - Nexus365/Chapter 4/Dataland.csv")
# dataland <- read.csv("data/dataland.csv")

###Here I define the year as having 50 weeks because that's the number of weeks for a "normal" year" (I exclude september from the analysis cause there is a ban on fishing)
weeks=49

#######Here I identify to which week number each register for each year belongs to and add that to the dataset 
X19 <- (dataland$X19Z)
week19=week(as.POSIXlt(X19, format="%d/%m/%Y"))
dataland['weeks19'] <- week19

X18 <- (dataland$X18Z)
week18=week(as.POSIXlt(X18, format="%d/%m/%Y"))
dataland['weeks18'] <- week18

X17 <- (dataland$X17Z)
week17=week(as.POSIXlt(X17, format="%d/%m/%Y"))
dataland['weeks17'] <- week17

X16 <- (dataland$X16Z)
week16=week(as.POSIXlt(X16, format="%d/%m/%Y"))
dataland['weeks16'] <- week16

X15 <- (dataland$X15Z)
week15=week(as.POSIXlt(X15, format="%d/%m/%Y"))
dataland['weeks15'] <- week15

X14 <- (dataland$X14Z)
week14=week(as.POSIXlt(X14, format="%d/%m/%Y"))
dataland['weeks14'] <- week14

###This conversion is just because years 2014 and 2015 the original data set is is kg, not boxes
L14= dataland$X15C/27*1000
L15= dataland$X15C/27*1000

###Here what I do is to aggregate landings data per week for each year
land19 <- round(aggregate(dataland$X19C ~ dataland$weeks19, data = dataland, FUN = sum))
land18 <- aggregate(dataland$X18C ~ dataland$weeks18, data = dataland, FUN = sum)
land17 <- aggregate(dataland$X17C ~ dataland$weeks17, data = dataland, FUN = sum,drop=FALSE)
land16 <- aggregate(dataland$X16C ~ dataland$weeks16, data = dataland, FUN = sum)
land15 <- round(aggregate(L15 ~ dataland$weeks15, data = dataland, FUN = sum))
land14 <- round(aggregate(L14 ~ dataland$weeks14, data = dataland, FUN = sum))

###Here I aggregate this in a dataset 
weekly_landings=matrix(0,weeks,9)
weekly_landings[1:weeks,1]=land19$`dataland$X19C`[1:weeks]
weekly_landings[1:weeks,2]=land18$`dataland$X18C`[1:weeks]
weekly_landings[1:weeks,3]=land17$`dataland$X17C`[1:weeks]
weekly_landings[1:weeks,4]=land16$`dataland$X16C`[1:weeks]
weekly_landings[1:weeks,5]=land15$L15[1:weeks]
weekly_landings[1:weeks,6]=land14$L14[1:weeks]

####And calculate mean and SD
weekly_landings[1:weeks,7]=round(rowMeans(weekly_landings[,1:6],na.rm = TRUE))
Landings=transform(weekly_landings[,1:6], SD=apply(weekly_landings[,1:6],1, sd, na.rm = TRUE))
weekly_landings[1:weeks,8]=weekly_landings[1:weeks,7]+Landings$SD
weekly_landings[1:weeks,9]=weekly_landings[1:weeks,7]-Landings$SD

plot(weekly_landings[,7])

colnames(weekly_landings) <- c("2019", "2018", "2017", "2016", "2015", "2014", "mean", "mean_p_std", "mean_m_std")

####Enforcement#### Only on VII region, exluding month 9 (september)
#This is where I store the file. I uploaded this file as Enforcement Data
Enfeffort <- read.csv("~/OneDrive - Nexus365/Chapter 4/EnforcementData.csv")
# Enfeffort <- read.csv("data/EnforcementData.csv")

##All this is is filtering and doing the sum calculation
Enfeffort = filter(Enfeffort,Enfeffort$Especie =="Merluza común Artesanal IV a 41 28.6 LS")
Enfeffort = filter(Enfeffort,Enfeffort$Región =="Maule")
Date <- (Enfeffort$Fecha)
Month=as.numeric(substring(Date,4,5))
Year=substring(Date,9,10)
Enfeffort$Month=Month
Enfeffort$Year=Year
Enfeffort = filter(Enfeffort,Enfeffort$Month !=9)
Date <- (Enfeffort$Fecha)
enfweeks=week(as.POSIXlt(Date, format="%d/%m/%Y"))
Enfeffort['weeks'] <- enfweeks
Enfeffort['count'] <- 1
#Enfeffort$Code=as.numeric(paste0(Enfeffort$Cometido,Enfeffort$Year)) DONT CONSIDER FOR NOW
#Enfeffort=Enfeffort %>% distinct(Code, .keep_all = TRUE) ####DONT CONSIDER FOR NOW
#EnforcementperWeekMean <- round(aggregate(Enfeffort$count ~ Enfeffort$weeks, data = Enfeffort, FUN = sum))
#EnforcementperWeekMean$`Enfeffort$count`=(EnforcementperWeekMean$`Enfeffort$count`)/6
#plot(EnforcementperWeekMean$`Enfeffort$count`)

#2014
Enf2014=filter(Enfeffort,Enfeffort$Year ==14)
EnfWeekMean14 <- round(aggregate(Enf2014$count ~ Enf2014$weeks, data = Enf2014, FUN = sum))

#2015
Enf2015=filter(Enfeffort,Enfeffort$Year ==15)
EnfWeekMean15 <- round(aggregate(Enf2015$count ~ Enf2015$weeks, data = Enf2015, FUN = sum))

#2016
Enf2016=filter(Enfeffort,Enfeffort$Year ==16)
EnfWeekMean16 <- round(aggregate(Enf2016$count ~ Enf2016$weeks, data = Enf2016, FUN = sum))

#2017
Enf2017=filter(Enfeffort,Enfeffort$Year ==17)
EnfWeekMean17 <- round(aggregate(Enf2017$count ~ Enf2017$weeks, data = Enf2017, FUN = sum))

#2018
Enf2018=filter(Enfeffort,Enfeffort$Year ==18)
EnfWeekMean18 <- round(aggregate(Enf2018$count ~ Enf2018$weeks, data = Enf2018, FUN = sum))

#2019
Enf2019=filter(Enfeffort,Enfeffort$Year ==19)
EnfWeekMean19 <- round(aggregate(Enf2019$count ~ Enf2019$weeks, data = Enf2019, FUN = sum))


####
#####Prices
###Precios in all country
AllPrices <- read.csv("~/OneDrive - Nexus365/Chapter 4/AllPrices.csv")
AllPrices['dia'] <- 1
AllPrices = filter(AllPrices,AllPrices$MES !=9)
format(AllPrices,digits=0)

###Merluza
PreciosMerluza = filter(AllPrices,AllPrices$Nom_Especie =="Merluza común")
PreciosMerluza$date <- paste(PreciosMerluza$AÑO, PreciosMerluza$MES,PreciosMerluza$dia , sep="-") %>% ymd() %>% as.Date()
priceseriesMerluza <- data_frame(date=as.Date(PreciosMerluza$date), price=(as.numeric(as.character(PreciosMerluza$PRECIO....ton.))/1000))
###this is the list of average price per month
pricesMerluza=priceseriesMerluza %>% group_by(month=floor_date(date, "month")) %>%
  summarize(price=mean(price))
###
PriceMerluza=pricesMerluza$price
plot(PriceMerluza)
hist(log(PriceMerluza))
plot(log(PriceMerluza))
hist(PriceMerluza)
shapiro.test(log(PriceMerluza))

