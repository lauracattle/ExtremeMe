## This script contains the Extreme Value analysis to understand the 1 in 20 monthly temperature for the 9 BOL areas. 
## This will feed into PRE, Reports and Repairs forecasts to understand the 1 in 20 temperature scenario for each year to understand the uplift
## 1 in 20 has been chosen as the suitable limit as this relates to the Gas Networks requirement to ensure the network can with stand a 1 in 20 winters day


# install require packages for EVA #

install.packages("devtools")
install.packages("texmex")
install.packages("ggfortify")
install.packages("mojito")
library(devtools)
library(texmex)
library(scales)
library(ggfortify)

library(mojito)
library(gridExtra)

# Set working directory and load data #

setwd("C:/Users/LThornley/Northern Gas/Emergency Response Staffing Analysis - Documents/2020/Data & Analysis/Auto Population")
meantempdata6016<-read.csv("./Data/meantemp1960_2016.csv",sep=",",header=T)
meantempdata1718<-read.csv("./Data/meantemp 170101_181231.csv",sep=",",header=T)

Date <- c(meantempdata6016$Date,meantempdata1718$date)
Month <- c(as.character(meantempdata6016$Month) ,as.character(meantempdata1718$Month))
Cumbria <- c(meantempdata6016$Cumbria ,meantempdata1718$E01Temp)
Tyne <- c(meantempdata6016$Tyne ,meantempdata1718$E02Temp)
Wear <- c(meantempdata6016$Wear ,meantempdata1718$E03Temp)
Darlington <- c(meantempdata6016$Darlington ,meantempdata1718$E04Temp)
North.Riding <- c(meantempdata6016$North.Riding ,meantempdata1718$E05Temp)
East.Riding <- c(meantempdata6016$East.Riding ,meantempdata1718$E06Temp)
Bradford <- c(meantempdata6016$Bradford ,meantempdata1718$E07Temp)
Leeds <- c(meantempdata6016$Leeds ,meantempdata1718$E08Temp)
Pennines <- c(meantempdata6016$Pennines ,meantempdata1718$E09Temp)

meantempdata = data.frame(Date,Month,Cumbria,Tyne,Wear,Darlington,North.Riding,East.Riding,Bradford,Leeds,Pennines)

attach(meantempdata)
summary(meantempdata)

### Leeds Mean Monthly Temp EVA ###

par(mfrow=c(1,1))

meantempdata$LeedsWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                             TRUE, FALSE)
plot(meantempdata$Leeds,
     ylab="Temperature (C)", col=ifelse(meantempdata$LeedsWinter=="TRUE","red","blue"))

plot(meantempdata$Leeds, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Leeds[meantempdata$LeedsWinter])+ceiling(max(meantempdata$Leeds[meantempdata$LeedsWinter]))
# Mean Residual Life Plot
mr2 <- mrl(r2)
par(mfrow=c(1, 1))
plot(mr2)


# Estimate Generalized Pareto Distribution Parameters Over A Range Of Values
ps2 <- gpdRangeFit(r2,umin=quantile(r2, .05),umax=quantile(r2, .95))
par(mfrow=c(1, 2))
plot(ps2)

# Parameter estimates with gg plots
p1 <- ggplot(ps2)
p2 <- ggplot(mr2)
grid.arrange(p1[[1]], p1[[2]], p2, ncol=2)

# Fitting generalized Pareto distributions

# Threshold of 9 to get around 5% outside 95% bands#

mod <- evm(r2, th=12)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=12, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=12, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 *30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E081in20<- -(20.3-ceiling(max(meantempdata$Leeds[meantempdata$LeedsWinter]))))

# 95% low #

(E081in20low<- -(20.7-ceiling(max(meantempdata$Leeds[meantempdata$LeedsWinter]))))

# 95% high #

(E081in20high<- -(19.9-ceiling(max(meantempdata$Leeds[meantempdata$LeedsWinter]))))



(extremetemp = cbind("BOL"=c("Leeds E08"), "1 in 20 Winter Temperature"=c(E081in20)))

write.csv(extremetemp,"./Data/1in20temperature.csv")
