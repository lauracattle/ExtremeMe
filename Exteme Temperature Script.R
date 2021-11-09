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

# Bradford Analysis Mean Temperature #

meantempdata$BradWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                                TRUE, FALSE)
plot(meantempdata$Bradford,
     ylab="Temperature (C)", col=ifelse(meantempdata$BradWinter=="TRUE","red","blue"))

plot(meantempdata$Bradford, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Bradford[meantempdata$BradWinter])+ceiling(max(meantempdata$Bradford[meantempdata$BradWinter]))
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

mod <- evm(r2, th=12.7)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=12.7, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=12.7, method="boot")
ggplot(modboot)

# significantly improved histograms #


# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E071in20<- -(21.1-ceiling(max(meantempdata$Bradford[meantempdata$BradWinter]))))

# 95% low #

(E071in20low<- -(21.4-ceiling(max(meantempdata$Bradford[meantempdata$BradWinter]))))

# 95% high #

(E071in20high<- -(20.8-ceiling(max(meantempdata$Bradford[meantempdata$BradWinter]))))



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


### Penrith ###

par(mfrow=c(1,1))

meantempdata$CumbriaWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                               TRUE, FALSE)
plot(meantempdata$Cumbria,
     ylab="Temperature (C)", col=ifelse(meantempdata$CumbriaWinter=="TRUE","red","blue"))

plot(meantempdata$Cumbria, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Cumbria[meantempdata$CumbriaWinter])+ceiling(max(meantempdata$Cumbria[meantempdata$CumbriaWinter]))
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

mod <- evm(r2, th=11.9)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=11.9, method="sim")
ggplot(modmcmc)

# The autocorrelation function dies down a little slowly, so let's rerun the simulation, but this time retain only 1 in every 5 steps.


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=11.9, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E011in20<- -(21.4-ceiling(max(meantempdata$Cumbria[meantempdata$CumbriaWinter]))))

# 95% low #

(E011in20low<- -(22.1-ceiling(max(meantempdata$Cumbria[meantempdata$CumbriaWinter]))))

# 95% high #

(E011in20high<- -(20.8-ceiling(max(meantempdata$Cumbria[meantempdata$CumbriaWinter]))))



### Newcast Upon Tyne Mean Teperature EVA ###

par(mfrow=c(1,1))

meantempdata$TyneWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                             TRUE, FALSE)
plot(meantempdata$Tyne,
     ylab="Temperature (C)", col=ifelse(meantempdata$TyneWinter=="TRUE","red","blue"))

plot(meantempdata$Tyne, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Tyne[meantempdata$TyneWinter])+ceiling(max(meantempdata$Tyne[meantempdata$TyneWinter]))
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

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E031in20<- -(22.3-ceiling(max(meantempdata$Tyne[meantempdata$TyneWinter]))))

# 95% low #

(E031in20low<- -(22.8-ceiling(max(meantempdata$Tyne[meantempdata$TyneWinter]))))

# 95% high #

(E031in20high<- -(21.8-ceiling(max(meantempdata$Tyne[meantempdata$TyneWinter]))))



### Seghill ###


par(mfrow=c(1,1))

meantempdata$WearWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                               TRUE, FALSE)
plot(meantempdata$Wear,
     ylab="Temperature (C)", col=ifelse(meantempdata$WearWinter=="TRUE","red","blue"))

plot(meantempdata$Wear, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Wear[meantempdata$WearWinter])+ceiling(max(meantempdata$Wear[meantempdata$WearWinter]))
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

mod <- evm(r2, th=12.1)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=12.1, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=12.1, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E021in20<- -(20.3-ceiling(max(meantempdata$Wear[meantempdata$WearWinter]))))

# 95% low #

(E021in20low<- -(20.7-ceiling(max(meantempdata$Wear[meantempdata$WearWinter]))))

# 95% high #

(E021in20high<- -(19.9-ceiling(max(meantempdata$Wear[meantempdata$WearWinter]))))



### Beverley Mean Temp EVA ###


par(mfrow=c(1,1))

meantempdata$ERidingWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                             TRUE, FALSE)
plot(meantempdata$East.Riding,
     ylab="Temperature (C)", col=ifelse(meantempdata$ERidingWinter=="TRUE","red","blue"))

plot(meantempdata$East.Riding, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$East.Riding[meantempdata$ERidingWinter])+ceiling(max(meantempdata$East.Riding[meantempdata$ERidingWinter]))
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

mod <- evm(r2, th=11.1)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=11.1, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=11.1, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E061in20<- -(20.6-ceiling(max(meantempdata$East.Riding[meantempdata$ERidingWinter]))))

# 95% low #

(E061in20low<- -(21-ceiling(max(meantempdata$East.Riding[meantempdata$ERidingWinter]))))

# 95% high #

(E061in20high<- -(20.3-ceiling(max(meantempdata$East.Riding[meantempdata$ERidingWinter]))))


### Malton ###


par(mfrow=c(1,1))

meantempdata$NRidingWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                                   TRUE, FALSE)
plot(meantempdata$North.Riding,
     ylab="Temperature (C)", col=ifelse(meantempdata$NRidingWinter=="TRUE","red","blue"))

plot(meantempdata$North.Riding, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$North.Riding[meantempdata$NRidingWinter])+ceiling(max(meantempdata$North.Riding[meantempdata$NRidingWinter]))
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

mod <- evm(r2, th=11.9)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=11.9, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=11.9, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E051in20<- -(21.1-ceiling(max(meantempdata$North.Riding[meantempdata$NRidingWinter]))))

# 95% low #

(E051in20low<- -(21.6-ceiling(max(meantempdata$North.Riding[meantempdata$NRidingWinter]))))

# 95% high #

(E051in20high<- -(20.8-ceiling(max(meantempdata$North.Riding[meantempdata$NRidingWinter]))))


### Huddersfield ###

par(mfrow=c(1,1))

meantempdata$HuddsWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                                   TRUE, FALSE)
plot(meantempdata$Pennines,
     ylab="Temperature (C)", col=ifelse(meantempdata$HuddsWinter=="TRUE","red","blue"))

plot(meantempdata$Pennines, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Pennines[meantempdata$HuddsWinter])+ceiling(max(meantempdata$Pennines[meantempdata$HuddsWinter]))
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

mod <- evm(r2, th=13.5)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=13.5, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=13.5, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E091in20<- -(20.8-ceiling(max(meantempdata$Pennines[meantempdata$HuddsWinter]))))

# 95% low #

(E091in20low<- -(21.2-ceiling(max(meantempdata$Pennines[meantempdata$HuddsWinter]))))

# 95% high #

(E091in20high<- -(20.5-ceiling(max(meantempdata$Pennines[meantempdata$HuddsWinter]))))


### Darlington ###

par(mfrow=c(1,1))

meantempdata$DarlWinter<- ifelse(meantempdata$Month %in% c("November","December","January","February","March"),
                               TRUE, FALSE)
plot(meantempdata$Darlington,
     ylab="Temperature (C)", col=ifelse(meantempdata$DarlWinter=="TRUE","red","blue"))

plot(meantempdata$Darlington, ylab="Mean Temperature (C)")


## EVA ##

r2 <- -(meantempdata$Darlington[meantempdata$DarlWinter])+ceiling(max(meantempdata$Darlington[meantempdata$DarlWinter]))
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

mod <- evm(r2, th=11.9)
mod
summary(mod)

# visualisation of Parento Distribution

ggplot(mod)

# PP and QQ plots look good #

# MCMC #
modmcmc <- evm(r2, th=11.9, method="sim")
ggplot(modmcmc)


# An alternative to MCMC is a parametric bootstrap.

modboot <- evm(r2, th=11.9, method="boot")
ggplot(modboot)

# significantly improved histograms #

# Return levels

predict(modmcmc, M=12/5 * 20 * 30)
max(r2)


# 1 in 20 extremes #

predict(modmcmc,M=12/5*20*30,ci.fit=TRUE)

# expected value #
(E041in20<- -(21.4-ceiling(max(meantempdata$Darlington[meantempdata$DarlWinter]))))

# 95% low #

(E041in20low<- -(21.9-ceiling(max(meantempdata$Darlington[meantempdata$DarlWinter]))))

# 95% high #

(E041in20high<- -(20.9-ceiling(max(meantempdata$Darlington[meantempdata$DarlWinter]))))


(extremetemp = cbind("BOL"=c("Cumbria E01", "Tyne E02", "Wear E03", "Darlington E04", "N Riding E05", "E Riding E06", "Bradford E07", "Leeds E08", "Pennines E09"), "1 in 20 Winter Temperature"=c(E011in20,E021in20,E031in20,E041in20,E051in20,E061in20,E071in20,E081in20,E091in20)))

write.csv(extremetemp,"./Data/1in20temperature.csv")
