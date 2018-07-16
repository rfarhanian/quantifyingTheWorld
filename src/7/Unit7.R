library(data.table)
#setwd("/Users/raminfarhanian/Documents/projects/R/qtw/unit7/")
nenana <- read.table(file="nenana.txt", header = TRUE, sep=",")
head(nenana)
str(nenana)
library(ggplot2)
lmfit <- lm(Julian.Date ~ Year, data= nenana)

plotdata <- ggplot(data=nenana, aes(x=Year, y=Julian.Date))+
  ggtitle("Nenana Ice Break")+
  geom_point(size=3)+
  geom_abline(intercept = coef(lmfit)[1], slope = coef(lmfit)[2]) 

plotdata
nenana$cp1970p <- pmax(0, nenana$Year-1970)

nenana$cp1970p
nenana$Year

cp1970p.fit <- lm(Julian.Date ~ Year + cp1970p, data= nenana)
summary(cp1970p.fit)


library(SiZer)


pw.model <- piecewise.linear(nenana$Year, nenana$Julian.Date, middle = 1, CI=TRUE)
pw.model


################----------------------Video 7.7
# Binary Segmentation vs. PELT methods
library(gstat)
data("wind", package= "gstat")
str(wind)
ts.plot(wind[,11], xlab="Index")
library(changepoint)
wind.pelt <- cpt.var(diff(wind[,11], method="PELT"))
plot(wind.pelt, xlab="Index")
logLik(wind.pelt)

wind.bs <- cpt.var(diff(wind[,11], method="BingSeg"))
# Binary Segmentation method suggests that there is only one changepoint.
ncpts(wind.bs)  
# Pelt method suggests that there are two changepoints
ncpts(wind.pelt) 
wind.bs <- cpt.var(diff(wind[,11], method="BingSeg"), Q=60)
plot(wind.bs, xlab="Index")
# The following line of code indicates whether this is a good model
logLik(wind.bs)

################----------------------Disovery Data
data("discoveries", package="datasets")
dis.pelt <- cpt.meanvar(discoveries, test.stat = "Poisson", method = "PELT")
plot(dis.pelt, cpt.width=3)
cpts.ts(dis.pelt)
dis.bs <- cpt.meanvar(discoveries, test.stat = "Poisson", method = "BinSeg")
cpts.ts(dis.bs)




