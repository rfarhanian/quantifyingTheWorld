library(data.table)
setwd("/Users/farhanir/Documents/projects/R/qtw/unit7/")
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
