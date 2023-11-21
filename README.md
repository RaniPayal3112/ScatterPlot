# ScatterPlot
Scatterplot
#Load Libraries
library(lattice)
library(psych)
library(gmodels)
#View Dataset
drug
#Mean of CESD
mean(drug$cesd)
#Standard Deviation of CESD
sd(drug$cesd)
#Variance of CESD
var(drug$cesd)
#Median of CESD
median(drug$cesd)
#Basic Statistics of CESD
describe(drug$cesd)
#Load Library
library(ggplot2)
#Load Data
data(mpg)
mpg
#Displ vs. hwy pointplot
ggplot(data=mpg, aes(x = displ, y = hwy, color = class)) + geom_point()
#Displ vs. hwy point plot – Blue colors
ggplot(data=mpg, aes(x = displ, y = hwy)) + geom_point(color = "red")
#Histogram of CESD
x=drug$cesd
h<-hist(x, breaks=10, col="red", xlab="CESD",
        main="Epidemiologic Studies")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)
#Plot with Line of MCS and CESD
plot(drug$mcs,drug$cesd,main="Epidemiologic Studies", xlab = 'cesd', ylab="mcs")
abline(lm(drug$mcs~drug$cesd),col="red")
#Corrolation between MCS and CESD
cor(drug$mcs,drug$cesd, use="complete.obs")
#Crosstab Table
mytable <- CrossTable(drug$sex,drug$homeless) 
#Displ vs. hwy pointplot
ggplot(data=mpg, aes(x = hwy, y = displ, color = class)) + geom_point()
#Displ vs. hwy point plot – Blue colors
ggplot(data=mpg, aes(x = displ, y = hwy)) + geom_point(color = "red")
# plot with both points and smoothed line with blue points
ggplot(data= mpg, aes(x = displ, y = hwy)) +geom_point(color = "blue") + geom_smooth(color = "red")
# position = "dodge": values next to each other
ggplot(data=mpg, aes(x = class, fill = drv)) + geom_bar(position = "dodge")
# position = "fill": percentage chart
ggplot(data=mpg, aes(x = class, fill = drv)) + geom_bar(position = "fill")
