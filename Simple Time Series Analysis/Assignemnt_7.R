
## Pre processing
#Loading the data into data frame and making column names suitable for working with R

# pre processing
rm(list=ls())
library(car)
library(rio)
china.visitors=import("6304 Module 7 Assignment Data.xlsx")
colnames(china.visitors)=tolower(make.names(colnames(china.visitors)))
china.visitors$index=seq(1:nrow(china.visitors))
names(china.visitors)

## Analysis 1
plot(china.visitors$index,china.visitors$china.visitors,pch=19,type="o",
     main="Chinese Visitors to US on a Quarterly basis 1998-2012")

# Analysis 2
visistors.out1=lm(china.visitors~index,data=china.visitors)
summary(visistors.out1)

##Analysis 3
plot(china.visitors$index,china.visitors$china.visitors,pch=19,type="o",
     main="Chinese Visitors to US (Quarterly basis,1998-2012")
abline(visistors.out1,col="red",lwd=3)

## Analysis 4
# Durbin Watson Test

durbinWatsonTest(visistors.out1)

## Analysis 5
# Building Seasonal Indices
# First Creating a Data Frame to hold them.

seasonal_indices=data.frame(month=1:4,average=0,index=0)
for(i in 1:4) {
  count=0
  for(j in 1:nrow(china.visitors)) {
    if(i==china.visitors$quarter[j]) {
      seasonal_indices$average[i]=
        seasonal_indices$average[i]+china.visitors$china.visitors[j]
      count=count+1
    }
  }
  seasonal_indices$average[i]=seasonal_indices$average[i]/count
  seasonal_indices$index[i]=
    seasonal_indices$average[i]/mean(china.visitors$china.visitors)}

# Deseasonalizing the original data

for(i in 1:4) {
  for(j in 1:nrow(china.visitors)) {
    if(i==china.visitors$quarter[j]) {
      china.visitors$deseason[j]=
        china.visitors$china.visitors[j]/seasonal_indices$index[i]
    }
  }
}

##Analysis 6
#simple regression model
simple_regout=lm(deseason~index,data=china.visitors)
plot(china.visitors$index,china.visitors$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot w regression line")
points(china.visitors$index,simple_regout$fitted.values,
       type="l",col="red",lwd=3)
summary(simple_regout)
#second order regression model
secondorder_regout=lm(deseason~poly(index,2),data=china.visitors)
plot(china.visitors$index,china.visitors$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot w regression line")
points(china.visitors$index,secondorder_regout$fitted.values,
       type="l",col="red",lwd=3)
summary(secondorder_regout)
#third order regression model
thirdorder_regout=lm(deseason~poly(index,3),data=china.visitors)
plot(china.visitors$index,china.visitors$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot w regression line")
points(china.visitors$index,thirdorder_regout$fitted.values,
       type="l",col="red",lwd=3)
summary(thirdorder_regout)
#fourth order regression model
fourthorder_regout=lm(deseason~poly(index,4),data=china.visitors)
plot(china.visitors$index,china.visitors$deseason,type="o",
     lwd=3,main="Deseasoned Data Plot w regression line")
points(china.visitors$index,fourthorder_regout$fitted.values,
       type="l",col="red",lwd=3)
summary(fourthorder_regout)
#Reseasonalizing
for(j in 1:nrow(china.visitors)) {
  xx=china.visitors$quarter[j]
  china.visitors$reseason.y1.hat[j]=
    simple_regout$fitted.values[j]*seasonal_indices$index[xx]
}
plot(china.visitors$index,china.visitors$china.visitors,pch=19,type="o",
     main="Actual data vs Reseasoned fitted vales ")
points(china.visitors$index,china.visitors$reseason.y1.hat,pch=19,type="o",lwd=3,col="red", main="Reseasoned Values")

#Reseasonalizing 2nd order
for(j in 1:nrow(china.visitors)) {
  xx=china.visitors$quarter[j]
  china.visitors$reseason.y2.hat[j]=
    secondorder_regout$fitted.values[j]*seasonal_indices$index[xx]
}
plot(china.visitors$index,china.visitors$china.visitors,pch=19,type="o",
     main="Actual data vs Reseasoned fitted vales ")
points(china.visitors$index,china.visitors$reseason.y2.hat,pch=19,type="o",lwd=3,col="red", main="Reseasoned Values")

#Reseasonalizing 3rd order
for(j in 1:nrow(china.visitors)) {
  xx=china.visitors$quarter[j]
  china.visitors$reseason.y3.hat[j]=
    thirdorder_regout$fitted.values[j]*seasonal_indices$index[xx]
}
plot(china.visitors$index,china.visitors$china.visitors,pch=19,type="o",
     main="Actual data vs Reseasoned fitted vales ")
points(china.visitors$index,china.visitors$reseason.y3.hat,pch=19,type="o",lwd=3,col="red", main="Reseasoned Values")


#Reseasonalizing for 4th order
for(j in 1:nrow(china.visitors)) {
  xx=china.visitors$quarter[j]
  china.visitors$reseason.y4.hat[j]=
    fourthorder_regout$fitted.values[j]*seasonal_indices$index[xx]
}

plot(china.visitors$index,china.visitors$china.visitors,pch=19,type="o",
     main="Actual data vs Reseasoned fitted vales ")
points(china.visitors$index,china.visitors$reseason.y4.hat,pch=19,type="o",lwd=3,col="red", main="Reseasoned Values")

# Rsquared values for 3rd order
cor(china.visitors$index,china.visitors$reseason.y3.hat)# r value
cor(china.visitors$index,china.visitors$reseason.y3.hat)^2# r squared value
# Rsquared values for 3rd order
cor(china.visitors$index,china.visitors$reseason.y4.hat)# r value
cor(china.visitors$index,china.visitors$reseason.y4.hat)^2# r squared value


