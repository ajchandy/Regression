library(rio)
boats.info=import("6304 Module 1 Assignment Data.xlsx",sheet="All Boats")
colnames(boats.info)=tolower(make.names(colnames(boats.info)))
attach(boats.info)

Price=boats.info$Price
State=boats.info$State
Length=boats.info$Length
Age=boats.info$Age

#creating data object
sub_state=subset(boats.info,State=="WA"|State=="GA")
sub_state

#taking random samples
set.seed(65111941)
smpl=sub_state[sample(1:nrow(sub_state),60,replace=FALSE),]
smpl

sub1=subset(smpl,state=="WA")
sub2=subset(smpl,state=="GA")

sub2

str(smpl)

lmts = range(sub1$price,sub2$price)

par(mfrow = c(1, 2))
boxplot(sub1$price,ylim=lmts) 
boxplot(sub2$price,ylim=lmts)

?boxplot
