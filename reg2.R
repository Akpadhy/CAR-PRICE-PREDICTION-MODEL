
library(ggplot2)
library(caTools)


carprice<-read.csv(file.choose(),na.strings = c("?",NA),stringsAsFactors = T)
str(carprice)

colSums(is.na(carprice))
# imputing missing values
library(missForest)
cp<-missForest(carprice)
cp$ximp
# impute missing value
carprice<-cp$ximp
colSums(is.na(carprice))
View(carprice)
#decimals reduction
options(digits = 4)
#Load the Essential libraries

library(dplyr)
library(tidyr)
library(lubridate)
library(car)
library(MASS)
library(stringr)


library(caret)
dummyify<-dummyVars(~.,data = carprice,fullRank = T)
nwdt<-predict(dummyify,carprice)
nwdt<-data.frame(nwdt)
str(nwdt)



# Train test Split
set.seed(123)
library(caret)
split<-createDataPartition(y = nwdt$price, p = 0.7, list = FALSE)
train_data<-nwdt[split,]
test_data<-nwdt[-split,]
nrow(train_data)
nrow(test_data)

#Executing the First Multi Model:

model_1<-lm(price~.,data = train_data)
summary(model_1)



#The Summary of the above model has many insignificant variables

#Using the StepAIC function on the Training dataset to find

st<-stepAIC(model_1,direction = "both")
st$coefficients
names(st$coefficients)
st$model
######################################################
colSums(is.na(train_data))
View(train_data)
class(train_data)
md<-lm(log(price)~CompName.bmw+CompName.chevrolet+CompName.dodge+       
CompName.isuzu       + CompName.jaguar      + CompName.mazda    +    CompName.mercury  +   
       CompName.mitsubishi  + CompName.nissan   +    CompName.peugot    +   CompName.plymouth +   
        CompName.porsche    +  CompName.renault    +  CompName.subaru    +   CompName.toyota +     
        CompName.volkswagen  + CompName.volvo     +   Fueltype.gas     +     Aaspiration.turbo+    
        CarBody.hatchback   +  enginelocation.rear  + DriveWheel        +    length    +           
        width          +       height              +  curb.weight       +    EngineType.ohcv+      
        CylinderNumber.five  + CylinderNumber.four +  CylinderNumber.six  +  CylinderNumber.twelve+
       engine.size       +    FuelSystem..2bbl   +   FuelSystem..mpfi    +  FuelSystem..spdi    +
          bore             +     stroke           +     peak.rpm         +     highway.mpg  ,data = train_data)
summary(md)
names(md$coefficients)
library(lmtest)
bptest(md)
par(mfrow=c(2,2))
plot(md)
library(lmtest)
ncvTest(md)
hist(md$residuals)
plot(md$residuals)
qqPlot(md$residuals)
#Evaluate: RMSE & R-Squraes Values on validation data
test_data$pricelog<-log(test_data$price)
predictedVal<-predict(md,test_data)  
predictedVal=predict(md, newdata=test_data)
modelvalues<-data.frame(obs = test_data$pricelog, pred=predictedVal)
defaultSummary(modelvalues)
table(test_data$price)
#MAPE
mape<-mean(abs(modelvalues$pred-modelvalues$obs)/modelvalues$obs)
mape
##########################################################
##########################################################


library(glmnet)

x.tr<-model.matrix(price~.,data=train_data)[,-1]
View(x.tr)
y.tr<-train_data$price
x.tst<-model.matrix(price~.,data=test_data)[,-c(1,67)]
View(x.tst)
y.tst<-test_data$price

#ridge
grid<-10^seq(10,-2,length.out = 100)
grid

ridge.model<-glmnet(x.tr,y.tr,alpha = 0,lambda = grid)
ridge.model
coef(ridge.model)
plot(ridge.model )
par(mfrow=c(1,1))
set.seed(101)
cv=cv.glmnet(x.tr,y.tr,alpha=0)
plot(cv)

cv$lambda.1se
cv$lambda.min->best

rr.fit<-glmnet(x.tr,y.tr,alpha = 0)
predict(rr.fit,type = "coefficients",s=best)
plot(rr.fit, xvar = "lambda", label = TRUE,cex=0.7)

rr.pred <- predict(rr.fit, s = best, newx = x.tst)
sqrt(mean((rr.pred - y.tst)^2))

mape<-mean(abs(rr.pred - y.tst)/y.tst)
mape

#lasso
lasso.model<-glmnet(x.tr,y.tr,alpha = 1,lambda = grid)
lasso.model
coef(lasso.model)

set.seed(102)
cv=cv.glmnet(x.tr,y.tr,alpha=1)
plot(cv)

cv$lambda.1se
cv$lambda.min->best

rr.fit<-glmnet(x.tr,y.tr,alpha = 1)
predict(rr.fit,type = "coefficients",s=best)
plot(rr.fit, xvar = "lambda", label = TRUE)

rr.pred <- predict(rr.fit, s = best, newx = x.tst)
sqrt(mean((rr.pred - y.tst)^2))

mape<-mean(abs(rr.pred - y.tst)/y.tst)
mape
#elastic
els.model<-glmnet(x.tr,y.tr,alpha = 0.5,lambda = grid)
els.model
coef(els.model)
set.seed(103)
cv=cv.glmnet(x.tr,y.tr,alpha=0.5)
plot(cv)

cv$lambda.1se
cv$lambda.min->best

rr.fit<-glmnet(x.tr,y.tr,alpha = 0.5)
predict(rr.fit,type = "coefficients",s=best)
plot(rr.fit, xvar = "lambda", label = TRUE)

rr.pred <- predict(rr.fit, s = best, newx = x.tst)
sqrt(mean((rr.pred - y.tst)^2))

mape<-mean(abs(rr.pred - y.tst)/y.tst)
mape
