#Initial Code & Preprocessing
setwd("C:/Users/HR/Documents/R code")
amsm_train=read.csv("amsm_train.csv")
amsm_test=read.csv("amsm_test.csv")
amsm_train=amsm_train[,2:91]
amsm_test=amsm_test[,2:91]
str(amsm_train)
amsm_train$Retired=as.factor(amsm_train$Retired)
amsm_train$AGE_Group=as.factor(amsm_train$AGE_Group)
amsm_train$Overseas=as.factor(amsm_train$Overseas)
str(amsm_test)
amsm_test$Retired=as.factor(amsm_test$Retired)
amsm_test$AGE_Group=as.factor(amsm_test$AGE_Group)
amsm_test$Overseas=as.factor(amsm_test$Overseas)
amsm_train$Base.Price=0.5*amsm_train$Base.Price
amsm_train$Final.Price=0.5*amsm_train$Final.Price
#Full Model
linreg1=lm(Final.Price ~.,data=amsm_train)
summary(linreg1)
#plot(linreg1)
y_pred1=predict(linreg1,newdata = amsm_test)
sse1=sum((y_pred1-amsm_test$Final.Price)^2)
sst1=sum((mean(amsm_train$Final.Price)-amsm_test$Final.Price)^2)
R_sq1=1-sse1/sst1
mse1=sse1/nrow(amsm_test)
#transformation
amsm1=amsm_train[-33,]
amsm1=amsm1[-57,]
amsm1$Final.Price=log(amsm1$Final.Price)
shapiro.test(amsm1$Final.Price)
linreg5=lm(Final.Price ~.,data=amsm1)
summary(linreg5)
#plot(linreg5)
amsm_test1=amsm_test
amsm_test1$Final.Price=log(amsm_test1$Final.Price)
y_pred5=predict(linreg5,newdata = amsm_test1)
sse5=sum((y_pred5-amsm_test1$Final.Price)^2)
sst5=sum((mean(amsm1$Final.Price)-amsm_test1$Final.Price)^2)
R_sq5=1-sse5/sst5
mse5=sse5/nrow(amsm_test1)
#Backward Selection
library(MASS)
step1=stepAIC(linreg1,direction = "forward")
step2=stepAIC(linreg1,direction = "backward")
linreg4=lm(Final.Price ~ ODI.Inn + ODI.Runs + ODI.100 + ODI.50 + T20I.NO + 
             T20I.Avg + T20I.SR + T20I.100 + T20I.50 + IPL.Inn + IPL.Runs + 
             IPL.50 + ODI.Inn.1 + ODI.Runs.1 + ODI.Wkts + ODI.5W + T20I.Inn.1 + 
             T20I.Runs.1 + T20I.Wkts + T20I.Econ + T20I.Avg.1 + T20I.SR.1 + 
             T20I.5W + IPL.Inn.1 + IPL.Runs.1 + IPL.Wkts + IPL.Avg.1 + 
             IPL.SR.1 + IPL.5W + FC.NO + FC.SR + LA.NO + LA.Ave + LA.100 + 
             LA.50 + t20.Inns + FC.Inns.1 + FC.Runs.1 + FC.Wkts + FC.SR.1 + 
             FC.5w + LA.Inns.1 + t20.Inns.1 + t20.Runs.1 + t20.5w + Retired + 
             Base.Price,data=amsm_train)
summary(linreg4)
y_pred4=predict(linreg4,newdata = amsm_test)
sse4=sum((y_pred4-amsm_test$Final.Price)^2)
sst4=sum((mean(amsm_train$Final.Price)-amsm_test$Final.Price)^2)
R_sq4=1-sse4/sst4
mse4=sse4/nrow(amsm_test)
#Both side Selection
step3=stepAIC(linreg1,direction = "both")
linreg2=lm(Final.Price ~ ODI.Inn + ODI.Runs + ODI.100 + ODI.50 + T20I.NO + 
             T20I.SR + T20I.50 + IPL.Inn + IPL.Runs + IPL.50 + ODI.Inn.1 + 
             ODI.Runs.1 + ODI.Wkts + ODI.5W + T20I.Inn.1 + T20I.Runs.1 + 
             T20I.Wkts + T20I.Econ + T20I.Avg.1 + T20I.SR.1 + T20I.5W + 
             IPL.Inn.1 + IPL.Runs.1 + IPL.Avg.1 + IPL.SR.1 + IPL.5W + 
             FC.NO + FC.SR + LA.NO + LA.Ave + LA.100 + LA.50 + t20.Inns + 
             FC.Inns.1 + FC.Runs.1 + FC.Wkts + FC.SR.1 + FC.5w + LA.Inns.1 + 
             t20.Inns.1 + t20.Runs.1 + t20.5w + Retired + Base.Price + 
             AGE_Group,data=amsm_train)
summary(linreg2)
y_pred2=predict(linreg2,newdata = amsm_test)
sse2=sum((y_pred2-amsm_test$Final.Price)^2)
sst2=sum((mean(amsm_train$Final.Price)-amsm_test$Final.Price)^2)
R_sq2=1-sse2/sst2
mse2=sse2/nrow(amsm_test)
#Manual Changes
linreg3=lm(Final.Price ~ ODI.Inn + ODI.Runs + ODI.100 + ODI.50 + T20I.NO + 
                          T20I.SR + T20I.50 + IPL.Inn + IPL.Runs + IPL.50 + ODI.Inn.1 + 
                          ODI.Runs.1 + ODI.Wkts + ODI.5W + T20I.Inn.1 + T20I.Wkts +
                          T20I.Econ + T20I.Avg.1 + T20I.SR.1 + T20I.5W + IPL.Inn.1 + 
                          IPL.Runs.1 + IPL.Avg.1 + IPL.SR.1 + IPL.5W + FC.NO + FC.SR + 
                          LA.NO + LA.100 + LA.50 + t20.Inns + FC.Wkts + FC.5w + LA.Inns.1 + 
                          t20.Inns.1 + t20.Runs.1  + Retired + Base.Price,data=amsm_train)
summary(linreg3)
y_pred3=predict(linreg3,newdata = amsm_test)
sse3=sum((y_pred3-amsm_test$Final.Price)^2)
sst3=sum((mean(amsm_train$Final.Price)-amsm_test$Final.Price)^2)
R_sq3=1-sse3/sst3
mse3=sse3/nrow(amsm_test)
#Ridge Regression
library(glmnet)
xtr=data.matrix(amsm_train[,-90])
ytr=amsm_train$Final.Price
lambdas=10^seq(3,-2,by=-0.1)
fit=glmnet(xtr,ytr,alpha = 0,lambda = lambdas)
summary(fit)
cv_fit <- cv.glmnet(xtr, ytr, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda
fit <- cv_fit$glmnet.fit
summary(fit)
xtt=data.matrix(amsm_test[,-90])
ytt=amsm_test$Final.Price
y_predicted <- predict(fit, s = opt_lambda, newx = xtt)
sst <- sum((ytt-mean(ytr))^2)
sse <- sum((y_predicted - ytt)^2)
rsq <- 1 - sse / sst
rsq
coef(fit,s=opt_lambda)
#Lasso
library(glmnet)
xtr1=data.matrix(amsm_train[,-90])
ytr1=amsm_train$Final.Price
lambdas1=10^seq(3,-2,by=-0.1)
fit1=glmnet(xtr1,ytr1,alpha = 1,lambda = lambdas1)
summary(fit1)
cv_fit1 <- cv.glmnet(xtr1, ytr1, alpha = 1, lambda = lambdas1)
plot(cv_fit1)
opt_lambda1 <- cv_fit1$lambda.min
opt_lambda1
fit1 <- cv_fit1$glmnet.fit
summary(fit1)
xtt1=data.matrix(amsm_test[,-90])
ytt1=amsm_test$Final.Price
y_predicted1 <- predict(fit1, s = opt_lambda1, newx = xtt1)
sst1 <- sum((ytt1-mean(ytr1))^2)
sse1 <- sum((y_predicted1 - ytt1)^2)
rsq1 <- 1 - sse1 / sst1
rsq1
coef(fit1,s=opt_lambda1)