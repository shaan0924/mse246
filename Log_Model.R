################
#Logisitic Model
################
library(lubridate)
library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
library("dplyr")
library("Matrix")
library(fastDummies)
library(leaps)
library(tidyverse)
library(ROCR)
library(glmnet)

############
#Data Import

log_model_data = read.csv("log_model_data.csv", header = TRUE)
#Variables: "LogGrossApproval", "TermInMonths", "LogGDP", "LogSP500", "LogFedFunds", "LogCPI", "URinProjectState", "URinBorrState", "BusinessType", "NAICS_Sector", "DeliveryMethod", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "BorrState", "ProjectState", "ThirdPartyDollars", "GrossApproval", "GrossChargeOffAmount"

train_size = round((nrow(log_model_data)/10)*7, 0)
validation_size = round((nrow(log_model_data)/10), 0)
test_size = nrow(log_model_data) - train_size - validation_size

raw_train_data = log_model_data[0:train_size,]
raw_validation_data = log_model_data[(train_size+1):(train_size+validation_size),]
raw_test_data = log_model_data[(train_size+validation_size+1):nrow(log_model_data),]

train_data = subset(raw_train_data, select = -c(GrossApproval, GrossChargeOffAmount))
validation_data = subset(raw_validation_data, select = -c(GrossApproval, GrossChargeOffAmount))
test_data = subset(raw_test_data, select = -c(GrossApproval, GrossChargeOffAmount))

######################
#Basic Logistic Model

#Training
log_model = glm(data=train_data, Default ~., family= binomial)
summary(log_model) 

#Training ROC AUC
log_model_train_prediction = predict(log_model, train_data, type="response")
log_model_train_prediction = prediction(log_model_train_prediction, train_data$Default)
auc = unlist(slot(performance(log_model_train_prediction, 'auc'), 'y.values'))
auc
#0.7547161

#Test ROC AUC
log_model_test_prediction = predict(log_model, newdata = test_data, type="response")
log_model_test_prediction = prediction(log_model_test_prediction, test_data$Default)
auc = unlist(slot(performance(log_model_test_prediction, 'auc'), 'y.values'))
auc
#0.59259

#Plotting ROCs
roc_train = performance(log_model_train_prediction,"tpr","fpr")
roc_test = performance(log_model_test_prediction,"tpr","fpr")
plot(roc_train, col = 'red', main = 'Basic Logistic Model Training ROC (red) vs. Testing ROC (blue)')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 

################################
#Ridge and Lasso Logisitic Model

#Training
x_train = model.matrix(Default ~., train_data)[, -1]
x_train = x_train[,order(colnames(x_train))]
y_train = train_data$Default

model_L1 = glmnet(x_train, y_train, alpha = 1, nlambda = 10, family="binomial")
model_L2 = glmnet(x_train, y_train, alpha = 0, nlambda = 10, family="binomial")

x_validation = model.matrix(Default ~., validation_data)[, -1]
x_validation = x_validation[,order(colnames(x_validation))]

prediction_L1_train = predict(model_L1, newx = x_train, type = "response")
prediction_L2_train = predict(model_L2, newx = x_train, type = "response")
prediction_L1_validation = predict(model_L1, newx = x_validation, type = "response")
prediction_L2_validation = predict(model_L2, newx = x_validation, type = "response")

#Validation: Tuning Lambda Hyperparameter
AUC_L1_train = vector()
AUC_L2_train = vector()
AUC_L1_validation = vector()
AUC_L2_validation = vector()
for(i in 1:10){
  AUC_L1_train = append(AUC_L1_train, unlist(slot(performance(prediction(prediction_L1_train[,i], train_data$Default), 'auc'), 'y.values')))
  AUC_L2_train = append(AUC_L2_train, unlist(slot(performance(prediction(prediction_L2_train[,i], train_data$Default), 'auc'), 'y.values')))
  AUC_L1_validation = append(AUC_L1_validation, unlist(slot(performance(prediction(prediction_L1_validation[,i], validation_data$Default), 'auc'), 'y.values')))
  AUC_L2_validation = append(AUC_L2_validation,  unlist(slot(performance(prediction(prediction_L2_validation[,i], validation_data$Default), 'auc'), 'y.values')))
}

#Best L1 Hyperparameter
best_L1_lambda_index = which.max(AUC_L1_validation)
best_L1_lambda = model_L1$lambda[best_L1_lambda_index]
best_L1_AUC = max(AUC_L1_validation)
best_L1_AUC
#0.6694895
best_model_L1_coef = as.matrix(coef(model_L1, s= model_L1$lambda[best_L1_lambda_index]))

#Best L2 Hyperparameter
best_L2_lambda_index = which.max(AUC_L2_validation)
best_L2_lambda = model_L2$lambda[best_L2_lambda_index]
best_L2_AUC = max(AUC_L2_validation)
best_L2_AUC
#0.6618006
best_model_L2_coef = as.matrix(coef(model_L2, s= model_L2$lambda[best_L2_lambda_index]))

#Plotting best L1 & l2 coefficients
best_L1_model_coeff_plot = qplot(y= best_model_L1_coef[,1])
best_L1_model_coeff_plot + labs(title = "L1 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
best_L2_model_coeff_plot = qplot(y= best_model_L2_coef[,1])
best_L2_model_coeff_plot + labs(title = "L2 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")

#Testing: best L1 model
x_test = model.matrix(Default ~., test_data)[, -1]
x_test = x_test[,order(colnames(x_test))]

prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda,  type = "response")
prediction_test = prediction(prediction_test, test_data$Default)
auc = unlist(slot(performance(prediction_test, 'auc'), 'y.values'))
auc
#0.5738905

#Plotting ROCs
roc_train = performance(prediction(prediction_L1_train[,best_L1_lambda_index], train_data$Default),"tpr","fpr")
roc_validation = performance(prediction(prediction_L1_validation[,best_L1_lambda_index], validation_data$Default),"tpr","fpr")
roc_test = performance(prediction_test,"tpr","fpr")
plot(roc_train, col = 'red', main = 'L1 Logistic Model Training ROC (red) vs. Validation ROC (green) vs. Testing ROC (blue)')
plot(roc_validation, add = TRUE, col = 'green')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 

################
#Loss at Default

test_default = subset(raw_test_data, raw_test_data$GrossChargeOffAmount != 0)
test_default$LossPercent = test_default$GrossChargeOffAmount/test_default$GrossApproval
lossPropDist = test_default$LossPercent[test_default$LossPercent <= 1]

numBins = round((max(lossPropDist) - min(lossPropDist))/(2*IQR(lossPropDist)/(length(lossPropDist)^(1/3))))
hist(lossPropDist, breaks =  numBins, main = "Histogram of Loan Loss Proportion at Default", xlab="Proportion of Loan Loss at Default", ylab="Test Count")

lossDist = test_default$GrossChargeOffAmount
numBins = round((max(lossDist) - min(lossDist))/(2*IQR(lossDist)/(length(lossDist)^(1/3))))
hist(lossDist, breaks =  numBins, main = "Histogram of Loan Loss at Default", xlab="Proportion of Loan Loss at Default", ylab="Test Count")


prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda,  type = "response")
mean_loss = mean(test_default$GrossChargeOffAmount)

N = 1000
lossDist = vector()

for (i in 1:N){
  U = runif(1)
  lossDist = append(lossDist, sum(prediction_test[prediction_test>U]*mean_loss))
}
numBins = round((max(lossDist) - min(lossDist))/(2*IQR(lossDist)/(length(lossDist)^(1/3))))
hist(lossDist, breaks =  numBins, main = "Histogram of Loan Loss at Default", xlab="Proportion of Loan Loss at Default", ylab="Test Count")


