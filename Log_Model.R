################
#Logisitic Model
################
library(lubridate)
library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
library(fastDummies)
library(leaps)
library(tidyverse)
library(ROCR)
library(glmnet)
library(fitdistrplus)
library(QRM)

############
#Data Import
setwd("/Users/jackparkin/Desktop/MS&E 246/Project")
log_model_data = read.csv("log_model_data.csv", header = TRUE)

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
#0.7541353

#Variable selection
qplot(y= log_model$coefficients) + labs(title = "Basic Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
log_model_coefficients = as.data.frame(log_model$coefficients)

#Test ROC AUC
log_model_test_prediction = predict(log_model, newdata = test_data, type="response")
log_model_test_prediction = prediction(log_model_test_prediction, test_data$Default)
auc = unlist(slot(performance(log_model_test_prediction, 'auc'), 'y.values'))
auc
#0.5949106

#Plotting ROCs
roc_train = performance(log_model_train_prediction,"tpr","fpr")
roc_test = performance(log_model_test_prediction,"tpr","fpr")
plot(roc_train, col = 'red', main = 'Basic Logistic Model Training ROC vs. Testing ROC')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 
legend(x = "bottomright", legend = c("Training ROC", "Testing ROC"), lty = c(1, 1), col = c("red", "blue"), lwd = 1) 

################################
#Ridge and Lasso Logisitic Model

#Training
x_train = model.matrix(Default ~., train_data)[, -1]
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
L1_summary = as.data.frame(model_L1$lambda)
L1_summary$AUC = AUC_L1_validation
#0.6679918

#Best L2 Hyperparameter
best_L2_lambda_index = which.max(AUC_L2_validation)
best_L2_lambda = model_L2$lambda[best_L2_lambda_index]
best_L2_AUC = max(AUC_L2_validation)
L2_summary = as.data.frame(model_L2$lambda)
L2_summary$AUC = AUC_L2_validation
#0.6633677

#Testing: best L1  model
x_test = model.matrix(Default ~., test_data)[, -1]
prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda, type = "response")
prediction_test = prediction(prediction_test, test_data$Default)
auc = unlist(slot(performance(prediction_test, 'auc'), 'y.values'))
auc
#0.5751568

#Plotting L1 coefficients
L1_coef = as.matrix(coef(model_L1, s=model_L1$lambda[best_L1_lambda_index]))
rownames(L1_coef) = rownames(coef(model_L1))
qplot(y= L1_coef[,1]) + labs(title = "Best L1 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
L1_coef = as.data.frame(L1_coef)

#Plotting ROCs
roc_train = performance(prediction(prediction_L1_train[,best_L1_lambda_index], train_data$Default),"tpr","fpr")
roc_validation = performance(prediction(prediction_L1_validation[,best_L1_lambda_index], validation_data$Default),"tpr","fpr")
roc_test = performance(prediction_test,"tpr","fpr")
plot(roc_train, col = 'red', main = 'Best L1 Logistic Model Training ROC vs. Validation ROC vs. Testing ROC')
plot(roc_validation, add = TRUE, col = 'green')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 
legend(x = "bottomright", legend = c("Training ROC", "Validation ROC", "Testing ROC"), lty = c(1, 1, 1), col = c("red", "green", "blue"), lwd = 1) 

################
#Loss at Default

x_train = model.matrix(Default ~., train_data)[, -1]
prediction_train = predict(model_L1, newx = x_train, s = best_L1_lambda, type = "response")

train_default = subset(raw_train_data, raw_train_data$GrossChargeOffAmount != 0)
train_default$LossPercent = train_default$GrossChargeOffAmount/train_default$GrossApproval
lossPropDist = train_default$LossPercent[train_default$LossPercent <= 1]
lossPropDist_beta = fitdist(lossPropDist, "beta")

numBins = round((max(lossPropDist) - min(lossPropDist))/(2*IQR(lossPropDist)/(length(lossPropDist)^(1/3))))
hist(lossPropDist, breaks =  numBins, main = "Histogram of Loan Loss Proportion at Default", xlab="Proportion of Loan Loss at Default", ylab="Train Count") + plot(lossPropDist_beta)

N = 10000
lossDist = vector()
for (i in 1:N){
  U = runif(1, min = min(prediction_train), max = max(prediction_train))
  sample = prediction_train[prediction_train>U]
  lossPropDist_sample = rbeta(length(sample), lossPropDist_beta$estimate[1], lossPropDist_beta$estimate[2])
  lossDist = append(lossDist, -sum(lossPropDist_sample*raw_train_data$GrossApproval[prediction_train>U]))
}

lossDist_norm = fitdist(lossDist, "norm")
numBins = round((max(lossDist) - min(lossDist))/(2*IQR(lossDist)/(length(lossDist)^(1/3))))
hist(lossDist, breaks =  numBins, main = "Histogram of Inverse Sampling Loan Loss with Sample Loss Proportion at Default with Fitted Normal Distribution", xlab="Portfolio Loss", ylab="Train Count") + plot(lossDist_norm)

lossDist_log = log(-lossDist)
lossDist_log_norm = fitdist(lossDist_log, "norm")
numBins = round((max(lossDist_log) - min(lossDist_log))/(2*IQR(lossDist_log)/(length(lossDist_log)^(1/3))))
hist(lossDist_log, breaks =  numBins, main = "Histogram of Inverse Sampling Log Loan Loss with Sample Loss Proportion at Default", xlab="Portfolio Loss", ylab="Train Count") + plot(lossDist_log_norm)


###############
#Non Parametric VaR based on log model
VaR_95 = -quantile(lossDist, prob = c(0.05))
VaR_95
#3900226795 
VaR_99 = -quantile(lossDist, prob = c(0.01))
VaR_99
#9107167225
Avg_VaR_95 = -sum(lossDist[lossDist < -VaR_95])/ length(lossDist[lossDist < -VaR_95])
Avg_VaR_95
#7429254131
Avg_VaR_99 = -sum(lossDist[lossDist <= -VaR_99])/ length(lossDist[lossDist <= -VaR_99])
Avg_VaR_99
#9178419284

hist(lossDist, breaks =  numBins, main = "Histogram of Inverse Sampling of Loan Loss with Sample Loss Proportion at Default with Non Parametric VaR", xlab="Portfolio Loss", ylab="Train Count", cex.main=1) 
abline(v = -VaR_95, col="blue", lwd = 3) 
abline(v = -VaR_99, col="red", lwd = 3)
abline(v = -Avg_VaR_95, col="blue", lty = 2, lwd = 3) 
abline(v = -Avg_VaR_99, col="red", lty = 2, lwd = 3)
legend(x = "bottomleft",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

#Parametric LogNormal Losses VaR based on log model
VaR_95 = exp(qnorm(0.95, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_95
#3946421326
VaR_99 = exp(qnorm(0.99, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_99
#4801390385

x = seq(0.95, 0.999, 0.001)
Avg_VaR_95 = sum(exp(qnorm(x, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
Avg_VaR_95
#31164969343

x = seq(0.99, 0.999, 0.001)
Avg_VaR_99 = sum(exp(qnorm(x, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
Avg_VaR_99
#107438865727

denscomp(ft = lossDist_log_norm, legendtext = "Normal",  main = "Histogram of Inverse Sampling Log Loan Loss with Sample Loss Proportion at Default", xlab="Portfolio Log Loss", ylab="Train Count", xlim = c(10,28))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3) 



