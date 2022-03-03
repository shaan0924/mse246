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

one_yr_period_one_log_model_data = read.csv("...", header = TRUE)

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
multi_period_log_models = list()
multi_period_train_predictions = list()
multi_period_train_auc = list()
multi_period_test_predictions = list()
multi_period_test_auc = list()

for (i in min(period):max(period)){
  data = train_data[train_data$period == i]
  log_model = glm(data=data, Default ~., family= binomial)
  multi_period_log_models = append(multi_period_log_models, log_model)
  
  #Training ROC AUC
  log_model_train_prediction = predict(log_model, train_data, type="response")
  multi_period_train_predictions = append(multi_period_train_predictions, log_model_train_prediction)
  log_model_train_prediction = prediction(log_model_train_prediction, train_data$Default)
  auc = unlist(slot(performance(log_model_train_prediction, 'auc'), 'y.values'))
  multi_period_train_auc = append(multi_period_train_auc, auc)
  
  #Test ROC AUC
  log_model_test_prediction = predict(log_model, newdata = test_data, type="response")
  multi_period_test_predictions = append(multi_period_test_predictions, log_model_test_prediction)
  log_model_test_prediction = prediction(log_model_test_prediction, test_data$Default)
  auc = unlist(slot(performance(log_model_test_prediction, 'auc'), 'y.values'))
  multi_period_test_auc = append(multi_period_test_auc, auc)
}

#####################
#Distribution of Loss

S =500
test_sample = raw_test_data[sample(nrow(raw_test_data), S),]
prediction_test_sample = predict(model_L1, test_sample,  type = "response")

train_default = subset(raw_train_data, raw_train_data$GrossChargeOffAmount != 0)
train_default$LossPercent = train_default$GrossChargeOffAmount/train_default$GrossApproval
lossPropDist = train_default$LossPercent[train_default$LossPercent <= 1]
lossPropDist_beta = fitdist(lossPropDist, "beta")

numBins = round((max(lossPropDist) - min(lossPropDist))/(2*IQR(lossPropDist)/(length(lossPropDist)^(1/3))))
hist(lossPropDist, breaks =  numBins, main = "Histogram of Loan Loss Proportion at Default", xlab="Proportion of Loan Loss at Default", ylab="Train Count") + plot(lossPropDist_beta)

N = 10000
lossDist = vector()
for (i in 1:N){
  U = runif(1, min = min(prediction_test_sample), max = max(prediction_test_sample))
  sample = prediction_test_sample[prediction_test_sample>U]
  lossPropDist_sample = rbeta(length(sample), lossPropDist_beta$estimate[1], lossPropDist_beta$estimate[2])
  lossDist = append(lossDist, -sum(lossPropDist_sample*test_sample$GrossApproval[prediction_test_sample>U]))
}

lossDist_norm = fitdist(lossDist, "norm")
numBins = round((max(lossDist) - min(lossDist))/(2*IQR(lossDist)/(length(lossDist)^(1/3))))
hist(lossDist, breaks =  numBins, main = "Histogram of Inverse Sampling Loan Loss with Sample Loss Proportion at Default", xlab="Portfolio Loss", ylab="Train Count") + plot(lossDist_norm)

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

dev.new(20, 8)
hist(lossDist, breaks =  numBins, main = "Histogram of Inverse Sampling Loan Loss with Sample Loss Proportion at Default with Non Parametric VaR", xlab="Portfolio Loss", ylab="Train Count") 
abline(v = -VaR_95, col="blue", lwd = 3) 
abline(v = -VaR_99, col="red", lwd = 3)
#abline(v = -Avg_VaR_95, col="blue", lty = 2, lwd = 3) 
#abline(v = -Avg_VaR_99, col="red", lty = 2, lwd = 3)
legend(x = "topright",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3) 

#Parametric LogNormal Losses VaR based on log model
VaR_95 = exp(qnorm(0.95, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_95
#3946421326
VaR_99 = exp(qnorm(0.99, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_99
#29369942288

dev.new(20, 8)
denscomp(ft = lossDist_log_norm, legendtext = "Normal",  main = "Histogram of Inverse Sampling Log Loan Loss with Sample Loss Proportion at Default", xlab="Portfolio Log Loss", ylab="Train Count", cex.main=1) 
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
#abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
#abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "topright", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3) 
