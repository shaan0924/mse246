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

continuous_cols = c("LogGrossApproval", "LogThirdPartyDollars", "TermInMonths", "LogGDP", "LogSP500", "LogFedFunds", "LogCPI", "URinProjectState", "URinBorrState")
continuous_means = colMeans(log_model_data[continuous_cols])
continuous_sd = colSums((log_model_data[continuous_cols] - continuous_means)^2)/(length(log_model_data[,1]))
log_model_data[continuous_cols] = (log_model_data[continuous_cols] - continuous_means)/continuous_sd

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

model_L1 = glmnet(x_train, y_train, alpha = 1, nlambda = 50, family="binomial")
model_L2 = glmnet(x_train, y_train, alpha = 0, nlambda = 50, family="binomial")

x_validation = model.matrix(Default ~., validation_data)[, -1]

prediction_L1_train = predict(model_L1, newx = x_train, type = "response")
prediction_L2_train = predict(model_L2, newx = x_train, type = "response")
prediction_L1_validation = predict(model_L1, newx = x_validation, type = "response")
prediction_L2_validation = predict(model_L2, newx = x_validation, type = "response")

#Validation: Tuning Lambda Hyperparameter
AUC_L1_train = vector()
AUC_L2_train = vector()
AUC_L1_validation = vector()
AUC_L2_validation = vector()
for(i in 1:50){
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
max(L1_summary$AUC)
# 0.6585998


#Best L2 Hyperparameter
best_L2_lambda_index = which.max(AUC_L2_validation)
best_L2_lambda = model_L2$lambda[best_L2_lambda_index]
best_L2_AUC = max(AUC_L2_validation)
L2_summary = as.data.frame(model_L2$lambda)
L2_summary$AUC = AUC_L2_validation
max(L2_summary$AUC)
#0.6582028

#Testing: best L1  model
x_test = model.matrix(Default ~., test_data)[, -1]
test_data = test_data[!(rownames(test_data) %in% setdiff(rownames(test_data), rownames(x_test))),]
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
  slossPropDist_sample = rbeta(length(sample), lossPropDist_beta$estimate[1], lossPropDist_beta$estimate[2])
  lossDist = append(lossDist, -sum(lossPropDist_sample*raw_train_data$GrossApproval[prediction_train>U]))
}

lossDist_norm = fitdist(lossDist, "norm")
numBins = round((max(lossDist) - min(lossDist))/(2*IQR(lossDist)/(length(lossDist)^(1/3))))
hist(lossDist, breaks =  numBins, main = "Inverse Sampled Portfolio Loss given Sampled Loan Loss Proportion", xlab="Portfolio Loss", ylab="Simulation Count") + plot(lossDist_norm)

lossDist_log = log(-lossDist)
lossDist_log_norm = fitdist(lossDist_log, "norm")
numBins = round((max(lossDist_log) - min(lossDist_log))/(2*IQR(lossDist_log)/(length(lossDist_log)^(1/3))))

hist(lossDist_log, breaks =  numBins, main = "Inverse Sampled Portfolio Log Loss given Sampled Loan Loss Proportion", xlab="Portfolio Log Loss", ylab="Simulation Count") + plot(lossDist_log_norm)


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

hist(lossDist, breaks =  numBins, main = "Inverse Sampled Portfolio Log Loss & Non-parametric VaR given Sampled Loan Loss Proportion", xlab="Portfolio Loss", ylab="Simulation Count", cex.main=1) 
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

denscomp(ft = lossDist_log_norm, legendtext = "Normal",  main = "Inverse Sampled Portfolio Log Loss & Parametric VaR given Sampled Loan Loss Proportion", xlab="Portfolio Log Loss", ylab="Simulation Count", xlim = c(10,28))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3) 

############################################################
#Multi Period 
############################################################

#Data Import
setwd("/Users/jackparkin/Desktop/MS&E 246/Project")
raw_data <- read.csv("input_pre_processing_VAR.csv")
periods <- 25

raw_data <- 
  raw_data %>% 
  mutate(
    term_years = TermInMonths %/% 12,
    terminus = 
      case_when(
        !is.na(ChargeOffDate) ~ as.integer(substring(ChargeOffDate, 0, 4)), # Loan is charged off
        ApprovalFiscalYear + term_years <= 2013 ~ as.integer(ApprovalFiscalYear + term_years),  # Loan is paid in full prior to end of dataset
        ApprovalFiscalYear + term_years > 2013 ~ as.integer(2013),
        TRUE ~ NA_integer_
      ),
    index = seq(1,length(raw_data[,1]))
  ) 


 raw_data_list <- list()
for (i in 1:periods) { 
  raw_data_list[[i]] = raw_data
}

for (i in 1:periods) {
  if (periods == 5) {
    active_years = seq(1990 + 5*(i - 1), 1990 + 5*i, 1)
    raw_data_list[[i]] <- 
      raw_data_list[[i]] %>% 
      filter(
        ApprovalFiscalYear %in% active_years | ApprovalFiscalYear < active_years[1],
        terminus %in% active_years | terminus > active_years[6]
      )
  } else {
    curr_year = 1990 + i - 1
    raw_data_list[[i]] <- 
      raw_data_list[[i]] %>% 
      filter(
        ApprovalFiscalYear <= curr_year,
        curr_year <= terminus
      )
  }
}

#GDP Import
GDP = read.csv("GDP.csv")
#SP500 Import
SP500 = getSymbols("^GSPC",from = "1990-01-01",to = "2014-12-31", periodicity = 'monthly', auto.assign = FALSE)
#FedFunds Import
FEDFUNDS = read.csv("FEDFUNDS.csv")
#CPI Import (Consumer Price Index for All Urban Consumers: All Items in U.S. City Average)
CPI = read.csv("CPIAUCSL.csv")
#Unemployment
UnemploymentUSbyState = read.csv('StateUR.csv', header = TRUE)

GDP <- 
  GDP %>%  
  transmute(month.bin = DATE, GDP = GDP) %>%  
  transmute(
    GDP,
    quarter.bin =
      case_when(
        as.numeric(substring(month.bin, 6, 7)) < 10 & as.numeric(substring(month.bin, 6, 7)) >= 7  ~ paste("Q3", substring(month.bin, 0, 4)),
        as.numeric(substring(month.bin, 6, 7)) < 7 & as.numeric(substring(month.bin, 6, 7)) >= 4   ~ paste("Q2", substring(month.bin, 0, 4)),
        as.numeric(substring(month.bin, 6, 7)) < 4                                                 ~ paste("Q1", substring(month.bin, 0, 4)),
        TRUE                                                                                       ~ paste("Q4", substring(month.bin, 0, 4)),
      )
  )

SP500 <- 
  SP500 %>% 
  as.data.frame() %>% 
  transmute(month.bin = rownames(.), GSPC.price = GSPC.Adjusted)

for (i in 1:periods) {
  repeat_borr = duplicated(raw_data_list[[i]]$BorrName)
  
  raw_data_list[[i]] <- 
    raw_data_list[[i]] %>%  
    mutate(
      quarter.bin =
        case_when(
          as.numeric(substring(ApprovalDate, 6, 7)) < 10 & as.numeric(substring(ApprovalDate, 6, 7)) >= 7  ~ paste("Q3", substring(ApprovalDate, 0, 4)),
          as.numeric(substring(ApprovalDate, 6, 7)) < 7 & as.numeric(substring(ApprovalDate, 6, 7)) >= 4   ~ paste("Q2", substring(ApprovalDate, 0, 4)),
          as.numeric(substring(ApprovalDate, 6, 7)) < 4                                                    ~ paste("Q1", substring(ApprovalDate, 0, 4)),
          TRUE                                                                                             ~ paste("Q4", substring(ApprovalDate, 0, 4)),
        )
    ) %>%  
    left_join(y = GDP, by = "quarter.bin") %>%  
    subset(select = -quarter.bin) %>%     # Up to here, I've added GDP. Onto S&P 500
    left_join(y = SP500, by = "month.bin") %>%  
    left_join(y = FEDFUNDS, by = c("month.bin" = "DATE")) %>%  
    left_join(y = CPI, by = c("month.bin" = "DATE")) %>% 
    left_join(y = UnemploymentUSbyState, by = c("ProjectState" = "State", "month.bin" = "DATE")) %>%  
    rename(URinBorrState = UR) %>%  
    left_join(y = UnemploymentUSbyState, by = c("BorrState" = "State", "month.bin" = "DATE")) %>%  
    rename(URinProjectState = UR) %>% 
    mutate(
      Default = 
        case_when(
          GrossChargeOffAmount == 0 ~ 0L,
          TRUE ~ 1L
        ),
      LogGrossApproval = log(GrossApproval),
      LogGDP = log(GDP),
      LogSP500 = log(GSPC.price),
      LogFedFunds = log(FEDFUNDS),
      LogCPI = log(CPIAUCSL),
      LogThirdPartyDollars = log(ThirdPartyDollars)
    )    
}

for (i in 1:periods) {
  raw_data_list[[i]] <- 
    raw_data_list[[i]][append(colnames(log_model_data), "index")]
  
  raw_data_list[[i]][continuous_cols][is.na(raw_data_list[[i]][continuous_cols])] = 0
  other = c("BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt")
  raw_data_list[[i]][other][is.na(raw_data_list[[i]][other])] = 0
}  


#####################
#Distribution of Loss

nonParamerticVars = data.frame(matrix(nrow = 0, ncol = 4))
colnames(nonParamerticVars) = c("95% VaR", "99% VaR", "95% Avg VaR", "99% Avg VaR")
paramerticVars = data.frame(matrix(nrow = 0, ncol = 4))
colnames(paramerticVars) = c("95% VaR", "99% VaR", "95% Avg VaR", "99% Avg VaR")

S = 500
sample_indexs = sample(as.data.frame(raw_data_list[1])$index,S)
for (i in 1:(periods-1)) {
  sample = as.data.frame(raw_data_list[i])
  sample = sample[(sample$index %in% sample_indexs),]
  sample[continuous_cols] = (sample[continuous_cols] - continuous_means)/continuous_sd
  sample_GrossApproval = sample$GrossApproval
  sample = subset(sample, select = -c(GrossApproval,GrossChargeOffAmount, index))
  sample_test = model.matrix(Default ~., sample)[, -1]
  sample_prediction = predict(model_L1, newx = sample_test, s = best_L1_lambda, type = "response")

  N = 10000
  lossDist = vector()
  for (j in 1:N){
    U = runif(1, min = min(sample_prediction), max = max(sample_prediction))
    sample = sample_prediction[sample_prediction>U]
    lossPropDist_sample = rbeta(length(sample), lossPropDist_beta$estimate[1], lossPropDist_beta$estimate[2])
    lossDist = append(lossDist, -sum(lossPropDist_sample*sample_GrossApproval[sample_prediction>U]))
  }
  lossDists = append(lossDists, c(lossDist))
  
  #Non Parametric VaR based on log model
  VaR_95 = -quantile(lossDist, prob = c(0.05))
  VaR_99 = -quantile(lossDist, prob = c(0.01))
  Avg_VaR_95 = -sum(lossDist[lossDist < -VaR_95])/ length(lossDist[lossDist < -VaR_95])
  Avg_VaR_99 = -sum(lossDist[lossDist <= -VaR_99])/ length(lossDist[lossDist <= -VaR_99])
  nonParamerticVars[paste("Period", i),] = c(VaR_95, VaR_99, Avg_VaR_95, Avg_VaR_99)
  
  #Parametric LogNormal Losses VaR based on log model
  lossDist_log = log(-lossDist)
  lossDist_log_norm = fitdist(lossDist_log, "norm")
  VaR_95 = exp(qnorm(0.95, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE))
  VaR_99 = exp(qnorm(0.99, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE))
  x = seq(0.95, 0.999, 0.001)
  Avg_VaR_95 = sum(exp(qnorm(x, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
  x = seq(0.99, 0.999, 0.001)
  Avg_VaR_99 = sum(exp(qnorm(x, mean = lossDist_log_norm$estimate[1], sd = lossDist_log_norm$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
  paramerticVars[paste("Period", i),] = c(VaR_95, VaR_99, Avg_VaR_95, Avg_VaR_99)
  
}

#Non Parametric Plot
numBins = round((max(lossDist) - min(lossDist))/(2*IQR(lossDist)/(length(lossDist)^(1/3))))
hist(lossDist, breaks =  numBins, main = paste("Inverse Sampled Portfolio Loss & Non Parametric VaR given Sampled Loan Loss Proportion for Period", i), xlab="Portfolio Loss", ylab="Simulation Count", cex.main=0.7, xlim = c(-max(nonParamerticVars[length(nonParamerticVars$`95% VaR`),][1], nonParamerticVars[length(nonParamerticVars$`95% VaR`),][2], nonParamerticVars[length(nonParamerticVars$`95% VaR`),][3], nonParamerticVars[length(nonParamerticVars$`95% VaR`),][4]),0)) 
abline(v = -nonParamerticVars[length(nonParamerticVars$`95% VaR`),][1], col="blue", lwd = 3) 
abline(v = -nonParamerticVars[length(nonParamerticVars$`95% VaR`),][2], col="red", lwd = 3)
abline(v = -nonParamerticVars[length(nonParamerticVars$`95% VaR`),][3], col="blue", lty = 2, lwd = 3) 
abline(v = -nonParamerticVars[length(nonParamerticVars$`95% VaR`),][4], col="red", lty = 2, lwd = 3)
legend(x = "top",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

#Parametric Plots
denscomp(ft = lossDist_log_norm, legendtext = "Normal",  main = paste("Inverse Sampled Portfolio Log Loss & Parametric VaR given Sampled Loan Loss Proportion for Period", i), xlab="Portfolio Log Loss", ylab="Simulation Count", cex.main=0.7, xlim = c(0,max(log(VaR_95), log(VaR_99), log(Avg_VaR_95), log(Avg_VaR_99))))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

qqcomp(lossDist_log_norm, legendtext = "Normal",  main = paste("Log Loss Normal Distribution QQ Plot for Period", i), xlab="Portfolio Log Loss", ylab="Simulation Count")

nonParamerticVars$Period = seq(1, length(nonParamerticVars$`95% VaR`))
nonParamerticVars <- melt(nonParamerticVars ,  id.vars = 'Period', variable.name = 'VaRs')
ggplot(nonParamerticVars, aes(Period, value)) +
  geom_line(aes(colour = VaRs)) + labs(title = "Non Parametric VaRs", y = "Portfolio VaRs")

paramerticVars$Period = seq(1, length(paramerticVars$`95% VaR`))
paramerticVars <- melt(paramerticVars ,  id.vars = 'Period', variable.name = 'VaRs')
ggplot(paramerticVars, aes(Period, value)) +
  geom_line(aes(colour = VaRs)) + labs(title = "Parametric VaRs", y = "Portfolio VaRs")
