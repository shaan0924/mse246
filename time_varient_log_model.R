library(lubridate)
library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
library("dplyr")
library("Matrix")
library(fastDummies)
library(ROCR)
library(glmnet)
library(fitdistrplus)
library("ROSE")

setwd("/Users/jackparkin/Desktop/MS&E 246/Project")
setwd("/Users/sihguat/Desktop/MSE_246/mse246")

###################
#Imports
raw_data = read.csv("SBA Loan data .csv", header = TRUE, na.strings=c("","NA"))
GDP = read.csv("GDP.csv")
SP500 = getSymbols("^GSPC",from = "1990-01-01",to = "2014-12-31", periodicity = 'monthly', auto.assign = FALSE)
FEDFUNDS = read.csv("FEDFUNDS.csv")
CPI = read.csv("CPIAUCSL.csv")
UnemploymentUSbyState = read.csv('StateUR.csv', header = TRUE)

###################
#Preprocessing

#Removing Cancelled and Exempt
raw_data = raw_data[!(raw_data$LoanStatus=="CANCLD" | raw_data$LoanStatus=="EXEMPT"),]
#Adding combined loan size
raw_data$ThirdPartyDollars[is.na(raw_data$ThirdPartyDollars)] = 0
raw_data$Size = raw_data$GrossApproval + raw_data$ThirdPartyDollars
#Removing unidentified  states
unidentified.states = unique(raw_data$ProjectState[!(raw_data$ProjectState %in% unique(UnemploymentUSbyState$State))])
raw_data = raw_data[!(raw_data$ProjectState %in% unidentified.states),]
#Adding BinaryIntergerTerm
raw_data$BinaryIntergerTerm[raw_data$TermInMonths %% 12 == 0] = 1
raw_data$BinaryIntergerTerm[raw_data$TermInMonths %% 12 != 0] = 0
#Adding BinaryRepeatBorrower
temp = duplicated(raw_data$BorrName)
raw_data$BinaryRepeatBorrower[temp == TRUE] = 1
raw_data$BinaryRepeatBorrower[temp == FALSE] = 0
#Adding BinaryBankStEqualBorrowerSt
raw_data$BinaryBankStEqualBorrowerSt[as.character(raw_data$BorrState) == as.character(raw_data$CDC_State)] = 1
raw_data$BinaryBankStEqualBorrowerSt[as.character(raw_data$BorrState)!= as.character(raw_data$CDC_State)] = 0
#Adding BinaryProjectStEqualBorrowerSt
raw_data$BinaryProjectStEqualBorrowerSt[as.character(raw_data$BorrState) == as.character(raw_data$ProjectState)] = 1
raw_data$BinaryProjectStEqualBorrowerSt[as.character(raw_data$BorrState) != as.character(raw_data$ProjectState)] = 0
#Transforming Dates
raw_data$ApprovalDate = as.Date(raw_data$ApprovalDate, "%m/%d/%y")
raw_data$ChargeOffDate = as.Date(raw_data$ChargeOffDate, "%m/%d/%y")
raw_data$ApprovalFiscalYear = as.integer(raw_data$ApprovalFiscalYear)
#Transforming NAICS
raw_data$NaicsCode = substring(raw_data$NaicsCode, 0 ,2)
#Adding Index
raw_data$Index = seq(1, length(raw_data[,1]))

#Adding Termius
raw_data <- raw_data %>% 
  mutate(term_years = TermInMonths %/% 12, terminus = 
      case_when(
        !is.na(ChargeOffDate) ~ as.integer(substring(ChargeOffDate, 0, 4)), # Loan is charged off
        ApprovalFiscalYear + term_years <= 2013 ~ as.integer(ApprovalFiscalYear + term_years),  # Loan is paid in full prior to end of dataset
        ApprovalFiscalYear + term_years > 2013 ~ as.integer(2013),
        TRUE ~ NA_integer_
      )
  ) 
#Spilting data by year
modified_data = as.data.frame(matrix(ncol = ncol(raw_data)+1, nrow = 0))

for (i in 1:length(raw_data[,1])){
  len = raw_data[i,]$terminus - raw_data[i,]$ApprovalFiscalYear  + 1
  temp = raw_data[rep(i, len), ]
  temp$Date = seq(raw_data[i,]$ApprovalDate, by = "years", length.out = len)
  modified_data = rbind(modified_data, temp)
}

#Adding correct default
modified_data$Default = 0
modified_data$Default[format(modified_data$ChargeOffDate, format="%Y") == format(modified_data$Date, format="%Y")] = 1

#Adding loan age
modified_data$Age = as.integer(format(modified_data$Date, format="%Y")) - as.integer(format(modified_data$ApprovalDate, format="%Y"))

#UnemploymentInProjectState
modified_data = transform(modified_data, month.bin = cut(Date, breaks = "month"))
temp_project_UR = rename(UnemploymentUSbyState, month.bin = DATE, ProjectState = State, URinProjectState = UR)
modified_data = merge(x=modified_data,y=temp_project_UR,by=c("ProjectState","month.bin"))

#UnemploymentInBorrowerState
temp_borrower_UR = rename(UnemploymentUSbyState, month.bin = DATE, BorrState = State, URinBorrState = UR)
modified_data = merge(x=modified_data,y=temp_borrower_UR,by=c("BorrState","month.bin"))

#SP500
tempSP500 = as.data.frame(SP500)
tempSP500$month.bin = rownames(tempSP500)
tempSP500 = transmute(tempSP500,  month.bin = rownames(tempSP500), GSPC.price= GSPC.Adjusted)
modified_data = merge(x=modified_data,y=tempSP500,by="month.bin")

#Fed Funds
tempFedFunds = transmute(FEDFUNDS, month.bin = DATE, FedFunds = FEDFUNDS)
modified_data = merge(x=modified_data,y=tempFedFunds,by="month.bin")

#CPI
tempCPI = transmute(CPI, month.bin = DATE, CPI = CPIAUCSL)
modified_data = merge(x=modified_data,y=tempCPI,by="month.bin")

#GDP
tempGDP = transmute(GDP, month.bin = DATE, GDP = GDP)
tempGDP$quarter.bin = paste("Q4", substring(tempGDP$month.bin, 0, 4))
tempGDP$quarter.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 10] = paste("Q3", substring(tempGDP$month.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 10], 0, 4))
tempGDP$quarter.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 7] = paste("Q2", substring(tempGDP$month.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 7], 0, 4))
tempGDP$quarter.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 4] = paste("Q1", substring(tempGDP$month.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 4], 0, 4))
tempGDP = subset(tempGDP, select = -month.bin)
modified_data$quarter.bin = paste("Q4", substring(modified_data$Date, 0, 4))
modified_data$quarter.bin[as.numeric(substring(modified_data$Date, 6, 7)) < 10] = paste("Q3", substring(modified_data$Date[as.numeric(substring(modified_data$Date, 6, 7)) < 10], 0, 4))
modified_data$quarter.bin[as.numeric(substring(modified_data$Date, 6, 7)) < 7] = paste("Q2", substring(modified_data$Date[as.numeric(substring(modified_data$Date, 6, 7)) < 7], 0, 4))
modified_data$quarter.bin[as.numeric(substring(modified_data$Date, 6, 7)) < 4] = paste("Q1", substring(modified_data$Date[as.numeric(substring(modified_data$Date, 6, 7)) < 4], 0, 4))
modified_data = merge(x=modified_data,y=tempGDP,by="quarter.bin")
modified_data = subset(modified_data, select = -quarter.bin)
modified_data = subset(modified_data, select = -month.bin)


#Logging
modified_data$LogSize = log(modified_data$Size)
modified_data$LogGDP = log(modified_data$GDP)
modified_data$LogSP500 = log(modified_data$GSPC.price)
modified_data$LogCPI = log(modified_data$CPI)
#Creating model data
colnames(modified_data)
model_data = modified_data[c("LogSize", "LogGDP", "LogSP500", "LogCPI", "FedFunds", "URinProjectState", "URinBorrState", "TermInMonths", "Age", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "BusinessType", "NaicsCode", "Date", "ApprovalDate", "Default", "Size", "GrossChargeOffAmount", "Index")]


#Continuous missing values
continuous_cols = c("LogSize", "LogGDP", "LogSP500", "LogCPI", "FedFunds", "URinProjectState", "URinBorrState", "TermInMonths", "Age", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt")
temp = model_data[continuous_cols]
temp[is.na(temp)] = 0
model_data[continuous_cols] = temp
scale_cols =  c("LogSize", "LogGDP", "LogSP500", "LogCPI", "FedFunds", "URinProjectState", "URinBorrState", "TermInMonths", "Age")
model_data[scale_cols] = as.data.frame(scale(model_data[scale_cols]))

#Discrete missing values
disc_cols = c("BusinessType", "NaicsCode")
temp = model_data[disc_cols]
temp[is.na(temp)] = "Blank"
model_data[disc_cols] = temp

#Adding categorical columns
categorical_cols = disc_cols[apply(temp,2,function(x) { !all(x %in% 0:1) })]
model_data = dummy_cols(model_data, select_columns = categorical_cols, remove_selected_columns = TRUE)
names(model_data) = make.names(names(model_data), unique=TRUE)


######################
#Log Model
model_data = model_data %>% arrange(ApprovalDate)

train_size = round((nrow(model_data)/10)*7, 0)
validation_size = round((nrow(model_data)/10), 0)
test_size = nrow(model_data) - train_size - validation_size

raw_train_data = model_data[0:train_size,]
raw_validation_data = model_data[(train_size+1):(train_size+validation_size),]
raw_test_data = model_data[(train_size+validation_size+1):nrow(model_data),]

train_data = subset(raw_train_data, select = -c(Date, Size, GrossChargeOffAmount))
validation_data = subset(raw_validation_data, select = -c(Date, Size, GrossChargeOffAmount))
test_data = subset(raw_test_data, select = -c(Date, Size, GrossChargeOffAmount))
sum(train_data$Default)
sum(validation_data$Default)
sum(test_data$Default)
######################
#Basic Logistic Model

#Training
log_model = glm(data=train_data, Default ~., family= binomial)
summary(log_model) 

#Training ROC AUC
log_model_train_prediction = predict(log_model, train_data, type="response")
log_model_train_prediction = ROCR::prediction(log_model_train_prediction, train_data$Default)
auc = unlist(slot(performance(log_model_train_prediction, 'auc'), 'y.values'))
auc
#0.7559549
#Variable selection
qplot(y= log_model$coefficients) + labs(title = "Basic Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
log_model_coefficients = as.data.frame(log_model$coefficients)


#Test ROC AUC
log_model_test_prediction = predict(log_model, newdata = test_data, type="response")
log_model_test_prediction = ROCR::prediction(log_model_test_prediction, test_data$Default)
auc = unlist(slot(performance(log_model_test_prediction, 'auc'), 'y.values'))
auc
#0.8036432

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
  AUC_L1_train = append(AUC_L1_train, unlist(slot(performance(ROCR::prediction(prediction_L1_train[,i], train_data$Default), 'auc'), 'y.values')))
  AUC_L2_train = append(AUC_L2_train, unlist(slot(performance(ROCR::prediction(prediction_L2_train[,i], train_data$Default), 'auc'), 'y.values')))
  AUC_L1_validation = append(AUC_L1_validation, unlist(slot(performance(ROCR::prediction(prediction_L1_validation[,i], validation_data$Default), 'auc'), 'y.values')))
  AUC_L2_validation = append(AUC_L2_validation,  unlist(slot(performance(ROCR::prediction(prediction_L2_validation[,i], validation_data$Default), 'auc'), 'y.values')))
}


#Best L1 Hyperparameter
best_L1_lambda_index = which.max(AUC_L1_validation)
best_L1_lambda = model_L1$lambda[best_L1_lambda_index]
best_L1_AUC = max(AUC_L1_validation)
L1_summary = as.data.frame(model_L1$lambda)
L1_summary$AUC = AUC_L1_validation
max(L1_summary$AUC)
# 0.7400222

#Best L2 Hyperparameter
best_L2_lambda_index = which.max(AUC_L2_validation)
best_L2_lambda = model_L2$lambda[best_L2_lambda_index]
best_L2_AUC = max(AUC_L2_validation)
L2_summary = as.data.frame(model_L2$lambda)
L2_summary$AUC = AUC_L2_validation
max(L2_summary$AUC)
#0.7261138

#Testing: best L1  model
x_test = model.matrix(Default ~., test_data)[, -1]
#test_data = test_data[!(rownames(test_data) %in% setdiff(rownames(test_data), rownames(x_test))),]
prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda, type = "response")
prediction_test = ROCR::prediction(prediction_test, test_data$Default)
auc = unlist(slot(performance(prediction_test, 'auc'), 'y.values'))
auc
#0.5934789

#Plotting L1 coefficients
L1_coef = as.matrix(coef(model_L1, s=model_L1$lambda[best_L1_lambda_index]))
rownames(L1_coef) = rownames(coef(model_L1))
qplot(y= L1_coef[,1]) + labs(title = "Best L1 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
L1_coef = as.data.frame(L1_coef)
#Plotting ROCs
roc_train = performance(ROCR::prediction(prediction_L1_train[,best_L1_lambda_index], train_data$Default),"tpr","fpr")

roc_validation = performance(ROCR::prediction(prediction_L1_validation[,best_L1_lambda_index], validation_data$Default),"tpr","fpr")

roc_test = performance(prediction_test,"tpr","fpr")
plot(roc_train, col = 'red', main = 'Best L1 Logistic Model Training ROC vs. Validation ROC vs. Testing ROC')
plot(roc_validation, add = TRUE, col = 'green')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 
legend(x = "bottomright", legend = c("Training ROC", "Validation ROC", "Testing ROC"), lty = c(1, 1, 1), col = c("red", "green", "blue"), lwd = 1) 

