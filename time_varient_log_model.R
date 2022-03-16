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
library(stats)
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
raw_data$Len = raw_data$terminus - raw_data$ApprovalFiscalYear
modified_data = raw_data
modified_data$Date = modified_data$ApprovalDate

for (i in 1:max(raw_data$Len)){
  temp = raw_data[raw_data$Len >= i,]
  temp$Date = temp$ApprovalDate %m+% years(i)
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
model_data = modified_data[c("LogSize", "LogGDP", "LogSP500", "LogCPI", "FedFunds", "URinProjectState", "URinBorrState", "TermInMonths", "Age", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "BusinessType", "NaicsCode","DeliveryMethod", "Date", "ApprovalDate", "Default", "Size", "GrossChargeOffAmount", "Index", "BorrState", "ProjectState")]

#Continuous missing values
continuous_cols = c("LogSize", "LogGDP", "LogSP500", "LogCPI", "FedFunds", "URinProjectState", "URinBorrState", "TermInMonths", "Age", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt")
temp = model_data[continuous_cols]
temp[is.na(temp)] = 0
model_data[continuous_cols] = temp
scale_cols =  c("LogSize", "LogGDP", "LogSP500", "LogCPI", "FedFunds", "URinProjectState", "URinBorrState", "TermInMonths", "Age")
model_data$Age_Raw = model_data$Age
model_data[scale_cols] = as.data.frame(scale(model_data[scale_cols]))

#Discrete missing values
disc_cols = c("BusinessType", "NaicsCode", "DeliveryMethod")
temp = model_data[disc_cols]
levels(temp$BusinessType) = c(levels(temp$BusinessType), "NA")
temp[is.na(temp)] = "NA"
model_data[disc_cols] = temp

#Adding categorical columns
categorical_cols = disc_cols[apply(temp,2,function(x) { !all(x %in% 0:1) })]
model_data = dummy_cols(model_data, select_columns = categorical_cols, remove_selected_columns = TRUE)
names(model_data) = make.names(names(model_data), unique=TRUE)


######################
#Log Model
model_data = model_data %>% arrange(Date)

train_size = round((nrow(model_data)/10)*7, 0)
validation_size = round((nrow(model_data)/10), 0)
test_size = nrow(model_data) - train_size - validation_size

raw_train_data = model_data[0:train_size,]
raw_validation_data = model_data[(train_size+1):(train_size+validation_size),]
raw_test_data = model_data[(train_size+validation_size+1):nrow(model_data),]

train_data = subset(raw_train_data, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw))
validation_data = subset(raw_validation_data, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw))
test_data = subset(raw_test_data, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw))
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
#0.7097231
#Variable selection
qplot(y= log_model$coefficients) + labs(title = "Basic Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
log_model_coefficients = as.data.frame(log_model$coefficients)
log_model$coefficients

#Test ROC AUC
log_model_test_prediction = predict(log_model, newdata = test_data, type="response")
log_model_test_prediction[is.na(log_model_test_prediction)] = 0
log_model_test_prediction = ROCR::prediction(log_model_test_prediction, test_data$Default)
auc = unlist(slot(performance(log_model_test_prediction, 'auc'), 'y.values'))
auc
# 0.6450365

#Plotting ROCs
roc_train = performance(log_model_train_prediction,"tpr","fpr")
roc_test = performance(log_model_test_prediction,"tpr","fpr")
plot(roc_train, col = 'red')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 
legend(x = "bottomright", legend = c("Training ROC", "Testing ROC"), lty = c(1, 1), col = c("red", "blue"), lwd = 1) 

################################
#Ridge and Lasso Logisitic Model

#Training
x_train = model.matrix(Default ~., train_data)[, -1]
y_train = train_data$Default

model_L1 = glmnet(x_train, y_train, alpha = 1, nlambda = 20, family="binomial")
model_L2 = glmnet(x_train, y_train, alpha = 0, nlambda = 20, family="binomial")

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
for(i in 1:min(length(prediction_L1_train[1,]), length(prediction_L2_train[1,]))){
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
L1_summary$AUC = AUC_L1_validation[1:length(L1_summary[,1])]
max(L1_summary$AUC)
#0.7180873

#Best L2 Hyperparameter
best_L2_lambda_index = which.max(AUC_L2_validation)
best_L2_lambda = model_L2$lambda[best_L2_lambda_index]
best_L2_AUC = max(AUC_L2_validation)
L2_summary = as.data.frame(model_L2$lambda)
L2_summary$AUC = AUC_L2_validation[1:length(L2_summary[,1])]
max(L2_summary$AUC, na.rm=TRUE)
#0.6892060

#Testing: best L1  model
x_test = model.matrix(Default ~., test_data)[, -1]
prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda, type = "response")
prediction_test = ROCR::prediction(prediction_test, test_data$Default)
auc = unlist(slot(performance(prediction_test, 'auc'), 'y.values'))
auc
#0.6578393


#Plotting L1 coefficients
L1_coef = as.matrix(coef(model_L1, s=model_L1$lambda[best_L1_lambda_index]))
rownames(L1_coef) = rownames(coef(model_L1))
qplot(y= L1_coef[,1]) + labs(title = "Best L1 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
L1_coef = as.data.frame(L1_coef)
#Plotting ROCs
roc_train = performance(ROCR::prediction(prediction_L1_train[,best_L1_lambda_index], train_data$Default),"tpr","fpr")
roc_validation = performance(ROCR::prediction(prediction_L1_validation[,best_L1_lambda_index], validation_data$Default),"tpr","fpr")
roc_test = performance(prediction_test,"tpr","fpr")
plot(roc_train, col = 'red')
plot(roc_validation, add = TRUE, col = 'green')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 
legend(x = "bottomright", legend = c("Training ROC", "Validation ROC", "Testing ROC"), lty = c(1, 1, 1), col = c("red", "green", "blue"), lwd = 1) 


################################
#Loss at Default Linear Model
library(neuralnet)

default_train_data = raw_train_data[raw_train_data$GrossChargeOffAmount > 0 & raw_train_data$Default == 1,]
default_validation_data = raw_validation_data[raw_validation_data$GrossChargeOffAmount > 0 & raw_validation_data$Default == 1,]
default_test_data = raw_test_data[raw_test_data$GrossChargeOffAmount > 0 & raw_test_data$Default == 1,]

default_train_data$LossProp = default_train_data$GrossChargeOffAmount / default_train_data$Size
default_validation_data$LossProp = default_validation_data$GrossChargeOffAmount / default_validation_data$Size
default_test_data$LossProp = default_test_data$GrossChargeOffAmount / default_test_data$Size

default_train_data = subset(default_train_data, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw, Default))
default_validation_data = subset(default_validation_data, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw, Default))
default_test_data = subset(default_test_data, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw, Default))
length(default_train_data[,1])
length(default_validation_data[,1])
length(default_test_data[,1])

loss_at_default_linear_model = lm(LossProp~.,data = default_train_data)
prediction = predict(loss_at_default_linear_model, default_train_data)
sqrt(mean((default_train_data$LossProp - prediction)^2))
#0.2939945
plot(residuals(loss_at_default_linear_model))

prediction = predict(loss_at_default_linear_model, default_validation_data)
sqrt(mean((default_validation_data$LossProp - prediction)^2))
#0.1853075
prediction = predict(loss_at_default_linear_model, default_test_data)
sqrt(mean((default_test_data$LossProp - prediction)^2))
#0.1860177

################################
#Loss at Default NN Model

#Basic training
loss_at_default_nn_model = neuralnet(LossProp ~., data = default_train_data, hidden= c(length(default_train_data)/4), act.fct = "logistic", linear.output = FALSE)
#plot(loss_at_default_nn_model)
prediction = neuralnet::compute(loss_at_default_nn_model,default_validation_data)
prediction = as.vector(prediction$net.result)
sqrt(mean((default_validation_data$LossProp - prediction)^2))
#0.3785059

#Validation: Tuning structure and activation fns
softplus <- function(x) log(1 + exp(x))
sigmoid = function(x) {1/(1+ exp(-x))}
swish = function(x) {x*sigmoid(x)}
functions =  c("logistic", "tanh", softplus, sigmoid, swish)
rmses = vector()
for (f in functions){
  loss_at_default_nn_model = neuralnet(LossProp ~., data = default_train_data, hidden= c(length(default_train_data)/4), act.fct = f, linear.output = FALSE)
  prediction = neuralnet::compute(loss_at_default_nn_model,default_validation_data)
  prediction = as.vector(prediction$net.result)
  rmses = append(rmses, sqrt(mean((default_validation_data$LossProp - prediction)^2)))
}
#0.3932205 0.5967044
best_function = functions[which.min(rmses)]
#"logistic"

structures = list(c(length(default_train_data)/4,length(default_train_data)/8, length(default_train_data)/16), c(length(default_train_data)/4,length(default_train_data)/8), c(length(default_train_data)/8,length(default_train_data)/16))
rmses = vector()
for (s in structures){
  loss_at_default_nn_model = neuralnet(LossProp ~., data = default_train_data, hidden= s, act.fct = best_function, linear.output = FALSE)
  prediction = neuralnet::compute(loss_at_default_nn_model,default_validation_data)
  prediction = as.vector(prediction$net.result)
  rmses = append(rmses, sqrt(mean((default_validation_data$LossProp - prediction)^2)))
}
#0.3352151 0.4104983 0.2306423
best_structure = structures[which.min(rmses)]
best_structure
#6 3

#Testing
loss_at_default_nn_model = neuralnet(LossProp ~., data = default_train_data, hidden= c(6,3), act.fct = "logistic", linear.output = FALSE)
prediction = neuralnet::compute(loss_at_default_nn_model,default_test_data)
prediction = as.vector(prediction$net.result)
sqrt(mean((default_test_data$LossProp - prediction)^2))
#0.2679258
################################
#Loss at Default Beta Model
train_lossProp_Dist = default_train_data$LossProp[default_train_data$LossProp <= 1]
train_lossPropDist_beta = fitdist(train_lossProp_Dist, "beta")

numBins = round((max(train_lossProp_Dist) - min(train_lossProp_Dist))/(2*IQR(train_lossProp_Dist)/(length(train_lossProp_Dist)^(1/3))))
hist(train_lossProp_Dist, breaks =  numBins, main = "Histogram of Loan Loss Proportion at Default", xlab="Proportion of Loan Loss at Default", ylab="Train Count")
plot(train_lossPropDist_beta)

################################
#Portfolio Selection
Last_Date = as.Date("2013-02-01")

model_data = model_data %>% arrange(Date)
portfolio = sample(raw_data$Index[raw_data$ApprovalDate > "2008-02-01" & raw_data$TermInMonths > 60 & is.na(raw_data$ChargeOffDate)], 500)
portfolio = model_data[model_data$Index %in% portfolio, ]
portfolio = portfolio %>% group_by(Index) %>% filter(Date==max(Date))
#Portfolio value
sum(portfolio$Size)
#749960838

########################################
#Projecting Forward Explantory Variables

#GDP Graph
ggplot(data = GDP[GDP$DATE>as.Date("1990-01-01"),], aes(DATE, GDP)) +  geom_line(aes(group=1))
#SP500 Graph
ggplot(data = tempSP500[tempSP500$month.bin>as.Date("1990-01-01"),], aes(month.bin, GSPC.price)) +  geom_line(aes(group=1))
#Fed Funds Graph
ggplot(data = FEDFUNDS[FEDFUNDS$DATE>as.Date("1990-01-01"),], aes(DATE, FEDFUNDS)) +  geom_line(aes(group=1))
#CPI Graph
ggplot(data = CPI[CPI$DATE>as.Date("1990-01-01"),], aes(DATE, CPIAUCSL)) +  geom_line(aes(group=1))
#UR Graph
ggplot(data = UnemploymentUSbyState[UnemploymentUSbyState$DATE>as.Date("1990-01-01") & UnemploymentUSbyState$State == "CA",], aes(DATE, UR)) +  geom_line(aes(colour = State, group = 1))

projections = data.frame(matrix(ncol = 3, nrow = 0))

#Modeling SP500 with ARMA GARCH
library(rugarch)
SP500_Log_returns = tempSP500[-1,]
SP500_Log_returns$Log_returns = Delt(tempSP500$GSPC.price, type = 'log')[2:length(tempSP500$GSPC.price)]
hist(SP500_Log_returns$Log_returns, main = NULL, xlab = "SP500 Log Returns")
plot(acf(SP500_Log_returns$Log_returns), main = NULL)

model_specifies = ugarchspec(mean.model = list(armaOrder = c(2,2), include.mean = FALSE), variance.model = list(model="sGARCH", garchOrder = c(1,1)), distribution.model = "std")
SP500_ARMA_2_1_GARCH_1_1 = ugarchfit(data = SP500_Log_returns$Log_returns, spec = model_specifies, out.sample = 0)

projection = ugarchboot(SP500_ARMA_2_1_GARCH_1_1, n.ahead = 60, method =  c("Partial", "Full")[1])
as.data.frame(projection, type = "summary")
plot(projection, which = 2)
projections = rbind(projections, c("SP500", "1yr", SP500_Log_returns$GSPC.price[SP500_Log_returns$month.bin == Last_Date]* exp(as.data.frame(projection, type = "summary", )[3,12])))
projections = rbind(projections, c("SP500", "5yr", SP500_Log_returns$GSPC.price[SP500_Log_returns$month.bin == Last_Date]* exp(as.data.frame(projection, type = "summary", )[3,60])))
projections

#10 year CAGR
projections = data.frame(matrix(ncol = 4, nrow = 0))
colnames(projections) = c("Metrics", "Period", "State", "Projection")

growth_rate = (GDP$GDP[GDP$DATE == as.Date("2013-01-01")]/GDP$GDP[GDP$DATE == (as.Date("2013-01-01") %m-% years(10))])^(12/60) - 1
projections = rbind(projections, c("GDP", "1yr", "NA", ((growth_rate + 1)^1 * GDP$GDP[GDP$DATE == as.Date("2013-01-01")])))
projections = rbind(projections, c("GDP", "5yr", "NA", ((growth_rate + 1)^5 *GDP$GDP[GDP$DATE == as.Date("2013-01-01")])))

growth_rate = (tempSP500$GSPC.price[tempSP500$month.bin == Last_Date]/tempSP500$GSPC.price[tempSP500$month.bin == (Last_Date %m-% years(10))])^(12/120) - 1
projections = rbind(projections, c("SP500", "1yr", "NA", ((growth_rate + 1)^1 * tempSP500$GSPC.price[tempSP500$month.bin == Last_Date])))
projections = rbind(projections, c("SP500", "5yr", "NA", ((growth_rate + 1)^5 * tempSP500$GSPC.price[tempSP500$month.bin == Last_Date])))

growth_rate = (FEDFUNDS$FEDFUNDS[FEDFUNDS$DATE == Last_Date]/FEDFUNDS$FEDFUNDS[FEDFUNDS$DATE == (Last_Date %m-% years(10))])^(12/120) - 1
projections = rbind(projections, c("FEDFUNDS", "1yr", "NA", ((growth_rate + 1)^1 * FEDFUNDS$FEDFUNDS[FEDFUNDS$DATE == Last_Date])))
projections = rbind(projections, c("FEDFUNDS", "5yr", "NA", ((growth_rate + 1)^5 * FEDFUNDS$FEDFUNDS[FEDFUNDS$DATE == Last_Date])))

growth_rate = (CPI$CPIAUCSL[CPI$DATE == Last_Date]/CPI$CPIAUCSL[CPI$DATE == (Last_Date %m-% years(10))])^(12/120) - 1
projections = rbind(projections, c("CPI", "1yr", "NA", ((growth_rate + 1)^1 * CPI$CPIAUCSL[CPI$DATE == Last_Date])))
projections = rbind(projections, c("CPI", "5yr", "NA", ((growth_rate + 1)^5 * CPI$CPIAUCSL[CPI$DATE == Last_Date])))

for (state in unique(UnemploymentUSbyState$State)){
  growth_rate = (UnemploymentUSbyState$UR[UnemploymentUSbyState$DATE == Last_Date & UnemploymentUSbyState$State == state ]/UnemploymentUSbyState$UR[UnemploymentUSbyState$DATE == (Last_Date %m-% years(10)) & UnemploymentUSbyState$State == state])^(12/120) - 1
  projections = rbind(projections, c("UR", "1yr", state, ((growth_rate + 1)^1 * UnemploymentUSbyState$UR[UnemploymentUSbyState$DATE == Last_Date & UnemploymentUSbyState$State == state])))
  projections = rbind(projections, c("UR", "5yr", state, ((growth_rate + 1)^5 * UnemploymentUSbyState$UR[UnemploymentUSbyState$DATE == Last_Date& UnemploymentUSbyState$State == state])))
}
colnames(projections) = c("Metrics", "Period", "State", "Projection")

###############
#1yr Period VaR
#Adding Projections
portfolio$LogGDP = (log(as.numeric(projections$Projection[projections$Metrics == "GDP" & projections$Period == "1yr"])) - mean(log(modified_data$GDP)))/ sd(log(modified_data$GDP))
portfolio$LogSP500 = (log(as.numeric(projections$Projection[projections$Metrics == "SP500" & projections$Period == "1yr"])) - mean(log(modified_data$GSPC.price)))/ sd(log(modified_data$GSPC.price))
portfolio$FedFunds = (as.numeric(projections$Projection[projections$Metrics == "FEDFUNDS" & projections$Period == "1yr"]) - mean(modified_data$FedFunds))/ sd(modified_data$FedFunds)
portfolio$LogCPI = (log(as.numeric(projections$Projection[projections$Metrics == "CPI" & projections$Period == "1yr"])) - mean(log(modified_data$CPI)))/ sd(log(modified_data$CPI))
temp = rename(projections, BorrState = State)
temp = temp[temp$Period == "1yr",]
temp = merge(x=portfolio,y=temp,by=c("BorrState"))
portfolio$URinBorrState =  (as.numeric(temp$Projection) - mean(modified_data$URinBorrState))/ sd(modified_data$URinBorrState)
temp = rename(projections, ProjectState = State)
temp = temp[temp$Period == "1yr",]
temp = merge(x=portfolio,y=temp,by=c("ProjectState"))
portfolio$URinProjectState =  (as.numeric(temp$Projection) - mean(modified_data$URinProjectState))/ sd(modified_data$URinProjectState)

portfolio$Age =  (portfolio$Age_Raw + 1 - mean(modified_data$Age))/ sd(modified_data$Age)

#Training best classification model
test_data = subset(portfolio, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw))
x_test = model.matrix(Default ~., test_data)[, -1]
prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda, type = "response")
hist(prediction_test, main= NULL, xlab="Best L1 Model Default Probability" )

#Inverse Sampling
N = 10000
beta_lossDist = vector()
linear_lossDist = vector()
for (j in 1:N){
  U = runif(1,min = min(prediction_test), max = max(prediction_test))
  sample = prediction_test[prediction_test>U]
  sample_sizes = portfolio$Size[prediction_test>U]
  
  beta_lossProp_sample = rbeta(length(sample), train_lossPropDist_beta$estimate[1], train_lossPropDist_beta$estimate[2])
  beta_lossDist = append(beta_lossDist, -sum(beta_lossProp_sample*sample_sizes))
  
  linear_lossProp_sample = predict(loss_at_default_linear_model, test_data[prediction_test>U,])
  linear_lossProp_sample = as.vector(linear_lossProp_sample)
  linear_lossDist = append(linear_lossDist, -sum(linear_lossProp_sample*sample_sizes))
}

#Nonparametric VaR
VaRs = data.frame(matrix(ncol = 6, nrow = 0))
colnames(VaRs) = c("Type", "Loss at Default Model", "95% VaR", "99% VaR", "95% Avg. VaR", "99% Avg. VaR")
VaRs
VaR_95 = -quantile(beta_lossDist, prob = c(0.05))
VaR_99 = -quantile(beta_lossDist, prob = c(0.01))
Avg_VaR_95 = -sum(beta_lossDist[beta_lossDist < -VaR_95])/ length(beta_lossDist[beta_lossDist < -VaR_95])
Avg_VaR_99 = -sum(beta_lossDist[beta_lossDist <= -VaR_99])/ length(beta_lossDist[beta_lossDist <= -VaR_99])
VaRs = rbind(VaRs, setNames(as.list(c("Nonparametric", "Beta", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))

hist(beta_lossDist, main = NULL, xlab="Portfolio Loss", xlim = c(-max(VaR_95, VaR_99, Avg_VaR_95, Avg_VaR_99),0)) 
abline(v = -VaR_95, col="blue", lwd = 3) 
abline(v = -VaR_99, col="red", lwd = 3)
abline(v = -Avg_VaR_95, col="blue", lty = 2, lwd = 3) 
abline(v = -Avg_VaR_99, col="red", lty = 2, lwd = 3)
legend(x = "top",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

VaR_95 = -quantile(linear_lossDist, prob = c(0.05))
VaR_99 = -quantile(linear_lossDist, prob = c(0.01))
Avg_VaR_95 = -sum(linear_lossDist[linear_lossDist < -VaR_95])/ length(linear_lossDist[linear_lossDist < -VaR_95])
Avg_VaR_99 = -sum(linear_lossDist[linear_lossDist <= -VaR_99])/ length(linear_lossDist[linear_lossDist <= -VaR_99])
VaRs = rbind(VaRs, setNames(as.list(c("Nonparametric", "Linear", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))
VaRs
hist(linear_lossDist, main = NULL, xlab="Portfolio Loss", xlim = c(-max(VaR_95, VaR_99, Avg_VaR_95, Avg_VaR_99),0)) 
abline(v = -VaR_95, col="blue", lwd = 3) 
abline(v = -VaR_99, col="red", lwd = 3)
abline(v = -Avg_VaR_95, col="blue", lty = 2, lwd = 3) 
abline(v = -Avg_VaR_99, col="red", lty = 2, lwd = 3)
legend(x = "top",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

#Parametric VaR
beta_lossDist_normal_fit = fitdistrplus::fitdist(beta_lossDist, "norm")
hist(beta_lossDist, main = NULL, xlab="Portfolio Loss")
plot(beta_lossDist_normal_fit)

beta_lossDist_log = log(-beta_lossDist)
beta_lossDist_log[beta_lossDist_log = "-inf"] = 0
beta_lossDist_log_normal_fit = fitdistrplus::fitdist(beta_lossDist_log, "norm")
hist(beta_lossDist_log, main = NULL, xlab="Portfolio Loss")
plot(beta_lossDist_log_normal_fit)
qqcomp(beta_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss")

VaR_95 = exp(qnorm(0.95, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_99 = exp(qnorm(0.99, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
x = seq(0.95, 0.999, 0.001)
Avg_VaR_95 = sum(exp(qnorm(x, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
x = seq(0.99, 0.999, 0.001)
Avg_VaR_99 = sum(exp(qnorm(x, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
VaRs = rbind(VaRs, setNames(as.list(c("Parametric", "Beta - Log Normal", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))

denscomp(ft = beta_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss",, xlim = c(0,max(log(VaR_95), log(VaR_99), log(Avg_VaR_95), log(Avg_VaR_99))))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

linear_lossDist_log = log(-linear_lossDist)
linear_lossDist_log_normal_fit = fitdistrplus::fitdist(linear_lossDist_log, "norm")
hist(linear_lossDist_log, main = NULL, xlab="Portfolio Loss")
plot(linear_lossDist_log_normal_fit)
qqcomp(linear_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss")

VaR_95 = exp(qnorm(0.95, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_99 = exp(qnorm(0.99, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
x = seq(0.95, 0.999, 0.001)
Avg_VaR_95 = sum(exp(qnorm(x, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
x = seq(0.99, 0.999, 0.001)
Avg_VaR_99 = sum(exp(qnorm(x, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
VaRs = rbind(VaRs, setNames(as.list(c("Parametric", "Linear - Log Normal", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))

denscomp(ft = linear_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss",, xlim = c(0,max(log(VaR_95), log(VaR_99), log(Avg_VaR_95), log(Avg_VaR_99))))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

colnames(VaRs) = c("Type", "Loss at Default Model", "95% VaR", "99% VaR", "95% Avg. VaR", "99% Avg. VaR")

#################
#Tranche:Risk Management (1yr)

#Fit Normal to Junior
pool.size = sum(portfolio$Size)
equity.size = -pool.size*.05
junior.size = -pool.size*.1
senior.size = -pool.size*.85
equity.size
beta_lossDist.equity = pmax(beta_lossDist,equity.size)
beta_lossDist.remaining = beta_lossDist - beta_lossDist.equity
beta_lossDist.junior = pmax(beta_lossDist.remaining,junior.size)
beta_lossDist.senior = (beta_lossDist.remaining - beta_lossDist.junior)
beta_lossPropDist.junior = beta_lossDist.junior/junior.size
beta_lossPropDist.senior = beta_lossDist.senior/senior.size
linear_lossDist.equity = pmax(linear_lossDist,equity.size)
linear_lossDist.remaining = linear_lossDist - linear_lossDist.equity
linear_lossDist.junior = pmax(linear_lossDist.remaining,junior.size)
linear_lossDist.senior = linear_lossDist.remaining - linear_lossDist.junior
linear_lossPropDist.junior = linear_lossDist.junior/junior.size
linear_lossPropDist.senior = linear_lossDist.senior/senior.size

beta_lossPropDist.junior_normal_fit = fitdistrplus::fitdist(beta_lossPropDist.junior, "norm")
hist(beta_lossPropDist.junior, main = NULL, xlab="Portfolio Loss")
plot(beta_lossPropDist.junior_normal_fit)

linear_lossPropDist.junior_normal_fit = fitdistrplus::fitdist(linear_lossDist.junior, "norm")
hist(linear_lossPropDist.junior, main = NULL, xlab="Portfolio Loss")
plot(linear_lossPropDist.junior_normal_fit)

#Fit Normal to Senior
beta_lossPropDist.senior_normal_fit = fitdistrplus::fitdist(beta_lossPropDist.senior, "norm")
hist(beta_lossPropDist.senior, main = NULL, xlab="Portfolio Loss")
plot(beta_lossPropDist.senior_normal_fit)

linear_lossPropDist.senior_normal_fit = fitdistrplus::fitdist(linear_lossPropDist.senior, "norm")
hist(linear_lossPropDist.senior, main = NULL, xlab="Portfolio Loss")
plot(linear_lossPropDist.senior_normal_fit)

#Comparison
cdfcomp(list(beta_lossPropDist.junior_normal_fit), main = "Junior Tranche (1yr)", legendtext = c( "Theoretical CDF"),xlab = "Tranche Loss (%)", xlim = c(0,1))
cdfcomp(list( beta_lossPropDist.senior_normal_fit), main = "Senior Tranche (1yr)", legendtext = c( "Theoretical CDF"),xlab = "Tranche Loss (%)", xlim = c(0,1))


###############
#5yr Period VaR

#Adding Projections
portfolio$LogGDP = (log(as.numeric(projections$Projection[projections$Metrics == "GDP" & projections$Period == "5yr"])) - mean(log(modified_data$GDP)))/ sd(log(modified_data$GDP))
portfolio$LogSP500 = (log(as.numeric(projections$Projection[projections$Metrics == "SP500" & projections$Period == "5yr"])) - mean(log(modified_data$GSPC.price)))/ sd(log(modified_data$GSPC.price))
portfolio$FedFunds = (as.numeric(projections$Projection[projections$Metrics == "FEDFUNDS" & projections$Period == "5yr"]) - mean(modified_data$FedFunds))/ sd(modified_data$FedFunds)
portfolio$LogCPI = (log(as.numeric(projections$Projection[projections$Metrics == "CPI" & projections$Period == "5yr"])) - mean(log(modified_data$CPI)))/ sd(log(modified_data$CPI))
temp = rename(projections, BorrState = State)
temp = temp[temp$Period == "5yr",]
temp = merge(x=portfolio,y=temp,by=c("BorrState"))
portfolio$URinBorrState =  (as.numeric(temp$Projection) - mean(modified_data$URinBorrState))/ sd(modified_data$URinBorrState)
temp = rename(projections, ProjectState = State)
temp = temp[temp$Period == "5yr",]
temp = merge(x=portfolio,y=temp,by=c("ProjectState"))
portfolio$URinProjectState =  (as.numeric(temp$Projection) - mean(modified_data$URinProjectState))/ sd(modified_data$URinProjectState)
portfolio$Age =  (portfolio$Age_Raw + 5 - mean(modified_data$Age))/ sd(modified_data$Age)

#Training best classification model
test_data = subset(portfolio, select = -c(ApprovalDate, Date, Size, GrossChargeOffAmount, Index, BorrState, ProjectState, Age_Raw))
x_test = model.matrix(Default ~., test_data)[, -1]
prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda, type = "response")
hist(prediction_test, main= NULL, xlab="Best L1 Model Default Probability" )

#Inverse Sampling
N = 10000
beta_lossDist = vector()
linear_lossDist = vector()
for (j in 1:N){
  U = runif(1,min = min(prediction_test), max = max(prediction_test))
  sample = prediction_test[prediction_test>U]
  sample_sizes = portfolio$Size[prediction_test>U]
  
  beta_lossProp_sample = rbeta(length(sample), train_lossPropDist_beta$estimate[1], train_lossPropDist_beta$estimate[2])
  beta_lossDist = append(beta_lossDist, -sum(beta_lossProp_sample*sample_sizes))
  
  linear_lossProp_sample = predict(loss_at_default_linear_model, test_data[prediction_test>U,])
  linear_lossProp_sample = as.vector(linear_lossProp_sample)
  linear_lossDist = append(linear_lossDist, -sum(linear_lossProp_sample*sample_sizes))
}

#Nonparametric VaR
VaRs = data.frame(matrix(ncol = 6, nrow = 0))
colnames(VaRs) = c("Type", "Loss at Default Model", "95% VaR", "99% VaR", "95% Avg. VaR", "99% Avg. VaR")
VaRs
VaR_95 = -quantile(beta_lossDist, prob = c(0.05))
VaR_99 = -quantile(beta_lossDist, prob = c(0.01))
Avg_VaR_95 = -sum(beta_lossDist[beta_lossDist < -VaR_95])/ length(beta_lossDist[beta_lossDist < -VaR_95])
Avg_VaR_99 = -sum(beta_lossDist[beta_lossDist <= -VaR_99])/ length(beta_lossDist[beta_lossDist <= -VaR_99])
VaRs = rbind(VaRs, setNames(as.list(c("Nonparametric", "Beta", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))

hist(beta_lossDist, main = NULL, xlab="Portfolio Loss", xlim = c(-max(VaR_95, VaR_99, Avg_VaR_95, Avg_VaR_99),0)) 
abline(v = -VaR_95, col="blue", lwd = 3) 
abline(v = -VaR_99, col="red", lwd = 3)
abline(v = -Avg_VaR_95, col="blue", lty = 2, lwd = 3) 
abline(v = -Avg_VaR_99, col="red", lty = 2, lwd = 3)
legend(x = "top",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

VaR_95 = -quantile(linear_lossDist, prob = c(0.05))
VaR_99 = -quantile(linear_lossDist, prob = c(0.01))
Avg_VaR_95 = -sum(linear_lossDist[linear_lossDist < -VaR_95])/ length(linear_lossDist[linear_lossDist < -VaR_95])
Avg_VaR_99 = -sum(linear_lossDist[linear_lossDist <= -VaR_99])/ length(linear_lossDist[linear_lossDist <= -VaR_99])
VaRs = rbind(VaRs, setNames(as.list(c("Nonparametric", "Linear", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))
VaRs
hist(linear_lossDist, main = NULL, xlab="Portfolio Loss", xlim = c(-max(VaR_95, VaR_99, Avg_VaR_95, Avg_VaR_99),0)) 
abline(v = -VaR_95, col="blue", lwd = 3) 
abline(v = -VaR_99, col="red", lwd = 3)
abline(v = -Avg_VaR_95, col="blue", lty = 2, lwd = 3) 
abline(v = -Avg_VaR_99, col="red", lty = 2, lwd = 3)
legend(x = "top",legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

#Parametric VaR
beta_lossDist_normal_fit = fitdistrplus::fitdist(beta_lossDist, "norm")
hist(beta_lossDist, main = NULL, xlab="Portfolio Loss")
plot(beta_lossDist_normal_fit)

beta_lossDist_log = log(-beta_lossDist)
beta_lossDist_log[beta_lossDist_log = "-inf"] = 0
beta_lossDist_log_normal_fit = fitdistrplus::fitdist(beta_lossDist_log, "norm")
hist(beta_lossDist_log, main = NULL, xlab="Portfolio Loss")
plot(beta_lossDist_log_normal_fit)
qqcomp(beta_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss")

VaR_95 = exp(qnorm(0.95, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_99 = exp(qnorm(0.99, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
x = seq(0.95, 0.999, 0.001)
Avg_VaR_95 = sum(exp(qnorm(x, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
x = seq(0.99, 0.999, 0.001)
Avg_VaR_99 = sum(exp(qnorm(x, mean = beta_lossDist_log_normal_fit$estimate[1], sd = beta_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
VaRs = rbind(VaRs, setNames(as.list(c("Parametric", "Beta - Log Normal", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))

denscomp(ft = beta_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss",, xlim = c(0,max(log(VaR_95), log(VaR_99), log(Avg_VaR_95), log(Avg_VaR_99))))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

linear_lossDist_log = log(-linear_lossDist)
linear_lossDist_log_normal_fit = fitdistrplus::fitdist(linear_lossDist_log, "norm")
hist(linear_lossDist_log, main = NULL, xlab="Portfolio Loss")
plot(linear_lossDist_log_normal_fit)
qqcomp(linear_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss")

VaR_95 = exp(qnorm(0.95, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
VaR_99 = exp(qnorm(0.99, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE))
x = seq(0.95, 0.999, 0.001)
Avg_VaR_95 = sum(exp(qnorm(x, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
x = seq(0.99, 0.999, 0.001)
Avg_VaR_99 = sum(exp(qnorm(x, mean = linear_lossDist_log_normal_fit$estimate[1], sd = linear_lossDist_log_normal_fit$estimate[2], lower.tail = TRUE, log.p = FALSE)))/length(x)
VaRs = rbind(VaRs, setNames(as.list(c("Parametric", "Linear - Log Normal", round(VaR_95), round(VaR_99), round(Avg_VaR_95), round(Avg_VaR_99))), names(VaRs)))

denscomp(ft = linear_lossDist_log_normal_fit, legendtext = "Normal",  main = NULL, xlab="Portfolio Log Loss",, xlim = c(0,max(log(VaR_95), log(VaR_99), log(Avg_VaR_95), log(Avg_VaR_99))))
abline(v = log(VaR_95), col="blue", lwd = 3) 
abline(v = log(VaR_99), col="red", lwd = 3)
abline(v = log(Avg_VaR_95), col="blue", lty = 2, lwd = 3) 
abline(v = log(Avg_VaR_99), col="red", lty = 2, lwd = 3)
legend(x = "bottomleft", legend = c("VaR 95% Level", "VaR 99% Level", "Avg VaR 95% Level", "Avg VaR 99% Level"), lty = c(1, 1, 2, 2), col = c("blue", "red", "blue", "red"), lwd = 3)

colnames(VaRs) = c("Type", "Loss at Default Model", "95% VaR", "99% VaR", "95% Avg. VaR", "99% Avg. VaR")
VaRs
#################
#Tranche:Risk Management (5yr)
#Fit Normal to Junior
pool.size = sum(portfolio$Size)
equity.size = -pool.size*.05
junior.size = -pool.size*.1
senior.size = -pool.size*.85
beta_lossDist.equity = pmax(beta_lossDist,equity.size)
beta_lossDist.remaining = beta_lossDist - beta_lossDist.equity
beta_lossDist.junior = pmax(beta_lossDist.remaining,junior.size)
beta_lossDist.senior = (beta_lossDist.remaining - beta_lossDist.junior)
beta_lossPropDist.junior = beta_lossDist.junior/junior.size
beta_lossPropDist.senior = beta_lossDist.senior/senior.size
linear_lossDist.equity = pmax(linear_lossDist,equity.size)
linear_lossDist.remaining = linear_lossDist - linear_lossDist.equity
linear_lossDist.junior = pmax(linear_lossDist.remaining,junior.size)
linear_lossDist.senior = linear_lossDist.remaining - linear_lossDist.junior
linear_lossPropDist.junior = linear_lossDist.junior/junior.size
linear_lossPropDist.senior = linear_lossDist.senior/senior.size

beta_lossPropDist.junior_normal_fit = fitdistrplus::fitdist(beta_lossPropDist.junior, "norm")
hist(beta_lossPropDist.junior, main = NULL, xlab="Portfolio Loss")
plot(beta_lossPropDist.junior_normal_fit)

linear_lossPropDist.junior_normal_fit = fitdistrplus::fitdist(linear_lossDist.junior, "norm")
hist(linear_lossPropDist.junior, main = NULL, xlab="Portfolio Loss")
plot(linear_lossPropDist.junior_normal_fit)

#Fit Normal to Senior
beta_lossPropDist.senior_normal_fit = fitdistrplus::fitdist(beta_lossPropDist.senior, "norm")
hist(beta_lossPropDist.senior, main = NULL, xlab="Portfolio Loss")
plot(beta_lossPropDist.senior_normal_fit)

linear_lossPropDist.senior_normal_fit = fitdistrplus::fitdist(linear_lossPropDist.senior, "norm")
hist(linear_lossPropDist.senior, main = NULL, xlab="Portfolio Loss")
plot(linear_lossPropDist.senior_normal_fit)

#Comparison
cdfcomp(list( beta_lossPropDist.junior_normal_fit), main = "Junior Tranche (5yr)", legendtext = c( "Theoretical CDF"),xlab = "Tranche Loss (%)", xlim = c(0,1))
cdfcomp(list( beta_lossPropDist.senior_normal_fit), main = "Senior Tranche (5yr)", legendtext = c( "Theoretical CDF"),xlab = "Tranche Loss (%)",xlim = c(0,1))
