#Neural Net
install.packages("ROCR")
library("Matrix")
library(neuralnet)
library(ROCR)

modified_data = read.csv("modified_data.csv", header = TRUE)

#################
#Data Paritioning
train_size = round((nrow(modified_data)/10)*7, 0)
validation_size = round((nrow(modified_data)/10), 0)
test_size = nrow(modified_data) - train_size - validation_size

train_data = modified_data[0:train_size,]
validation_data = modified_data[(train_size+1):(train_size+validation_size),]
test_data = modified_data[(train_size+validation_size+1):nrow(modified_data),]

#Setup
set.seed(1)
softplus <- function(x) log(1 + exp(x))
sigmoid = function(x) {1/(1+ exp(-x))}
swish = function(x) {x*sigmoid(x)}
################
#Nueral Net
#Training: Default hidden layers c(4,2) & sigmoid & rep = 3
train_data_2 = train_data[sample(nrow(train_data), 5000), ]
hidden_variables = c(2, 4, 8, 16, 32)

#AUC
auc_compute = function(nn,data){
  nn_prediction = neuralnet::compute(nn,data)
  nn_prediction = as.vector(nn_prediction$net.result)
  nn_prediction = ROCR::prediction(nn_prediction, data$Default)
  auc = unlist(slot(performance(nn_prediction, 'auc'), 'y.values'))
  return(auc)
}

nn_log = neuralnet(Default ~., data = train_data_2, hidden= c(3), linear.output = FALSE)
#nn_tanh = neuralnet(Default ~., data = train_data, hidden= c(3),rep = 2, threshold = .05, act.fct = 'tanh', linear.output = FALSE)
#nn_softplus = neuralnet(Default ~., data = train_data, hidden= c(3), act.fct = softplus, linear.output = FALSE)
#nn_swish = neuralnet(Default ~., data = train_data, hidden= c(3),  act.fct = swish, linear.output = FALSE)
#Training AUC
auc_compute(nn_log,train_data)
#Validation AUC
auc_compute(nn_log,validation_data)
############################
#Best Architecture

#Validation: Hidden Variables  with Default 2 hidden layers & best Act Fn
arch_list = list(c(2), c(3), c(4), c(6), c(2,1), c(4,2))
nn1 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[[1]], err.fct = "sse", linear.output = FALSE)
nn2 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[[2]], err.fct = "sse", linear.output = FALSE)
nn3 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[[3]], err.fct = "sse", linear.output = FALSE)
nn4 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[[4]], err.fct = "sse", linear.output = FALSE)
nn5 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[[5]], err.fct = "sse", linear.output = FALSE)
nn6 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[[6]], err.fct = "sse", linear.output = FALSE)

auc_val_arch =c(auc_compute(nn1,validation_data),
                auc_compute(nn2,validation_data),
                auc_compute(nn3,validation_data),
                auc_compute(nn4,validation_data),
                auc_compute(nn5,validation_data),
                auc_compute(nn6,validation_data))
c(auc_compute(nn1,train_data),
  auc_compute(nn2,train_data),
  auc_compute(nn3,train_data),
  auc_compute(nn4,train_data),
  auc_compute(nn5,train_data),
  auc_compute(nn6,train_data))

auc_val_arch

#c(auc_compute(nn1,test_data),
#  auc_compute(nn2,test_data),
#  auc_compute(nn3,test_data),
#  auc_compute(nn4,test_data),
#  auc_compute(nn5,test_data),
#  auc_compute(nn6,test_data))
best_arch_index = which.max(auc_val_arch)
best_auc.val = max(auc_val_arch)
best_arch = arch_list[[best_arch_index]]
#########################
#EPOCHS

epoch_auc.train = vector()
epoch_auc.val = vector()
count = vector()
for(i in 1:10){
  epoch_nn = neuralnet(Default ~., data = train_data, hidden= best_arch,rep = i, err.fct = "ce", linear.output = FALSE)
  auc.train = auc_compute(epoch_nn,train_data)
  auc.val = auc_compute(epoch_nn,validation_data)
  epoch_auc.train = append(epoch_auc.train, auc.train)
  epoch_auc.val = append(epoch_auc.val, auc.val)
}
epoch_auc.train
epoch_auc.val

best_epoch = which.max(epoch_auc.val)
best_epoch_auc.val = max(epoch_auc.val)
#########################
#LOO
#install.packages("gridExtra")
library(gridExtra)
library(grid)
cols = colnames(train_data)
cols = cols[cols != "Default" ]
auc_list.LOO = c() 
for( i in 1:length(cols)){
  excluded = cols[i]
  nn.LOO = neuralnet(Default ~. - excluded, data = train_data, hidden = best_arch, rep = best_epoch, linear.output = FALSE)
  auc.LOO = auc_compute(nn.LOO,validation_data)
  auc_list.LOO = c(auc_list.LOO, auc.LOO)
}
ranked_cols = cols[order(auc_list.LOO)]
ranked_auc = auc_list.LOO[order(auc_list.LOO)]
d = data.frame(unlist(ranked_cols[1:10]),unlist(ranked_auc[1:10]))
names(d) = c("Column names","AUC values")
grid.table(d)

##########################
#Final Model
#install.packages("ROSE")
library("ROSE")
train_data.over <- ovun.sample(Default~., data = train_data, method = "over", N = 70934)$data
best_nn = neuralnet(Default ~., data = train_data, hidden = best_arch, threshold = .01, act.fct = "logistic",linear.output = FALSE)
best_nn.over = neuralnet(Default ~., data = train_data.over, hidden = best_arch, rep = .01, act.fct = "logistic",linear.output = FALSE)

best_auc.train = auc_compute(best_nn,train_data)
best_auc.val = auc_compute(best_nn,validation_data)
best_auc.test = auc_compute(best_nn,test_data)

best_auc.train
best_auc.val
best_auc.test
#save(best_nn, file = "trained_nn.RData")

##############################
#EVALUATION
#Explain fitting results via LOO tests
#install.packages("caret")
library(caret)
load("trained_nn.RData")
load("trained_L1_log_model.RData")
##AUC
auc_compute(best_nn,test_data)

##ROC Curve NN
nn_pred.train = neuralnet::compute(best_nn,train_data)
nn_pred.train = as.vector(nn_pred.train$net.result)
nn_pred.train = ROCR::prediction(nn_pred.train, train_data$Default)

nn_pred.test = neuralnet::compute(best_nn,test_data)
nn_pred.test = as.vector(nn_pred.test$net.result)
nn_pred.test = ROCR::prediction(nn_pred.test, test_data$Default)

roc_train.nn = performance(nn_pred.train,"tpr","fpr")
roc_test.nn = performance(nn_pred.test,"tpr","fpr")
plot(roc_train.nn, col = 'red', main = 'NN Model Training ROC (red) vs. NN Test ROC (blue)')
plot(roc_test.nn, add = TRUE, col = 'blue')
abline(a = 0, b = 1)

##ROC Curve LR comparision NN
##Log-R data
log_model_data = read.csv("log_model_data.csv", header = TRUE)

train_size = round((nrow(log_model_data)/10)*7, 0)
validation_size = round((nrow(log_model_data)/10), 0)
test_size = nrow(log_model_data) - train_size - validation_size

raw_train_data = log_model_data[0:train_size,]
raw_validation_data = log_model_data[(train_size+1):(train_size+validation_size),]
raw_test_data = log_model_data[(train_size+validation_size+1):nrow(log_model_data),]

train_data.log = subset(raw_train_data, select = -c(GrossApproval, GrossChargeOffAmount))
validation_data.log = subset(raw_validation_data, select = -c(GrossApproval, GrossChargeOffAmount))
test_data.log = subset(raw_test_data, select = -c(GrossApproval, GrossChargeOffAmount))
##ROC
x_test = model.matrix(Default ~., test_data.log)[, -1]
prediction_test = predict(model_L1, newx = x_test, s = best_L1_lambda, type = "response")
log_model_test_prediction = prediction(prediction_test, test_data.log$Default)

roc_test = performance(log_model_test_prediction,"tpr","fpr")
plot(roc_test, col = 'red', main = 'Basic Logistic Model Test ROC (red) vs. NN Model Test ROC (blue)')
plot(roc_test.nn, add = TRUE, col = 'blue')
legend(x = "bottomright", legend = c("Logistic Model ROC", "NN Model ROC"), lty = c(1, 1), col = c("red", "blue"), lwd = 1)
abline(a = 0, b = 1)
