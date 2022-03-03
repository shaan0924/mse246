#Neural Net
#install.packages("ROCR")
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
  nn_prediction = compute(nn,data)
  nn_prediction = as.vector(nn_prediction$net.result)
  nn_prediction = prediction(nn_prediction, data$Default)
  auc = unlist(slot(performance(nn_prediction, 'auc'), 'y.values'))
  return(auc)
}

nn_log = neuralnet(Default ~., data = train_data, hidden= c(3), linear.output = FALSE)
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
arch_list = c(c(3), c(5), c(2,1), c(3,1), c(3,2), c(4,1), c(4,2), c(6,2), c(8,4))

nn1 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[1], linear.output = FALSE)
nn2 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[2], linear.output = FALSE)
nn3 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[3], linear.output = FALSE)
nn4 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[4], linear.output = FALSE)
nn5 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[5], linear.output = FALSE)
nn6 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[6], linear.output = FALSE)
nn7 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[7], linear.output = FALSE)
nn8 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[8], linear.output = FALSE)
nn9 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[9], linear.output = FALSE)

auc_val_arch =c(auc_compute(nn1,validation_data),
                auc_compute(nn2,validation_data),
                auc_compute(nn3,validation_data),
                auc_compute(nn4,validation_data),
                auc_compute(nn5,validation_data),
                auc_compute(nn6,validation_data),
                auc_compute(nn7,validation_data),
                auc_compute(nn8,validation_data),
                auc_compute(nn9,validation_data))
auc_val_arch
best_arch_index = which.max(auc_val_arch)
best_auc.val = max(auc_val_arch)
best_arch = arch_list[best_arch_index]

#########################
#EPOCHS

epoch_auc.train = vector()
epoch_auc.val = vector()
count = vector()
for(i in 1:10){
  epoch_nn = neuralnet(Default ~., data = train_data, hidden= best_arch,rep = i, linear.output = FALSE)
  auc.train = auc_compute(epoch_nn,train_data)
  auc.val = auc_compute(epoch_nn,validation_data)
  epoch_auc.train = append(epoch_auc.train, auc.train)
  epoch_auc.val = append(epoch_auc.val, auc.val)
}
epoch_auc.train
epoch_auc.val

best_epoch = which.max(epoch_auc.val)
best_epoch_auc.val = max(epoch_auc.val)

##########################
#Final Model

best_nn = neuralnet(Default ~., data = train_data, hidden= best_arch, rep = best_epoch, linear.output = FALSE)
best_auc.val = auc_compute(best_nn,validation_data)
best_auc.test = auc_compute(best_nn,test_data)

best_auc.val
best_auc.test
#save(best_nn, file = "trained_nn.RData")

##############################
#EVALUATION
#Explain fitting results via LOO tests
#install.packages("caret")
library(caret)
load("trained_nn.RData")
##AUC
auc_compute(best_nn,test_data)
##ROC Curve
nn_pred.train = compute(best_nn,train_data)
nn_pred.train = as.vector(nn_pred.train$net.result)
nn_pred.train = prediction(nn_pred.train, train_data$Default)

nn_pred.test = compute(best_nn,test_data)
nn_pred.test = as.vector(nn_pred.test$net.result)
nn_pred.test = prediction(nn_pred.test, test_data$Default)

roc_train.nn = performance(nn_pred.train,"tpr","fpr")
roc_test.nn = performance(nn_pred.test,"tpr","fpr")
plot(roc_train.nn, col = 'red', main = 'NN Model Training ROC (red) vs. NN Test ROC (blue)')
plot(roc_test.nn, add = TRUE, col = 'blue')
abline(a = 0, b = 1)

plot(roc_test, col = 'red', main = 'Basic Logistic Model Test ROC (red) vs. NN Model Test ROC (blue)')
plot(roc_test.nn, add = TRUE, col = 'blue')
abline(a = 0, b = 1)