#Neural Net
#install.packages("neuralnet")
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
nn_tanh = neuralnet(Default ~., data = train_data, hidden= c(3),  act.fct = 'tanh', linear.output = FALSE)
#nn_softplus = neuralnet(Default ~., data = train_data, hidden= c(3), act.fct = softplus, linear.output = FALSE)
#nn_swish = neuralnet(Default ~., data = train_data, hidden= c(3),  act.fct = swish, linear.output = FALSE)

#Training AUC
auc_compute(nn_log,train_data)
#Validation AUC
auc_train =c(auc_compute(nn_log,train_data),
             auc_compute(nn_tanh,train_data))
#auc_compute(nn_softplus,train_data),
#auc_compute(nn_swish,train_data))
auc_val =c(auc_compute(nn_log,validation_data),
           auc_compute(nn_tanh,validation_data),
           auc_compute(nn_softplus,validation_data),
           auc_compute(nn_swish,validation_data))
best_auc = which.max(auc_val)
#2 tanh

#...

#Validation: Hidden Variables  with Default 2 hidden layers & best Act Fn
auc_arch = vector()
arch_list = c(c(3), c(5), c(2,1), c(4,2), c(6,2), c(8,4), c(16,8), c(6,3,2))

nn1 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[1], threshold = .05, act.fct = 'tanh', linear.output = FALSE)
nn2 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[2], threshold = .05, act.fct = 'tanh', linear.output = FALSE)
nn3 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[3], threshold = .05, act.fct = 'tanh', linear.output = FALSE)
nn4 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[4], threshold = .05, act.fct = 'tanh', linear.output = FALSE)
nn5 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[5], threshold = .05, act.fct = 'tanh', linear.output = FALSE)
nn6 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[6], threshold = .04, act.fct = 'tanh', linear.output = FALSE)
nn7 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[7], threshold = .04, act.fct = 'tanh', linear.output = FALSE)
nn8 = neuralnet(Default ~., data = train_data_2, hidden= arch_list[8], threshold = .02, act.fct = 'tanh', linear.output = FALSE)

auc_val_arch =c(auc_compute(nn1,validation_data),
                auc_compute(nn2,validation_data),
                auc_compute(nn3,validation_data),
                auc_compute(nn4,validation_data),
                auc_compute(nn5,validation_data),
                auc_compute(nn6,validation_data),
                auc_compute(nn7,validation_data),
                auc_compute(nn8,validation_data))
auc_val_arch
best_arch_index = which.max(AUC_arch)
best_arch_AUC = max(AUC_arch)
#best_arch = architectures[best_arch_index]
best_arch = c(4)

best_nn = neuralnet(Default ~., data = train_data, hidden= best_arch, threshold = .05, act.fct = 'tanh', linear.output = FALSE)
best_auc = auc_compute(best_nn,test_data)

save(best_nn, file = "trained_model.RData")
#First attempt to scale in training to avoid that issue
#m <- colMeans(training)
#s <- apply(training, 2, sd)
#training <- scale(training, center = m, scale = s)
#test <- scale(test, center = m, scale = s)

# then attempt 
##############################
#EVALUATION
#Explain fitting results via LOO tests
#install.packages("caret")
library(caret)
load("trained_model.RData")
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