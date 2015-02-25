library(caret)
setwd("C:/Joel_Work/Coursera/6_Practical_Machine_Learning_JH/Assignments")

training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")
#training = read.csv("pml-training.csv", stringsAsFactors=FALSE)
#testing = read.csv("pml-testing.csv", stringsAsFactors=FALSE)

#Restrict training data to values of "no" in the new_window field
training = training[training$new_window == "no",]
testing = testing[testing$new_window == "no",]

#Remove columns that have all values of "NA"
training <- training[,colSums(is.na(training))<nrow(training)]
testing <- testing[,colSums(is.na(testing))<nrow(testing)]

#Remove columns that have all blank values
#How is colSums working here? Is it summing each column and then checking for a blank total? What if it was searching for 0 and there were two records with values of 3 and -3?
training <- training[,colSums(training == "")<nrow(training)]
testing <- testing[,colSums(testing == "")<nrow(testing)]

#Someone also said that we can remove both types of missing with "something like na.strings=c("","NA")"

#Change the format type of a few variables in the test set:
testing$magnet_dumbbell_z = as.numeric(testing$magnet_dumbbell_z)
testing$magnet_forearm_y = as.numeric(testing$magnet_forearm_y)
testing$magnet_forearm_z = as.numeric(testing$magnet_forearm_z)
training$classe = as.factor(training$classe)

qplot(training$X, training$classe)

#nsv = nearZeroVar(training,saveMetrics = TRUE)
#nsv

#Now drop that first column:
training$X = NULL
testing$X = NULL
#Have to EITHER take out these columns, or set them as non-factors, or maybe factors with the same level in testing and training, to get the randomForest package to run:
#training$user_name = NULL
#testing$user_name = NULL
#training$cvtd_timestamp = NULL
#testing$cvtd_timestamp = NULL
#training$raw_timestamp_part_1 = NULL
#testing$raw_timestamp_part_1 = NULL
#training$raw_timestamp_part_2 = NULL
#testing$raw_timestamp_part_2 = NULL
#training$new_window = NULL
#testing$new_window = NULL
#training$num_window = NULL
#testing$num_window = NULL

#str(training)
#str(testing)

set.seed(1)

#split data using a single partition
validation_split = createDataPartition(training$classe, p = .98)[[1]]
training = training[validation_split,]
validation = training[-validation_split,]

#rpart (caret): The below code only results in nodes that end in A, B, C, and E. Accuracy = 50%
#rf (caret): xxx
#gbm (caret)
#randomForest: 500 trees, 7 variables tried at each split, OOB estimate of error rate = 0.06%

#modFit_rpart = train(classe ~., method = "rpart", data = training)
#print(modFit_rpart$finalModel)
#plot(modFit_rpart$finalModel, uniform = TRUE, main = "Classification Tree")
#text(modFit_rpart$finalModel, use.n=TRUE, all=TRUE, cex = 0.8)

#train_predict_rpart = predict(modFit_rpart,training)
#cf_train_rpart = confusionMatrix(train_predict_rpart, training$classe)

#validation_predict_rpart = predict(modFit_rpart,validation)
#cf_validation_rpart = confusionMatrix(validation_predict_rpart, validation$classe)


#modFit_rf = train(classe ~., method = "rf", data = training, prox = TRUE)
#print(modFit_rf$finalModel)
#plot(modFit_rf$finalModel, uniform = TRUE, main = "Classification Tree")
#text(modFit_rf$finalModel, use.n=TRUE, all=TRUE, cex = 0.8)
#train_predict_rf = predict(modFit_rf,validation)
#cf_train_rf = confusionMatrix(train_predict_rf, validation$classe)


#modFit_gbm = train(classe ~., method = "gbm", data = training, verbose = FALSE)
#print(modFit_gbm$finalModel)
#plot(modFit_gbm$finalModel, uniform = TRUE, main = "Classification Tree")
#text(modFit_gbm$finalModel, use.n=TRUE, all=TRUE, cex = 0.8)
#train_predict_gbm = predict(modFit_gbm,validation)
#cf_train_gbm = confusionMatrix(train_predict_gbm, validation$classe)

#cf_train_rpart
#cf_validation_rpart



library(randomForest)
modFit_randomForest = randomForest(classe ~.,data = training)
print(modFit_randomForest)
plot(modFit_randomForest,log="y")
randomForest_prediction = predict(modFit_randomForest, newdata = testing)
randomForest_prediction
#MDSplot(modFit_randomForest, training$classe)
#summary(modFit_randomForest)
require(tree)
tree(formula = classe ~., data = training)


train_predict_randomForest = predict(modFit_randomForest,training)
cf_train_randomForest = confusionMatrix(train_predict_randomForest, training$classe)

validation_predict_randomForest = predict(modFit_randomForest,validation)
cf_validation_randomForest = confusionMatrix(validation_predict_randomForest, validation$classe)

cf_train_randomForest
cf_validation_randomForest

#kfolds_control = trainControl(method = "cv", number =10)
#modFit = train(classe~., data = training, method = "rpart", trControl = kfolds_control)
#k_folds_predict = predict(modFit, training)
#cf_kfolds = confusionMatrix(k_folds_predict, training$classe)
#cf_kfolds


#for(i in 1:length(randomForest_prediction)){
#  filename = paste0("randomForest_prediction_",i,".txt")
#  write.table(randomForest_prediction[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
#}