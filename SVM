################ Given code is  for SVM classification ###############

######## installing library for SVM And Confusion matrix respectively ###########
library(e1071) 
library(caret)

data  <- read.csv("file:///D:/ML/Datasets/data.csv")
mdata <- data[,-c(1,33)]

############# randomly shuffle the data set  ####################
shuffle <- mdata[sample(1:nrow(mdata)),]

############ Applying 5 fold CV #################
CV <- cut(1:nrow(mdata),breaks=5, labels=FALSE)

kernals <- c("radial","poly","linear")
best_kernal_acc <- NULL

for(j in kernals)
{
  acc <- NULL
  for(i in 1:5)
  {
    #### changing train and test set for 5 folds ####
    test  <- shuffle [which(i==CV,arr.ind=T),]
    train <- shuffle [-(which(i==CV,arr.ind = T)),]
    
    ######### preparing model with SVM ########
    model <- svm (diagnosis~.,data = train,kernel= j)
    pred  <- predict(model,test)
    cm <- confusionMatrix(pred,test$diagnosis)
    
    #### find accuracy from confusion matrix ####
    accuracy <- as.numeric(cm$overall[1])
    acc <- c(acc,accuracy)
  }
  
  ###### Mean Accuracy for 5 fold ###########
  Kfold_acc <- mean(acc)
  best_kernal_acc <- c( best_kernal_acc, Kfold_acc)
}
c <-  max ( best_kernal_acc)
x <- which.max( best_kernal_acc)
kernal <- (kernals[x])

