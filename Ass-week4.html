install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("rattle")
install.packages("e1071")
install.packages("randomForest")
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(e1071)
library(randomForest)
set.seed(1)

train.url <-
  "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
test.url <- 
  "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

path <- paste(getwd(),"/", "machine", sep="")
train.file <- file.path(path, "machine-train-data.csv")
test.file <- file.path(path, "machine-test-data.csv")
if (!file.exists(train.file)) {
  download.file(train.url, destfile=train.file)
}
if (!file.exists(test.file)) {
  download.file(test.url, destfile=test.file)
}

train.data.raw <- read.csv(train.file, na.strings=c("NA","#DIV/0!",""))
test.data.raw <- read.csv(test.file, na.strings=c("NA","#DIV/0!",""))
train.data.clean1 <- train.data.raw[,8:length(colnames(train.data.raw))]
test.data.clean1 <- test.data.raw[,8:length(colnames(test.data.raw))]

# Drop colums with NAs
train.data.clean1 <- train.data.clean1[, colSums(is.na(train.data.clean1)) == 0] 
test.data.clean1 <- test.data.clean1[, colSums(is.na(test.data.clean1)) == 0] 

# Check for near zero variance predictors and drop them if necessary
nzv <- nearZeroVar(train.data.clean1,saveMetrics=TRUE)
zero.var.ind <- sum(nzv$nzv)

if ((zero.var.ind>0)) {
  train.data.clean1 <- train.data.clean1[,nzv$nzv==FALSE]
}in.training <- createDataPartition(train.data.clean1$classe, p=0.70, list=F)
train.data.final <- train.data.clean1[in.training, ]
validate.data.final <- train.data.clean1[-in.training, ]
control.parms <- trainControl(method="cv", 5)
rf.model <- train(classe ~ ., data=train.data.final, method="rf",
                  trControl=control.parms, ntree=251)
rf.model
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## 
## Summary of sample sizes: 10990, 10990, 10989, 10990, 10989 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9905366  0.9880275  0.001543981  0.001955033
##   27    0.9906094  0.9881210  0.001295651  0.001638512
##   52    0.9828201  0.9782677  0.004693225  0.005936194
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27
rf.predict <- predict(rf.model, validate.data.final)
confusionMatrix(validate.data.final$classe, rf.predict)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1670    1    2    0    1
##          B    5 1130    3    1    0
##          C    0    7 1016    3    0
##          D    0    0    4  957    3
##          E    0    1    2    2 1077
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9941          
##                  95% CI : (0.9917, 0.9959)
##     No Information Rate : 0.2846          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9925          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9970   0.9921   0.9893   0.9938   0.9963
## Specificity            0.9990   0.9981   0.9979   0.9986   0.9990
## Pos Pred Value         0.9976   0.9921   0.9903   0.9927   0.9954
## Neg Pred Value         0.9988   0.9981   0.9977   0.9988   0.9992
## Prevalence             0.2846   0.1935   0.1745   0.1636   0.1837
## Detection Rate         0.2838   0.1920   0.1726   0.1626   0.1830
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9980   0.9951   0.9936   0.9962   0.9976
accuracy <- postResample(rf.predict, validate.data.final$classe)
acc.out <- accuracy[1]

overall.ose <- 
  1 - as.numeric(confusionMatrix(validate.data.final$classe, rf.predict)
                 $overall[1]) ##The accuracy of this model is 0.9940527 and the Overall Out-of-Sample error is 0.0059473
results <- predict(rf.model, 
                   test.data.clean1[, -length(names(test.data.clean1))])
results
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
treeModel <- rpart(classe ~ ., data=train.data.final, method="class")
fancyRpartPlot(treeModel)


