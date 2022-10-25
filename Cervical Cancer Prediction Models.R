#The following Models predicts the Predictive Proabability of a Woman Having Cervical Cancer when Biopsy is conducted on them

#############################Logistic Regression Model#######################################################

library(readxl)
library(fastDummies)
#Read data
df_lr<- read_excel("Cervical Cancer Dataset.xlsx")

# Remove column STDs:genital herpes, STDs:molluscum contagiosum, STDs:AIDS, STDs:Hepatitis B, 
#STDs:HPV cause they contain only one factor aftering spliting into train data. 

df_lr$Smokes <- as.factor(df_lr$Smokes)
df_lr$`Hormonal Contraceptives` <- as.factor(df_lr$`Hormonal Contraceptives`)
df_lr$IUD <- as.factor(df_lr$IUD)
df_lr$STDs <- as.factor(df_lr$STDs)
df_lr$`STDs:condylomatosis`<- as.factor(df_lr$`STDs:condylomatosis`)
df_lr$`STDs:cervical condylomatosis`<-NULL
df_lr$`STDs:vaginal condylomatosis` <- as.factor(df_lr$`STDs:vaginal condylomatosis`)
df_lr$`STDs:vulvo-perineal condylomatosis` <- as.factor(df_lr$`STDs:vulvo-perineal condylomatosis`)
df_lr$`STDs:syphilis` <- as.factor(df_lr$`STDs:syphilis`)
df_lr$`STDs:pelvic inflammatory disease` <- as.factor(df_lr$`STDs:pelvic inflammatory disease`)
df_lr$`STDs:genital herpes` <- NULL
df_lr$`STDs:molluscum contagiosum` <- NULL
df_lr$`STDs:AIDS` <- NULL
df_lr$`STDs:HIV` <- as.factor(df_lr$`STDs:HIV`)
df_lr$`STDs:Hepatitis B` <- NULL
df_lr$`STDs:HPV`<- NULL
df_lr$...1 <- NULL
df_lr$Citology <- NULL
df_lr$Schiller <- NULL
df_lr$Hinselmann <- NULL
df_lr$Biopsy<-as.numeric(df_lr$Biopsy)

#Split the data into train and test data
set.seed(12345)
inTrain <- sample(nrow(df_lr), 0.7*nrow(df_lr))
lr_train <- data.frame(df_lr[inTrain,])
lr_test <- data.frame(df_lr[-inTrain,])

#Build logistic regression model
lr_fit <-glm(lr_train$Biopsy~.,data=lr_train,family = "binomial")
summary(lr_fit)

#Compute the accuracy, sensitivity, specificity, PPV, NPV of train data
Actual_train <- lr_train$Biopsy
predicted.probability.train <- predict(lr_fit, type = "response") 
## Note the predictions are probabilities
cutoff <- 0.5
Predicted_train <- ifelse(predicted.probability.train > cutoff, "T","F")
Predicted_train <- factor(Predicted_train,levels=c("F","T"))

#Confusion Matrix of Train Data 
((CM_train_lr <- table(Actual_train,Predicted_train)))
#Accuracy of Train Data
(AC_train_lr = (CM_train_lr[1,1]+CM_train_lr[2,2]) / sum(CM_train_lr))
#Error rate of train data
(Error_Rate_train_lr=1-AC_train_lr)
# Sensitivity of Train Data
(sensitivity_train_lr <- sum(Predicted_train == "T" & Actual_train == "T")/sum(Actual_train == "T"))
## Specificity of Train Data
(specificity_train_lr <- sum(Predicted_train == "F" & Actual_train == "F")/sum(Actual_train == "F"))
## PPV of Train Data
(PPV_train_lr <- sum(Predicted_train == "T" & Actual_train == "T")/sum(Predicted_train == "T"))
## NPV of Train Data
(NPV_train_lr <- sum(Predicted_train == "F" & Actual_train == "F")/sum(Predicted_train == "F"))

#Compute the accuracy, sensitivity, specificity, PPV, NPV of test data
Actual_test <- lr_test$Biopsy
predicted.probability.test <- predict(lr_fit, newdata = lr_test, type = "response") 
cutoff <- 0.5
Predicted_test <- ifelse(predicted.probability.test > cutoff, "T","F")
Predicted_test <- factor(Predicted_test,levels=c("F","T"))

#Confusion Matrix of Test Data 
(CM_test_lr <- table(Actual_test,Predicted_test))
## Accuracy of test data
(AC_test_lr = (CM_test_lr[1,1]+CM_test_lr[2,2]) / sum(CM_test_lr))
##Error rate of test data
(Error_Rate_test_lr=1-AC_test_lr)
## Sensitivity
(sensitivity_test_lr <- sum(Predicted_test == "T" & Actual_test == "T")/sum(Actual_test == "T"))
## Specificity
(specificity_test_lr <- sum(Predicted_test == "F" & Actual_test == "F")/sum(Actual_test == "F"))
## PPV
(PPV_test_lr <- sum(Predicted_test == "T" & Actual_test == "T")/sum(Predicted_test == "T"))
## NPV
(NPV_test_lr <- sum(Predicted_test == "F" & Actual_test == "F")/sum(Predicted_test == "F"))

###########################################################################################################




###################################Naive Bayes Model########################################################

#reading data
df_NB <- read_xlsx("Cervical Cancer Dataset.xlsx")
df_NB<-subset(df_NB,select=-c(...1))
#renaming columns
colnames(df_NB)[15] <- "cervical_condylomatosis"
colnames(df_NB)[22] <- "AIDS"

#removal of columns containing only one category and other category not being used for Analysis
df_NB<-subset(df_NB,select=-c(cervical_condylomatosis, AIDS, Citology, Schiller, Hinselmann))

#conversion of categorical columns to categories
df_NB$Smokes <- as.factor(df_NB$Smokes)
df_NB$`Hormonal Contraceptives` <- as.factor(df_NB$`Hormonal Contraceptives`)
df_NB$IUD <- as.factor(df_NB$IUD)
df_NB$STDs <- as.factor(df_NB$STDs)
df_NB$`STDs:condylomatosis`<- as.factor(df_NB$`STDs:condylomatosis`)
df_NB$`STDs:vaginal condylomatosis` <- as.factor(df_NB$`STDs:vaginal condylomatosis`)
df_NB$`STDs:vulvo-perineal condylomatosis` <- as.factor(df_NB$`STDs:vulvo-perineal condylomatosis`)
df_NB$`STDs:syphilis` <- as.factor(df_NB$`STDs:syphilis`)
df_NB$`STDs:pelvic inflammatory disease` <- as.factor(df_NB$`STDs:pelvic inflammatory disease`)
df_NB$`STDs:genital herpes` <- as.factor(df_NB$`STDs:genital herpes`)
df_NB$`STDs:molluscum contagiosum` <- as.factor(df_NB$`STDs:molluscum contagiosum`)
df_NB$`STDs:HIV` <- as.factor(df_NB$`STDs:HIV`)
df_NB$`STDs:Hepatitis B` <- as.factor(df_NB$`STDs:Hepatitis B`)
df_NB$`STDs:HPV`<- as.factor(df_NB$`STDs:HPV`)
View(df_NB)


#splitting into train and test
set.seed(12345)
train <- sample(nrow(df_NB),0.7*nrow(df_NB))
dftrain_NB <- df_NB[train,]
dfvalidation_NB <- df_NB[-train,]

# We require the library e1071 for Naive Bayes Model
library(e1071)
model_NB <- naiveBayes(Biopsy~., data=dftrain_NB)
model_NB
prediction <- predict(model_NB, newdata = dfvalidation[,-25])
table_NB <-table(dfvalidation_NB$Biopsy,prediction,dnn=list('actual','predicted'))
model_NB$apriori

#
# For class probabilities
predicted.probability <- predict(model_NB, newdata = dfvalidation_NB[,-25], type="raw")
#
# The first column is class 0, the second is class 1
B <- as.numeric(dfvalidation_NB$Biopsy)-1
prob <- predicted.probability[,2]
dfB1 <- data.frame(prediction, B, prob)

#Confusion Matrix of NB Model
table_NB
#Accuracy of the Model
(accuracy <- (table[1,1] + table[2,2])/ sum(table))
#Error Rate of the Model
(error_rate<-1-accuracy)
#Sensitivity of the Model
(sensitivity<-table[2,2]/sum(table[2,]))
#Specificity of the Model
(specificity<-table[1,1]/sum(table[1,]))
#PPV of the Model
(ppv<-table[2,2]/sum(table[,2]))
#NPV of the Model
(npv<-table[1,1]/sum(table[,1]))

##########################################################################################################





##################################KNN Model###############################################################
df_knn <- read_excel('Cervical Cancer Dataset.xlsx')
df_knn <- df_knn[-c(1)]
df_knn <- df_knn[-c(27:29)]
df_knn <- df_knn[-c(15)]
df_knn <- df_knn[-c(21)]
View(df_knn)
fun <- function(x){ 
  a <- mean(x) 
  b <- sd(x) 
  (x - a)/(b) 
} 


df_knn[,1:4] <- apply(df_knn[,1:4], 2, fun)
df_knn[,6:7] <- apply(df_knn[,6:7], 2, fun)
df_knn[,9] <- apply(df_knn[,9], 2, fun)
df_knn[,11] <- apply(df_knn[,11], 2, fun)
df_knn

library("caret")
set.seed(12345)
inTrain <- sample(nrow(df_knn), 0.7*nrow(df_knn))
#
dftrain_knn <- data.frame(df_knn[inTrain,])
dfvalidation_knn <- data.frame(df_knn[-inTrain,])


library(class)
# 
train_input <- as.matrix(dftrain_knn[,-25])
train_output <- as.vector(dftrain_knn[,25])
validate_input <- as.matrix(dfvalidation_knn[,-25])


kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)

set.seed(3)
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output, k=i)
  prediction2 <- knn(train_input, validate_input,train_output, k=i)
  #
  # The confusion matrix for training data is:
  CM1 <- table(prediction, dftrain_knn$Biopsy)
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(prediction2, dfvalidation_knn$Biopsy)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}

plot(c(1,kmax),c(0,0.1),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(9, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)

# Scoring at optimal k
prediction <- knn(train_input, train_input,train_output, k=z)
prediction2 <- knn(train_input, validate_input,train_output, k=z,prob=T)
predicted.probability.knn <- attr(prediction2, "prob")
Predicted_class <- knn(train_input,validate_input,train_output, k=i)
predicted.probability.knn <- ifelse(Predicted_class ==1, predicted.probability.knn, 1-predicted.probability.knn)


#Confusion Matrix of Train Data 
(CM1 <- table(prediction, dftrain_knn$Biopsy))
#Accuracy of Train Data
(AC_train_knn = (CM1[1,1]+CM1[2,2]) / sum(CM1))
#Error rate of train data
(Error_Rate_train_knn=1-AC_train_knn)
# Sensitivity of Train Data
(sensitivity_train_knn <- (CM1[2,2]/sum(CM1[2,])))
## Specificity of Train Data
(specificity_train_knn <- (CM1[1,1]/sum(CM1[1,])))
## PPV of Train Data
(PPV_train_knn <- (CM1[2,2]/sum(CM1[,2])))
## NPV of Train Data
(NPV_train_knn <-(CM1[1,1]/sum(CM1[,1])))


#Confusion Matrix of Test Data 
(CM2 <- table(prediction2, dfvalidation_knn$Biopsy))
## Accuracy of test data
(AC_test_knn = (CM2[1,1]+CM2[2,2]) / sum(CM2))
##Error rate of test data
(Error_Rate_test_knn=1-AC_test_knn)
## Sensitivity
(sensitivity_test_knn <- (CM2[2,2]/sum(CM2[2,])))
## Specificity
(specificity_test_knn <- (CM2[1,1]/sum(CM2[1,])))
## PPV
(PPV_test_knn <-(CM2[2,2]/sum(CM2[,2])))
## NPV
(NPV_test_knn <- (CM2[1,1]/sum(CM2[,1])))


###########################################################################################################




######################################Classification Tree Models###########################################

df_tree <- read_excel("Cervical Cancer Dataset.xlsx")

# remove unwanted columns
df_tree$Hinselmann <- NULL
df_tree$Schiller <- NULL
df_tree$Citology <- NULL
df_tree[1] <- NULL

# create factors
df_tree$Smokes <- as.factor(df_tree$Smokes )
df_tree$`Hormonal Contraceptives`<- as.factor(df_tree$`Hormonal Contraceptives`)
df_tree$IUD<- as.factor(df_tree$IUD)
df_tree$STDs<- as.factor(df_tree$STDs)
df_tree$`STDs:condylomatosis`<- as.factor(df_tree$`STDs:condylomatosis`)
df_tree$`STDs:cervical condylomatosis`<- as.factor(df_tree$`STDs:cervical condylomatosis`)
df_tree$`STDs:vaginal condylomatosis`<- as.factor(df_tree$`STDs:vaginal condylomatosis`)
df_tree$`STDs:vulvo-perineal condylomatosis`<- as.factor(df_tree$`STDs:vulvo-perineal condylomatosis`)
df_tree$`STDs:syphilis`<- as.factor(df_tree$`STDs:syphilis`)
df_tree$`STDs:pelvic inflammatory disease`<- as.factor(df_tree$`STDs:pelvic inflammatory disease`)
df_tree$`STDs:genital herpes`<- as.factor(df_tree$`STDs:genital herpes`)
df_tree$`STDs:molluscum contagiosum`<- as.factor(df_tree$`STDs:molluscum contagiosum`)
df_tree$`STDs:AIDS`<- as.factor(df_tree$`STDs:AIDS`)
df_tree$`STDs:HIV`<- as.factor(df_tree$`STDs:HIV`)
df_tree$`STDs:Hepatitis B`<- as.factor(df_tree$`STDs:Hepatitis B`)
df_tree$`STDs:HPV`<- as.factor(df_tree$`STDs:HPV`)

df_tree$Biopsy <- as.factor(df_tree$Biopsy)

# split the data
set.seed(12345)
inTrain <- sample(nrow(df_tree),0.7*nrow(df_tree)) 
dftrain_tree <- data.frame(df_tree[inTrain,])
dftest_tree <- data.frame(df_tree[-inTrain,])

# build a classification tree model using the training data, with Biopsy as the output variable.
library(tree)
tree.biopsy=tree(Biopsy~.,dftrain_tree) 
summary(tree.biopsy)

# plot the unpruned tree
plot(tree.biopsy) 
text(tree.biopsy,pretty=0)

# predict train data
train.pred = predict(tree.biopsy, type = 'class')
actual_train = dftrain_tree$Biopsy

#Confusion matrix of Train Data
(CM_train_tree = table(dftrain_tree$Biopsy, train.pred))
# Accuracy of Train
(AC_train_tree = (CM_train_tree [1,1] +CM_train_tree[2,2] )/ sum(CM_train_tree))
# Training sensitivity
(Training_sensitivity <- sum(train.pred == "1" & actual_train == "1")/sum(actual_train == "1"))
# Training specificity
(Training_specificity <- sum(train.pred == "0" & actual_train == "0")/sum(actual_train == "0"))
# Training PPV
(Training_PPV <- sum(train.pred == "1" & actual_train == "1")/sum(train.pred == "1"))
# Training NPV
(Training_NPV <- sum(train.pred == "0" & actual_train == "0")/sum(train.pred == "0"))

# predict test data
test.pred = predict(tree.biopsy, newdata = dftest_tree, type = 'class')
actual_test = dftest_tree$Biopsy

#Confusion matrix of Test Data
(CM_test = table(actual_test, test.pred))
# Accuracy
(AC_test = (CM_test [1,1] +CM_test[2,2] )/ sum(CM_test))
# Test sensitivity
(Test_sensitivity <- sum(test.pred == "1" & actual_test == "1")/sum(actual_test == "1"))
# Test specificity
(Test_specificity <- sum(test.pred == "0" & actual_test == "0")/sum(actual_test == "0"))
# Test PPV
(Test_PPV <- sum(test.pred == "1" & actual_test == "1")/sum(test.pred == "1"))
# Test NPV
(Test_NPV <- sum(test.pred == "0" & actual_test == "0")/sum(test.pred == "0"))



# pruned tree
set.seed(12345)
cv.biopsy = cv.tree(tree.biopsy, FUN = prune.misclass)
names(cv.biopsy) 
cv.biopsy

# find the best tree
plot(cv.biopsy$size, cv.biopsy$dev, type='b')
prune.biopsy = prune.misclass(tree.biopsy, best = 5)
plot(prune.biopsy)
text(prune.biopsy, pretty=0)

test.pruned.pred = predict(prune.biopsy, newdata = test, type = 'class')
actual_pruned_test = test$Biopsy
(pruned.CM = table(actual_pruned_test, test.pruned.pred))

# Accuracy of the pruned tree
(pruned.AC = (CM [1,1] +CM[2,2] )/ sum(CM))
# Test sensitivity
(pruned_sensitivity <- sum(test.pruned.pred == "1" & actual_pruned_test == "1")/sum(actual_pruned_test == "1"))
# Test specificity
(pruned_specificity <- sum(test.pruned.pred == "0" & actual_pruned_test == "0")/sum(actual_pruned_test == "0"))
# Test PPV
(pruned_PPV <- sum(test.pruned.pred == "1" & actual_pruned_test == "1")/sum(test.pruned.pred == "1"))
# Test NPV
(pruned_NPV <- sum(test.pruned.pred == "0" & actual_pruned_test == "0")/sum(test.pruned.pred == "0"))


###########################################################################################################




####################################Random Forest & Bagging Model#################################################

library(randomForest)

setwd("C:/Users/tanve/OneDrive/Documents/Semester 2/Data Mining and Predictive Analytics/Group Project")
df_rf<-read.csv("Cervical Cancer Dataset.csv")
df_rf<-df_rf[,-(29:31)]
df_rf[,"Smokes"]<-as.factor(df_rf[,"Smokes"])
df_rf[,"Hormonal.Contraceptives"]<-as.factor(df_rf[,"Hormonal.Contraceptives"])
df_rf[,"IUD"]<-as.factor(df_rf[,"IUD"])
df_rf[,"STDs"]<-as.factor(df_rf[,"STDs"])
df_rf[,"STDs.condylomatosis"]<-as.factor(df_rf[,"STDs.condylomatosis"])
df_rf[,"STDs.vaginal.condylomatosis"]<-as.factor(df_rf[,"STDs.vaginal.condylomatosis"])
df_rf[,"STDs.vulvo.perineal.condylomatosis"]<-as.factor(df_rf[,"STDs.vulvo.perineal.condylomatosis"])
df_rf[,"STDs.syphilis"]<-as.factor(df_rf[,"STDs.syphilis"])
df_rf[,"STDs.pelvic.inflammatory.disease"]<-as.factor(df_rf[,"STDs.pelvic.inflammatory.disease"])
df_rf[,"STDs.genital.herpes"]<-as.factor(df_rf[,"STDs.genital.herpes"])
df_rf[,"STDs.molluscum.contagiosum"]<-as.factor(df_rf[,"STDs.molluscum.contagiosum"])
df_rf[,"STDs.HIV"]<-as.factor(df_rf[,"STDs.HIV"])
df_rf[,"STDs.Hepatitis.B"]<-as.factor(df_rf[,"STDs.Hepatitis.B"])
df_rf[,"STDs.HPV"]<-as.factor(df_rf[,"STDs.HPV"])
df_rf[,"Dx.df_rf"]<-as.factor(df_rf[,"Dx.df_rf"])
df_rf[,"Dx.CIN"]<-as.factor(df_rf[,"Dx.CIN"])
df_rf[,"Dx.HPV"]<-as.factor(df_rf[,"Dx.HPV"])
df_rf[,"Dx"]<-as.factor(df_rf[,"Dx"])
df_rf[,"Biopsy"]<-as.factor(df_rf[,"Biopsy"])


# Bagging
set.seed(1)
train = sample(nrow(df_rf), nrow(df_rf)*0.5)
(bag.cancer=randomForest(Biopsy~.,data=df_rf,subset=train,mtry=28,importance=T))
yhat.bag = predict(bag.cancer,newdata=df_rf[-train,])
cancer.test=df_rf[-train,"Biopsy"]

#Confusion Matrix of model
(c = table(cancer.test,yhat.bag))
#Accuracy of model
(acc = (c[1,1]+c[2,2])/sum(c))
#error rate of model
(err_bag<-1-acc)
#Sensitivity of model
(sens_bag<-(c[2,2]/sum(c[2,])))
#specificity of model
(spec_bag<-(c[1,1]/sum(c[1,])))
#ppv of model
(ppv_bag<-(c[2,2]/sum(c[,2])))
#npv of model
(npv_bag<-(c[1,1]/sum(c[,1])))

importance(bag.cancer)
varImpPlot(bag.cancer)

#
# Randomforest

(rf.cancer=randomForest(Biopsy~.,data=df_rf,subset=train,mtry=5,importance=T))
yhat.rf = predict(rf.cancer,newdata=df_rf[-train,])
cancer.test=df_rf[-train,"Biopsy"]


#Confusion Matrix of model
(c1 = table(cancer.test,yhat.rf))
#Accuracy of model
(acc = (c1[1,1]+c1[2,2])/sum(c1))
#error rate of model
(err_rf<-1-acc)
#Sensitivity of model
(sens_rf<-(c1[2,2]/sum(c1[2,])))
#specificity of model
(spec_rf<-(c1[1,1]/sum(c1[1,])))
#ppv of model
(ppv_rf<-(c1[2,2]/sum(c1[,2])))
#npv of model
(npv_rf<-(c1[1,1]/sum(c1[,1])))


importance(rf.cancer)
varImpPlot(rf.cancer)

###########################################################################################################





###############################################Boosting####################################################
df_bs<-read_excel("Cervical Cancer Dataset.xlsx")
summary(df_bs)

# remove unwanted columns
df_bs$Hinselmann <- NULL
df_bs$Schiller <- NULL
df_bs$Citology <- NULL
df_bs[1] <- NULL


# create factors
df_bs$Smokes <- as.factor(df_bs$Smokes )
df_bs$`Hormonal Contraceptives`<- as.factor(df_bs$`Hormonal Contraceptives`)
df_bs$IUD<- as.factor(df_bs$IUD)
df_bs$STDs<- as.factor(df_bs$STDs)
df_bs$`STDs:condylomatosis`<- as.factor(df_bs$`STDs:condylomatosis`)
df_bs$`STDs:cervical condylomatosis`<- as.factor(df_bs$`STDs:cervical condylomatosis`)
df_bs$`STDs:vaginal condylomatosis`<- as.factor(df_bs$`STDs:vaginal condylomatosis`)
df_bs$`STDs:vulvo-perineal condylomatosis`<- as.factor(df_bs$`STDs:vulvo-perineal condylomatosis`)
df_bs$`STDs:syphilis`<- as.factor(df_bs$`STDs:syphilis`)
df_bs$`STDs:pelvic inflammatory disease`<- as.factor(df_bs$`STDs:pelvic inflammatory disease`)
df_bs$`STDs:genital herpes`<- as.factor(df_bs$`STDs:genital herpes`)
df_bs$`STDs:molluscum contagiosum`<- as.factor(df_bs$`STDs:molluscum contagiosum`)
df_bs$`STDs:AIDS`<- as.factor(df_bs$`STDs:AIDS`)
df_bs$`STDs:HIV`<- as.factor(df_bs$`STDs:HIV`)
df_bs$`STDs:Hepatitis B`<- as.factor(df_bs$`STDs:Hepatitis B`)
df_bs$`STDs:HPV`<- as.factor(df_bs$`STDs:HPV`)

df_bs$Biopsy <- as.factor(df_bs$Biopsy)


#Boosting
library(gbm)
set.seed(1)
train <- sample(nrow(df_bs),0.7*nrow(df_bs))
dftrain_bs <- df_bs[train,]
dftest_bs <- df_bs[-train,]


df_bs.boost=gbm(Biopsy~.,data=dftrain_bs,distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(df_bs.boost)


par(mfrow=c(1,2))
plot(df_bs.boost,i="Age")


yhat.boost=predict(df_bs.boost,newdata=dftest_bs,n.trees=5000,type="response")
predicted <- ifelse(yhat.boost>=0.5,1,0)
yhat.test=df_bs$Biopsy[-train]


(c = table(predicted,yhat.test))
(acc = (c[1,1]+c[2,2])/sum(c))










###################################Comparison Between AUC Curves#################################################################



library(pROC)
roc_rose <- plot(roc(lr_test$Biopsy, predicted.probability.test), print.auc = TRUE, 
                 col = "green",legacy.axes=T)

roc_rose <- plot(roc(dfvalidation_NB$Biopsy,prob), print.auc = TRUE, 
                 col = "blue", print.auc.y = .4, add = TRUE, legacy.axes=T)

roc_rose <- plot(roc(dfvalidation_knn$Biopsy, predicted.probability.knn), print.auc = TRUE, 
                 col = "orange", print.auc.y = .3, add = TRUE, legacy.axes=T)



########################################THE END###########################################################

