##Clustering Analysis prepare data V4
##KS test,t-test,et al. Select Rule
## DA Method
#getwd()   					
rm(list = ls())
#setwd('/Users/ksj/Desktop/B00_4FGL/_20190715_RF_Gini')
setwd('/home/ksj/work/AAS19081_data_v4')
#getwd()
#dir()
#library(mclust)
par(mfrow = c(1,1))
#options()
library(randomForest)
library(e1071)
library(nnet)

#dir()
#####读取数据，准备数据，training set; test set, predict
#FSRQ_BLLAC
databf32 <- read.csv("gll_psc_v20_select_Col_38_order_bll_fsrq_1825.csv", header = T)
#dim(databf32)
#names(databf32)
databf <- databf32[,c(2:39)]
#dim(databf)
#databf <- subset(databf_0, curve_significance != 0)
#dim(databf)
#names(databf)
#X <- log10(databf[,c(64:70)])
Class <- databf[,c(37)]
#names(X);
#names(Class)


#BCU402
BCU40032  <- read.csv("gll_psc_v20_select_Col_38_order_bcu_1312.csv", header = T)
BCU400 <- BCU40032[,c(2:39)]

#dim(BCU400)
#dim(BCU400)
#Y <- log10(BCU400[,c(64:70)])
Classbcu <- BCU400[,37]
#names(Classbcu)
#names(Y)


xx <- c(1:23)
n=length(xx);n









    func_j <- function(j){
    x3 <- zuhe[,j]
    #n_cal <- n_cal+1
    #cat(n_cal,x3,"\n")
    RF_Accuracy0 <- 0.90
    ANN_Accuracy0 <- 0.890
    SVM_Accuracy0 <- 0.90
    
#RF
#x3 <- c(1,3,4,5,9,14,17,21,22,23)
#ANN
#x3 <- c(6,	8,	9,	13,	14,	19,	22)
#SVM
#x3 <- c(1,	2,	3,	4,	10, 13,	16,	17,	20,	21,	23)
###############################################################################
X <- (databf[,c(x3)]);
Y <- (BCU400[,c(x3)]);
##样本中随机去4/5的源，作为train set, 剩余1/5，为test set;
###############################################################################
set.seed(123)
train <- sample(1:nrow(X),size = round(nrow(X)*4/5),replace = FALSE)
X.train <- X[train,];  Class.train <- Class[train]
X.test  <- X[-train,]; Class.test  <- Class[-train]
###############################################################################
set.seed(123)
bcu_predict <- sample(1:nrow(Y),size = round(nrow(Y)*3/3), replace = FALSE)
Y.bcu_predict      <- Y[bcu_predict,]
Classu.bcu_predict <- Classbcu[bcu_predict]
###############################################################################
Class.train <- as.data.frame(Class.train)
colnames(Class.train) <- c("Optical.Class")
XX.train <- cbind(X.train,Class.train)
#第二种方法，Random Forest discriminate analysis
###############################################################################
library(randomForest)
# model
RF_fit <- randomForest(Optical.Class~.,data=XX.train)
##--------------------------------------------------------------
# test
RF_predct<- predict(RF_fit,newdata=X.test)
RF_perf <- table(Class.test, RF_predct,dnn=c("Actual", "Predicted"))
# predict
RF_predctbcu <- predict(RF_fit,newdata=Y.bcu_predict)
##--------------------------------------------------------------
###############################################################################
###############################################################################
###############################################################################
X <- (databf[,c(x3)]);
Y <- (BCU400[,c(x3)]);
##样本中随机去4/5的源，作为train set, 剩余1/5，为test set;
###############################################################################
set.seed(123)
train <- sample(1:nrow(X),size = round(nrow(X)*4/5),replace = FALSE)
X.train <- X[train,];  Class.train <- Class[train]
X.test  <- X[-train,]; Class.test  <- Class[-train]
###############################################################################
set.seed(123)
bcu_predict <- sample(1:nrow(Y),size = round(nrow(Y)*3/3), replace = FALSE)
Y.bcu_predict      <- Y[bcu_predict,]
Classu.bcu_predict <- Classbcu[bcu_predict]
###############################################################################
Class.train <- as.data.frame(Class.train)
colnames(Class.train) <- c("Optical.Class")
XX.train <- cbind(X.train,Class.train)
#第二种方法，Random Forest discriminate analysis
###############################################################################
library(nnet)
ANN_fit <-  nnet(Optical.Class~.,data=XX.train,size=2,trace=FALSE)
ANN_predct  <-  predict(ANN_fit,X.test,type="class")
ANN_predctbcu <- predict(ANN_fit,Y.bcu_predict,type="class")
ANN_perf <- table(Class.test, ANN_predct, dnn=c("Actual", "Predicted"))
##############################################################################
###############################################################################
###############################################################################
library(e1071)
SVM_fit <- svm(Optical.Class~.,data=XX.train)
SVM_predct <- predict(SVM_fit, na.omit(X.test))
SVM_predctbcu <- predict(SVM_fit, na.omit(Y.bcu_predict))
SVM_perf <- table(Class.test, SVM_predct,dnn=c("Actual", "Predicted"))
###############################################################################
library(e1071)
#Random Forest
RF_Accuracy <- classAgreement(RF_perf)
RF_number   <- table(RF_predctbcu)
RF_number   <- as.data.frame(RF_number)
RF_data <- c(RF_number[1,2],RF_number[2,2],RF_Accuracy[["diag"]])
RF_data


# SVM
SVM_Accuracy <- classAgreement(SVM_perf)
SVM_number   <- table(SVM_predctbcu)
SVM_number   <- as.data.frame(SVM_number)
SVM_data <- c(SVM_number[1,2],SVM_number[2,2],SVM_Accuracy[["diag"]])
SVM_data

# ANN
ANN_Accuracy <- classAgreement(ANN_perf)
ANN_number   <- table(ANN_predctbcu)
ANN_number   <- as.data.frame(ANN_number)
ANN_data <- c(ANN_number[1,2],ANN_number[2,2],ANN_Accuracy[["diag"]])
ANN_data

#print(classAgreement(RF_perf)$diag); print(table(RF_predctbcu))
#print(classAgreement(ANN_perf)$diag); print(table(ANN_predctbcu))
#print(classAgreement(SVM_perf)$diag); print(table(SVM_predctbcu))


#某组参数的总汇
RF_result_data1 <-   c(length(x3),RF_data, x3)
as.data.frame(RF_result_data1)
RF_result_data2 <- t(data.frame(RF_result_data1))

ANN_result_data1 <-   c(length(x3),ANN_data, x3)
as.data.frame(ANN_result_data1)
ANN_result_data2 <- t(data.frame(ANN_result_data1))

SVM_result_data1 <-   c(length(x3),SVM_data, x3)
as.data.frame(SVM_result_data1)
SVM_result_data2 <- t(data.frame(SVM_result_data1))


if(RF_Accuracy[["diag"]] >= RF_Accuracy0) {
  RF_Accuracy0 <- RF_Accuracy[["diag"]]
  write.table(RF_result_data2,
              file = "ML4FGL_parallel_v4_RF_20191003_test.csv",
              sep = ",",col.names = FALSE,append = TRUE )}


if(ANN_Accuracy[["diag"]] >= ANN_Accuracy0) {
  ANN_Accuracy0 <- ANN_Accuracy[["diag"]]
  write.table(ANN_result_data2,
              file = "ML4FGL_parallel_v4_ANN_20191003_test.csv",
              sep = ",",col.names = FALSE,append = TRUE )}

if(SVM_Accuracy[["diag"]] >= SVM_Accuracy0) {
  SVM_Accuracy0 <- SVM_Accuracy[["diag"]]
  write.table(SVM_result_data2,
              file = "ML4FGL_parallel_v4_SVM_20191003_test.csv",
              sep = ",",col.names = FALSE,append = TRUE )}



return (max_A); 
  }





classiftime1 <- proc.time() # record classification time




library(parallel)
# Calculate the number of cores
no_cores <- detectCores();
#no_cores <-24
print('The core of the computer is '); print(no_cores)
# Initiate cluster
#cl <- makeCluster(no_cores)
# 启用parallel作为foreach并行计算的后???
#library(help="doParallel")
library(doParallel)
cl <- makeCluster(no_cores)
registerDoParallel(cl) 

num_sum <- matrix(1:n)
for (i in 2:n){
  zuhe <- combn(xx,i)
  max_A <-  ncol(zuhe)
# 并行计算方式

x <- foreach(j=1:max_A,.combine='rbind') %dopar%  func_j(j)
print(max(x))
num_sum[i] <- max(x)
write.table(num_sum, file = "ML4FGL_thread_line.txt",
            sep = ",",col.names = FALSE)
}
stopCluster(cl)
cat(sum(num_sum))



classiftime <- proc.time() - classiftime1
print(classiftime)
print(sum(x))
print(classiftime[3])
print(sum(x)/classiftime[3])

print("Approximately days")
print(8388607/(sum(num_sum)/classiftime[3]*24*60*60))




