##Clustering Analysis prepare data
##KS test,t-test,et al. Select Rule
## DA Method
getwd()   					
rm(list = ls())
setwd('/Users/ksj/Desktop/A04_AAS14053_R1/AAS10453_Code')
getwd()
dir()
library(mclust)
par(mfrow = c(1,1))
#options()


#####读取数据，准备数据，training set; test set, predict
#FSRQ_BLLAC
databf_0 <- read.csv("Select_Paramters_data8_1018bf.csv", header = T)
dim(databf_0)
databf <- subset(databf_0, curve_significance != 0)
dim(databf)
names(databf)
X <- databf[,c(3:10)]
Class <- databf[,11]
#BCU402
databcu <- read.csv("Select_Paramters_data8_402bcu.csv", header = T)
dim(databcu)
BCU400 <- subset(databcu, Radio.flux.mJy.!=-999)
dim(BCU400)
Y <- BCU400[,c(3:10)]

Classbcu <- BCU400[,11]
names(databcu)
names(Y)

X[,2:3] <- log10(X[,2:3])
X[,5:8] <- log10(X[,5:8])
Y[,2:3] <- log10(Y[,2:3])
Y[,5:8] <- log10(Y[,5:8])
#样本中随机去4/5的源，作为train set, 剩余1/5，为test set;
set.seed(123)
train <- sample(1:nrow(X), 
                size = round(nrow(X)*4/5), replace = FALSE)
X.train <- X[train,]
names(X.train)
Class.train <- Class[train]
names(Class.train)
table(Class.train)
X.test <- X[-train,]
Class.test <- Class[-train]
table(Class.test)

set.seed(123)
bcu_predict <- sample(1:nrow(Y), 
                      size = round(nrow(Y)*3/3), replace = FALSE)
Y.bcu_predict <- Y[bcu_predict,]
Classu.bcu_predict <- Classbcu[bcu_predict]
table(Classu.bcu_predict)

dim(X.train);table(Class.train)
dim(X.test);table(Class.test)
dim(Y.bcu_predict);
#table(Classu.bcu_predict)
############################################################
############################################################
#two function
############################################################
############################################################
performance <- function(table, n=6){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

## Function to evaluate classification performance
class_eval <- function(pred, act, plot=TRUE, ...){
  iact <- as.integer(act)
  ipred <- as.integer(pred)
  acc <- sum(ipred==iact)/length(iact)  # accuracy
  if (isTRUE(plot)){
    plot(jitter(ipred), jitter(iact), pch=20, cex=0.5, 
         xlab='Predicted Class', ylab='True class',lab=c(3,3,1), ...)
    mtext(paste("Accuracy =", round(acc, 3)))
  }
  return(list("Confusion Table"=table("True Class"=iact, 
                                      "Predicted Class"=ipred), Accuracy=acc))
}













#第一种方法，Mclust discriminate analysis "MclustDA"
#第一种方法，Mclust discriminate analysis "MclustDA"
#第一种方法，Mclust discriminate analysis "MclustDA"
#第一种方法，Mclust discriminate analysis "MclustDA"
#第一种方法，Mclust discriminate analysis "MclustDA"

##############################################################
#mod1 <- MclustDA(X.train, Class.train, modelType = "EDDA")
mod1 <- MclustDA(X.train, Class.train, modelType = "MclustDA")
summary(mod1,parameters = T)
summary(mod1)
cv <- cvMclustDA(mod1,nfold = 10)
#cv <- cvMclustDA(mod1)
unlist(cv[c("error", "se")])
mclust.pred <- predict.MclustDA(mod1, 
                                newdata = X.test, 
                                newclass = Class.test)
classError(mclust.pred$classification, 
           Class.test)$errorRate
mclust.perf <- table(Class.test, mclust.pred$classification,
                     dnn=c("Actual", "Predicted"))
mclust.perf

#need Call Function performance();class_eval()
## Function to evaluate classification performance
performance(mclust.perf)
# class_eval(mclust.pred$classification, 
#            Class.test, 
#            main="Mclust Classification")


# plot(mod1, what = "scatterplot")
# plot(mod1, what = "classification")
# plot(mod1, what = "classification", 
#      newdata = Y.bcu_predict)
# plot(mod1, what = "train&test", 
#      newdata = Y.bcu_predict)




#利用mod1的监督学习，去预测BCU
mod2 <- predict.MclustDA(mod1, newdata = Y.bcu_predict,
                         newclass = Classu.bcu_predict)
table(mod2$classification)
#summary(mod1, newdata = Y.bcu_predict, 
#        newclass = Classu.bcu_predict)

pred <- predict(mod1, Y.bcu_predict)
table(pred$classification)


bcu_class <- mod2$classification
t2 <- as.data.frame(bcu_class)
colnames(t2) <- c("Mclust")

total <- cbind(mod2$z, t2)
totalY <- cbind(Y.bcu_predict, total)

totalYY <- totalY[order(bcu_predict), ]

totalYYY <- cbind(BCU400[1:2], totalYY)

write.csv(totalYYY, file = "BCU_predict_result_Mclust.csv")





dim(X.train);table(Class.train)
dim(X.test);table(Class.test)
dim(Y.bcu_predict);table(Classu.bcu_predict)
Class.train <- as.data.frame(Class.train)
colnames(Class.train) <- c("Optical.Class")
XX.train <- cbind(X.train,Class.train)
names(XX.train)
#第二种方法，Random Forest discriminate analysis
##############################################################
library(randomForest)
forestfit <- randomForest(Optical.Class~Spectral.Index+Radio.flux.mJy.
                          +flux_density+curve_significance
                          +flux_100_300_mev+flux_0p3_1_gev
                          +flux_10_100_gev+variability_index,
                          data=XX.train,mtry=2, importance=TRUE, 
                          do.trace=TRUE)
                         # ,ntree=1000)
print(forestfit)
importance(forestfit, type=2)


forestpredct<- predict(forestfit,newdata=X.test) 
#print(forestpredct)
table(forestpredct)

forest.perf <- table(Class.test, forestpredct,
                     dnn=c("Actual", "Predicted"))
forest.perf
performance(forest.perf)

#class_eval(forestpredct, 
#           Class.test, 
#            main="RandomForest Classification")

forestpredctbcu <- predict(forestfit,newdata=Y.bcu_predict) 
table(forestpredctbcu)

forest_class <- as.data.frame(forestpredctbcu)
colnames(forest_class) <- c("RandomForest")
forest_result <- cbind(Y.bcu_predict,forest_class)

 
forest_result_list <- forest_result[order(bcu_predict), ]

write.csv(forest_result_list, 
          file = "BCU_predict_result_RandomForest.csv")
names(forest_result_list)


#第三种方法，Rpart t discriminate analysis
##############################################################
library(rpart)
library(rpart.plot) 
Rpart_fit <- rpart(Optical.Class~Spectral.Index+Radio.flux.mJy.
             +flux_density+curve_significance
             +flux_100_300_mev+flux_0p3_1_gev
             +flux_10_100_gev+variability_index,
             method="class",
             data=XX.train)

plotcp(Rpart_fit)
Rpart_fit$cptable
 summary(Rpart_fit)
 
 dtree.pruned <- prune(Rpart_fit, cp=0.01335312)
 
Rpart_test <- predict(dtree.pruned,X.test,type="class")
summary(Class.test)
summary(Rpart_test)

Rpart.perf <- table(Class.test, Rpart_test,
                     dnn=c("Actual", "Predicted"))
Rpart.perf
performance(Rpart.perf)
class_eval(Rpart_test, 
           Class.test, 
           main="Rpart Classification")


Rpart_result <- predict(dtree.pruned,Y.bcu_predict,type="class") 

table(Rpart_result)
rpart_result <- data.frame(Rpart_result)
rpart_result
colnames(rpart_result) <- c("rpart")

rpart_result_list <- rpart_result[order(bcu_predict), ]

total_all3 <- cbind(BCU400[1:2],totalYY,
                    rpart_result_list,
                    forest_result_list[9])
names(total_all3)


table(mod2$classification) #Mclust result
table(forestpredctbcu) #randomForest_result
table(rpart_result) #rpart result
write.csv(total_all3, file = "BCU_total_all_3_methods.csv")



#第三种方法，SVM discriminate analysis
##############################################################
library(e1071)
LAC3_svm <- svm(Optical.Class~Spectral.Index+Radio.flux.mJy.
                +flux_density+curve_significance
                +flux_100_300_mev+flux_0p3_1_gev
                +flux_10_100_gev+variability_index,
                data=XX.train,
                gamma=0.01,
                cost=1000)
# LAC3_svm_tuned <- tune.svm(Optical.Class~Spectral.Index+Radio.flux.mJy.
#                            +flux_density+curve_significance
#                            +flux_100_300_mev+flux_0p3_1_gev
#                            +flux_10_100_gev+variability_index,
#                            data=XX.train,
#                            gamma=10^(-6:1),
#                            cost=10^(-10:10))
# LAC3_svm_tuned



summary(LAC3_svm, paramters = T)

svm_test <- predict(LAC3_svm, na.omit(X.test))
summary(Class.test)
summary(svm_test)

SDSS_svm <- svm(as.factor(Optical.Class) ~Spectral.Index+Radio.flux.mJy.
                +flux_density+curve_significance
                +flux_100_300_mev+flux_0p3_1_gev
                +flux_10_100_gev+variability_index,
                data=XX.train,
                cost=1, gamma=0.1)
                #cost=1000, gamma=0.01)

summary(SDSS_svm)
svm_test1 <- predict(SDSS_svm, na.omit(X.test))
summary(svm_test1)
summary(Class.test)

svm.perf <- table(Class.test,
                  svm_test1, 
                  dnn=c("Actual", "Predicted"))
svm.perf

performance(svm.perf)
class_eval(svm_test1, 
           Class.test, 
           main="SVM Classification")



SDSS_svm_result <- predict(SDSS_svm, na.omit(Y.bcu_predict))

table(SDSS_svm_result)
svm_result <- data.frame(SDSS_svm_result)
svm_result
colnames(svm_result) <- c("svm")

svm_result_list <- svm_result[order(bcu_predict), ]
names(svm_result_list)

total_all4 <- cbind(BCU400[1:4],totalYY,
                    rpart_result_list,
                    forest_result_list[9],
                    svm_result_list)
names(total_all4)
write.csv(total_all4, file = "BCU_total_all_4_methods.csv")









###计算准确率，，，
pdf("Accuracy.pdf")
postscript("Accuracy.eps")
par(mfrow = c(2,2))
 class_eval(mclust.pred$classification, 
            Class.test, 
            main="Mclust Classification")
class_eval(forestpredct, 
           Class.test, 
           main="RandomForest Classification")
class_eval(Rpart_test, 
           Class.test, 
           main="Rpart Classification")
class_eval(svm_test1, 
           Class.test, 
           main="SVM Classification")
dev.off()






performance(mclust.perf)

performance(forest.perf)
performance(Rpart.perf)
performance(svm.perf)


table(mod2$classification) #Mclust result
table(forestpredctbcu) #randomForest_result
table(rpart_result) #rpart result
table(SDSS_svm_result)








