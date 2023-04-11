rm(list = ls())
setwd("~")
mydata <- read.csv("~.csv",stringsAsFactors = TRUE)
set.seed(7)
require(caret)
folds <- createFolds(y=mydata$group,k=10)

max=0
num=0
for(i in 1:10){
 
 fold_test <- mydata[folds[[i]],] 
 fold_train <- mydata[-folds[[i]],] 
 
 print("***组号***")
 
 fold_pre <- glm(group ~.,family=binomial(link='logit'),data=fold_train)
 fold_predict <- predict(fold_pre,type='response',newdata=fold_test)
 fold_predict =ifelse(fold_predict>0.5,1,0)
 fold_test$predict = fold_predict
 fold_test_true_value<-fold_test$group
 fold_test_predict_value<-fold_test$predict
 fold_error = fold_test_predict_value-fold_test_true_value
 fold_accuracy = (nrow(fold_test)-sum(abs(fold_error)))/nrow(fold_test) 
 print(i)
 print("***测试集精确度***")
 print(fold_accuracy)
 print("***训练集精确度***")
 fold_predict2 <- predict(fold_pre,type='response',newdata=fold_train)
 fold_predict2 =ifelse(fold_predict2>0.5,1,0)
 fold_train$predict = fold_predict2
 fold_train_true_value<-fold_train$group
 fold_train_predict_value<-fold_train$predict
 fold_error2 = fold_train_predict_value-fold_train_true_value
 fold_accuracy2 = (nrow(fold_train)-sum(abs(fold_error2)))/nrow(fold_train) 
 print(fold_accuracy2)
 
 
 if(fold_accuracy>max)
 {
 max=fold_accuracy 
 num=i
 }
 
}
 
print(max)
print(num)
 
testi <- mydata[folds[[num]],]
traini <- mydata[-folds[[num]],] 
prei <- glm(group ~.,family=binomial(link='logit'),data=traini)
predicti <- predict.glm(prei,type='response',newdata=testi)
predicti =ifelse(predicti>0.5,1,0)
testi$predict = predicti
#write.csv(testi,"ausfold_test.csv")
testi_true_value<- testi$group
testi_predict_value<- testi$predict
errori = testi_predict_value-testi_true_value
accuracyi = (nrow(testi)-sum(abs(errori)))/nrow(testi) 
 
predicti2 <- predict.glm(prei,type='response',newdata=traini)
predicti2 =ifelse(predicti2>0.5,1,0)
traini$predict = predicti2
traini_true_value<- traini$group
traini_predict_value<- traini$predict
errori2 = traini_predict_value-traini_true_value
accuracyi2 = (nrow(traini)-sum(abs(errori2)))/nrow(traini) 
accuracyi;num;accuracyi2

summary(prei)
or_train=exp(cbind(OR=coef(prei),confint(prei))) 
#write.csv(or_train,"or.csv")

#library(pROC)
#roc(traini$group,fitted(prei),plot=TRUE)
#roc_curve <- roc(traini$group,fitted(prei))
#names(roc_curve)
#x <- 1-roc_curve$specificities
#y <- roc_curve$sensitivities
#p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
#p + geom_line(colour = 'red') +geom_abline(intercept = 0, slope = 1)+annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')

library(pROC)
modelroc1 <- roc(traini_true_value,predicti2)
plot(modelroc1, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2),
 grid.col=c("green", "red"), max.auc.polygon=TRUE,
 auc.polygon.col="red", print.thres=TRUE,main='ROC of LR Model(train-all)') #画出ROC曲线，标出坐标，并标出AUC的值
modelroc2 <- roc(testi_true_value,predicti)
plot(modelroc2, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2),
 grid.col=c("green", "red"), max.auc.polygon=TRUE,
 auc.polygon.col="pink", print.thres=TRUE,main='ROC of LR Model(test-all)') #画出ROC曲线，标出坐标，并标出AUC的值

model<-step(object = prei,trace = 0)
summary(model)
confint(model)
or_step=exp(cbind(OR=coef(model),confint(model))) 
#write.csv(or_step,"or_step.csv")

#library(pROC)
#roc_curve <- roc(traini$group,fitted(model))
#names(roc_curve)
#x <- 1-roc_curve$specificities
#y <- roc_curve$sensitivities
#library(ggplot2)
#p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
#p + geom_line(colour = 'green') +geom_abline(intercept = 0, slope = 1)+annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,2)))+labs(x = '1-specificities',y = 'sensitivities', title = 'ROC Curve')
#
library(pROC)
modelroc3 <- roc(traini$group,fitted(model))
plot(modelroc3, print.auc=TRUE, auc.polygon=TRUE,legacy.axes=TRUE, grid=c(0.1, 0.2),
 grid.col=c("green", "red"), max.auc.polygon=TRUE,
 auc.polygon.col="orange", print.thres=TRUE,main='ROC of LR Model(train-step)')


	
