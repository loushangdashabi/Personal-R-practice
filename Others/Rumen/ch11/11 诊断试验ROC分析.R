## 诊断试验ROC分析代码开始
# 案例1
example21_2  <- read.table ("example21_2.csv", header=TRUE, sep=",")
example21_2
attach(example21_2)
summary(example21_2)
str(example21_2) #structure of dataframe
#install.packages("ROCR")
library(ROCR)
pred <- prediction(example21_2$value, example21_2$group)
pred
perf <- performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(a=0,b=1,col="blue")
perf1 <- performance(pred, "prec", "rec")
plot(perf1)
perf2 <- performance(pred, "sens", "spec")
plot(perf2)
auc <- performance(pred,"auc")
auc
detach(example21_2)

# 案例2
example21_3  <- read.table ("example21_3.csv", header=TRUE, sep=",")
attach(example21_3)
summary(example21_3)
str(example21_3)
#install.packages("ROCR")
library(ROCR)
pred1 <- prediction(example21_3$mRNA, example21_3$oncology)
pred1
pred2 <- prediction(example21_3$dna, example21_3$oncology)
pred2
perf1 <- performance(pred1,"tpr","fpr")
perf2 <- performance(pred2,"tpr","fpr")
auc1 <- performance(pred1,"auc")
auc1
auc2 <- performance(pred2,"auc")
auc2
plot(perf1, col="blue")
plot(perf2, col="red", add=TRUE)
abline(a=0,b=1,col="gray")
legend(locator(n=1),legend=c("mRNA","dna"),lty=1,col=c("blue","red"))

## 第11章代码结束