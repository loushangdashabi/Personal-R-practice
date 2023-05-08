#C-statistics计算
library(foreign) 
library(rms)

mydata<-read.spss("lweight.sav")
mydata<-as.data.frame(mydata)
head(mydata)

mydata$low <- ifelse(mydata$low =="低出生体重",1,0)
mydata$race1 <- ifelse(mydata$race =="白种人",1,0)
mydata$race2 <- ifelse(mydata$race =="黑种人",1,0)
mydata$race3 <- ifelse(mydata$race =="其他种族",1,0)

attach(mydata)
dd<-datadist(mydata)
options(datadist='dd')

fit1<-lrm(low~age+ftv+ht+lwt+ptl+smoke+ui+race1+race2,data=mydata,x=T,y=T)
fit1 #直接读取模型中Rank Discrim.参数 C

mydata$predvalue<-predict(fit1)
library(ROCR)
pred <- prediction(mydata$predvalue, mydata$low)
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1)
auc <- performance(pred,"auc")
auc #auc即是C-statistics

#library(Hmisc)
somers2(mydata$predvalue, mydata$low) #somers2 {Hmisc}
