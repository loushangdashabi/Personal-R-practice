##here consider pbc dataset in survival package as an example
library(survival)
dat=pbc[1:312,]
dat$sex=ifelse(dat$sex=='f',1,0)
##subjects censored before 2000 days are excluded
dat=dat[dat$time>2000|(dat$time<2000&dat$status==2),]
##predciting the event of 'death' before 2000 days
event=ifelse(dat$time<2000&dat$status==2,1,0)
##standard prediction model: age, bilirubin, and albumin
z.std=as.matrix(subset(dat,select=c(age,bili,albumin)))
##new prediction model: age, bilirubin, albumin, and protime
z.new=as.matrix(subset(dat,select=c(age,bili,albumin,protime)))
##glm fit (logistic model)
mstd=glm(event~.,binomial(logit),data.frame(event,z.std),x=TRUE)
mnew=glm(event~.,binomial(logit),data.frame(event,z.new),x=TRUE)
##Using PredictABEL package
library(PredictABEL)
pstd<-mstd$fitted.values
pnew<-mnew$fitted.values
##用cbind函数把前面定义的event变量加入数据集，并定义为dat_new
dat_new=cbind(dat,event)
##计算NRI,同时报告了IDI,IDI计算与cutoff点设置无关。
##cOutcome指定结局变量的列序号
##predrisk1, predrisk2为新旧logistic回归模型
reclassification(data=dat_new,cOutcome=21,
                 predrisk1=pstd,predrisk2=pnew,
                 cutoff=c(0,0.2,0.4,1))
