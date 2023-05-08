##
library(foreign)
library(survival)
library(rms)

pancer <- read.spss('pancer.sav')
pancer <- as.data.frame(pancer)
head(pancer)

pancer$censor <- ifelse(pancer$censor=='死亡',1,0)
pancer$Gender <- as.factor(ifelse(pancer$sex=='男',"Male","Female"))
pancer$ch <- as.factor(ifelse(pancer$ch=='CH3', "ch","nonch"))
#pancer$ch <- relevel(pancer$ch,ref="CH0") #设置因子的参照水平
#pancer$ch<- factor(pancer$ch,order=TRUE) #设置为等级变量
#options(contrasts=c("contr.treatment", "contr.treatment")) #指定等级变量的参照水平
#pancer$Gender <- relevel(pancer$Gender,ref='Female')

#f<-coxph(Surv(time,censor==1)~age+Gender+trt+bui+ch+p+stage,data=pancer)
#f
#summary(f)

dd<-datadist(pancer)
options(datadist='dd')

coxm1 <- cph(Surv(time,censor==1)~age+Gender+trt+bui+ch+p+stage,x=T,y=T,data=pancer,surv=T)
coxm1
summary(coxm1)

surv <- Survival(coxm1)
surv1 <- function(x)surv(1*3,lp=x)
surv2 <- function(x)surv(1*6,lp=x)
surv3 <- function(x)surv(1*12,lp=x)

nom1<-nomogram(coxm1,fun=list(surv1,surv2,surv3),lp = F,
               funlabel=c('3-Month Survival probability',
                          '6-Month survival probability',
                          '12-Month survival probability'),
               maxscale=100,
               fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nom1)

#plot(nomogram(coxm1,fun=list(surv1,surv2,surv3),lp= F,funlabel=c('3-Month Survival probability','6-Month survival probability','12-Month survival probability'),maxscale=100,fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1')),xfrac=.30)

coxm2 <- cph(Surv(time,censor==1)~age+trt+bui+p+stage,x=T,y=T,data=pancer,surv=T)
coxm2
summary(coxm2)

surv <- Survival(coxm2)
surv1 <- function(x)surv(1*3,lp=x)
surv2 <- function(x)surv(1*6,lp=x)
surv3 <- function(x)surv(1*12,lp=x)

nom2<-nomogram(coxm2,fun=list(surv1,surv2,surv3),lp = F,
               funlabel=c('3-Month Survival probability',
                          '6-Month survival probability',
                          '12-Month survival probability'),
               maxscale=100,
               fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nom2)

##
library(survival)
f<-coxph(Surv(time,censor==1)~age+Gender+trt+bui+ch+p+stage,data=pancer)
summary(f)
sum.surv<-summary(f)
c_index<-sum.surv$concordance
c_index

cal <- calibrate(coxm2, cmethod='KM', method='boot', u=3, m=20, B=1000)
plot(cal,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),xlab="Nomogram-Predicted Probabilityof 3 months OS",ylab="Actual 3 months OS (proportion)",col=c(rgb(192,98,83,maxColorValue=255)))
#lines(cal[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
#abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))
