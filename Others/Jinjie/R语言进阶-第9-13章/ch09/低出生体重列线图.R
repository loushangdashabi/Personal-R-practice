##
##install.packages("rms")
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

#f<-glm(low ~ age+ftv+ht+lwt+ptl+smoke+ui+race2+race3,data=mydata,family = binomial())
#summary(f)

dd<-datadist(mydata)
options(datadist='dd')

fit1<-lrm(low~age+ftv+ht+lwt+ptl+smoke+ui+race2+race3,data=mydata,x=T,y=T)
fit1
summary(fit1)

nom1 <- nomogram(fit1, fun=plogis,fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),lp=F, funlabel="Low weight rate")
plot(nom1)

cal1 <- calibrate(fit1, cmethod='hare', method='boot', B=1000)
plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0))


mydata$race <- as.factor(ifelse(mydata$race=="白种人", "白种人","黑人及其他种族"))

dd<-datadist(mydata)
options(datadist='dd')

fit2<-lrm(low~age+ftv+ht+lwt+ptl+smoke+ui+race,data=mydata,x=T,y=T)
fit2
summary(fit2)

nom2 <- nomogram(fit2, fun=plogis,fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),lp=F, funlabel="Low weight rate")
plot(nom2)
cal2 <- calibrate(fit2, cmethod='hare',method='boot', B=1000)
plot(cal2,xlim=c(0,1.0),ylim=c(0,1.0))


fit3<-lrm(low~ht+lwt+ptl+smoke+race,data=mydata,x=T,y=T)
fit3
summary(fit3)

nom3 <- nomogram(fit3, fun=plogis,fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),lp=F, funlabel="Low weight rate")
plot(nom3)

cal3 <- calibrate(fit3, cmethod='hare',method='boot', B=1000)
plot(cal3,xlim=c(0,1.0),ylim=c(0,1.0))
