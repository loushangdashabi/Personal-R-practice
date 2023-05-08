## 第7章代码开始
## 竞争风险模型
## http://www.stat.unipg.it/luca/R/

##fine-gray test
#library(foreign)
bmt <-read.csv('bmtcrr.csv')
head(bmt)
bmt$D <- as.factor(bmt$D)

library(survival)
library(cmprsk)
library(splines)

attach(bmt)
crmod <- cuminc(ftime,Status,D)
crmod

plot(crmod,xlab = 'Month', ylab = 'CIF',lwd=2,lty=1,
     col = c('red','blue','black','forestgreen'))

##cmprsk model
cov1 <- data.frame(age = bmt$Age,
                   sex_F = ifelse(bmt$Sex=='F',1,0),
                   dis_AML = ifelse(bmt$D=='AML',1,0),
                   phase_cr1 = ifelse(bmt$Phase=='CR1',1,0),
                   phase_cr2 = ifelse(bmt$Phase=='CR2',1,0),
                   phase_cr3 = ifelse(bmt$Phase=='CR3',1,0),
                   source_PB = ifelse(bmt$Source=='PB',1,0)) ## 手动设置哑变量

cov1

mod1 <- crr(bmt$ftime, bmt$Status, cov1, failcode=1, cencode=0)
summary(mod1)

library(aod)
wald.test(mod1$var,mod1$coef,Terms = 4:6)

## 第7章代码结束