## 第8章代码开始
# Kaplan-Meier法估计
library(survival)
library(ISwR)
attach(melanom)
names(melanom)

Surv(days, status==1)
survfit(Surv(days, status==1)~1)
surv.all <- survfit(Surv(days,status==1)~1)
summary(surv.all)
plot(surv.all,col="blue")

surv.bysex <- survfit(Surv(days,status==1)~sex)
plot(surv.bysex)
plot(surv.bysex, conf.int=T, col=c("red","blue"))
legend(locator(n=1),legend=c("male","female"),lty=1,col=c("blue","red"))


## log-rank
survdiff(Surv(days,status==1)~sex)
survdiff(Surv(days,status==1)~sex+strata(ulc)) # With rho = 0 this is the log-rank or Mantel-Haenszel test, and with rho = 1 it is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test.

## cox regression
summary(coxph(Surv(days,status==1)~sex))
summary(coxph(Surv(days,status==1)~sex+log(thick)+strata(ulc)))
plot(survfit(coxph(Surv(days,status==1)~
                       log(thick)+sex+strata(ulc))),col=c("red","blue"))
legend(locator(n=1),legend=c("ulceration present","ulceration absent"),lty=1,col=c("red","blue"))

# LifeTable寿命表法
hmohiv<-read.table("hmohiv.csv", sep=",", header = TRUE)
attach(hmohiv)
head(hmohiv)
library(KMsurv)
library(nlme)
t6m<-floor(time/6)
tall<-data.frame(t6m, censor)
die<-gsummary(tall, sum, groups=t6m)
total<-gsummary(tall, length, groups=t6m)
rm(t6m)
ltab.data<-cbind(die[,1:2], total[,2])
detach(hmohiv)
attach(ltab.data)

lt=length(t6m)
t6m[lt+1]=NA
nevent=censor
nlost=total[,2] - censor
mytable<-lifetab(t6m, 100, nlost, nevent)
mytable[,1:5]
plot(t6m[1:11], mytable[,5], type="s", xlab="Survival time in every 6 month", 
     ylab="Proportion Surviving")
detach(ltab.data)

#logrank检验案例1
#install.packages("survival")
library(survival)
example15_3 <- read.table ("example15_3.csv", header=TRUE, sep=",")
attach(example15_3)
total <- survfit(Surv(t, censor)~1)
summary(total)
plot(total)
separate <- survfit(Surv(t, censor)~group)
summary(separate)
plot(separate, lty = c('solid','dashed'), col=c('black','blue'),
     xlab='survival time in days',ylab='survival probabilities')
legend('topright', c('Group A',' Group B'), lty=c('solid','dashed'),
       col=c('black','blue'))
survdiff(Surv(t, censor)~group)
survdiff(Surv(t, censor)~group,rho=1) # rho = 1 it is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test.

#logrank检验案例2
library(coin)
data(glioma)

library(survival)
g3 <- subset(glioma, histology =='Grade3')

fit <- survfit(Surv(time, event)~group,data = g3)
plot(fit, lty = c(2,1), col = c(2,1))
legend('bottomright', legend = c('Control','Treatment'), lty = c(2,1), col = c(2,1))

survdiff(Surv(time, event)~group,data = g3) 

logrank_test(Surv(time, event)~group,data = g3, distribution ="exact")
logrank_test(Surv(time, event)~group|histology,data = glioma, distribution = approximate(B = 1000)) #两组比较,coin包 logrank_test函数#SurvivalTests {coin}

#画一幅高水准的生存曲线
library(survival)
library(survminer)
fit <- survfit(Surv(time, status) ~ sex, data = lung)

ggsurvplot(fit,
           pval = TRUE, # 在图上添加log rank检验的p值
           conf.int = TRUE,# 添加置信区间
           risk.table = TRUE, # 在图下方添加风险表
           risk.table.col = "strata", # 根据数据分组为风险表添加颜色
           linetype = "strata", # 改变不同组别的生存曲线的线型
           surv.median.line = "hv", # 标注出中位生存时间
           ggtheme = theme_bw(), # 改变图形风格
           palette = c("#E7B800", "#2E9FDF")) # 图形颜色风格


ggsurvplot(
  fit,                    
  pval = FALSE,             
  conf.int = TRUE, 
  fun = "cumhaz",
  conf.int.style = "ribbon",  # 设置置信区间的风格
  xlab = "Time in days",   # 设置x轴标签
  break.time.by = 200,     # 将x轴按照200为间隔进行切分
  ggtheme = theme_light(), # 设置图形风格
  risk.table = "abs_pct",  # 在风险表中添加绝对数和相对数
  risk.table.y.text.col = TRUE,# 设置风险表的文字颜色
  risk.table.y.text = FALSE,# 以条柱展示风险表的标签，而非文字
  ncensor.plot = TRUE,      # 展示随访过程中不同时间点死亡和删失的情况
  surv.median.line = "hv",  # 添加中位生存时间
  legend.labs = 
    c("Male", "Female"),    # 改变图例标签
  palette = 
    c("#E7B800", "#2E9FDF") # 设置颜色
)


ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", 
           ggtheme = theme_bw(), 
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")
dev.off()

#Cox回归案例1
library(foreign)
library(survival)

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

f<-coxph(Surv(time,censor==1)~age+Gender+trt+bui+ch+p+stage,data=pancer)
summary(f)
sum.surv<-summary(f)
c_index<-sum.surv$concordance
c_index

#Cox回归案例2
library(survival)
example15_4  <- read.table ("example15_4.csv", header=TRUE, sep=",")
attach(example15_4)
coxmodel  <- coxph(Surv(days, censor)~group)
summary(coxmodel)
coxmode2  <- coxph(Surv(days, censor)~group+renal)
summary(coxmode2)
anova(coxmodel,coxmode2)
detach(example15_4)

#Cox回归案例3
data('GBSG2',package = 'TH.data')
head(GBSG2)
plot(survfit(Surv(time, cens)~horTh,data = GBSG2),lty = c(2,1), col = c(2,1), mark.time = T)
legend('bottomright', legend = c('yes','no'), lty = c(2,1), col = c(2,1))

coxreg <- coxph(Surv(time,cens)~.,data = GBSG2)
summary(coxreg)

#install.packages("party")
library(party)
tree <- ctree(Surv(time,cens)~.,data = GBSG2)
plot(tree)

#Cox回归亚组分析森林图案例 
library(forestplot)
test_forest <- read.csv('forest_test2.csv',header = FALSE)
attach(test_forest)
forestplot(labeltext = as.matrix(test_forest[,1:6]),
           #设置用于文本展示的列，此处我们用数据的前六列作为文本，在图中展示
           mean = test_forest$V7, #设置均值
           lower = test_forest$V8, #设置均值的下限
           upper = test_forest$V9, #设置均值的上限
           is.summary = c(T,T,F,F,T,F,F,T,F,F,F,F,T,F,F,F,T,F,T,F,F,T),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.2, #设置点估计的方形大小
           lineheight = unit(10,'mm'),#设置图形中的行距
           colgap = unit(8,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           lwd.xaxis=2,#设置X轴线的粗细
           xlog=FALSE,
           ci.vertices.height = 0.1,
           clip = c(0.2,1.3), # 设置森林图展示的可信区间范围，超过的部分用箭头展示
           grid = FALSE,
           lty.ci = 1,
           col=fpColors(box='#458B00',  summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           xlab="Hzard Ratio(HR)",#设置x轴标签
           graph.pos = 5)#设置森林图的位置，此处设置为5，则出现在第五列

#Cox回归模型列线图及校正曲线绘制案例
library(foreign)
library(survival)
library(rms)

pancer <- read.spss('pancer.sav')
pancer <- as.data.frame(pancer)
head(pancer)

pancer$censor <- ifelse(pancer$censor=='死亡',1,0)
pancer$Gender <- as.factor(ifelse(pancer$sex=='男',"Male","Female"))
pancer$ch <- as.factor(ifelse(pancer$ch=='CH3', "ch","nonch"))

#pancer$ch1 <- as.factor(ifelse(pancer$ch=='CH1',1,0))
#pancer$ch2 <- as.factor(ifelse(pancer$ch=='CH2',1,0))
#pancer$ch3 <- as.factor(ifelse(pancer$ch=='CH3',1,0))
#pancer$ch0 <- as.factor(ifelse(pancer$ch=='CH0',1,0))

#pancer$ch <- relevel(pancer$ch,ref="CH0") #设置因子的参照水平
#pancer$ch<- factor(pancer$ch,order=TRUE) #设置为等级变量
#options(contrasts=c("contr.treatment", "contr.treatment")) #指定等级变量的参照水平
#pancer$Gender <- relevel(pancer$Gender,ref='Female')

dd<-datadist(pancer)
options(datadist='dd')

coxm1 <- cph(Surv(time,censor==1)~age+Gender+trt+bui+ch+p+stage,x=T,y=T,data=pancer,surv=T)
coxm1
surv <- Survival(coxm1)
surv1 <- function(x)surv(1*3,lp=x)
surv2 <- function(x)surv(1*6,lp=x)
surv3 <- function(x)surv(1*12,lp=x)

nom1<-nomogram(coxm1,fun=list(surv1,surv2,surv3),lp= F,funlabel=c('3-Month Survival probability','6-Month survival probability','12-Month survival probability'),maxscale=100,fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
plot(nom1,xfrac=.30)

#plot(nomogram(coxm,fun=list(surv1,surv2,surv3),lp = F,funlabel=c('3-Month Survival probability','6-Month survival probability','12-Month survival probability'),maxscale=100,fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1')),xfrac=.30)

library(survival)
f<-coxph(Surv(time,censor==1)~age+Gender+trt+bui+ch+p+stage,data=pancer)
summary(f)
sum.surv<-summary(f)
c_index<-sum.surv$concordance
c_index

cal1 <- calibrate(coxm1, cmethod='KM', method='boot', u=6, m=20, B=1000)
plot(cal1,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),xlab="Nomogram-Predicted Probabilityof 6 m OS",ylab="Actual 6 m OS (proportion)",col=c(rgb(192,98,83,maxColorValue=255)))
lines(cal1[,c("mean.predicted","KM")],type="b",lwd=2,col=c(rgb(192,98,83,maxColorValue=255)),pch=16)
abline(0,1,lty=3,lwd=2,col=c(rgb(0,118,192,maxColorValue=255)))

# coxm2 <- cph(Surv(time,censor==1)~age+trt+bui+p+stage,x=T,y=T,data=pancer,surv=T)
# nom2<-nomogram(coxm2,fun=list(surv1,surv2,surv3),lp= F,funlabel=c('3-Month Survival probability','6-Month survival probability','12-Month survival probability'),maxscale=100,fun.at=c('0.9','0.85','0.80','0.70','0.6','0.5','0.4','0.3','0.2','0.1'))
# plot(nom2,xfrac=.30)
# cal2 <- calibrate(coxm1, cmethod='KM', method='boot', u=6, m=20, B=1000)
# plot(cal2,lwd=2,lty=1,errbar.col=c(rgb(0,118,192,maxColorValue=255)),xlim=c(0,1),ylim=c(0,1),xlab="Nomogram-Predicted Probabilityof 6 m OS",ylab="Actual 6 m OS (proportion)",col=c(rgb(192,98,83,maxColorValue=255)))

# 竞争风险模型
#http://www.stat.unipg.it/luca/R/

library(foreign)
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

# 第8章代码结束