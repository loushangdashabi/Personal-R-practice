## 第6章代码开始
# Kaplan-Meier法估计
library(survival)
library(ISwR)
attach(melanom)
names(melanom)

#Surv(days, status==1)
#survfit(Surv(days, status==1)~1)
surv.all <- survfit(Surv(days,status==1)~1)
summary(surv.all)
plot(surv.all,col="blue")

surv.bysex <- survfit(Surv(days,status==1)~sex)
summary(surv.bysex)
plot(surv.bysex)
plot(surv.bysex, conf.int=T, col=c("red","blue"))
legend(locator(n=1),legend=c("male","female"),lty=1,col=c("blue","red"))


## log-rank test
f1<-survdiff(Surv(days,status==1)~sex,rho=0)
f1
f2<-survdiff(Surv(days,status==1)~sex,rho=1)
f2
f3<-survdiff(Surv(days,status==1)~sex+strata(ulc)) # With rho = 0 this is the log-rank or Mantel-Haenszel test, and with rho = 1 it is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test.
f3

## cox regression
f4<-coxph(Surv(days,status==1)~sex)
summary(f4)
#summary(coxph(Surv(days,status==1)~sex))
f5<-coxph(Surv(days,status==1)~sex+log(thick)+ulc)
summary(f5)
#summary(coxph(Surv(days,status==1)~sex+log(thick)+strata(ulc)))
plot(survfit(coxph(Surv(days,status==1)~
                     log(thick)+sex+ulc)),col=c("red","blue"))
#plot(survfit(coxph(Surv(days,status==1)~
                       #log(thick)+sex+strata(ulc))),col=c("red","blue"))
legend(locator(n=1),legend=c("ulceration present","ulceration absent"),lty=1,col=c("red","blue"))

detach(melanom)

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
total <- survfit(Surv(t, censor==1)~1)
summary(total)
plot(total,conf.int=F)
separate <- survfit(Surv(t, censor==1)~group)
summary(separate)
plot(separate, lty = c('solid','dashed'), col=c('black','blue'),
     xlab='survival time in days',ylab='survival probabilities')
legend('topright', c('Group A',' Group B'), lty=c('solid','dashed'),
       col=c('black','blue'))
survdiff(Surv(t, censor)~group)
survdiff(Surv(t, censor)~group,rho=1) # rho = 1 it is equivalent to the Peto & Peto modification of the Gehan-Wilcoxon test.
detach(example15_3)

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

## 第6章代码结束