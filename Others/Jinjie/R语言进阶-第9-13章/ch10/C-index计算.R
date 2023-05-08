#模拟一组数据并设置为数据框结构
age <- rnorm(200,50,5)
bp <- rnorm(200,120,10)
d.time <- rexp(200)
cens <- runif(200,.5,2)
death <- d.time <= cens
os <- pmin(d.time, cens)
sample.data <- data.frame(age = age,bp = bp,os = os,death = death)
head(sample.data) #展示数据框sample.data的前6行

#方法1 survival包
library(survival) # 载入survival包
fit <- coxph(Surv(os, death) ~ age + bp,data = sample.data) # coxph函数拟合cox回归模型
sum.surv<-summary(fit) # summary函数展示模型结果并赋值给对象sum.surv
c_index <-sum.surv$concordance #展示模型参数concordance，即是c-index
c_index

#方法2 rms包
library(rms)
set.seed(1)#这里设置种子，目的是为了能重复最后的结果，因为validate函数的校正结果是随机的。
dd<-datadist(sample.data)
options(datadist='dd')
fit.cph <- cph(Surv(os, death)~ age + bp, data = sample.data, x = TRUE, y = TRUE, surv = TRUE)
fit.cph #模型参数 Dxy*0.5+0.5 即是c-index

# Get the Dxy
v <- validate(fit.cph, dxy=TRUE, B=1000)
v
Dxy = v[rownames(v)=="Dxy", colnames(v)=="index.corrected"]
orig_Dxy = v[rownames(v)=="Dxy", colnames(v)=="index.orig"]

# The c-statistic according to Dxy=2(c-0.5)
orig_c_index <- abs(orig_Dxy)/2+0.5 #计算未校正c-index
orig_c_index

bias_corrected_c_index  <- abs(Dxy)/2+0.5 #计算校正c-index
bias_corrected_c_index 