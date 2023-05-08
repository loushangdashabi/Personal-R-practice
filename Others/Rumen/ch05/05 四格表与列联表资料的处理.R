# 第5章代码开始

# 样本率与总体比较
p <- 0.2
n_case <- 48
n <- 152
stde <- sqrt (p *(1- p)/n)
u  <- (n_case/n-p)/stde
u
pvalue <- 2*(1-pnorm(u))
pvalue

# 两样本率的比较
x1 <-23
x2 <-13
n1 <- 80
n2 <-85
p  <- (x1+x2)/(n1+n2)
std<-sqrt(p*(1-p)*(1/n1+1/n2))
u  <- (x1/n1-x2/n2)/std
pvalue <- 2*(1-pnorm(u))
u
pvalue

# 普通卡方检验案例1
Example13_2  <- read.table ("example13_2.csv", header=TRUE, sep=",")
attach(Example13_2)
mytable  <-  xtabs(~a + b)
library(gmodels)
CrossTable(a, b)
chisq.test(mytable)
detach (Example13_2)

# 普通卡方检验案例2
Example13_3  <- read.table ("example13_3.csv", header=TRUE, sep=",")
attach(Example13_3)
mytable  <-  xtabs(~a + b)
library(gmodels)
CrossTable(a, b)
# CrossTable(a, b, expected=TRUE,fisher=TRUE)
chisq.test(mytable)
detach (Example13_3)

# 配对卡方检验案例
Example13_5  <- read.table ("example13_5.csv", header=TRUE, sep=",")
attach(Example13_5)
mytable  <-  xtabs(~a + b)
library(gmodels)
CrossTable(a, b)
mcnemar.test(mytable)
# Kappa.test(mytable, conf.level=0.95) # wrong 需提前载入 fmsb 包
library(vcd)
K<-Kappa(mytable)
K
confint(K)
summary(K)
print(K, CI = TRUE,level=0.95)
print(K, CI = TRUE,level=0.99)
detach (Example13_5)

# 行均分检验 Cochran-Mantel-Haenszel
Example13_7  <- read.table ("example13_7.csv", header=TRUE, sep=",")
attach(Example13_7)
mytable  <-  xtabs(~a + b)
library(gmodels)
CrossTable(a, b)
library(vcdExtra)
CMHtest(mytable)     
detach (Example13_7)

# 用Ordinal logistic回归模型
Example13_7  <- read.table ("example13_7.csv", header=TRUE, sep=",")
attach(Example13_7)
Example13_7$x1  <- ifelse (a==1, 1, 0)
Example13_7$x2  <- ifelse (a==2, 1, 0)
Example13_7$x3  <- ifelse (a==3, 1, 0)
library(rms)
fit1 <- lrm(b~ x1 + x2 ,  data=Example13_7, model=FALSE, x=FALSE, y=FALSE)
fit1
coefficients(fit1)
exp(coefficients(fit1))
detach (Example13_7)
#对于累积比数因变量模型，平行性假设决定了每个自变量的OR值对于前g-1 个模型是相同的。
#例如，自变量xl 的OR=8.044 ，表示使用A 药物治愈的可能性是C药物的8.044 倍;
#也表示使用A 药物显效或治愈的可能性是C药的8.044倍;
#同时也表示使用A 药物至少好转的可能性是C药的8.044 倍。

# 双向有序属性不同 秩相关
Example13_8  <- read.table ("example13_8.csv", header=TRUE, sep=",")
attach(Example13_8)
cor(Example13_8, method="spearman")
cor.test(a,  b, method="spearman")
detach (Example13_8)

# 双向有序属性不同 线性趋势检验
Example13_8  <- read.table ("example13_8.csv", header=TRUE, sep=",")
attach(Example13_8)
library(gmodels)
CrossTable(a, b)
mytable <-  xtabs(~a + b)
chisq.test(mytable)
fit <- lm(a~b)
summary(fit)
coefficients(fit)
confint(fit)
detach (Example13_8)

# 一致性检验
Example13_10  <- read.table ("example13_10.csv", header=TRUE, sep=",")
attach(Example13_10)
library(gmodels)
CrossTable(a, b)
mytable  <-  xtabs(~a + b)
mcnemar.test(mytable)
library(fmsb)
Kappa.test(mytable, conf.level=0.95)
detach (Example13_10)

# Mantel-Haenszel 检验
Example13_12  <- read.table ("example13_12.csv", header=TRUE, sep=",")
attach(Example13_12)
mytable  <-  xtabs(~drug + case + hos) #分层输出各家医院四格表频数
mytable
prop.table(mytable,3) #分层输出各家医院四格表百分数
addmargins(mytable) #分层输出各家医院四格表边际频数
mantelhaen.test(mytable)
detach (Example13_12)

# 趋势卡方检验
Example13_13  <- read.table ("example13_13.csv", header=TRUE, sep=",")
attach(Example13_13)
library(gmodels)
CrossTable(a, b)
mytable  <-  xtabs(~a + b)
chisq.test(mytable)
fit  <-  lm(a~b)
summary(fit)
coefficients(fit)
confint(fit)
detach (Example13_13)


# 两个组的等级指标的非参数检验
example14_15  <- read.table ("example14_15.csv", header=TRUE, sep=",")
attach(example14_15)
wilcox.test(x ~ g)
detach(example14_15)

# 多个组的等级指标的非参数检验
example14_16  <- read.table ("example14_16.csv", header=TRUE, sep=",")
attach(example14_16)
kruskal.test(x~ g)
library(nparcomp)
nparcomp(x ~ g, data=example14_16, alternative = "two.sided")
detach(example14_16)

# 第5章代码结束
