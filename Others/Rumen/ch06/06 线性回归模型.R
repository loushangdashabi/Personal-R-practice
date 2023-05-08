# 第6章代码开始
# Regression                                                 #
# requires packages car, gvlma, MASS, leaps to be installed  #
# install.packages(c("car", "gvlma", "MASS", "leaps")) 

par(ask=TRUE)
opar <- par(no.readonly=TRUE)

# Listing 8.1 - Simple linear regression
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,
     main="Women Age 30-39", 
     xlab="Height (in inches)", 
     ylab="Weight (in pounds)")
# add the line of best fit
abline(fit)

# Listing 8.2 - Polynomial regression
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
plot(women$height,women$weight,
     main="Women Age 30-39",
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))

fit3 <- lm(weight ~ height + I(height^2) +I(height^3), data=women)
summary(fit3)
plot(women$height,women$weight,
     main="Women Age 30-39",
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit3))

# Enhanced scatterplot for women data
library(car)
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")

# simple regression diagnostics
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
newfit <- lm(weight ~ height + I(height^2), data=women)
par(opar)
par(mfrow=c(2,2))
plot(newfit)
par(opar)

# basic regression diagnostics for women data
opar <- par(no.readonly=TRUE)
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
par(opar)

fit2 <- lm(weight ~ height + I(height^2), data=women)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(fit2)
par(opar)

#简单线性回归案例2
Example9_4  <- read.table ("example9_4.csv", header=TRUE, sep=",")
attach(Example9_4)
plot(x, y)
fit <-lm(y~x)
anova(fit)
summary (fit)
confint(fit)
y
fitted (fit)
residuals (fit)
detach (Example9_4)

#简单线性回归案例3
Example9_5 <- read.table ("example9_5.csv", header=TRUE, sep=",")
attach(Example9_5)
fit <-  lm(SALES~ADV + ADVLAG1 + ADVLAG2)
anova(fit)
summary (fit)
SALES
fitted (fit)
residuals (fit)
detach (Example9_5)

# 残差&回归值&预测域&置信带代码
library(ISwR)
attach(thuesen)
fit<- lm(short.velocity~blood.glucose)
summary(fit)

plot(blood.glucose,short.velocity)
abline(fit)

lm.velo <- lm(short.velocity~blood.glucose)
fitted(lm.velo)
plot(blood.glucose,short.velocity)
# lines(blood.glucose,fitted(lm.velo)) # wrong 因有缺失值，所以此命令报错
lines(blood.glucose[!is.na(short.velocity)],fitted(lm.velo))

# cc <- complete.cases(thuesen) 
# options(na.action=na.exclude)
# lm.velo <- lm(short.velocity~blood.glucose)
# fitted(lm.velo)
# lines(blood.glucose,fitted(lm.velo))

segments(blood.glucose,fitted(lm.velo),
         blood.glucose,short.velocity) #带有残差线段的图形

plot(fitted(lm.velo),resid(lm.velo)) # 残差与回归值的散点图

qqnorm(resid(lm.velo)) # Q-Q图

predict(lm.velo)
predict(lm.velo,int="c") # 置信区间confidence interval
predict(lm.velo,int="p") # 预测区间prediction interval.预测区间PI总是要比对应的置信区间CI大，这是因为在对单个响应与响应均值的预测中包括了更多的不确定性。

# pred.frame <- data.frame(thuesen[4:20,]) # blood.glucose 的值是随机排列的，我们不希望置信曲线上的线段杂乱无章地排列
pred.frame <- data.frame(blood.glucose= 4:20)
pp <- predict(lm.velo, int="p", newdata=pred.frame)
pc <- predict(lm.velo, int="c", newdata=pred.frame)
plot(blood.glucose,short.velocity,
     ylim=range(short.velocity, pp, na.rm=T))
pred.gluc <- pred.frame$blood.glucose
matlines(pred.gluc, pc, lty=c(1,2,2), col="black")
matlines(pred.gluc, pp, lty=c(1,3,3), col="black")
detach(thuesen)

# 第三节多元线性回归案例1
Example10_1  <- read.table ("example10_1.csv", header=TRUE, sep=",")
library(MASS)
attach(Example10_1)
fit1  <- lm(y~ x1 + x2 + x3)
fit2 <- lm(y ~ 1)
stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
anova(fit1)
summary (fit1)
y #观察值
fitted (fit1) #预测值
residuals (fit1) #残差
detach (Example10_1)

# 第三节多元线性回归案例2
Example10_2  <- read.table ("example10_2.csv", header=TRUE, sep=",")
library(MASS)
attach(Example10_2)
fit1  <- lm(Y~ X1 + X2 + X3)
fit2 <- lm(Y ~ 1)
stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))
fit  <- lm(Y~ X1 + X2)
anova(fit)
summary (fit)
Y
fitted (fit)
residuals (fit)
detach (Example10_2)

# 第四节 案例1 回归分析做ANOVA
library(multcomp)
levels(cholesterol$trt)
fit.aov <- aov(response ~ trt, data=cholesterol)
summary(fit.aov)
fit.lm <- lm(response ~ trt, data=cholesterol)
summary(fit.lm)
# fit.lm <- lm(response ~ trt, data=cholesterol, contrasts="contr.helmert") # wrong
fit.lm <- lm(response ~ trt, data=cholesterol, contrasts=list(trt="contr.helmert" ))
summary(fit.lm)
                                                                 
# 案例2 回归分析做ANCOVA
library(ISwR)
hellung
summary(hellung)

hellung$glucose <- factor(hellung$glucose, labels=c("Yes","No"))
summary(hellung)

attach(hellung)

plot(conc,diameter,pch=as.numeric(glucose)) #图形描述

legend(locator(n=1),legend=c("glucose","no glucose"),pch=1:2)

plot(conc,diameter,pch=as.numeric(glucose),log="x")

plot(conc,diameter,pch=as.numeric(glucose),log="xy")

tethym.gluc <- hellung[glucose=="Yes",]
tethym.nogluc <- hellung[glucose=="No",]

lm.nogluc <- lm(log10(diameter)~ log10(conc),data=tethym.nogluc) #亚组分析
lm.gluc <- lm(log10(diameter)~ log10(conc),data=tethym.gluc) #亚组分析
abline(lm.nogluc)
abline(lm.gluc)

summary(lm(log10(diameter)~ log10(conc), data=tethym.gluc))  #亚组分析结果展示
summary(lm(log10(diameter)~ log10(conc), data=tethym.nogluc)) #亚组分析结果展示　

var.test(lm.gluc,lm.nogluc) #比较两亚组回归模型

summary(lm(log10(diameter)~log10(conc)*glucose)) #考虑交互 全数据集
summary(lm(log10(diameter)~log10(conc)+glucose)) #未考虑交互，全数据集

anova(lm(log10(diameter)~log10(conc)*glucose)) #注意比较以下三种模型，自变量顺序很重要
anova(lm(log10(diameter)~glucose+log10(conc)))
anova(lm(log10(diameter)~log10(conc)+ glucose))

t.test(log10(diameter)~glucose)
anova(lm(log10(diameter)~glucose))
summary(lm(log10(diameter)~glucose))

# 第6章代码结束