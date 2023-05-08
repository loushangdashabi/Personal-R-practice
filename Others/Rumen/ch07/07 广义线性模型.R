## 第7章代码开始
# Generalized linear models                    
# requires packages AER, robust, gcc           
# install.packages(c("AER", "robust", "qcc"))

## Logistic Regression

# get summary statistics
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)

# create binary outcome variable
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair, 
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# fit full model
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating,
                data=Affairs,family=binomial())
summary(fit.full)

# fit reduced model
fit.reduced <- glm(ynaffair ~ age + yearsmarried + religiousness + 
                     rating, data=Affairs, family=binomial())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test="Chisq")

# interpret coefficients
coef(fit.reduced)
exp(coef(fit.reduced))

# calculate probability of extramariatal affair by marital ratings
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                       age = mean(Affairs$age),
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# calculate probabilites of extramariatal affair by age
testdata <- data.frame(rating = mean(Affairs$rating),
                       age = seq(17, 57, 10), 
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# evaluate overdispersion
fit <- glm(ynaffair ~ age + yearsmarried + religiousness +
             rating, family = binomial(), data = Affairs)
fit.od <- glm(ynaffair ~ age + yearsmarried + religiousness +
                rating, family = quasibinomial(), data = Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual,  
       fit$df.residual, lower = F)

# Logistic回归案例2
Example11_4  <- read.table ("example11_4.csv", header=TRUE, sep=",")
attach(Example11_4)

fit1 <- glm(y~ x1 + x2, family= binomial(), data=Example11_4)
summary(fit1)
coefficients(fit1)
exp(coefficients(fit1))
exp (confint(fit1))

fit2 <- glm(y~ x1 + x2 + x1:x2 ,  family= binomial(), data=Example11_4)
summary(fit2)
coefficients(fit2)
exp(coefficients(fit2))
exp (confint(fit2))

Example11_4$x11  <- ifelse (x1==1 & x2==1, 1, 0)
Example11_4$x10  <- ifelse (x1==1 & x2==0, 1, 0)
Example11_4$x01  <- ifelse (x1==0 & x2==1, 1, 0)
Example11_4$x00  <- ifelse (x1==0 & x2==0, 1, 0)

fit3 <- glm(y~ x11 + x10 + x01, family= binomial(), data=Example11_4)
summary(fit3)
coefficients(fit3)
exp(coefficients(fit3))
exp(confint(fit3))
detach (Example11_4)

# Logistic回归案例3
Example11_5 <- read.table ("example11_5.csv", header=TRUE, sep=",")
attach(Example11_5)
fullfit <- glm(y~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 ,  family= binomial(), data=Example11_5)
summary(fullfit)
nothing <- glm(y~ 1, family= binomial(), data=Example11_5)
summary(nothing)
bothways <- step(nothing, list(lower=formula(nothing), upper=formula(fullfit)),  direction="both")
fit1 <- glm(y~ x6 + x5 + x8 + x1 + x2 ,  family= binomial(), data=Example11_5)
summary(fit1)
fit2 <- glm(y~ x6 + x5 + x8 + x1, family= binomial(), data=Example11_5)
summary(fit2)
coefficients(fit2)
exp(coefficients(fit2))
exp (confint(fit2))
detach (Example11_5)

#条件Logistic回归案例 4
#install.packages("survival")
library(survival)
Example11_6  <- read.table ("example11_6.csv", header=TRUE, sep=",")
attach(Example11_6)
model <- clogit(outcome~ exposure+ strata(id))
summary(model)
detach(Example11_6)

#Logistic回归案例 5 低出生体重儿列线图
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
fit1
summary(fit1)
nom1 <- nomogram(fit1, fun=plogis,fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),lp=F, funlabel="Low weight rate")
plot(nom1)
cal1 <- calibrate(fit1, method='boot', B=1000)
plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0))

mydata$race <- as.factor(ifelse(mydata$race=="白种人", "白种人","黑人及其他种族"))

dd<-datadist(mydata)
options(datadist='dd')

fit2<-lrm(low~age+ftv+ht+lwt+ptl+smoke+ui+race,data=mydata,x=T,y=T)
fit2
summary(fit2)

nom2 <- nomogram(fit2, fun=plogis,fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),lp=F, funlabel="Low weight rate")
plot(nom2)
cal2 <- calibrate(fit2, method='boot', B=1000)
plot(cal2,xlim=c(0,1.0),ylim=c(0,1.0))

fit3<-lrm(low~ht+lwt+ptl+smoke+race,data=mydata,x=T,y=T)
fit3
summary(fit3)

nom3 <- nomogram(fit3, fun=plogis,fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),lp=F, funlabel="Low weight rate")
plot(nom3)
cal3 <- calibrate(fit3, method='boot', B=1000)
plot(cal3,xlim=c(0,1.0),ylim=c(0,1.0))

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
auc <- performance(pred,"auc")
auc #auc即是C-statistics

#library(Hmisc)
somers2(mydata$predvalue, mydata$low) #somers2 {Hmisc}

#Logistic回归案例 6 亚组分析森林图
library(forestplot)
rs_forest <- read.csv('rs_forest.csv',header = FALSE)
# 读入数据的时候大家一定要把header设置成FALSE，保证第一行不被当作列名称。
# tiff('Figure 1.tiff',height = 1600,width = 2400,res= 300)
forestplot(labeltext = as.matrix(rs_forest[,1:3]),
           #设置用于文本展示的列，此处我们用数据的前三列作为文本，在图中展示
           mean = rs_forest$V4, #设置均值
           lower = rs_forest$V5, #设置均值的lowlimits限
           upper = rs_forest$V6, #设置均值的uplimits限
           is.summary = c(T,T,T,F,F,T,F,F,T,F,F),
           #该参数接受一个逻辑向量，用于定义数据中的每一行是否是汇总值，若是，则在对应位置设置为TRUE，若否，则设置为FALSE；设置为TRUE的行则以粗体出现
           zero = 1, #设置参照值，此处我们展示的是OR值，故参照值是1，而不是0
           boxsize = 0.4, #设置点估计的方形大小
           lineheight = unit(10,'mm'),#设置图形中的行距
           colgap = unit(3,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 1.5,#设置区间估计线的粗细
           col=fpColors(box='#458B00', summary= "#8B008B",lines = 'black',zero = '#7AC5CD'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           xlab="The estimates",#设置x轴标签
           graph.pos = 3)#设置森林图的位置，此处设置为3，则出现在第三列

## Poisson Regression

# look at dataset
data(breslow.dat, package="robust")
names(breslow.dat)
summary(breslow.dat[c(6, 7, 8, 10)])

# plot distribution of post-treatment seizure counts
opar <- par(no.readonly=TRUE)
par(mfrow=c(1, 2))
attach(breslow.dat)
hist(sumY, breaks=20, xlab="Seizure Count", 
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")
par(opar)

# fit regression
fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
summary(fit)

# interpret model parameters
coef(fit)
exp(coef(fit))

# evaluate overdispersion
deviance(fit)/df.residual(fit)
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type="poisson")

# fit model with quasipoisson
fit.od <- glm(sumY ~ Base + Age + Trt, data=breslow.dat,
              family=quasipoisson())
summary(fit.od)

## 第7章代码结束