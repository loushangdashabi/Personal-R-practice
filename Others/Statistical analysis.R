###描述性分析###
library(reshape2)
data("tips")
table(tips$sex,tips$smoker) #生成行×列表,第一个参数为行,第二个参数为列
hightip <- tips[,'tip']>mean(tips[,'tip'])

tt <- table(esoph$agegp,esoph$ncases)
addmargins(tt, margin = c(1,2)) #增加每行的总和或每列的总和

xtabs(~tips$sex+hightip, subset = tips$smoker=='No')
as.data.frame(xtabs(~tips$sex+hightip, subset = tips$smoker=='No'))
as.data.frame(xtabs(~tips$sex+hightip, subset = tips$day %in% c('Sun','Sat')))

xtabs(ncontrols~agegp+alcgp,data = esoph)
addmargins(xtabs(ncontrols~agegp+alcgp,data = esoph),margin = c(1,2))

xtabs(cbind(ncases,ncontrols)~agegp+alcgp,data = esoph) #对ncases和ncontrols进行分别计数，用c()和直接+的方式都无法实现
ftable(xtabs(cbind(ncases,ncontrols)~agegp+alcgp,data = esoph)) #扁平化表格形成类似data.frame的形式

#各种总体描述方式
summary()
library(psych)
describe(iris) #psych包中的describe()各个变量n、mean、sd、median等
library(Hmisc)
describe(iris) #Hmisc包中的describe()更为详细



###t检验###
data1 <- sample(1:100,50)

#1.正态性检验
shapiro.test(data1)
install.packages('nortest')
library(nortest)
lillie.test(data1)
ad.test(data1)
cvm.test(data1)
pearson.test(data1)
sf.test(data1)
#以上六种均为检验正态性的方法,算法不同导致的结果也可能不同

data2 <- rnorm(200,3,5) #生成100个均数为3标准差为5的数
data3 <- rnorm(200,3.4,8)
shapiro.test(data2)
shapiro.test(data3)

var.test(data2,data3) #检验方差齐性,仅检查两个变量之间

t.test(data2, mu = 3.2) #与总体均数进行比较
t.test(data2, data3, var.equal = F) #方差不齐var.equal设置为F(default)
t.test(data2, data3, paired = T) #配对样本的t检验



###数据变换###
data4 <- runif(100, min = 1, max = 2) #生成100个在1-2之间的数据

library(MASS)
data("trees")
shapiro.test(trees$Volume)

bc <- boxcox(Volume~log(Height)+log(Girth), data=trees,
             lambda = seq(-0.25,0.25,length=10))
lambda <- bc$x[which.max(bc$y)]
volume_bc <- (trees$Volume^lambda-1)/lambda

#Q-Q图
qqnorm(trees$Volume)
qqline(trees$Volume)
qqnorm(volume_bc)
qqline(volume_bc)

library(forecast)
BoxCox.lambda(trees$Volume)
#直接返回lambda的值,根据返回值进行不同的处理;-1取1/x,-0.5取1/sqrt(x),0取对数,0.5取sqrt(),1不进行变换

library(car)
powerTransform(trees$Volume) #生成指数,对原数据进行指数转换



###方差分析###
install.packages('multcomp')
library(multcomp)
data('cholesterol')
str(cholesterol)

#方差齐性检验
bartlett.test(response~trt, data = cholesterol)
fligner.test(response~trt, data = cholesterol)
library(car)
ncvTest(lm(response~trt, data = cholesterol))
leveneTest(response~trt, data = cholesterol)

shapiro.test(cholesterol$response)

#两种单因素方差分析的方法
summary(aov(response~trt, data = cholesterol))
oneway.test(response~trt, data = cholesterol, var.equal = T)

#方差分析的可视化
install.packages('gplots')
library(gplots)
plotmeans(response~trt, data = cholesterol)

#两两比较及可视化
TukeyHSD(aov(response~trt, data = cholesterol))
plot(TukeyHSD(aov(response~trt, data = cholesterol)))

#多因素方差分析
data('ToothGrowth')
str(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)
aov(len~supp + dose, data = ToothGrowth)
#公式的基本写法:
#~为分隔符,左边是应变量,右边是解释变量
#+为分隔符,分隔解释变量
#:表示两变量间有交互作用,eg: y~A+B+A:B
#*表示所有可能交互项,eg: y~A*B*C → y~A+B+C+A:B+A:C+B:C+A:B:C
#^表示交互项达到次数,eg: y~(A+B+C)^2 → y~A+B+C+A:B+A:C+B:C
#.表示包含除应变量之外的所有变量')
summary(aov(len~supp * dose, data = ToothGrowth))
interaction.plot(ToothGrowth$dose, ToothGrowth$supp, ToothGrowth$len, type = 'b', pch = c(1,10), col = c('red','green'))

#协方差分析
data('litter')
aov(weight~gesttime * dose, data = litter)

install.packages('effects')
library(effects)
effect('dose', aov(weight~gesttime * dose, data = litter)) #去除协变量的影响后的均数

#常见研究的表达式书写
#单因素ANOVA: y~A; 含有单个协变量: y~x+A
#双因素ANOVA: y~A*B; 含有两个协变量的双因素ANOVA: y~x1+x2+A*B
#随机化区组: y~B+A, B为区组因子
#单因素组内ANOVA: y~A+Error (Subject/A)
#含单个组内因子(W)和单个组内因子(B)的重复测量ANOVA: y~B*W+Error (Subject/W)



###卡方检验###
#1.拟合优度检验
men <- c(11,120,60,45)
women <- c(20,102,39,30)
df <- as.data.frame(rbind(men,women))
colnames(df) <- c('AB','A','B','O')

chisq.test(men, p = c(0.1,0.5,0.2,0.2)) #检测men组中各个血型的分布是否相同,p参数为总体的分布情况

#2.卡方齐性检验、卡方独立性检验
chisq.test(df)

#3.CMH检验,行变量为无序分类、列变量为有序分类,不能进行Pearson卡方检验
rabbits <- array(c(0,0,6,5,
                   3,0,3,6,
                   6,2,0,4,
                   5,6,1,0,
                   2,5,0,0), dim = c(2,2,5), dimnames = list(dalay = c('None','1.5h'), response = c('Cured','Died'),
                                                             Penicillin.Level = c('1/8','1/4','1/2','1','4')))
mantelhaen.test(rabbits)

satisfaction <- as.table(array(c(1,2,0,0,3,3,1,2,
                                 11,17,8,4,2,3,5,2,
                                 1,0,0,0,1,3,0,1,
                                 2,5,7,9,1,1,3,6), dim = c(4,4,2),
                               dimnames = list(Income = c('<5000','5000-15000','15000-25000','>25000'),
                                               'Job Satisfaction'=c('V_D','L_S','M_S','V_S'),
                                               Gender = c('Female','Male'))))
mantelhaen.test(satisfaction)

#4.配对四格表的卡方检验
paired <- as.table(matrix(c(157,24,69,18), nrow = 2, dimnames = list(case = c('A','B'), control = c('A','B'))))
mcnemar.test(paired)



###回归分析###
x <- seq(1,5,len = 100)
noise <- rnorm(n = 100,mean = 0,sd = 1)
beta0 <- 1
beta1 <- 2
y <- beta0 + beta1*x +noise
plot(y~x)
summary(lm(y~x))

x <- factor(rep(c(0,1,2),each = 20))
y <- c(rnorm(20,0,1),rnorm(20,1,1),rnorm(20,2,1))
summary(lm(y~x))



###模型诊断###
#★1.非正态性检验
data(LMdata, package = 'rinds')
model <- lm(y~x, data = LMdata$NonL)
res1 <- residuals(model) #生成残差
shapiro.test(res1) #检验残差是否符合正态性

#★2.非线性
model2 <- lm(y~x, data = LMdata$NonL)
plot(model2)
model2 <- lm(y~x + I(x^2), data = LMdata$NonL)
model3 <- update(model2,y~.-x)
summary(model3)

AIC(model,model2,model3)
plot(model3$residuals~LMdata$NonL$x)

#3.异方差
model4 <- lm(y~x, data = LMdata$Hetero)
plot(model4$residuals~LMdata$Hetero$x)
model5 <- lm(y~x, weights = 1/x^2, data = LMdata$Hetero) #加权最小二乘法

library(nlme)
summary(gls(y~x, weights = varFixed(~x), data = LMdata$Hetero)) #迭代最小二乘法,返回一个最佳模型

#4.自相关
model6 <- lm(y~x, data = LMdata$AC)

library(lmtest)
dwtest(model6) #p小于0.05则表示残差存在自相关,默认最小二乘法不能使用
summary(gls(y~x, correlation = corAR1(), data = LMdata$AC)) #进行广义的最小二乘法

#5.异常值,离群点、杠杆点、★高影响点
model7 <- lm(y~x, data = LMdata$Outlier)
plot(model7)
abline(model7)

library(car)
infl <- influencePlot(model7)
model8 <- update(model7, y~x, subset = -32, data = LMdata$Outlier)
plot(y~x, data = LMdata$Outlier)
abline(model7, col = 'red')
abline(model8, col = 'green')

#★6.多重共线性
model9 <- lm(y~x1+x2+x3, data = LMdata$Mult)
summary(model9) #单个因素没有统计学意义,但总体因素具有统计学意义,考虑多重共线性

vif(model9) #求方差膨胀因子,大于10则存在共线性

model10 <- step(model9) #返回AIC值取最小的模型
summary(model10)



###logistic回归###
library(HSAUR2)
data("plasma")
head(plasma)

fit1 <- glm(ESR~fibrinogen +globulin,data=plasma,family = binomial())
summary(fit1)

exp(coef(fit1)['fibrinogen']) #提取参数并求OR值
exp(confint(fit1,parm='fibrinogen')) #求95%的可信区间



###生存分析###
library(coin)
data(glioma)

library(survival)
g3 <-subset(glioma,histology='Grade3') #取histology='Grade3'的子集
fit <- survfit(Surv(time,event)~group,data = g3)

plot(fit,lty=c(2,1),col=c(2,1))
legend('bottomright',legend = c('control','treatment'),lty=c(2,1),col=c(2,1))#在底部右侧加入图例

#logrank检验
#不能传模型,要传原先的公式
#survival包中的检验
survdiff(Surv(time,event)~group,data = g3)

#coin包中的检验
logrank_test(Surv(time,event)~group,data = g3,distribution='exact')

#两种组织学的与对照组相比都有统计学差异,不方便进行精确比对,采用反复代入1000次
logrank_test(Surv(time,event)~group|histology,data = glioma,distribution=approximate(nresample=1000))




###COX回归###
data('GBSG2',package = 'TH.data')
head(GBSG2)
#mark.time显示生存时间标记,一般不标
plot(survfit(Surv(time,cens)~horTh,data=GBSG2),lty=c(2,1),col=c(2,1),mark.time = F)
legend('bottomright',legend = c('yes','no'),lty=c(2,1),col=c(2,1))

#生存分析
coxreg <- coxph(Surv(time,cens)~.,data=GBSG2)

#深入挖掘cox
library(party)
tree <- ctree(Surv(time,cens)~.,data=GBSG2)
plot(tree)

