x <- seq(1,5,len = 100)
noise <- rnorm(n = 100,mean = 0,sd = 1)
beta0 <- 1
beta1 <- 2
y <- beta0 + beta1*x +noise
plot(y~x)



library(devtools)
install_github('lijian13/rinds')

library(HSAUR2)
data("plasma")
head(plasma)


fit1 <- glm(ESR~fibrinogen +globulin,data=plasma,family = binomial())

exp(coef(fit1)['fibrinogen'])

exp(confint(fit1,parm='fibrinogen'))



#生存分析

#创建coin包中的数据集
library(coin)
data(glioma)
g3 <-subset(glioma,histology='Grade3')#取histology='Grade3'的子集

fit <- survfit(Surv(time,event)~group,data = g3)

plot(fit,lty=c(2,1),col=c(2,1))
#在底部右侧加入图例
legend('bottomright',legend = c('control','treatment'),lty=c(2,1),col=c(2,1))

#logrank检验
#不能传模型，要穿原先的公式
#survival包中的检验
survdiff(Surv(time,event)~group,data = g3)

#coin包中的检验
logrank_test(Surv(time,event)~group,data = g3,distribution='exact')

#两种组织学的与对照组相比都有无统计学差异，不方便进行精确比对，采用反复代入1000次
logrank_test(Surv(time,event)~group|histology,data = glioma,distribution=approximate(nresample=1000))

###COX回归###
library(survival)
data('GBSG2',package = 'TH.data')
head(GBSG2)
#mark.time显示生存时间标记，一般不标
plot(survfit(Surv(time,cens)~horTh,data=GBSG2),lty=c(2,1),col=c(2,1),mark.time = T)
legend('bottomright',legend = c('yes','no'),lty=c(2,1),col=c(2,1))
#生存分析
coxreg <- coxph(Surv(time,cens)~.,data=GBSG2)

#深入挖掘cox
library(party)
tree <- ctree(Surv(time,cens)~.,data=GBSG2)
plot(tree)

###双因素方差分析###
#交互作用显著，主效应不显著，也应该保留主效应
data('ToothGrowth')
head(ToothGrowth)
table(ToothGrowth$supp,ToothGrowth$dose)
#如果只研究主效应用“+”，如果只研究交互作用用“:”，两者都考虑用“*”
fangcha <- aov(len~supp*dose,data = ToothGrowth)
summary(fangcha)
#可视化
interaction.plot(ToothGrowth$dose,ToothGrowth$supp,ToothGrowth$len,
                 type = 'b',col = c('blue','black'))



#包的加载方法
installed.packages()#查看已经安装的R包
.libPaths()#显示R包所在的位置

install.packages('ggplot2')
install.packages('devtools')
library(devtools)#包的名字不用引号

install_github('lijian13/rinds')#加载GitHub上的包

#R包的卸载
#remove.packages('包名')，或者直接在packages里面点×

#向量
x <- 1 #Alt + -快捷输入 <- 
z <- 1:5 #形成1-5的正整数
a <- c(1,2,4,3,5)

identical(a,z) #判断a,z是否相同，R语言的方向体现在index的一致性

vector1 <- 1:10
vector1[1:4] #访问vector中index为1-4的元素
vector1[-c(2,5,8)] #去掉index为2,5,8的元素；c为combine，为结合的意思
vector2 <- c(1,4,'ABC','nihao')

x <- 1:5
y <- 6:10
result <- x+y #相同index的元素相加
x <- 1
result2 <- x+y #x中仅有一个元素，所有y中元素都加x
x <- 1:2
result3 <- x+y #报错，长目标元素个数不是短目标元素个数的倍数
y <- 6:11
result3 <- x+y #按照短目标元素index以此循环加至长目标元素

#数值型向量
vector1 <- c(1:3,6:10)
x <- 1
class(x)
is.integer(x) #判断是否是整型
y <- 1:3
is.integer(y)

seq(from = 1, to = 5, by = 1) #从1到5，生成步长为1
seq(10,1,-1) #简写
seq(1,5,length.out = 8) #生成8位的向量，也可以直接len=8
seq(1,5,along.with = 1:3) #以along.with后面的序列长度为模板生成特定规律的向量
seq(1,7,along.with = 1:3)

rep(c(1,3),times=5) #有规律有方法地重复元素，输出1 3 1 3 1 3 1 3 1 3
rep(c(1,3),each=5) #输出1 1 1 1 1 3 3 3 3 3
rep(c(1,3),len=9)

x <- rep(c(1:2),times=4)
length(x)

#逻辑型向量
logit <- rep(c(T,F),len=5) #T为1，F为0
sum(logit)

#逻辑表达式
#逻辑判断符&表示和、|表示或，其余与python中相同,仅输出TRUE和FALSE
x <- seq(1,100,len=20)
index <- x>80 #index为逻辑型
x[index] #输出index中为TURE的

which(x>80) #which函数为数值型
x[which(x>80)]

x[which(x>80 & x<90)]
x[x>80 & x<90]

#字符串
string <- c('abc','def',1,2) #统一被c函数整合为
is.character(string) #字符串格式被称为是character

letters #内置的所有小写字母的字符串向量
LETTERS #内置的所有大写字母的字符串向量
#如果提取的index超过向量的最大index会返回NA(NOT available)


#因子型向量（分类变量，分为有序和无序）
my_fac <- factor(x =rep(c(1,2), time=5), levels = c(1,2), labels = c('Male','Female'))
class(my_fac) #输出"factor"

my_fac2 <- factor()































