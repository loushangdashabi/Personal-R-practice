#一些常用的环境代码
Sys.setlocale(locale = "chinese")
setwd('D:/R_workspace')
getwd()










###3.图形初阶####
attach(mtcars) #绑定数据框
plot(wt,mpg) #生成散点图
abline(lm(mpg~wt)) #生成最优拟合曲线
title('Regression of MPG on Weight') 
detach(mtcars) #解除数据框绑定

#生成pdf文件,win.metafile(),png(),jpeg(),tiff(),xfig(),postscript()可以生成其他格式
#或者直接右键图片另存
pdf('mygraph.pdf')
attach(mtcars) #绑定数据框
plot(wt,mpg) #生成散点图
abline(lm(mpg~wt)) #生成最优拟合曲线
title('Regression of MPG on Weight') 
detach(mtcars) #解除数据框绑定
dev.off()

dev.new() #打开新的作图窗口
dev.next()
dev.prev()
dev.set()

dose <- c(20,30,40,45,60)
drugA <- c(16,20,27,40,60)
drugB <- c(15,18,25,31,40)
plot(dose, drugA, type = 'b') #type = 'b'表示同时绘制点和线
par() #用于修改作图时的参数,无参数运行返回当前所有参数值列表,也可以直接在plot()中设置参数
par(no.readonly = T) #返回当前图形的参数设置
#pch为绘制点的形状(0-25),cex表示点的大小,lyt为绘制线条的类型(1-6),lwd是绘制线条的粗细

install.packages('RColorBrewer')
library(RColorBrewer)
#调用Set1中所有的颜色
mc <- brewer.pal(7,'Set1')
barplot(rep(1,7),col = mc)
brewer.pal.info #返回所有的颜色数据
display.brewer.all() #返回所有调色板及颜色

pie(rep(1,10),labels = rainbow(10),col = rainbow(10))
pie(rep(1:10),labels = gray(0:10/10),col = gray(1:10/10))
rainbow(10)

opar <- par(no.readonly = T)
par(pin=c(2,3)) #修改图形大小为宽2英寸、长3英寸
par(lwd=2,cex=1.5) #修改线条宽度为原来两倍、符号大小为原来1.5倍
par(cex.axis=.75,font.axis=3) #修改刻度线文本符号大小和字体
plot(dose, drugA, type = 'b', pch=19, lty=2, col='red')
plot(dose, drugB, type = 'b', pch=23, lty=6, col='blue', bg='green')
par(opar)

plot(dose, drugA, type = 'b', col = 'red', lty = 2, pch = 2, lwd = 2,
     main = 'Clinical Trials for DrugA',
     sub = 'This is hypothetical data',
     xlab = 'Dosage', ylab = 'Drug Response',
     xlim = c(0,60), ylim = c(0,70))

#增加标题
title(main = 'Clinical Trials for DrugA',
      sub = 'This is hypothetical data',
      xlab = 'Dosage', ylab = 'Drug Response',
      col.lab = 'green', cex.lab = 0.75)

#自定义坐标轴
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly = T)
par(mar = c(5,4,4,8) + 0.1)
plot(x, y, type = 'b', pch = 21, col = 'red', yaxt = 'n', lty = 3, ann = F)
#xaxt = 'n',yaxt = 'n'表示禁用x轴、y轴,axes = F表示禁用所有坐标轴,ann=F禁用所有标题
lines(x, z, type = 'b', pch = 22, col = 'blue', lty = 2) #为现有图形添加新的图形元素
axis(2, at = x, labels = x, col.axis = 'red', las = 2)
#las=0平行坐标轴,las=2垂直坐标轴,at为刻度线的位置,labels为刻度线的标识
axis(4, at = z, labels = round(z, digits = 2),
     col.axis = 'blue', las = 2, cex.axis = 0.7, tck = -.01) #tck表示刻度线相对于绘图区域大小,负右正左
mtext('y=1/x', side = 4, line = 3, cex.lab = 1, las = 2, col = 'blue') #在图形边界添加文本
title('An Example of Create Axes', xlab = 'X values', ylab = 'Y=X')
par(opar)

#添加次要刻度线
library(Hmisc)
minor.tick(nx=2, ny=3, tick.ratio = 0.5)

#添加参考线
abline(h = c(1,5,7), v = seq(1,10,2), lty = 2, col = 'blue')

legend()
par(lwd = 2, cex = 1.5, font.lab = 2)
plot(dose, drugA, type = 'b', pch = 15, lty = 1, col = 'red', ylim = c(0,60),
     main = 'DrugA vs. DrugB', xlab = 'Drug Doseage', ylab = 'Drug Response')
lines(dose, drugB, type = 'b', pch = 17, lty = 2, col = 'blue')
abline(h = 30, lwd = 1.5, lty = 2, col = 'gray')
minor.tick(nx = 3, ny = 3, tick.ratio = 0.5)
legend('topleft', inset = .05, title = 'Drug Type', c('A','B'),
       lty = c(1,2), pch = c(15,17), col = c('red','blue')) #添加图例

attach(mtcars)
plot(wt, mpg)
text(wt, mpg, row.names(mtcars), cex = 0.6, pos = 4, col = 'red')
#在点右侧添加文本标注(pos参数1下2左3上4右)
detach(mtcars)

plot(1:7, 1:7, type = 'n')
text(3,3,'Example of default text')
text(4,4,family='mono','Example of mono-spaced text')
text(5,5,family='serif','Example of serif text')

help(plotmath)
demo(plotmath)
plotmath() #添加公式等

#图形的组合
attach(mtcars)
par(mfrow = c(2,2)) #通过mfrow的图形参数实现
plot(wt, mpg, main = 'Scatterplot of wt vs. mpg')
plot(wt, disp, main = 'Scatterplot of wt vs. disp')
hist(wt, main = 'Histogram of wt') #hist()自带标题
boxplot(wt, main = 'Boxplot of wt')
detach(mtcars)
dev.new()

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

#图形布局的精细控制
par(fig = c(0,0.8,0,0.8))
plot(mtcars$wt, mtcars$mpg, xlab = 'Mile Per Gallon', ylab = 'Car Weight')
#上方添加箱图
par(fig = c(0,0.8,0.55,1), new = T)
boxplot(mtcars$wt, horizontal = T, axes = F)
#右方添加箱图
par(fig = c(0.65,1,0,0.8), new = T)
boxplot(mtcars$mpg, axes = F)
mtext('Enhanced Scatterplot', side = 3, outer = T, line = -3)



###4.基本数据管理####
manager <- c(1, 2, 3, 4, 5)
date <- c("10/24/08", "10/28/08", "10/1/08", "10/12/08", "5/1/09")
country <- c("US", "US", "UK", "UK", "UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32, 45, 25, 39, 99)
q1 <- c(5, 3, 3, 3, 2)
q2 <- c(4, 5, 5, 3, 2)
q3 <- c(5, 2, 5, 4, 1)
q4 <- c(5, 5, 5, NA, 2)
q5 <- c(5, 5, 2, NA, 1)
leadership <- data.frame(manager, date, country, gender, age, q1, q2, q3, q4, q5, stringsAsFactors=FALSE)
mydata <- data.frame(x1 = c(2, 2, 6, 4), x2 = c(3, 4, 2, 8))
##1.三种方式增加变量
#1
mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2
#2
attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)
#3
mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2)/2)

##2.变量的重编码
leadership$age[leadership$age == 99] <- NA
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 & leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"
#紧凑写法
leadership <- within(leadership, {
  agecat <- NA
  agecat[age > 75] <- 'Elder'
  agecat[age >= 55 & age <= 75] <- 'Middle Aged'
  agecat[age < 55] <- 'Young'
})

#3.变量的重命名
fix(leadership) #弹出交互式编辑器进行
names(leadership)[2] <- 'testDate'
names(leadership)[6:10] <- c('item1', 'item2', 'item3', 'item4', 'item5')

leadership <- plyr::rename(leadership, c(manager = 'managerID', date = 'testDate'))
#Inf和-Inf表示无穷,不可能的值用NaN
leadership <- na.omit(leadership) #消除所有含NA的行

##4.日期
mydates <- as.Date(c("2007-06-22", "2004-02-13"))






###6.基本图形####
opar <- par(no.readonly = T)
par(opar)

##1.条形图
#1.1简单条形图
library(vcd)
counts <- table(Arthritis$Improved)
barplot(counts, main = 'Horizontal Bar Plot', 
        xlab = 'Frequency', ylab = 'Improvement', horiz = T) #horiz参数把条形图转为水平条形图
plot(Arthritis$Improved, main = 'Horizontal Bar Plot', 
     xlab = 'Frequency', ylab = 'Improvement', horiz = T) #或者直接用plot函数
#1.2堆砌条形图和分组条形图
counts <- table(Arthritis$Improved, Arthritis$Treatment)
#堆砌条形图
barplot(counts, main = 'Stacked Bar Plot', xlab = 'Treatment', ylab = 'Frequency',
        col = c('red', 'yellow', 'green'), legend = rownames(counts))
#分组条形图
barplot(counts, main = 'Grouped Bar Plot', xlab = 'Treatment', ylab = 'Frequency',
        col = c('red', 'yellow', 'green'), legend = rownames(counts), beside = T)
#1.3均值条形图
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by = list(state.region), FUN = mean)
means <- means[order(means$x),] #将均值从小到大排序
barplot(means$x, names.arg = means$Group.1) #names.arg表示添加分组为坐标轴标题
title('Mean Illiteracy Rate')
#1.4条形图的微调
par(mar = c(5,8,4,2)) #增加y边界的大小
par(las = 2) #旋转条形标签
counts <- table(Arthritis$Improved)
barplot(counts, main = 'Treatment Outcome', horiz = T, cex.names = 0.8, names.arg = c('No Iprovement','Some Improvement','Marked Improvement'))
#1.5棘状图
library(vcd)
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = 'Spinogram Example')
detach(Arthritis)

##2.饼图
par(mfrow = c(2,2)) #分成2行2列的作图格局
slices <- c(10,12,4,16,8)
pie(slices, labels = c('US','UK','Australia','Germany','France'), main = 'Simple Pie Example')
pct <- round(slices/sum(slices)*100)
pie(slices, labels = paste(c('US','UK','Australia','Germany','France'), ' ', pct, '%', sep = ''),
    col = rainbow(5),main = 'Pie Chart with Percentages')
install.packages('plotrix')
library(plotrix)
pie3D(slices, labels = c('US','UK','Australia','Germany','France'), explode = 0.1, main = '3D Pie Chart')
pie(table(state.region), labels = paste(names(state.region), '\n', table(state.region), sep = ''),
    main = 'Pie Chart from a Table\n (with sample sizes)')
fan.plot(slices, labels = c('US','UK','Australia','Germany','France'), main = 'Fan Plot')

##3.直方图
par(mfrow = c(2,2))
hist(mtcars$mpg) #简单直方图
hist(mtcars$mpg, breaks = 12, col = 'red', 
     xlab = 'Miles Per Gallon', main = 'Colored histogram with 12 bins') #自定义组段数直方图
hist(mtcars$mpg, freq = F, breaks = 12,
     xlab = 'Miles Per Gallon', main = 'Histogram, rug plot, density curve')
rug(jitter(mtcars$mpg)) #增加轴须图
lines(density(mtcars$mpg), col = 'blue', lwd = 2) #增加密度曲线(核密度估计)
rug(jitter(mtcars$mpg, amount = 0.01)) #打散数据
h <- hist(mtcars$mpg, breaks = 12, xlab = 'Miles Per Gallon', main = 'Histogram with normal curve and box')
#增加正态拟合曲线
xfit <- seq(min(mtcars$mpg), max(mtcars$mpg), length = 40)
yfit <- dnorm(xfit, mean = mean(mtcars$mpg), sd = sd(mtcars$mpg))
yfit <- yfit*diff(h$mids[1:2])*length(mtcars$mpg)
lines(xfit, yfit, col = 'blue', lwd = 2)
box() #增加外框

##4.核密度图
par(mfrow = c(2,1))
plot(density(mtcars$mpg))
plot(density(mtcars$mpg), main = 'Kernel Density of Miles Per Gallon')
polygon(density(mtcars$mpg), col = 'red', border = 'blue') #用于根据x和y值绘制多边形,红色填充,蓝色曲线
rug(mtcars$mpg, col = 'brown') #增加棕色轴须图
#可视化分布与比较组间差异
install.packages('sm')
library(sm)
attach(mtcars)
cylf <- factor(cyl, levels = c(4,6,8), labels = c('4 cylinder', '6 cylinder', '8 cylinder'))
sm.density.compare(mpg, cylf, xlab = 'Miles Per Gallon') #无法增加标题?
title(main = 'MPG Distribution by Car Cylinders')
colfill <- c(2:(1+length(levels(cylf))))
legend(locator(1), levels(cylf), fill = colfill) #locator(1)表示鼠标点击出现图例,fill为指定颜色函数
detach(mtcars)

##5.箱线图
boxplot(mtcars$mpg, main = "Box Plot", ylab = 'Miles per Gallon')
boxplot.stats(mtcars$mpg) #返回用于构建图形的统计量
#5.1箱线图的跨组比较
boxplot(mpg~cyl, data = mtcars, varwidth = T, horizontal = T,
        main = 'Car Mileage Data', xlab = 'Number of Cylinders', ylab = 'Miles per Gallon')
#varwidth使箱线图的宽度与样本量的平方根成正比
boxplot(mpg~cyl, data = mtcars, varwidth = T, notch = T,
        main = 'Car Mileage Data', xlab = 'Number of Cylinders', ylab = 'Miles per Gallon')
#notch参数用于生成含有凹槽的箱图,凹槽不重叠表明中位数之间存在显著差异
mtcars$cylf <- factor(mtcars$cyl, levels = c(4,6,8), labels = c('4','6','8'))
mtcars$amf <- factor(mtcars$am, levels = c(0,1), labels = 'auto','standard')
boxplot(mpg~amf*cylf, data = mtcars, varwidth = T, col = c('gold','darkgreen'),
        main = 'MPG Distribution by Auto Type', xlab = 'Auto Type', ylab = 'Miles per Gallon', las = 1)
#5.2小提琴图(箱线图与核密度图的结合)
install.packages('vioplot')
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4]
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3, names = c('4 cylinder', '6 cylinder', '8 cylinder'))

##6.点图
dotchart(mtcars$mpg, labels = row.names(mtcars), cex = .7,
         main = 'Gas Mileage for Car Models', xlab = 'Miles per Gallon', las = 1)
x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- 'red'
x$color[x$cyl==6] <- 'blue'
x$color[x$cyl==8] <- 'darkgreen'
dotchart(x$mpg, labels = row.names(x), cex = .7, groups = x$cyl, gcolor = 'black', color = x$color,
         pch = 19, main = 'Gas Mileage for Car Models\ngrouped by cylinder', xlab = 'Miles per Gallon')



###7.基本统计分析####
##1.描述性统计分析
myvars <- c('mpg','hp','wt')
head(mtcars[myvars])
summary(mtcars[myvars])

#自定义函数进行描述性分析
mystate <- function(x, na.omit = F) {
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n-3
  return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
}
sapply(mtcars[myvars], mystate)

library(Hmisc)
describe(mtcars[myvars])

library(pastecs)
stat.desc(mtcars[myvars], basic = T, desc = T, norm = T, p = 0.95)
#basic描述所有值,缺失值,空值的数量,最大最小值,极差
#desc描述中位数,均数,均数标准误,均数95%置信区间,方差,标准差,变异系数
#norm描述正态分布统计量,偏度,峰度及SW正态检验的结果

library(psych)
describe(mtcars[myvars])
#不同包存在相同函数名时,后载入包的函数会覆盖前载入的包,可以用Hmisc::describe()调用

#分组描述
aggregate(mtcars[myvars], by = list(am = mtcars$am), mean)
aggregate(mtcars[myvars], by = list(am = mtcars$am), sd)
by(mtcars[myvars], mtcars$am, function(x)sapply(x, mystate))

library(doBy)
summaryBy(mpg+hp+wt~am, data = mtcars, FUN = mystate)

psych::describeBy(mtcars[myvars], list(am = mtcars$am)) #可以存在交叉变量,list(name1 = groupvar1, name2 = groupvar2,……)

##2.频数表和列联表
library(vcd)
mytable <- with(Arthritis, table(Improved))
table(Arthritis$Improved)
prop.table(mytable)*100 #转化为比例或百分比

mytable <- table(Arthritis$Treatment, Arthritis$Improved) #前面为行变量,后面为列变量
xtabs(~ Treatment + Improved, data = Arthritis)
margin.table(mytable, 2) #返回边际数,数字表示返回第几个因子
prop.table(mytable, 1) #返回边际比例数,数字表示返回第几个因子,不加参数返回占总体的比例数
addmargins(mytable) #添加边际数,1添加一行边际数,2添加一列边际数
addmargins(prop.table(mytable)) #添加边际比例
addmargins(prop.table(mytable, 2), 1)
#table()中默认忽略NA,useNA = 'ifany'可以将NA设定为有意义

library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved, 
           digits = 3, expected = T, #设置小数位数,添加期望值并返回Pearson's Chi-squared test结果
           prop.r = T, prop.c = F, prop.t = F, prop.chisq = F, #添加行比例,列比例,总体比例,卡方检验贡献
           chisq = T, fisher = T, mcnemar = F #返回Pearson's Chi-squared test,fisher,mcnemar结果
           )
#类似SPSS的输出风格,可以用来做卡方,Fisher,Mcnemar

#多维列联表
mytable <- xtabs(~ Treatment + Sex + Improved, data = Arthritis)
ftable(mytable) #重新整合多维列联表,形成紧凑的表达形式
margin.table(mytable, c(1,3)) #返回第一个因子×第三个因子的列联表
ftable(prop.table(mytable, c(1,2))) #返回第一个因子×第二个因子的比例情况
ftable(addmargins(prop.table(mytable, c(1,2)),3))

#独立性检验
mytable <- xtabs(~Treatment + Improved, data = Arthritis)
chisq.test(mytable) #p-value<0.05表示不独立
fisher.test(mytable) #不能用于2×2表
#Cochran-Mantel-Haenszel卡方检验
#原假设:两个名义变量在第三个变量的每一层中是条件独立
mytable <- xtabs(~Treatment + Improved + Sex, data = Arthritis)
mantelhaen.test(mytable) #第三个变量为检测的第三层,注意生成table时变量的顺序

#相关性的度量
mytable <- xtabs(~Treatment + Improved, data = Arthritis)
vcd::assocstats(mytable) #返回phi系数,列联系数,Cramer's V系数
vcd::Kappa() #返回kappa系数

##3.相关
cor(state.x77, use = 'everything', method = 'pearson') #计算相关系数,method可为"pearson" (default), "kendall", or "spearman"
cov(state.x77) #计算协方差

#偏相关
library(ggm)
state <- state.x77[,1:6]
ggm::pcor(c(1,5,2,3,6), cov(state.x77[,1:6]))

#相关性的显著性检验
cor.test(state[,3], state[,5]) #检验两个变量之间相关性是否显著
psych::corr.test(state, use = 'complete') #检验多对变量之间相关性是否显著,use采用'pairwise'或'complete'分别表示成对或成行删除缺失值

##4.t检验
#4.1独立样本t检验
t.test(y ~ x, data, var.equal = F, alternative = 'less') #y为数值型变量,x为二分变量,方差设置为不等,单侧检验设置为'less','greater'
t.test(y1, y2)
library(MASS)
t.test(Prob ~ So, data = UScrime)
#4.2非独立样本t检验(配对t检验)
sapply(UScrime[c('U1','U2')], function(x)(c(mean = mean(x), sd = sd(x))))
with(UScrime, t.test(U1, U2, paired = T))

##5.组间差异的非参数检验
#5.1Wilicoxon秩和检验
wilcox.test(y ~ x, data) #y为数值型变量,x为二分变量,添加exact参数精确检验,单侧检验设置为'less','greater'
wilcox.test(y1, y2)
wilcox.test(Prob ~ So, data = UScrime)
with(UScrime, wilcox.test(U1, U2, paired = T)) #配对样本的秩和检验
#5.2多于两组的比较
#Kruskal-Wallis检验,用于各组独立的情况
kruskal.test(y ~ A, data) #y为数值型变量,A为分组变量
state <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data = state)
#wmc()函数,采用p.adj()调整
source("http://www.statmethods.net/RiA/wmc.txt") #得到函数
states <- data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data=states, method="holm")



###8.回归####
##1.OLS回归
fit <- lm(weight ~ height, data = women)
summary(fit)
fitted(fit)
residuals(fit)
plot(women$height, women$weight, xlab = 'Height (in inches)', ylab = 'Weight (in pounds)')
abline(fit)

##2.多项式回归
fit2 <- lm(weight ~ height + I(height^2), data = women)
summary(fit2)
plot(women$height, women$weight, xlab = 'Height (in inches)', ylab = 'Weight (in pounds)')
lines(women$height, fitted(fit2))
#绘制二元关系图
library(car)
scatterplot(weight ~ height, data = women, spread = T, smoother.args = list(lty = 2), pch = 19, col = 'black',
            main = 'Women Age 30-39', xlab = 'Height (in inches)', ylab = 'Weight (lbs.)')
#spread用于增加残差正负均方根在平滑曲线上的展开和非对称信息

##3.多元线性回归
state.x77
states <- as.data.frame(state.x77[, c('Murder', 'Population', 'Illiteracy', 'Income', 'Frost')])
cor(states) #计算相关系数
car::scatterplotMatrix(states, spread = F, smoother.args = list(lty = 2), main = 'Scatter Plot Matrix')
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data = states)
summary(fit)
fit2 <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit2)
library(effects)
plot(effect('hp:wt', fit2, , list(wt = c(2.2, 3.2, 4.2))), multiline = T)

#3.回归诊断
fit <- lm(weight ~height, data = women)
par(mfrow = c(2, 2))
plot(fit)

dev.new()





























###11.中级绘图####
opar <- par(no.readonly = T)
par(opar)
dev.new()

##1.散点图
attach(mtcars)
plot(wt, mpg, pch = 19, las = 1)
abline(lm(mpg~wt), col = '#12338E', lwd = 2, lty = 1)
lines(lowess(wt, mpg), col = '#FF0F97', lwd = 2, lty = 2) #lowess用于添加一条平滑曲线
detach(mtcars)

library(car)
scatterplot(mpg~wt|cyl, data = mtcars, lwd = 2, span = .75,
            main = 'Scatter Plot of MPG vs. Weight by # Cylinders',
            xlabel = 'Weight of Car (lbs/1000)', ylab = 'Miles per Gallon',
            legend.plot = T, boxplots = 'xy')
with(mtcars, showLabels(wt, mpg,
                        labels = row.names(mtcars), method="identify"))
#1.1散点矩阵图
pairs(~mpg+disp+drat+wt, data = mtcars, upper.panel = NULL)
#生成的矩阵图主对角线上下图片对称相同,upper.panel = NULL可以只生成下三角的图
library(car)
scatterplotMatrix(~mpg+disp+drat+wt, data = mtcars, spread = F, smoother.args = list(lty=2))
#spread = F表示不添加展示分散度和对称信息的直线
#1.2高密度散点图
set.seed(08021000)
c1 <- matrix(rnorm(10000, mean = 0, sd = .5), ncol = 2)
c2 <- matrix(rnorm(10000, mean = 3, sd = 2), ncol = 2)
mydata <- as.data.frame(rbind(c1,c2))
names(mydata) <- c('x','y')
with(mydata, plot(x, y, pch = 19)) #普通的散点图重叠严重
with(mydata, smoothScatter(x, y)) #通过颜色密度解决重叠问题
#以六边形单元格形式展现颜色密度
install.packages('hexbin')
library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins = 50)
  plot(bin)
  })
#1.3三维散点图
install.packages('scatterplot3d')
library(scatterplot3d)
attach(mtcars)
s3d <- scatterplot3d(wt, disp, mpg, pch = 16, highlight.3d = T, type = 'h')
s3d$plane3d(lm(mpg~wt+disp)) #增加拟合平面
detach(mtcars)
#1.4旋转散点图
install.packages('rgl')
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col = 'red', size = 5)
detach(mtcars)
library(car)
with(mtcars, scatter3d(wt, disp, mpg))
#1.5气泡图
symbols(x, y, circle = sqrt(z/pi)) #circle为半径参数,此方法以面积代表第三个变量
attach(mtcars)
symbols(wt, mpg, circles = sqrt(disp/pi), inches = 0.30,
        fg = 'white', bg = 'lightblue')
text(wt, mpg, rownames(mtcars), cex = 0.6)
detach(mtcars)

##2.折线图
par(mfrow = c(1,2))
t1 <- subset(Orange, Tree == 1)
plot(t1$age, t1$circumference)
plot(t1$age, t1$circumference, type = 's')
lines(t1$age, t1$circumference) #在原图上增加折线
#type参数设置:p只有点,l只有线,o实心点和线,b空心点和线,c不绘点的线,s(先横后竖)和S(先竖后横)阶梯线,h为直方图式垂直线,n无线无点
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$Tree)
yrange <- range(Orange$Tree)
plot(xrange, yrange, type = 'n', xlab = 'Age (days)', ylab = 'Circumference (mm)')
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18,18+ntrees,1)
for (i in 1:ntrees){
  tree <- subset(Orange, Tree == i)
  lines(tree$age, tree$circumference, type = 'b', lwd = 2, lty = linetype[i],
        col = colors, pch = plotchar[i])
}
title('Tree Growth', 'example of line plot')
legend(xrange[1], yrange[2], 1:ntrees, cex = 0.8, col = colors, pch = plotchar, lty = linetype, title = 'Tree')

##3.相关图
#获取各个变量间的相关系数
options(digits = 2) #设置显示证书表达位数,默认为7位,取值1-22
cor(mtcars)
install.packages('corrgram')
library('corrgram')
corrgram(mtcars, order = T, lower.panel = panel.shade, upper.panel = panel.pie,
         text.panel = panel.txt)
#蓝色左下到右上的线表示正相关,红色左上到右下的线表示负相关
#order参数设置为T的时候表示采用主成分分析法对变量进行排序
corrgram(mtcars, order = T, lower.panel = panel.ellipse, upper.panel = panel.pts,
         text.panel = panel.txt, diag.panel = panel.minmax)
corrgram(mtcars, lower.panel = panel.shade, upper.panel = NULL, text.panel = panel.txt,
         col.regions = colorRampPalette(c('darkgoldenrod4','burlywood1','darkkhaki','darkgreen')))

#4.马赛克图
library(vcd)
mosaic(Titanic, shade = T, legend = T)
#shade = T将根据拟合模型的Pearson残差值图进行上色,legend显示图例
#蓝色表示生存率高于预期值,红色相反
example("mosaic")











###19.使用ggplot2进行高级绘图####
library(car)
library(lattice)
library(gridExtra)
library(ggplot2)
ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + 
  labs(title = 'Automobile Data', x = 'Weight', y = 'Miles per Gallon')
ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
  geom_point(pch = 17, color = 'blue', size = 2) + 
  geom_smooth(method = 'lm', color = 'red', linetype = 2) + 
  labs(title = 'Automobile Data', x = 'Weight', y = 'Miles per Gallon')

mtcars$am1 <- factor(mtcars$am, levels = c(0,1), labels = c('Automatic', 'Manual'))
mtcars$vs1 <- factor(mtcars$vs, levels = c(0,1), labels = c('V-Engine', 'Straight Engine'))
mtcars$cyl1 <- factor(mtcars$cyl)
ggplot(data = mtcars, aes(x = hp, y = mpg, shape = cyl1, color = cyl1)) + 
  geom_point(size = 3) + facet_grid(am1~vs1) +
  labs(title = 'Automobile Data', x = 'Weight', y = 'Miles per Gallon')

data(singer, package = 'lattice')
ggplot(singer, aes(x = height)) + geom_histogram()
ggplot(singer, aes(x = voice.part, y = height)) + geom_boxplot()

#几何函数的常见参数用法
data(Salaries, package = 'car')
ggplot(Salaries, aes(x = rank, y = salary)) + 
  geom_boxplot(fill = 'cornflowerblue', color = 'black', notch = T) + 
  geom_point(position = 'jitter', color = 'blue', alpha = 0.5) + 
  geom_rug(sides = 'l', color = 'darkred', alpha = 0.5)

ggplot(singer, aes(x = voice.part, y = height)) + geom_violin(fill = 'lightblue') + geom_boxplot(fill = 'lightgreen', width = .2)

#分组
ggplot(data = Salaries, aes(x = salary, fill = rank)) + geom_density(alpha = .3)
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary, color = rank, shape = sex)) + geom_point()
#不同position的条图
ggplot(Salaries, aes(x = rank, fill = sex)) + geom_bar(position = 'stack') +
  labs(title = 'position = "stack"')
ggplot(Salaries, aes(x = rank, fill = sex)) + geom_bar(position = 'dodge') +
  labs(title = 'position = "dodge"')
ggplot(Salaries, aes(x = rank, fill = sex)) + geom_bar(position = 'fill') +
  labs(title = 'position = "fill"') #默认的纵坐标错误,是比例(proportion)而非数量(count)
#以下三条代码分别表示不同的颜色填充方式,一般变量在aes()中,分配常数在aes()外
ggplot(Salaries, aes(x = rank, fill = sex)) + geom_bar()
ggplot(Salaries, aes(x = rank)) + geom_bar(fill = 'red')
ggplot(Salaries, aes(x = rank, fill = 'red')) + geom_bar()

#刻面
ggplot(data = singer, aes(x = height)) + geom_histogram() + facet_wrap(~voice.part, nrow = 4)
ggplot(Salaries, aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) + geom_point() + facet_grid(.~sex)
#每个rowvar水平的独立图,配置成一个单列,单行使用facet_grid(.~colvar)
ggplot(data = singer, aes(x = height, fill = voice.part)) + geom_density() + facet_grid(voice.part~.)

#添加光滑曲线
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary)) + geom_smooth() + geom_point() #采用非参数光滑曲线拟合
#n次多项式拟合,用formula = y~poly(x, n)实现
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary, linetype = sex, shape =sex, color = sex)) + 
  geom_smooth(method = lm, formula = y~poly(x, 2), se = F, size = 1) + geom_point(size = 2)

#自定义坐标轴和图例
ggplot(data = Salaries, aes(x = rank, y = salary, fill = sex)) + geom_boxplot() + 
  scale_x_discrete(breaks = c('AsstProf', 'AssocProf', 'Prof'), #设定放置在x轴上的因子
                   labels = c('Assistant\nProfessor', 'Associate\nProfessor', 'Full\nProfessor')) + #设置因子的标签
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000), labels = c('$50K', '$100K', '$150K', '$200K')) + #设置y轴刻度及刻度标签
  labs(title = 'Faculty Salary by Rank and Sex', x='', y='') + #设置标题,通过空字符赋值形式取消x和y轴的坐标轴标签
  theme(legend.position = c(.1,.8)) #设置图例位置

#标尺
#通过变量控制点的大小
ggplot(mtcars, aes(x = wt, y = mpg, size = disp)) + geom_point(shape = 21, color = 'black', fill = 'cornsilk') + 
  labs(x = 'weight', y = 'miles per gallon', title = 'Bubble Chart', size = 'Engine\nDisplacement') + 
  theme(plot.title = element_text(hjust = 0.5)) #解决标题居中问题
#通过改变颜色显示
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary, color = rank)) + 
  scale_color_manual(values = c('orange', 'olivedrab','navy')) + 
  geom_point(size = 2)
#采用预设的颜色集
ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary, color = rank)) + 
  scale_color_brewer(palette = 'Set1') +geom_point(size = 2)
library(RColorBrewer)
display.brewer.all() #显示所有的颜色集

#主题
mytheme <- theme(plot.title = element_text(face = 'bold.italic', size = 14, color = 'brown', hjust = 0.5),
                 axis.title = element_text(face = 'bold.italic', size = 10, color = 'brown'),
                 axis.text = element_text(face = 'bold.italic', size = 9, color = 'darkblue'),
                 panel.background = element_rect(fill = 'white', color = 'darkblue'),
                 panel.grid.major.y = element_line(color = 'grey', linetype = 1),
                 panel.grid.minor.y = element_line(color = 'grey', linetype = 2),
                 panel.grid.minor.x = element_blank(), legend.position = 'top')
ggplot(Salaries, aes(x = rank, y = salary, fill = sex)) + geom_boxplot() + 
  labs(title = 'Salary by Rank and Sex', x = 'Rank', y = 'Rank') + mytheme

#多重图
#通过修改par中mfrow参数的方式达到多图的效果对ggplot2不适用
library(gridExtra)
p1 <- ggplot(data = Salaries, aes(x = rank)) + geom_bar()
p2 <- ggplot(data = Salaries, aes(x = sex)) + geom_bar()
p3 <- ggplot(data = Salaries, aes(x = yrs.since.phd, y = salary)) + geom_point()
grid.arrange(p1, p2, p3, nrow = 2, ncol = 2)

#保存图形
myplot <- ggplot(data = mtcars, aes(x = mpg)) + geom_histogram()
ggsave(file = 'hhhh.png', plot = myplot, width = 5, height = 4)
#保存在当前工作空间5英寸×4英寸,扩展名可以为ps,tex,jpeg,pdf,tiff,png,bmp,svg,wmf
#忽略plot参数,则保存最近创建的图形
ggsave(file = 'wuh.jpeg', plot = grid.arrange(p1, p2, p3, nrow = 2, ncol = 2), width = 5, height = 5)
#文件名若相同则直接覆盖原文件













































