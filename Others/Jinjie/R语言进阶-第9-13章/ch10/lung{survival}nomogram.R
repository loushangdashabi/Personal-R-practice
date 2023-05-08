## 绘制nomogram图
## 第一步 读取rms程序包及辅助程序包
library(rms)
#library(Hmisc)
#library(grid)
#library(lattice)
#library(Formula)
#library(ggplot2) 
## 第二步 读取数据，以survival程序包的lung数据来进行演示
## 列举survival程序包中的数据集
#library(survival)
#data(package = "survival")

## 读取lung数据集
data(lung)

## 显示lung数据集的前6行结果
head(lung)

## 显示lung数据集的变量说明
help(lung)

lung$status <- ifelse(lung$status==2,1,0)

## 添加变量标签以便后续说明
lung$sex <- 
  factor(lung$sex,
         levels = c(1,2),
         labels = c("male", "female"))

## 第三步 按照nomogram要求“打包”数据，绘制nomogram的关键步骤,??datadist 可查看详细说明
dd=datadist(lung)
options(datadist="dd") 

## 第四步 构建模型
## 构建logisitc回归模型
f1 <- lrm(status~ age + sex, data = lung) 

## 绘制logisitc回归的风险预测值的nomogram图
nom <- nomogram(f1, fun= function(x)1/(1+exp(-x)), # or "fun=plogis"
                lp=F, funlabel="Risk")
plot(nom)


## 构建COX比例风险模型
#f2 <- psm(Surv(time,status) ~ age + sex, data = lung, dist='lognormal') 
f2 <- cph(Surv(time,status) ~ age + sex, data = lung, x=T,y= T,surv=TRUE) 
f2
medi <- Quantile(f2) # 计算中位生存时间
medi
surv <- Survival(f2) # 构建生存概率函数

## 绘制COX回归中位生存时间的Nomogram图
nom2m <- nomogram(f2, fun=function(x) medi(lp=x),
                funlabel="Median Survival Time")
plot(nom2m)

## 绘制COX回归生存概率的Nomogram图
## 注意lung数据的time是以”天“为单位
nom2p <- nomogram(f2, fun=list(function(x) surv(365, x),
                             function(x) surv(730, x)),
                funlabel=c("1-year Survival Probability",
                           "2-year Survival Probability"))
plot(nom2p, xfrac=.4)

## 评价COX回归的预测效果
## 第一步 计算c-index
rcorrcens(Surv(time,status) ~ predict(f2), data =  lung)

## 第二步 绘制校正曲线
## 参数说明：
## 1、绘制校正曲线前需要在模型函数中添加参数x=T, y=T，详细参考帮助
## 2、u需要与之前模型中定义好的time.inc一致，即365或730；
## 3、m要根据样本量来确定，由于标准曲线一般将所有样本分为3组（在图中显示3个点）
## 而m代表每组的样本量数，因此m*3应该等于或近似等于样本量；
## 4、b代表最大再抽样的样本量

## 重新调整模型函数f2，也即添加x=T, y=T
#f2 <- psm(Surv(time,status) ~ age+sex, data =  lung, x=T, y=T, dist='lognormal') 
f2 <- cph(Surv(time,status) ~ age + sex, data = lung, x=T, y=T,surv=TRUE) 
## 构建校正曲线
cal <- calibrate(f2, cmethod='KM', method="boot", u=365, m=50, B=100)

## 绘制校正曲线，??rms::calibrate 可查看详细参数说明
#par(mar=c(8,5,3,2),cex = 1.0)
plot(cal,lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),
     xlim=c(0.1,1),ylim=c(0.1,1),
     xlab="Nomogram-Predicted Probability of 1-Year DFS",
     ylab="Actual 1-Year DFS (proportion)",
     col=c(rgb(192,98,83,maxColorValue=255)))

## rms::nomogram的完整示例详见rms程序包的帮助文件
## rms程序包的帮助文件下载网址：https://cran.r-project.org/web/packages/rms/rms.pdf
## 代表性参考文献1：http://jco.ascopubs.org/content/26/8/1364.long
## 代表性参考文献2：http://jco.ascopubs.org/content/31/9/1188.long
复制代码