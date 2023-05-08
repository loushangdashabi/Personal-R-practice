## here consider pbc dataset in survival package as an example
library(survival)
dat=pbc[1:312,]
dat$time=as.numeric(dat$time)
##定义生存结局
dat$status=ifelse(dat$status==2, 1, 0)
##定义时间点
t0=365*5
##基础回归模型变量矩阵
indata0=as.matrix(subset(dat, select=c(time,status,age,bili,albumin)))
##增加1个预测变量新模型
indata1=as.matrix(subset(dat, select=c(time,status,age,bili,albumin,protime)))
##旧模型中预测变量矩阵
covs0<-as.matrix(indata0[,c(-1,-2)])
##新模型中预测变量矩阵
covs1<-as.matrix(indata1[,c(-1,-2)])
library(survIDINRI)
x<-IDI.INF(dat[,2:3], covs0, covs1, t0, npert=1000)
##dat[,2:3]设置生存结局，dat数据集第2、3两列分别是生存时间与终点。
##covs0, covs1,为旧模型与新模型的协变量矩阵
##t0为设置的时间。npert设置迭代次数。
IDI.INF.OUT(x)
##输出结果IDI.INF计算结果：
##m1：Result of IDI. 
##m2：Result of continuous-NRI.
##m3：Result of median improvement in risk score.
IDI.INF.GRAPH(x)
