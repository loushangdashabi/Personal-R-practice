# 以下为第3章代码
# 第1节代码
x <- rnorm(50)
mean(x)
sd(x)
var(x)
median(x)
quantile(x)

pvec <- seq(0,1,0.1)
pvec
quantile(x,pvec)

# install.packages("ISwR")
library(ISwR)
attach(juul)
mean(igf1)
mean(igf1,na.rm=T)
sum(!is.na(igf1))
summary(igf1)
summary(juul)
detach(juul)

juul$sex <- factor(juul$sex,labels=c("M","F"))
juul$menarche <- factor(juul$menarche,labels=c("No","Yes"))
juul$tanner <- factor(juul$tanner,
                      labels=c("I","II","III","IV","V"))
attach(juul)
summary(juul)
detach(juul)

#以下代码与上述5行代码等价
juul <- transform(juul,
                  sex=factor(sex,labels=c("M","F")),
                  menarche=factor(menarche,labels=c("No","Yes")),
                  tanner=factor(tanner,labels=c("I","II","III","IV","V")))
attach(juul)
summary(juul)

hist(x)

mid.age <- c(2.5,7.5,13,16.5,17.5,19,22.5,44.5,70.5)
acc.count <- c(28,46,58,20,31,64,149,316,103)
age.acc <- rep(mid.age,acc.count)
brk <- c(0,5,10,16,17,18,20,25,60,80)
hist(age.acc,breaks=brk)

n <- length(x)
plot(sort(x),(1:n)/n,type="s",ylim=c(0,1))

qqnorm(x)

par(mfrow=c(1,2))
boxplot(IgM)
boxplot(log(IgM))
par(mfrow=c(1,1))


attach(red.cell.folate)
tapply(folate,ventilation,mean)
tapply(folate,ventilation,sd)
tapply(folate,ventilation,length)

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
cbind(mean=xbar, std.dev=s, n=n)

tapply(igf1, tanner, mean)
tapply(igf1, tanner, mean, na.rm=T)

aggregate(juul[c("age","igf1")],
          list(sex=juul$sex), mean, na.rm=T)
aggregate(juul[c("age","igf1")], juul["sex"], mean, na.rm=T)
by(juul, juul["sex"], summary)

attach(energy)
expend.lean <- expend[stature=="lean"]
expend.obese <- expend[stature=="obese"] # energy 数据集中expend变量被分割成两个向量

par(mfrow=c(2,1))
hist(expend.lean,breaks=10,xlim=c(5,13),ylim=c(0,4),col="white")
hist(expend.obese,breaks=10,xlim=c(5,13),ylim=c(0,4),col="grey")
par(mfrow=c(1,1))

boxplot(expend ~ stature)

opar <- par(mfrow=c(2,2), mex=0.8, mar=c(3,3,2,1)+.1)
stripchart(expend ~ stature)
stripchart(expend ~ stature, method="stack")
stripchart(expend ~ stature, method="jitter")
stripchart(expend ~ stature, method="jitter", jitter=.03)
par(opar)

stripchart(list(lean=expend.lean, obese=expend.obese))
# stripchart(expend.lean, expend.obese) #错误用法

caff.marital <- matrix(c(652,1537,598,242,36,46,38,21,218
                         ,327,106,67),
                       nrow=3,byrow=T)
caff.marital
colnames(caff.marital) <- c("0","1-150","151-300",">300")
rownames(caff.marital) <- c("Married","Prev.married","Single")
caff.marital
names(dimnames(caff.marital)) <- c("marital","consumption")
caff.marital
as.data.frame(as.table(caff.marital))

attach(juul)
table(sex) #juul dataset
table(sex,menarche)
table(menarche,tanner)

xtabs(~ tanner + sex, data=juul)
xtabs(~ dgn + diab + coma, data=stroke)

ftable(coma + diab ~ dgn, data=stroke)

caff.marital
t(caff.marital)

attach(juul)
tanner.sex <- table(tanner,sex)
tanner.sex
margin.table(tanner.sex,1) #1代表行
margin.table(tanner.sex,2) #2代表列
prop.table(tanner.sex,1)
tanner.sex/sum(tanner.sex) #对上述结果计算百分比

total.caff <- margin.table(caff.marital,2)
total.caff
barplot(total.caff, col="white")

par(mfrow=c(2,2))
barplot(caff.marital, col="white")
barplot(t(caff.marital), col="white")
barplot(t(caff.marital), col="white", beside=T)
barplot(prop.table(t(caff.marital),2), col="white", beside=T)
par(mfrow=c(1,1))

barplot(prop.table(t(caff.marital),2),beside=T,
        legend.text=colnames(caff.marital),
        col=c("white","grey80","grey50","black"))

dotchart(t(caff.marital), lcolor="black")

opar <- par(mfrow=c(2,2),mex=0.8, mar=c(1,1,2,1))
slices <- c("white","grey80","grey50","black")
pie(caff.marital["Married",], main="Married", col=slices)
pie(caff.marital["Prev.married",],
    main="Previously married", col=slices)
pie(caff.marital["Single",], main="Single", col=slices)
par(opar)

#第2节代码
library(ISwR)
attach(thuesen)
cor(blood.glucose,short.velocity) # WRONG
cor(blood.glucose,short.velocity,use="complete.obs")
cor(thuesen,use="complete.obs")

cor(blood.glucose,short.velocity,use="complete.obs",method = ("spearman"))
cor(blood.glucose,short.velocity,use="complete.obs",method = ("kendall"))

cor.test(blood.glucose,short.velocity)
cor.test(blood.glucose,short.velocity,method="spearman")
cor.test(blood.glucose,short.velocity,method="kendall")

detach(thuesen)

# partial correlations
# partial correlation of population and murder rate, controlling
# for income, illiteracy rate, and HS graduation rate
states<- state.x77[,1:6] #state.x77 dataset
cov(states)
cor(states)
cor(states, method="spearman")
x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

library(ggm)
colnames(states)
pcor(c(1,5,2,3,6), cov(states))
pcor.test(pcor(c(1,5,2,3,6), cov(states)),5,50)

# 第3节&第4节代码

daily.intake <- c(5260,5470,5640,6180,6390,6515,
                  6805,7515,7515,8230,8770)
mean(daily.intake)
sd(daily.intake)
quantile(daily.intake)

t.test(daily.intake,mu=7725)

wilcox.test(daily.intake, mu=7725)

library(ISwR)
attach(energy)
energy
t.test(expend~stature)

t.test(expend~stature, var.equal=T)

var.test(expend~stature)

wilcox.test(expend~stature)

attach(intake)
intake
post - pre
t.test(pre, post, paired=T)

t.test(pre, post) #WRONG!

wilcox.test(pre, post, paired=T)

#第3章代码结束