# 第4章代码如下
# install.packages(c("multcomp", "gplots", "car", "HH", "effects",
#                    "rrcov", "mvoutlier"))  

par(ask=TRUE)
opar <- par(no.readonly=TRUE) # save original parameters

# Listing 9.1 - One-way ANOVA
library(multcomp)
attach(cholesterol)
table(trt)     
aggregate(response, by=list(trt), FUN=mean) 
aggregate(response, by=list(trt), FUN=sd) 
fit <- aov(response ~ trt)                                  
summary(fit)

library(gplots)
plotmeans(response ~ trt, xlab="Treatment", ylab="Response", 
          main="Mean Plot\nwith 95% CI")
detach(cholesterol)


# Listing 9.2 - Tukey HSD pairwise group comparisons
TukeyHSD(fit)
par(las=2)
par(mar=c(5,8,4,2)) 
plot(TukeyHSD(fit))
par(opar)

# Multiple comparisons the multcomp package
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(fit, linfct=mcp(trt="Tukey"))
plot(cld(tuk, level=.05),col="lightgrey")
par(opar)

# Assessing normality
library(car)
qqPlot(lm(response ~ trt, data=cholesterol), 
       simulate=TRUE, main="Q-Q Plot", labels=FALSE)

# Assessing homogeneity of variances
bartlett.test(response ~ trt, data=cholesterol)

# Assessing outliers
library(car)
outlierTest(fit)


# Listing 9.3 - One-way ANCOVA
data(litter, package="multcomp")
attach(litter)
table(dose) 
aggregate(weight, by=list(dose), FUN=mean)
fit <- aov(weight ~ gesttime + dose)                             
summary(fit)

# Obtaining adjusted means
library(effects)
effect("dose", fit)

#  Listing 9.4 - Multiple comparisons using user supplied contrasts
library(multcomp)
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1))
summary(glht(fit, linfct=mcp(dose=contrast)))


# Listing 9.5 - Testing for homegeneity of regression slopes
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)


# Visualizing a one-way ANCOVA
library(HH)
ancova(weight ~ gesttime + dose, data=litter)

# Listing 9.6 - Two way ANOVA
attach(ToothGrowth)
table(supp,dose)
aggregate(len, by=list(supp,dose), FUN=mean)
aggregate(len, by=list(supp,dose), FUN=sd)
dose <- factor(dose)
fit <- aov(len ~ supp*dose) # fit1<- aov(len~supp+dose+supp:dose)
summary(fit)

# plotting interactions
interaction.plot(dose, supp, len, type="b", 
                 col=c("red","blue"), pch=c(16, 18),
                 main = "Interaction between Dose and Supplement Type")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "),
          connect=list(c(1, 3, 5),c(2, 4, 6)), 
          col=c("red","darkgreen"),
          main = "Interaction Plot with 95% CIs", 
          xlab="Treatment and Dose Combination")
library(HH)
interaction2wt(len~supp*dose)

#  Listing 9.7 - Repeated measures ANOVA with one between and within groups factor
CO2$conc <- factor(CO2$conc)
w1b1 <- subset(CO2, Treatment=='chilled')
fit <- aov(uptake ~ (conc*Type) + Error(Plant/(conc)), w1b1)
summary(fit)

par(las=2)
par(mar=c(10,4,4,2))
with(w1b1, 
     interaction.plot(conc,Type,uptake, 
                      type="b", col=c("red","blue"), pch=c(16,18),
                      main="Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type*conc, data=w1b1, col=(c("gold","green")),
        main="Chilled Quebec and Mississippi Plants", 
        ylab="Carbon dioxide uptake rate (umol/m^2 sec)")
par(opar)

# 重复测量方差分析案例2
Example8_12  <- read.table ("example8_12.csv", header=TRUE, sep=",")
attach(Example8_12)
type  <-factor(type, order=FALSE)
time  <-factor(time, order=FALSE)
subject  <-factor(subject, order=FALSE)
fit <- aov(rate ~type*time +Error(subject/time))
summary(fit)
detach(Example8_12)

# 重复测量方差分析案例3
Example8_13  <- read.table ("example8_13.csv", header=TRUE, sep=",")
attach(Example8_13)
a  <-factor(a, order=FALSE)
b  <-factor(b, order=FALSE)
s  <-factor(s, order=FALSE)
time  <-factor(time, order=FALSE)
fit <- aov(y ~a*b*time +Error(s/time))
summary(fit)
detach(Example8_13)

# Listing 9.8 - One-way MANOVA
library(MASS)
attach(UScereal)
shelf <- factor(shelf)
y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN=mean)
cov(y)
fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit)


#  Listing 9.9 - Assessing multivariate normality
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y,center,cov)
coord <- qqplot(qchisq(ppoints(n),df=p),
                d, main="QQ Plot Assessing Multivariate Normality",
                ylab="Mahalanobis D2")
abline(a=0,b=1)

identify(coord$x, coord$y, labels=row.names(UScereal))


# multivariate outliers
library(mvoutlier)
outliers <- aq.plot(y)
outliers

# Listing 9.10 - Robust one-way MANOVA
library(rrcov)
Wilks.test(y,shelf, method="mcd")  # this can take a while


# Listing 9.11 - A regression approach to the Anova problem
library(multcomp)
levels(cholesterol$trt)
fit.aov <- aov(response ~ trt, data=cholesterol)
summary(fit.aov)

fit.lm <- lm(response ~ trt, data=cholesterol)
summary(fit.lm)
contrasts(cholesterol$trt)

fit.lm1 <- lm(response ~ trt, data=cholesterol, contrasts="contr.helmert") # WRONG
summary(fit.lm1)

fit.lm2 <- lm(response ~ trt, data=cholesterol, contrasts="contr.SAS") # WRONG
summary(fit.lm2)

fit.lm3 <- lm(response ~ trt, data=cholesterol, contrasts=list(trt="contr.SAS"))
summary(fit.lm3)

fit.lm4 <- lm(response ~ trt, data=cholesterol, contrasts=list(trt="contr.sum"))
summary(fit.lm4)

# Kruskal Wallis test
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)

example14_11  <- read.table ("example14_11.csv", header=TRUE, sep=",")
attach(example14_11)
kruskal.test(rate ~ group)
library(nparcomp)
nparcomp(rate ~ group, data=example14_11, alternative = "two.sided")
detach(example14_11)

example14_18  <- read.table ("example14_18.csv", header=TRUE, sep=",")
attach(example14_18)
friedman.test (rate~ treat|block)
library(PMCMR)
posthoc.friedman.nemenyi.test(rate,treat,block)
detach(example14_18)
