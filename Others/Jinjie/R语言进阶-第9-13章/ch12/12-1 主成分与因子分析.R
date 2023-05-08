## 第12-1章主成分与因子分析代码开始

par(ask=TRUE)
set.seed(1234) # make results reproducible

# install.packages("psych") 
library(psych)
fa.parallel(USJudgeRatings[,-1], fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis") #判断主成分个数
abline(h=1,lwd=1,col="green") 

# Listing 14.1 - Principal components analysis of US Judge Ratings
#library(psych)
pc <- principal(USJudgeRatings[,-1], nfactors=1)
pc

# Principal components analysis Harman23.cor data
#library(psych)
fa.parallel(Harman23.cor$cov, n.obs=302, fa="pc", n.iter=100,
            show.legend=FALSE, main="Scree plot with parallel analysis")
abline(h=1,lwd=1,col="green")

# Listing 14.2 - Principal components analysis of body measurements
#library(psych)
PC <- principal(Harman23.cor$cov, nfactors=2, rotate="none")
PC

# Listing 14.3 - Principal components analysis with varimax rotation
rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
rc

# Listing 14.4 - Obtaining componenet scores from raw data
library(psych)
pc <- principal(USJudgeRatings[,-1], nfactors=1, score=TRUE)
head(pc$scores)
cor(USJudgeRatings$CONT, pc$score)

# Listing 14.5 - Obtaining principal component scoring coefficients
library(psych)
rc <- principal(Harman23.cor$cov, nfactors=2, rotate="varimax")
round(unclass(rc$weights), 2)

## 主成分分析
# 案例3
#install.packages("car")
library(car)
example16_2  <- read.table ("example16_2.csv", header=TRUE, sep=",")
example16_2
fit <- lm(y~x1+x2+x3, data=example16_2)
summary(fit)
vif(fit)
library(psych)
describe(example16_2)
fa.parallel(example16_2[-4], fa="pc", n.iter=100, show.legend=FALSE, main="Screen plot with parallel analysis")
abline(1,0)
pc <- principal(example16_2[-4], nfactors=2, rotate= "varimax", score=TRUE)
pc
pc$weights
pc$scores
newdata <- data.frame(example16_2,  pc$scores)
newdata
fit <- lm(y~ RC1+RC2, data=newdata)
summary(fit)
vif(fit)


## Exploratory factor analysis of ability.cov data
#options(digits=2)
library(psych)
covariances <- ability.cov$cov
# convert covariances to correlations
correlations <- cov2cor(covariances)
correlations

# determine number of factors to extract
fa.parallel(correlations, n.obs=112, fa="both", n.iter=100,
            main="Scree plots with parallel analysis")
abline(h=0,lwd=1,col="green")

# Listing 14.6 - Principal axis factoring without rotation
fa <- fa(correlations, nfactors=2, rotate="none", fm="pa")
fa

# Listing 14.7 - Factor extraction with orthogonal rotation
fa.varimax <- fa(correlations, nfactors=2, rotate="varimax", fm="pa")
fa.varimax

# Listing 14.8 - Factor extraction with oblique rotation
library(GPArotation)
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa")
fa.promax

# calculate factor loading matrix
fsm <- function(oblique) {
  if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    P <- unclass(oblique$loading)
    F <- P %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2")
    return(F)
  }
}
fsm(fa.promax)

# plot factor solution
factor.plot(fa.promax, labels=rownames(fa.promax$loadings))
fa.diagram(fa.promax, simple=FALSE)

# factor scores
fa.promax$weights

## 因子分析
# 案例2
library(psych)
example17_3  <- read.table ("example17_3.csv", header=TRUE, sep=",")
example17_3
fa.parallel(example17_3, fa="fa", n.iter=100, main="Screen plots with parallel analysis")
abline(0,0)

fa <- fa(example17_3, nfactors=4, rotate="none", fm="ml", score=TRUE)
fa
fa$weights
fa$scores
factor.plot(fa, labels=rownames(fa$loadings))
fa.diagram(fa, simple=FALSE)

fa2 <- fa(example17_3, nfactors=4, rotate="varimax", fm="ml", score=TRUE)
fa2
fa2$weights
fa2$scores
factor.plot(fa2, labels=rownames(fa$loadings))
fa.diagram(fa2, simple=FALSE)

library(GPArotation)
fa.promax <- fa(example17_3, nfactors=4, rotate="promax", fm="ml")
fa.promax
fa.diagram(fa.promax, simple=FALSE)
# 第12-1章代码结束