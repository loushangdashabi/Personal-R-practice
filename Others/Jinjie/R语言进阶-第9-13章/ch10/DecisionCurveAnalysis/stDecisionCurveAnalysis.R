##生存资料DCA
library(MASS)

source("stdca.R")

data.set <- Melanoma
data.set$diedcancer = ifelse(data.set$status==1, 1, 0)

##Decision Curve Analysis
stdca(data=data.set, outcome="diedcancer", ttoutcome="time", timepoint=545, predictors="thickness", probability=FALSE, xstop=.25)
stdca(data=data.set, outcome="diedcancer", ttoutcome="time", timepoint=545, predictors="thickness", probability="FALSE", xstop=.25, intervention="TRUE")
