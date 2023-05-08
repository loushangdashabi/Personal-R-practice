library(MatchIt)
data(lalonde)
head(lalonde)
f=matchit(treat~re74+re75+educ+black+hispan+age+married+nodegree,data=lalonde,method="nearest")
#f1=matchit(treat~re74+re75+educ+black+hispan+age+married+nodegree,data=lalonde,method="nearest",caliper=0.05)
summary(f)
matchdata=match.data(f)
matchdata

library(foreign)
matchdata$id<-1:nrow(matchdata)
write.dta(matchdata,"matchdata.dta")
write.csv(matchdata,"matchdata.csv")