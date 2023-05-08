## pscore {nonrandom}
library(nonrandom)
## STU1
data(stu1)
stu1.ps <- pscore(data = stu1, 
                  formula = therapie~tgr+age)
stu1.match <- ps.match(object = stu1.ps,
                       ratio  = 2,
                       caliper = 0.05,
                       givenTmatchingC = FALSE,
                       matched.by = "pscore",
                       setseed = 38902,
                       combine.output=TRUE)
matchdata<- stu1.match$data.matched
matchdata

library(foreign)
matchdata$id<-1:nrow(matchdata)
write.dta(matchdata,"stu1matchdata.dta")
write.csv(matchdata,"stu1matchdata.csv")

## STU1
data(stu1)
stu1.ps <- pscore(data = stu1, 
                  formula = therapie~tgr+age)
plot.pscore(x = stu1.ps,
            main = "PS distribution",
            xlab = "",
            par.1=list(col="red"),
            par.0=list(lwd=2),
            par.dens=list(kernel="gaussian"))

##pscore {nonrandom}
