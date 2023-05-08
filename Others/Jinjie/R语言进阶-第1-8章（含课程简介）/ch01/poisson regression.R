##possion regression

data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)

# fit full model
fit.full <- glm(affairs ~ gender + age + yearsmarried + children + 
                  religiousness + education + occupation +rating,
                data=Affairs,family=poisson())
summary(fit.full)

# fit reduced model
fit.reduced <- glm(affairs ~ age + yearsmarried + religiousness + 
                     occupation + rating, data=Affairs, family=poisson())
summary(fit.reduced)

# compare models
anova(fit.reduced, fit.full, test="Chisq")

# interpret coefficients
coef(fit.reduced)
exp(coef(fit.reduced))

# calculate probability of extramariatal affair by marital ratings
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                       age = mean(Affairs$age),
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# calculate probabilites of extramariatal affair by age
testdata <- data.frame(rating = mean(Affairs$rating),
                       age = seq(17, 57, 10), 
                       yearsmarried = mean(Affairs$yearsmarried),
                       religiousness = mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# evaluate overdispersion
fit <- glm(affairs ~ age + yearsmarried + religiousness +
             occupation + rating, family = poisson(), data = Affairs)
fit.od <- glm(affairs ~ age + yearsmarried + religiousness +
             occupation + rating, family = quasipoisson(), data = Affairs)
pchisq(summary(fit.od)$dispersion * fit$df.residual,  
       fit$df.residual, lower = F)