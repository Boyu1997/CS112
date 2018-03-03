library(Matching)
library(arm)
data(lalonde)

lm.1 <- lm(re78 ~ age + educ + married + re74 + re75 + treat, data=lalonde)
summary(lm.1)
confint(lm.1, level=0.95)

subgroup.nodegr = subset(lalonde, nodegr == 1)
subgroup.yesdegr = subset(lalonde, nodegr == 0)

lm.nodegr.1 <- lm(re78 ~ age + educ + married + re74 + re75 + treat, data=subgroup.nodegr)
display(lm.nodegr.1)
summary(lm.nodegr.1)
confint(lm.nodegr.1, level=0.95)
lm.yesdegr.1 <- lm(re78 ~ age + educ + married + re74 + re75 + treat, data=subgroup.yesdegr)
summary(lm.yesdegr.1)
confint(lm.yesdegr.1, level=0.95)

lm.2 <- lm(re78 ~ treat + nodegr, data=lalonde)
summary(lm.2)

lm.4 <- lm(re78 ~ I(treat*nodegr), data=lalonde)
summary(lm.4)

lm.3 <- lm(re78 ~ treat + nodegr + I(treat*nodegr), data=lalonde)
summary(lm.3)
confint(lm.3, level=0.95)
lm.3.sim <- sim (lm.3)
coef(lm.3.sim)
plot (NA, NA, xlim=c(0, 1), ylim=c(-2000,6000),
      xlab="nodegr", ylab="treatment effect",
      main="lalonde program treatment effect")
abline (h = 0, lwd=.5, lty=2)
for (i in 1:100) {
  abline (a = coef(lm.3.sim)[i,2], b = coef(lm.3.sim)[i,4],
          lwd = .5, col = "gray")
}
abline (a = lm.3$coefficients[2], b = lm.3$coefficients[4],
        lwd = 1, col = "black")

lalonde$u78 = 1*(lalonde$re78 == 0)   # define u78 = 1 for those earned 0 income in 1978
subgroup.nodegr = subset(lalonde, nodegr == 1)
subgroup.yesdegr = subset(lalonde, nodegr == 0)
glm.nodegr <- glm(u78 ~ age + educ + black + hisp + married + re74 + re75 + u74 + u75 + treat, data=subgroup.nodegr, family=binomial)
summary(glm.nodegr)
confint(glm.nodegr, level=0.95)

glm.yesdegr <- glm(u78 ~ age + educ + black + hisp + married + re74 + re75 + u74 + u75 + treat, data=subgroup.yesdegr, family=binomial)
summary(glm.yesdegr)
confint(glm.yesdegr, level=0.95)







k <- 10
folds <- kfold(subgroup.yesdegr, k)
lm.yesdegr.kfold <- NA
min.SE <- Inf
for (i in 1:k)
{
  set.train <- subgroup.yesdegr[which(folds!=i),]
  set.test <- subgroup.yesdegr[which(folds==i),]
  lm.yesdegr.kfold <- lm(re78 ~ educ + black + treat, data=set.train)
  predict.kfold <- predict(lm.yesdegr.kfold, set.test)
  SE.kfold = (sum((predict.kfold-set.test$re78)^2)/length(set.test))**(1/2)
  if (SE < min.SE)
  {
    min.SE.kfold = SE.kfold
    min.lm.yesdegr.kfold = lm.yesdegr.kfold
  }
}
summary(min.lm.yesdegr.kfold)
confint(min.lm.yesdegr.kfold, level=0.95)

