setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code")
getwd()

library(Matching)
data(lalonde)

glm <- glm(treat ~ age + educ + black + hisp + married + re74 + re75 + u74 + u75, data=lalonde, family=binomial)
summary(glm)
