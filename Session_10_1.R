setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code")
getwd()

install.packages("quantreg")
library(quantreg)
library(Matching)

data(lalonde)
example(rq)

attach(lalonde)
# GenMatch function
X = cbind(age, educ, black, hisp, married, nodegr, re75,
          re74)
BalanceMat <- cbind(age, educ, black, hisp, married, nodegr, re75,
                    re74, I(re74*re75))
genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat,
                   estimand="ATE", M=1,
                   pop.size=16, max.generations=10, wait.generations=1)

rq(re78 ~ treat,.5)
