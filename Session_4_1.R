setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code")
getwd()

# install.packages("tree")
# install.packages("ISLR")
library(tree)
library(ISLR)
attach(Carseats)
High = ifelse(Sales <=8,"No","Yes ")
Carseats = data.frame(Carseats ,High)
tree.carseats = tree(High ~ Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats

set.seed(2)
train=sample (1:nrow(Carseats ), 200)
Carseats.test = Carseats [train,]
High.test = High[train]
tree.carseats = tree(High ~ Sales, Carseats, subset=train)
tree.pred = predict(tree.carseats, Carseats.test ,type="class")
table(tree.pred, High.test)

set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

prune.carseats = prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

prune.carseats = prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty =0)
tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)



library(Matching)
data(lalonde)
tree.u75 = tree(u75 ~ age + educ + black + hisp + married + nodegr, data=lalonde)
summary(tree.u75)
plot(tree.u75)
text(tree.u75, pretty=0)
tree.u75
