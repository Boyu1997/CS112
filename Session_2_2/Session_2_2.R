setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code/Session_2_2")


load("haha.RData")
haha <- data.frame(post.test, pre.test, treatment)


# treatment effect (as we have both Y0 and Y1)
subset.control = subset(haha, treatment==0)
subset.treated = subset(haha, treatment==1)
# average treatement effect for the control
mean(subset.control$post.test - subset.control$pre.test)
# average treatement effect for the treated
mean(subset.treated$post.test - subset.treated$pre.test)


# linear regression model
lm <- lm(post.test ~ pre.test + treatment, data=haha)
summary(lm)
