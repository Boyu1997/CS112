setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code")

age <- sample(20:50, 100, replace=TRUE)
education <- sample(1:5, 100, replace=TRUE)
married <- sample(0:1, 100, replace=TRUE)
income <- 3000*log10(age) + 5*log2(age)*education^(6+married) - 80*married + runif(1, -500, 500)

data <- data.frame(income, age, education, married)

lm = lm(income ~ age + education + married, data=data)
summary(lm)
