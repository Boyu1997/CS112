setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code")
getwd()

city.names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
observed.turnout = c(18, 30, 14, 52, 24, 29, 48, 49, 17, 30)

observed.diffmeans <- mean(observed.turnout[c(2,4,6,8,10)]) - 
  mean(observed.turnout[c(1,3,5,7,9)])

print(observed.diffmeans)

foo <- data.frame(city.names, observed.turnout)

# Assignment function
assignment <- function() {
  # Four coin flips, establishing random assignment
  assig        <- foo[sample(1:2),]
  assig[3:4,]  <- foo[sample(3:4),]
  assig[5:6,]  <- foo[sample(5:6),]
  assig[7:8,]  <- foo[sample(7:8),]
  assig[9:10,]  <- foo[sample(9:10),]
  
  treatment.group   <- assig[c(1,3,5,7,9),]
  control.group     <- assig[c(2,4,6,8,10),]
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Assignment function
iter.RI <- function(iterations = 100000) {
  for (i in 1:iterations) 
  {storage.vector[i] <- assignment()
  }
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()

# Exploring the results

quantile(results, prob = c(0.025, 0.975))

length(unique(results))

hist(results)
plot(density(results))
abline(v = -13.8, lwd = 2, col = "red")
abline(v = 13.8, lwd = 2, col = "red")

