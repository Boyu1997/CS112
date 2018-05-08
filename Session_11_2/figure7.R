# load data
greece <- read.csv('../data/greecelong.csv')

# plot time series
plot(greece[,1],greece[,2],type="l",axes=FALSE,main="",xlab="Year",ylab="Debt/GDP")
abline(v=1999,lty="dotted")
axis(1) 
axis(2,las=1)
box()
