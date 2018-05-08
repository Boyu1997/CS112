# load data
aggregates <- read.csv(file="../data/aggregates.csv,header=TRUE")


# plot time series
plot(debtgdpratio~Year,data=aggregates[aggregates$country=="European Union (27 countries)",],type="o",pch=3,axes=FALSE, ylim=c(40,160),ylab="",xlab="")
par(new=TRUE)
plot(debtgdpratio~Year,data=aggregates[aggregates$country=="Euro area (17 countries)",],type="o",pch=1,axes=FALSE, ylim=c(40,160),ylab="",xlab="")
par(new=TRUE)
plot(debtgdpratio~Year,data=aggregates[aggregates$country=="Greece",],type="o",pch=6,ylim=c(40,160),main="Time Series of Debt/GDP (1995-2012)",axes=FALSE,ylab="Debt/GDP",xlab="Year")
axis(1)
axis(2,las=1)
box()
