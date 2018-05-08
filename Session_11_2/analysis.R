####################################################################################
############ This is file analysis.R
############ I load the data and run the analyses for the article:
############ Fiscal Governance in the Eurozone: How effectively does the 
############ Stability and Growth Pact limit governmental debt in the Eurozone?
############ by Sebastian Koehler and Thomas König
############ forthcoming in: Political Science Research and Methods
####################################################################################


#### load required packages
library(Synth)
library(lattice)
lattice.options(default.theme = modifyList(standard.theme(color = FALSE), list(strip.background = list(col = "transparent"))))


#### clear workspace
rm(list=ls())

#### set your working directory here (adjust the path to the files on your computer)
dir <- "~/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code/Session_11_2"
setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code/Session_11_2")

#### load dataset (adjust the path to the files on your computer)
load("scdata.Rdata")



#### Define aggregates we need later
Euro12 <- c('Germany','Netherlands','Greece','Spain','Portugal','Italy',#
            'Finland','France','Luxembourg','Belgium','Austria','Ireland')

Euro17 <- c('Germany','Netherlands','Greece','Spain','Portugal','Italy','Finland','France','Luxembourg','Belgium','Austria','Ireland','Slovakia','Cyprus','Malta','Slovenia','Estonia')

donor <- c("Germany","Netherlands","Belgium","Austria","Finland","France")

recipient <- c("Portugal","Spain","Greece","Italy","Ireland")

#### Define list element to store estimation results
scresults <- vector(mode="list",length=11)
names(scresults) <- c("Euro11","donor","recipient","Greece","Portugal","Spain","Ireland","Italy","robustness","deficit","general")



#### source scripts with individual analyses for the following aggregates/countries

#### Euro 11
source(paste(dir,"euro11.R",sep="/"))

#### Donor
source(paste(dir,"donor.R",sep="/"))

#### Recipient
source(paste(dir,"recipient.R",sep="/"))

#### Greece
source(paste(dir,"Greece.R",sep="/"))

#### Portugal
source(paste(dir,"Portugal.R",sep="/"))

#### Spain
source(paste(dir,"Spain.R",sep="/"))

#### Ireland
source(paste(dir,"Ireland.R",sep="/"))

#### Italy
source(paste(dir,"Italy.R",sep="/"))

#### Robustness checks
source(paste(dir,"robustness.R",sep="/"))

#### General Government Debt
source(paste(dir,"general.R",sep="/"))

#### Deficit
source(paste(dir,"deficit.R",sep="/"))

#### Save Results as Rdata file
save(scresults,file='../data/scresults.Rdata')

