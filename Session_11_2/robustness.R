#####################################################################
####### this is file "robustness.R"
####### Last change: 2014-02-04
####### I run additional robustness checks
#####################################################################



#### Define predictors
pred <- names(scdata)[c(#
	7, # pop65+
	8, # pop15-	
	#9,# unemployment (1)
	#10,# system
	11,# yrcurnt
	12,# allhouse
  	13,# legelec
	14, # pluralty
	15, # pr
	#16, # checks 
	#17, # fri
	#18,# gdpgrowth	
	#19,# growth in multi factor productivity
	#20,# labor productivity annual growth	
	21, # health/GDP
	22,# GDP expenditure approach
	#23,# tax revenue %GDP (general)	
	24,# taxrev %GDP
	25, # CO2 emissions
	#26, # FDI	
  	#27, # GDP growth	
	#28,# Gini index	
	#29,#,# Inflation (Consumer Prices)
	#30, # Poverty	
	31,#, # unemployment (World Bank)	
	#32#, #Population
	#33,#, #openness (PWT)
	34,#,# openness (expenditure)
	35, # Expenditure on Families %GDP	
	36, # Polcon III
	#37, Polcon V
	38, #Potrafke Ideology
	39, # maj
	#40,# Herfindahl Index 
	41,# lag debt/gdp (RR)
	42#,# Rae fractionalisation index (government),#
	#43,# Rae fractionalisation index (total) ,	
)]

#### define controls
contr <- sort(unique(scdata$ccode[is.element(scdata$country,setdiff(scdata$country,c(Euro12,"Euro 11","Slovenia")))]))

contr <- setdiff(contr, c(1111,2222,70,155,225,269,290,310,316,317,349,355,360,366,666,732,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))


#### Show countries that are available for Synthetic Control Group
print(country <- sort(unique(scdata$country[scdata$ccode %in% contr])))



##################################### hypothetical treatment period: 1992 #########################
#### Generate Synth object (debt/gdp ratio), 
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
		predictors = pred,
		dependent = names(scdata[6]),
		unit.variable = "ccode",
		time.variable = "Year", 
		treatment.identifier = 0,
		controls.identifier = contr, 
		time.predictors.prior = c(1983:1991),
		time.optimize.ssr = c(1983:1992),
		unit.names.variable = "country",
		time.plot = 1983:2010
)

#### Run analysis 
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)


####
synth.tables <- synth.tab(dataprep.res = sdata, synth.res=synth.out)


#### Plot Path
path.plot(synth.res = synth.out, 
	dataprep.res = sdata,
	Ylab="Debt/GDP (Nominal)",
	Xlab="Year",
	Legend=c("Euro 11","Synthetic Euro 11"),
	Legend.position="bottomright", abline(v=1992,lty="dashed")
)



#### Plot Gaps
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
	Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
	Main=NA, abline(v=1992,lty="dashed"))


#### Extract and plot country weights
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]


#### store results in list
scresults[[9]] <- synth.out

#### print results on screen
print(synth.out)



##################################### hypothetical treatment period: 1998 #########################
#### Generate Synth object (debt/gdp ratio), 
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
		predictors = pred,
		dependent = names(scdata[6]),
		unit.variable = "ccode",
		time.variable = "Year", 
		treatment.identifier = 0,
		controls.identifier = contr, 
		time.predictors.prior = c(1983:1996),
		time.optimize.ssr = c(1983:1998),
		unit.names.variable = "country",
		time.plot = 1983:2010
)

#### Run analysis 
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)


####
synth.tables <- synth.tab(dataprep.res = sdata, synth.res=synth.out)


#### Plot Path
path.plot(synth.res = synth.out, 
	dataprep.res = sdata,
	Ylab="Debt/GDP (Nominal)",
	Xlab="Year",
	Legend=c("Euro 11","Synthetic Euro 11"),
	Legend.position="bottomright", abline(v=1998,lty="dashed")
)



#### Plot Gaps
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
	Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
	Main=NA, abline(v=1998,lty="dashed"))


#### Extract and plot country weights
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]


#### store results in list
scresults[[9]] <- synth.out

#### print results on screen
print(synth.out)



##################################### hypothetical treatment period: 2000 #########################
#### Generate Synth object (debt/gdp ratio), 
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
		predictors = pred,
		dependent = names(scdata[6]),
		unit.variable = "ccode",
		time.variable = "Year", 
		treatment.identifier = 0,
		controls.identifier = contr, 
		time.predictors.prior = c(1983:1998),
		time.optimize.ssr = c(1983:2000),
		unit.names.variable = "country",
		time.plot = 1983:2010
)

#### Run analysis 
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)


####
synth.tables <- synth.tab(dataprep.res = sdata, synth.res=synth.out)


#### Plot Path
path.plot(synth.res = synth.out, 
	dataprep.res = sdata,
	Ylab="Debt/GDP (Nominal)",
	Xlab="Year",
	Legend=c("Euro 11","Synthetic Euro 11"),
	Legend.position="bottomright", abline(v=2000,lty="dashed")
)



#### Plot Gaps
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
	Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
	Main=NA, abline(v=2000,lty="dashed"))


#### Extract and plot country weights
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]


#### store results in list
scresults[[9]] <- synth.out

#### print results on screen
print(synth.out)







##################################### hypothetical treatment period: 2001 #########################
#### Generate Synth object (debt/gdp ratio), 
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
		predictors = pred,
		dependent = names(scdata[6]),
		unit.variable = "ccode",
		time.variable = "Year", 
		treatment.identifier = 0,
		controls.identifier = contr, 
		time.predictors.prior = c(1983:1999),
		time.optimize.ssr = c(1983:2001),
		unit.names.variable = "country",
		time.plot = 1983:2010
)

#### Run analysis 
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)


####
synth.tables <- synth.tab(dataprep.res = sdata, synth.res=synth.out)


#### Plot Path
path.plot(synth.res = synth.out, 
	dataprep.res = sdata,
	Ylab="Debt/GDP (Nominal)",
	Xlab="Year",
	Legend=c("Euro 11","Synthetic Euro 11"),
	Legend.position="bottomright", abline(v=2001,lty="dashed")
)



#### Plot Gaps
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
	Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
	Main=NA, abline(v=2001,lty="dashed"))


#### Extract and plot country weights
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]


#### store results in list
scresults[[9]] <- synth.out

#### print results on screen
print(synth.out)





