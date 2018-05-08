########################################################################
####### This is file "euro11.R"
####### This version: 2014-02-04
####### I run the Synthetic control analyses for the Euro 11 countries
########################################################################





#### Let's begin with defining some predictors:

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
	#18,# GDP growth	
	#19,# growth in multi factor productivity
	20,# labor productivity annual growth	
	21, # health expenditure/GDP
	22,# GDP expenditure approach
	#23,# tax revenue %GDP (general)	
	24,# taxrev %GDP, no Social Security
	25, # CO2 emissions
	#26, # FDI	
	#27, # GDP growth	
	#28,# Gini index	
	#29,#,# Inflation (Consumer Prices)
	#30, # Poverty	
	31,#, # unemployment (World Bank)	
	##32, #Population
	#33,#, #openness (PWT)
	34,#,# openness (expenditure)
	#35, # Expenditure on Families %GDP
	36, # PolconIII
	#37, # PolconV
	38,  # Potrafke ideology
	39, # Majority margin
	#40, # Herfindahl Index Government
	41, #lag debt/gdp (RR)	
	42#,# Rae Fractionalisation index (government)
	#43 # Rae Fractionalisation Index (total)
	)]

#### define countries used for synthetic control group

contr <- sort(unique(scdata$ccode[is.element(scdata$country,setdiff(scdata$country,c(Euro12,"Euro 11","Slovenia")))]))

#### The following countries have to be excluded due to data constraints (missing values)
contr <- setdiff(contr, c(1111,2222,70,155,225,269,290,310,316,317,349,355,360,366,666,732,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))


#### Show countries that are available for Synthetic Greece
country <- sort(unique(scdata$country[scdata$ccode %in% contr]))


#### Generate Synth object (debt/gdp ratio) to be used to run the analysis
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
		predictors = pred,
		dependent = names(scdata[6]),
		unit.variable = "ccode",
		time.variable = "Year", 
		treatment.identifier = 0,
		controls.identifier = contr, 
		time.predictors.prior = c(1983:1998),
		time.optimize.ssr = c(1983:1999),
		unit.names.variable = "country",
		time.plot = 1983:2010
)


#### Run the synthetic control analysis:
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps from the results
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)


#### 
synth.tables <- synth.tab(dataprep.res = sdata, synth.res=synth.out)





#### Plot the Path of the Debt to GDP ratio for the Euro11 and the Synthetic control
path.plot(synth.res = synth.out, 
	dataprep.res = sdata,
	Ylab="Debt/GDP (Nominal)",
	Xlab="Year",
	Legend=c("Euro 11","Synthetic Euro 11"),
	Legend.position="bottomright", abline(v=1999,lty="dashed")
)



#### Plot the gap of the Debt to GDP ratio for the Euro11 and the Synthetic control
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
	Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
	Main=NA, abline(v=1999,lty="dashed"))



#### extract Country weights from Synthetic control 
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]

#### Plot Country weights
dotchart(a[,1],pch=16)

#### store estimation results in list
scresults[[1]] <- synth.out

#### Print estimation results on screen
print(synth.out)

# generate table 3
xtable(synth.tables$tab.pred)



