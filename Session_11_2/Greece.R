#####################################################################
####### this is file "Greece.R"
####### This version 2014-02-04
####### I run the analysis for the synthetic control study of Greece
#####################################################################


#### define predictors
pred <- names(scdata)[c(#
	7, # pop65+
	8, # pop15-	
	#9,# unemployment (1)
	#10,# system
	11,# yrcurnt
	12,# allhouse
	#13,# legelec
	#14, # pluralty
	#15, # pr
	#16, # checks 
	#17, # fri
	#18,# gdpgrowth	
	#19,# growth in multi factor productivity
	20,# labor productivity annual growth	
	21, # health/GDP
	22,# GDP expenditure approach
	#23,# tax revenue %GDP (general)	
	24,# taxrev %GDP
	25, # CO2 emissions
	#26, # FDI	
	27, # GDP growth	
	#28,# Gini index	
	29,#,# Inflation (Consumer Prices)
	#30, # Poverty	
	31,#, # unemployment (World Bank)	
	#32#, #Population
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


#### define controls
contr <- sort(unique(scdata$ccode[is.element(scdata$country,setdiff(scdata$country,c(Euro17,"Euro 11","Slovenia")))]))

contr <- setdiff(contr, c(0,70,1111,2222,155,225,290,310,316,317,349,355,360,366,666,732,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))

#### Show countries available for analysis
print(country <- sort(unique(scdata$country[scdata$ccode %in% contr])))



#### Generate Synth object (debt/gdp ratio)
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | is.element(scdata$country,c("Greece"))==TRUE,],
		predictors = pred,
		dependent = names(scdata[6]),
		unit.variable = "ccode",
		time.variable = "Year", 
		treatment.identifier =  350,
		controls.identifier = contr, 
		time.predictors.prior = c(1985:1998),
		time.optimize.ssr = c(1983:1998),
		unit.names.variable = "country",
		time.plot = 1980:2010
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
	Legend=c("Greece","Synthetic Greece"),
	Legend.position="topleft",
          abline(v=1999,lty="dashed")
)


#### Plot Gaps
gaps.plot(synth.res = synth.out, dataprep.res = sdata,
	Ylab= "Gap in Debt/GDP (percentage points, 1983-2010)", Xlab="Year",
	Main=NA,  abline(v=1999,lty="dashed"))





#### extract country weights
a <- cbind(synth.tables$tab.w[,1],synth.tables$tab.w[,2])
row.names(a) <- synth.tables$tab.w[,2]

dotchart(a[,1],pch=16)


#### Store results in list object
scresults[[4]] <- synth.out


#### Print results on Screen
print(synth.out)
