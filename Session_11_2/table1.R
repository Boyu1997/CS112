

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
	20,# labor productivity annual growth	
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
	#35, # Expenditure on Families %GDP	
	36, # Polcon III
	#37, Polcon V
	38, #Potrafke Ideology
	39, # maj
	#40,# Herfindahl Index 
	41,# lag debt/gdp (RR)
	42#,# Rae fractionalisation index (government),#
	#43,# Rae fractionalisation index (total) ,	
)]

# replicate table 1
tab1 <- data.frame(cbind(apply(scdata[,pred],2,min,na.rm=TRUE),apply(scdata[,pred],2,max,na.rm=TRUE),apply(scdata[,pred],2,mean,na.rm=TRUE)))
names(tab1) <- c('Minimum','Maximum','Mean')
xtable(tab1)


