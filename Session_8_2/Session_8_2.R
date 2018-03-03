# know your working directory:
setwd("/Users/boyu/Documents/Minerva/Second\ Year/Sesond\ Semester/CS112/R\ Code/Session_8_2")
getwd()

####### PART 1: Get the 2 RCT data sets...
# website with data is here: http://users.nber.org/~rdehejia/data/nswdata2.html

### For the original Lalonde data
# STEP 1: Download the two text files, nsw_control.txt and nsw_treated.txt

# STEP 2: Ensure that they are in R's working directory

# STEP 3: Read them into your R workspace
nsw_controls <- read.table("nsw_control.txt")
nsw_treated <- read.table("nsw_treated.txt")

# STEP 4: Bind the treated and controls together, into one data frame
nsw_data <- rbind(nsw_treated, nsw_controls)
head(nsw_data)
additional_column_to_label_data_set <- rep(c("Original Lalonde Sample"), length(nsw_data[,1]))
nsw_data <- cbind(additional_column_to_label_data_set, nsw_data)
names(nsw_data) <-  c("data_id", "treat", "age", "educ", "black", "hisp",
                      "married", "nodegr", "re75", "re78")

head(nsw_data)

### For Dehejia's version of the Lalonde Data 
### I show you how to deal with files in STATA format below (very easy)
### Download the file: "nsw_dw.dta" and confirm it's in R's working directory
library(foreign)
DW_data <- read.dta("nsw_dw.dta")

head(DW_data)

###### PART II: CREATE THE FAKE OBSERVATIONAL DATA SETS

### Now to create the 2 simulated observational data sets that each combine the
### treatment group from the data sets above, with CPS-1 survey data

### A. ### First with the original Lalonde RCT sample

# Step 1: download "cps_controls.dta" and make sure it's in R's working directory

# Step 2: read in cps_controls.dta and confirm it has the same structure
cps_controls <- read.dta("cps_controls.dta")
head(cps_controls)

# notice that the columns of cps_controls and nsw_data are different:  
# nsw_data lacks the re74 column... we have to make the columns consistent b4 rbinding them
names(nsw_data)
names(cps_controls)

cps_controls_without_re74 <- cps_controls[,-9]
(cps_controls_without_re74) <- names(nsw_data)

# Step 3: erase the RCT experiment's control group data from the Lalonde ("nsw_data") data set
nsw_data_nocontrols <- nsw_data[-which(nsw_data$treat == 0),]

# Step 4: rbind the nsw_data_nocontrols and the cps_controls together
nsw_treated_data_with_CPS <- rbind(nsw_data_nocontrols, cps_controls_without_re74)

### B. Second with Dehejia's experimental sample, which includes re74... 
###    in other words, 2 years of pre-treatment earnings -- Dehejia thought it was necessary
###    to control for more than 1 year of pre-treatment earnings...

# Make sure you have downloaded "cps_controls.dta" and make sure it's in R's working directory
# Read it in and check it out... (you probably did this already, above)
cps_controls <- read.dta("cps_controls.dta")
head(cps_controls)

# NEXT, make sure cps_controls has the same column names as Dehejia's experiment's data set
cps_controls_new_names <- cps_controls
names(cps_controls_new_names) <- names(DW_data)

# Step 3: erase the RCT experiment's control group data from Dehejia's ("nsw_data") data set
DW_data_nocontrols <- DW_data[-which(DW_data$treat == 0),]

# Step 4: rbind the nsw_data_nocontrols and the cps_controls together
DW_treated_data_with_CPS <- rbind(DW_data_nocontrols, cps_controls_new_names)

########## CONCLUSION
# you now have 4 data sets--
# 2 derived from RCTs, 
# 2 'hybrids' with treated units from each RCT and control units from the CPS survey