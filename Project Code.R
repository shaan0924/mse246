
library(lubridate)
library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
library("dplyr")
library("Matrix")
setwd("/Users/jackparkin/Desktop/MS&E 246/Project/mse246")
#setwd("/Users/sihguat/Desktop/MSE_246/mse246")
############
#Data Import
############
#raw_data_xlxs = read_excel('SBA Loan data .xlsx')
#write.csv(raw_data_xlxs,"SBA Loan data .csv", row.names = FALSE)
raw_data = read.csv("SBA Loan data .csv", header = TRUE)

#################
#Data Exploration
#################

######################################
#GrossApproval vs. Annual Default Rate
temp = raw_data
temp = transform(raw_data, bin = cut(GrossApproval, breaks= c(0,100000, 250000, 500000, 750000, 1000000, 2000000, 10000000)))
raw_data$Size[temp$bin == "(0,1e+05]"] = "<$100k"
raw_data$Size[temp$bin == "(1e+05,2.5e+05]"] = "$100k-250K"
raw_data$Size[temp$bin == "(2.5e+05,5e+05]"] = "$250k-500k"
raw_data$Size[temp$bin == "(5e+05,7.5e+05]"] = "$500k-750k"
raw_data$Size[temp$bin == "(7.5e+05,1e+07]"] = "$750k+"
raw_data$Size[temp$bin == "(7.5e+05,1e+06]"] = "$750k-$1M"
raw_data$Size[temp$bin == "(1e+06,2e+0+6]"] = "$1M-$2M"
raw_data$Size[temp$bin == "(2e+06,1e+07]"] = "$2M+"

temp = raw_data %>% drop_na(Size) %>% group_by(Size) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=Size, y=Volume, color = Size)) + geom_bar(stat="identity") + ggtitle("Total Loan Volume by Loan Size") + xlab("Size") + ylab("Total Loan Volume")

temp = raw_data %>% drop_na(Size) %>% group_by(ApprovalFiscalYear, Size) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = Size, color = Size)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Gross Approval Size") + xlab("Approval Year") + ylab("Annual Default Rate") 

########################################
#Borrower Region vs. Annual Default Rate
raw_data$BorrowerRegion[raw_data$BorrState == "ME"| raw_data$BorrState == "VT"| raw_data$BorrState == "NH"|raw_data$BorrState == "MA"| raw_data$BorrState == "RI"| raw_data$BorrState == "NY"| raw_data$BorrState == "CT"| raw_data$BorrState == "NJ"|raw_data$BorrState == "PA"] = "Northeast"
raw_data$BorrowerRegion[raw_data$BorrState == "ND"| raw_data$BorrState == "SD"| raw_data$BorrState == "NE"|raw_data$BorrState == "KS"| raw_data$BorrState == "MN"| raw_data$BorrState == "IA"| raw_data$BorrState == "MO"| raw_data$BorrState == "WI" | raw_data$BorrState == "IL"| raw_data$BorrState == "IN"| raw_data$BorrState == "MI" | raw_data$BorrState == "OH"] = "Midwest"
raw_data$BorrowerRegion[raw_data$BorrState == "TX"| raw_data$BorrState == "OK"| raw_data$BorrState == "AR"|raw_data$BorrState == "LA"| raw_data$BorrState == "MS"| raw_data$BorrState == "AL"| raw_data$BorrState == "TN"| raw_data$BorrState == "KY"| raw_data$BorrState == "WV"| raw_data$BorrState == "VA"|raw_data$BorrState == "MD"| raw_data$BorrState == "DC"| raw_data$BorrState == "DE"| raw_data$BorrState == "NC"| raw_data$BorrState == "SC"| raw_data$BorrState == "GA" | raw_data$BorrState == "FL" | raw_data$BorrState == "PR" | raw_data$BorrState == "VI"] = "South"
raw_data$BorrowerRegion[raw_data$BorrState == "WA"| raw_data$BorrState == "OR"| raw_data$BorrState == "CA"|raw_data$BorrState == "NV"| raw_data$BorrState == "ID"| raw_data$BorrState == "MT"| raw_data$BorrState == "UT"| raw_data$BorrState == "WY"|raw_data$BorrState == "CO"| raw_data$BorrState == "NM"| raw_data$BorrState == "AZ"| raw_data$BorrState == "AK"| raw_data$BorrState == "HI" | raw_data$BorrState == "GU"] = "West"

temp = raw_data %>% drop_na(BorrowerRegion) %>% group_by(BorrowerRegion) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=BorrowerRegion, y=Volume, color = BorrowerRegion)) + geom_bar(stat="identity") + ggtitle("Total Loan Volume by Borrower Region") + xlab("Borrower Region") + ylab("Total Loan Volume")

temp = raw_data %>% drop_na(BorrowerRegion) %>%  group_by(ApprovalFiscalYear, BorrowerRegion) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = BorrowerRegion, color = BorrowerRegion)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Borrower Region") + xlab("Approval Year") + ylab("Annual Default Rate")

#######################################
#Project Region vs. Annual Default Rate
raw_data$ProjectRegion[raw_data$ProjectState == "ME"| raw_data$ProjectState == "VT"| raw_data$ProjectState == "NH"|raw_data$ProjectState == "MA"| raw_data$ProjectState == "RI"| raw_data$ProjectState == "NY"| raw_data$ProjectState == "CT"| raw_data$ProjectState == "NJ"|raw_data$ProjectState == "PA"] = "Northeast"
raw_data$ProjectRegion[raw_data$ProjectState == "ND"| raw_data$ProjectState == "SD"| raw_data$ProjectState == "NE"|raw_data$ProjectState == "KS"| raw_data$ProjectState == "MN"| raw_data$ProjectState == "IA"| raw_data$ProjectState == "MO"| raw_data$ProjectState == "WI" | raw_data$ProjectState == "IL"| raw_data$ProjectState == "IN"| raw_data$ProjectState == "MI" | raw_data$ProjectState == "OH"] = "Midwest"
raw_data$ProjectRegion[raw_data$ProjectState == "TX"| raw_data$ProjectState == "OK"| raw_data$ProjectState == "AR"|raw_data$ProjectState == "LA"| raw_data$ProjectState == "MS"| raw_data$ProjectState == "AL"| raw_data$ProjectState == "TN"| raw_data$ProjectState == "KY"| raw_data$ProjectState == "WV"| raw_data$ProjectState == "VA"|raw_data$ProjectState == "MD"| raw_data$ProjectState == "DC"| raw_data$ProjectState == "DE"| raw_data$ProjectState == "NC"| raw_data$ProjectState == "SC"| raw_data$ProjectState == "GA" | raw_data$ProjectState == "FL" | raw_data$ProjectState == "PR" | raw_data$ProjectState == "VI"] = "South"
raw_data$ProjectRegion[raw_data$ProjectState == "WA"| raw_data$ProjectState == "OR"| raw_data$ProjectState == "CA"|raw_data$ProjectState == "NV"| raw_data$ProjectState == "ID"| raw_data$ProjectState == "MT"| raw_data$ProjectState == "UT"| raw_data$ProjectState == "WY"|raw_data$ProjectState == "CO"| raw_data$ProjectState == "NM"| raw_data$ProjectState == "AZ"| raw_data$ProjectState == "AK"| raw_data$ProjectState == "HI" | raw_data$ProjectState == "GU"] = "West"

temp = raw_data %>% drop_na(ProjectRegion) %>% group_by(ProjectRegion) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=ProjectRegion, y=Volume, color = ProjectRegion)) + geom_bar(stat="identity") + ggtitle("Total Loan Volume by Project Region") + xlab("Project Region") + ylab("Total Loan Volume")

temp = raw_data %>% drop_na(ProjectRegion) %>%  group_by(ApprovalFiscalYear, ProjectRegion) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = ProjectRegion, color = ProjectRegion)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Project Region") + xlab("Approval Year") + ylab("Annual Default Rate")

######################################
#Business Type vs. Annual Default Rate
temp = raw_data[!(raw_data$BusinessType == ""), ]
temp = temp %>% drop_na(BusinessType) %>% group_by(BusinessType) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=BusinessType, y=Volume, color = BusinessType)) + geom_bar(stat="identity") + ggtitle("Total Loan Volume by Business Type") + xlab("Business Type") + ylab("Total Loan Volume")

temp = raw_data[!(raw_data$BusinessType == ""), ]
temp = temp %>% group_by(ApprovalFiscalYear, BusinessType) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = BusinessType, color = BusinessType)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Business Type") + xlab("Approval Year") + ylab("Annual Default Rate")

##############################
#NAICS vs. Annual Default Rate
temp = raw_data
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "11"] = "Agriculture"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "21"] = "Mining & Oil"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "22"] = "Utilities"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "23"] = "Construction"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 1) == "3"] = "Manufacturing"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "42"] = "Wholesale Trade"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "44" | substring(raw_data$NaicsCode, 1, 2) == "45"] = "Retail Trade"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "48" | substring(raw_data$NaicsCode, 1, 2) == "49"] = "Transportation & Warehousing"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "51"] = "Information"
temp$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "52"] = "Finance & Insurance"

temp = temp %>% drop_na(NAICS_Sector) %>%  group_by(ApprovalFiscalYear, NAICS_Sector) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = NAICS_Sector, color = NAICS_Sector)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by NAICS Sector (Subset 1)") + xlab("Approval Year") + ylab("Annual Default Rate")

raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "53"] = "Real Estate & Leasing"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "54"] = "Professional Services"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "55"] = "Management Services"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "56"] = "Administrative and Waste Management Services"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "61"] = "Educational Services"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "62"] = "Health Care"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "71"] = "Arts & Entertainment"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "72"] = "Accommodation and Food Services"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "81"] = "Other Services"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "92"] = "Public Administration"

temp = raw_data %>% drop_na(NAICS_Sector) %>%  group_by(ApprovalFiscalYear, NAICS_Sector) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = NAICS_Sector, color = NAICS_Sector)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by NAICS Sector (Subset 2)") + xlab("Approval Year") + ylab("Annual Default Rate")

raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "11"] = "Agriculture"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "21"] = "Mining & Oil"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "22"] = "Utilities"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "23"] = "Construction"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 1) == "3"] = "Manufacturing"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "42"] = "Wholesale Trade"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "44" | substring(raw_data$NaicsCode, 1, 2) == "45"] = "Retail Trade"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "48" | substring(raw_data$NaicsCode, 1, 2) == "49"] = "Transportation & Warehousing"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "51"] = "Information"
raw_data$NAICS_Sector[substring(raw_data$NaicsCode, 1, 2) == "52"] = "Finance & Insurance"

temp = raw_data %>% drop_na(NAICS_Sector) %>% group_by(NAICS_Sector) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=NAICS_Sector, y=Volume, color = NAICS_Sector)) + geom_bar(stat="identity") + ggtitle("Total Loan Volume by NAICS Sector") + xlab("NAICS Sector") + ylab("Total Loan Volume")

####################################
#Term Length vs. Annual Default Rate

temp = raw_data
temp = transform(raw_data, bin = cut(TermInMonths, breaks= c(0, 119, 180, 240, 500)))
raw_data$TermLength[temp$bin == "(0,119]"] = "<120mths"
raw_data$TermLength[temp$bin == "(119,180]"] = "120-180mths"
raw_data$TermLength[temp$bin == "(180,240]"] = "181-240mths"
raw_data$TermLength[temp$bin == "(240,500]"] = "240mths+"

temp = raw_data %>% drop_na(TermLength) %>% group_by(TermLength) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=TermLength, y=Volume, color = TermLength)) + geom_bar(stat="identity") + ggtitle("Total Loan Volume by Term Length") + xlab("Term Length") + ylab("Total Loan Volume")

temp = raw_data %>% drop_na(TermLength) %>% group_by(ApprovalFiscalYear, TermLength) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = TermLength, color = TermLength)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Term Length") + xlab("Approval Year") + ylab("Annual Default Rate") 

############
#IMPORTS
############
#GDP Import
GDP = read.csv("GDP.csv")
#SP500 Import
SP500 = getSymbols("^GSPC",from = "1990-01-31",to = "2014-12-31", periodicity = 'monthly', auto.assign = FALSE)
#FedFunds Import
FEDFUNDS = read.csv("FEDFUNDS.csv")
#CPI Import (Consumer Price Index for All Urban Consumers: All Items in U.S. City Average)
CPI = read.csv("CPIAUCSL.csv")
#Unemployment
UnemploymentUSbyState = read.csv('StateUR.csv', header = TRUE)

##############
#Preprocessing
##############
raw_data = raw_data[!(raw_data$LoanStatus=="CANCLD" | raw_data$LoanStatus=="EXEMPT"),]
#15 occurances of different states or NA thus delete those loans

unidentified.states = unique(raw_data$ProjectState[!(raw_data$ProjectState %in% unique(UnemploymentUSbyState$State))])
raw_data = raw_data[!(raw_data$ProjectState %in% unidentified.states),]

unidentified.states.Borr = unique(raw_data$BorrState[!(raw_data$BorrState %in% unique(UnemploymentUSbyState$State))])

###################################
#UnemploymentInProjectState
raw_data$ApprovalDate = as.Date(raw_data$ApprovalDate, "%m/%d/%y")
temp.data = transform(raw_data, month.bin = cut(ApprovalDate, breaks = "month"))

tempUR = rename(UnemploymentUSbyState, month.bin = DATE, ProjectState = State, URinProjectState = UR)
temp = merge(x=temp.data,y=tempUR,by=c("ProjectState","month.bin"))

###################################
#UnemploymentInBorrowerState
tempUR.Borr = rename(UnemploymentUSbyState, month.bin = DATE, BorrState = State, URinBorrState = UR)
temp = merge(x=temp,y=tempUR.Borr,by=c("BorrState","month.bin"))

###################################
#GDP
tempGDP = transmute(GDP, month.bin = DATE, GDP = GDP)
tempGDP$quarter.bin = paste("Q4", substring(tempGDP$month.bin, 0, 4))
tempGDP$quarter.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 10] = paste("Q3", substring(tempGDP$month.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 10], 0, 4))
tempGDP$quarter.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 7] = paste("Q2", substring(tempGDP$month.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 7], 0, 4))
tempGDP$quarter.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 4] = paste("Q1", substring(tempGDP$month.bin[as.numeric(substring(tempGDP$month.bin, 6, 7)) < 4], 0, 4))
tempGDP = subset(tempGDP, select = -month.bin)

temp$quarter.bin = paste("Q4", substring(temp$ApprovalDate, 0, 4))
temp$quarter.bin[as.numeric(substring(temp$ApprovalDate, 6, 7)) < 10] = paste("Q3", substring(temp$ApprovalDate[as.numeric(substring(temp$ApprovalDate, 6, 7)) < 10], 0, 4))
temp$quarter.bin[as.numeric(substring(temp$ApprovalDate, 6, 7)) < 7] = paste("Q2", substring(temp$ApprovalDate[as.numeric(substring(temp$ApprovalDate, 6, 7)) < 7], 0, 4))
temp$quarter.bin[as.numeric(substring(temp$ApprovalDate, 6, 7)) < 4] = paste("Q1", substring(temp$ApprovalDate[as.numeric(substring(temp$ApprovalDate, 6, 7)) < 4], 0, 4))

temp = merge(x=temp,y=tempGDP,by="quarter.bin")
temp = subset(temp, select = -quarter.bin)

###################################
#SP500
tempSP500 = as.data.frame(SP500)
tempSP500$month.bin = rownames(tempSP500)
tempSP500 = transmute(tempSP500,  month.bin = rownames(tempSP500), GSPC.price= GSPC.Adjusted)
temp = merge(x=temp,y=tempSP500,by="month.bin")

###################################
#Fed Funds
tempFedFunds = transmute(FEDFUNDS, month.bin = DATE, FedFunds = FEDFUNDS)
temp = merge(x=temp,y=tempFedFunds,by="month.bin")

###################################
#CPI
tempCPI = transmute(CPI, month.bin = DATE, CPI = CPIAUCSL)
temp = merge(x=temp,y=tempCPI,by="month.bin")
raw_data = subset(temp, select = -month.bin)

###################
#BinaryIntergerTerm
raw_data$BinaryIntergerTerm[raw_data$TermInMonths %% 12 == 0] = 1
raw_data$BinaryIntergerTerm[raw_data$TermInMonths %% 12 != 0] = 0

#####################
#BinaryRepeatBorrower
temp = duplicated(raw_data$BorrName)
raw_data$BinaryRepeatBorrower[temp == TRUE] = 1
raw_data$BinaryRepeatBorrower[temp == FALSE] = 0

############################
#BinaryBankStEqualBorrowerSt

raw_data$BinaryBankStEqualBorrowerSt[as.character(raw_data$BorrState) == as.character(raw_data$CDC_State)] = 1
raw_data$BinaryBankStEqualBorrowerSt[as.character(raw_data$BorrState)!= as.character(raw_data$CDC_State)] = 0

###############################
#BinaryProjectStEqualBorrowerSt

raw_data$BinaryProjectStEqualBorrowerSt[as.character(raw_data$BorrState) == as.character(raw_data$ProjectState)] = 1
raw_data$BinaryProjectStEqualBorrowerSt[as.character(raw_data$BorrState) != as.character(raw_data$ProjectState)] = 0

###############################
#Modification
#key continous: "LogGrossApproval", "LogThirdPartyDollars", "TermInMonths", "LogGDP", "LogSP500", "LogFedFunds", "LogCPI", "URinProjectState", "URinBorrState"
raw_data$LogGrossApproval = log(raw_data$GrossApproval)
raw_data$LogThirdPartyDollars = log(raw_data$ThirdPartyDollars)
raw_data$LogGDP = log(raw_data$GDP)
raw_data$LogSP500 = log(raw_data$GSPC.price)
raw_data$LogFedFunds = log(raw_data$FedFunds)
raw_data$LogCPI = log(raw_data$CPI)

#key discrete: "BusinessType", "NAICS_Sector", "DeliveryMethod", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "ApprovalFiscalYear", "BorrowerRegion", "ProjectRegion"

modified_data = raw_data[c("LogGrossApproval", "LogThirdPartyDollars", "TermInMonths", "LogGDP", "LogSP500", "LogFedFunds", "LogCPI", "URinProjectState", "URinBorrState", "BusinessType", "NAICS_Sector", "DeliveryMethod", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "ApprovalFiscalYear", "BorrowerRegion", "ProjectRegion")]

modified_data$Default[raw_data$LoanStatus == "CHGOFF"] = 1
modified_data$Default[raw_data$LoanStatus != "CHGOFF"] = 0

##############
#Normalization
c = 0.1
modified_data$LogGrossApproval = (modified_data$LogGrossApproval - mean(modified_data$LogGrossApproval))/(c + sd(modified_data$LogGrossApproval))
modified_data$LogThirdPartyDollars = (modified_data$LogThirdPartyDollars - mean(modified_data$LogThirdPartyDollars))/(c + sd(modified_data$LogThirdPartyDollars))
modified_data$TermInMonths = (modified_data$TermInMonths - mean(modified_data$TermInMonths))/(c + sd(modified_data$TermInMonths))
modified_data$LogGDP = (modified_data$LogGDP - mean(modified_data$LogGDP))/(c + sd(modified_data$LogGDP))
modified_data$LogSP500 = (modified_data$LogSP500 - mean(modified_data$LogSP500))/(c + sd(modified_data$LogSP500))
modified_data$LogFedFunds = (modified_data$LogFedFunds - mean(modified_data$LogFedFunds))/(c + sd(modified_data$LogFedFunds))
modified_data$LogCPI = (modified_data$LogCPI - mean(modified_data$LogCPI))/(c + sd(modified_data$LogCPI))
modified_data$URinProjectState = (modified_data$URinProjectState - mean(modified_data$URinProjectState))/(c + sd(modified_data$URinProjectState))
modified_data$URinBorrState = (modified_data$URinBorrState - mean(modified_data$URinBorrState))/(c + sd(modified_data$URinBorrState))

###############
#Missing Values
#Continous
temp = modified_data[c("LogGrossApproval", "LogThirdPartyDollars", "TermInMonths", "LogGDP", "LogSP500", "LogFedFunds", "LogCPI", "URinProjectState", "URinBorrState")]
temp[is.na(temp)] = 0
modified_data[c("LogGrossApproval", "LogThirdPartyDollars", "TermInMonths", "LogGDP", "LogSP500", "LogFedFunds", "LogCPI", "URinProjectState", "URinBorrState")] = temp

#Discrete
temp = modified_data[c("BusinessType", "NAICS_Sector", "DeliveryMethod", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "ApprovalFiscalYear", "BorrowerRegion", "ProjectRegion")]
temp[is.na(temp)] = "NA"
missing = which(temp=="", arr.ind=TRUE)[,1]
temp$DeliveryMethod[temp$DeliveryMethod == "504REFI"] = "504"
temp = temp[-missing,]
modified_data = modified_data[-missing,]

modified_data[c("BusinessType", "NAICS_Sector", "DeliveryMethod", "BinaryIntergerTerm", "BinaryRepeatBorrower", "BinaryBankStEqualBorrowerSt", "BinaryProjectStEqualBorrowerSt", "ApprovalFiscalYear", "BorrowerRegion", "ProjectRegion")] = temp

#################
#Data Paritioning
train_size = round((nrow(modified_data)/10)*7, 0)
validation_size = round((nrow(modified_data)/10), 0)
test_size = nrow(modified_data) - train_size - validation_size

train_data = modified_data[0:train_size,]
validation_data = modified_data[(train_size+1):(train_size+validation_size),]
test_data = modified_data[(train_size+validation_size+1):nrow(modified_data),]

################
#Logisitic Model
################
library(leaps)
library(tidyverse)
library(ROCR)
library(glmnet)

######################
#Basic Logistic Model

#Training
log_model = glm(data=train_data, Default ~., family= binomial)
summary(log_model) 

#Training ROC AUC
log_model_train_prediction = predict(log_model, train_data, type="response")
log_model_train_prediction = prediction(log_model_train_prediction, train_data$Default)
auc = unlist(slot(performance(log_model_train_prediction, 'auc'), 'y.values'))
auc
#0.7256392

#Test ROC AUC
log_model_test_prediction = predict(log_model, newdata = test_data, type="response")
temp = test_data$Default[!is.na(log_model_test_prediction)]
log_model_test_prediction = log_model_test_prediction[!is.na(log_model_test_prediction)]
log_model_test_prediction = prediction(log_model_test_prediction, temp)
auc = unlist(slot(performance(log_model_test_prediction, 'auc'), 'y.values'))
auc
#0.6053772

#Plotting ROCs
roc_train = performance(log_model_train_prediction,"tpr","fpr")
roc_test = performance(log_model_test_prediction,"tpr","fpr")
plot(roc_train, col = 'red', main = 'Basic Logistic Model Training ROC (red) vs. Testing ROC (blue)')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 

################################
#Ridge and Lasso Logisitic Model

#Training
x_train = model.matrix(Default ~., train_data)[, -1]
x_train = x_train[,order(colnames(x_train))]
y_train = train_data$Default

model_L1 = glmnet(x_train, y_train, alpha = 1, nlambda = 10, family="binomial")
model_L2 = glmnet(x_train, y_train, alpha = 0, nlambda = 10, family="binomial")

x_validation = model.matrix(Default ~., validation_data)[, -1]
x_validation = cbind(x_validation, "NAICS_SectorPublic Administration" = 0)
x_validation = cbind(x_validation, "NAICS_SectorBlank" = 0)
x_validation = x_validation[,order(colnames(x_validation))]

prediction_L1_train = predict(model_L1, newx = x_train, type = "response")
prediction_L2_train = predict(model_L2, newx = x_train, type = "response")
prediction_L1_validation = predict(model_L1, newx = x_validation, type = "response")
prediction_L2_validation = predict(model_L2, newx = x_validation, type = "response")

#Validation: Tuning Lambda Hyperparameter
AUC_L1_train = vector()
AUC_L2_train = vector()
AUC_L1_validation = vector()
AUC_L2_validation = vector()
for(i in 1:10){
  AUC_L1_train = append(AUC_L1_train, unlist(slot(performance(prediction(prediction_L1_train[,i], train_data$Default), 'auc'), 'y.values')))
  AUC_L2_train = append(AUC_L2_train, unlist(slot(performance(prediction(prediction_L2_train[,i], train_data$Default), 'auc'), 'y.values')))
  AUC_L1_validation = append(AUC_L1_validation, unlist(slot(performance(prediction(prediction_L1_validation[,i], validation_data$Default), 'auc'), 'y.values')))
  AUC_L2_validation = append(AUC_L2_validation,  unlist(slot(performance(prediction(prediction_L2_validation[,i], validation_data$Default), 'auc'), 'y.values')))
}

#Best L1 Hyperparameter
best_L1_lambda_index = which.max(AUC_L1_validation)
best_L1_lambda = model_L1$lambda[best_L1_lambda_index]
best_L1_AUC = max(AUC_L1_validation)
best_L1_AUC
#0.6607889
best_model_L1_coef = as.matrix(coef(model_L1, s= model_L1$lambda[best_L1_lambda_index]))

#Best L2 Hyperparameter
best_L2_lambda_index = which.max(AUC_L2_validation)
best_L2_lambda = model_L2$lambda[best_L2_lambda_index]
best_L2_AUC = max(AUC_L2_validation)
best_L2_AUC
#0.6566973
best_model_L2_coef = as.matrix(coef(model_L2, s= model_L2$lambda[best_L2_lambda_index]))

#Plotting best L1 & l2 coefficients
best_L1_model_coeff_plot = qplot(y= best_model_L1_coef[,1])
best_L1_model_coeff_plot + labs(title = "L1 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")
best_L2_model_coeff_plot = qplot(y= best_model_L2_coef[,1])
best_L2_model_coeff_plot + labs(title = "L2 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")

#Ensemble Training
B = 100
for (b in 1:B){
  sample_indices = sample(nrow(x_train),nrow(x_train), replace = TRUE)
  x_sample = x_train[sample_indices,]
  y_sample = y_train[c(sample_indices)]
  model_L1 = glmnet(x_sample, y_sample, alpha = 1, lambda = best_L1_lambda, family="binomial")
  if (b == 1){
    ensemble_coeff = coef(model_L1)
  }
  else{
    ensemble_coeff = ensemble_coeff + coef(model_L1)
  }
}
ensemble_coeff = ensemble_coeff/B
for (i in 1:(length(ensemble_coeff)-1)){
  if (i == 1){
    model_L1$a0[1] = ensemble_coeff[i]
  }
  else {
    model_L1$beta[i-1] = ensemble_coeff[i]
  }
}
best_ensemble_model_L1_coef = as.matrix(coef(model_L1))
best_ensemble_L1_model_coeff_plot = qplot(y= best_model_L1_coef[,1])
best_ensemble_L1_model_coeff_plot + labs(title = "Ensemble L1 Logistic Model Coeffcients", x= "Covariates", y= "Coeffcient Value")


#Testing best L1 ensemble model
x_test = model.matrix(Default ~., test_data)[, -1]
x_test = cbind(x_test, "NAICS_SectorPublic Administration" = 0)
x_test = cbind(x_test, "NAICS_SectorBlank" = 0)
x_test = x_test[,order(colnames(x_test))]

prediction_L1_test = predict(model_L1, newx = x_test, type = "response")
prediction_L1_test = prediction(prediction_L1_test, test_data$Default)
auc = unlist(slot(performance(prediction_L1_test, 'auc'), 'y.values'))
auc
#0.6079187

#Plotting ROCs
roc_train = performance(prediction(prediction_L1_train[,best_L1_lambda_index], train_data$Default),"tpr","fpr")
roc_validation = performance(prediction(prediction_L1_validation[,best_L1_lambda_index], validation_data$Default),"tpr","fpr")
roc_test = performance(prediction_L1_test,"tpr","fpr")
plot(roc_train, col = 'red', main = 'L1 Logistic Model Training ROC (red) vs. Validation ROC (green) vs. Testing ROC (blue)')
plot(roc_validation, add = TRUE, col = 'green')
plot(roc_test, add = TRUE, col = 'blue')
abline(a = 0, b = 1) 

