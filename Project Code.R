
library(lubridate)
library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
setwd("/Users/jackparkin/Desktop/MS&E 246/Project/mse246")


############
#Data Import
############

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


###########################################
#Data Manipulation / Additional Data Import
###########################################

#key continous: log(GrossApproval), log(ThirdPartyDollars), TermInMonths, log(GSP), log(FedFunds), log(SP500), UnemploymentInBorrowerState, UnemploymentInProjectState

#key discrete: BusinessType, NAICS_Sector, DeliveryMethod, BinaryIntergerTerm, BinaryRepeatBorrower, BinaryBankStEqualBorrowerSt, BinaryProjectStEqualBorrowerSt, ApprovalFiscalYear, BorrowerRegion, ProjectRegion

#To do: log(GSP),log(FedFunds), log(SP500), UnemploymentInBorrowerState, UnemploymentInProjectState, BinaryBankStEqualBorrowerSt, BinaryProjectStEqualBorrowerSt

#?: AgeOfLoan, BorrowerRegion vs. State, ProjectRegion vs. State?


############
#GDP Import

####TO DO####

############
#FedFunds Import

FEDFUNDS = read.csv("FEDFUNDS.csv")

#############
#SP500 Import

SP500 = getSymbols("^GSPC",from = "1990-01-31",to = "2014-12-31", periodicity = 'monthly', auto.assign = FALSE)

###################################
#UnemploymentInBorrowerState Import

####TO DO####

##################################
#UnemploymentInProjectState Import

####TO DO####

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

raw_data$BinaryBankStEqualBorrowerSt[raw_data$BorrState == raw_data$CDC_State] = 1
raw_data$BinaryBankStEqualBorrowerSt[raw_data$BorrState != raw_data$ThirdPartyLender_State] = 0
###############################
#BinaryProjectStEqualBorrowerSt

raw_data$BinaryProjectStEqualBorrowerSt[raw_data$BorrState == raw_data$ProjectState] = 1
raw_data$BinaryProjectStEqualBorrowerSt[raw_data$BorrState != raw_data$ProjectState] = 0

##############
#Preprocessing
##############

###############
#Missing Values

##############
#Normalization

#################
#Data Paritioning

