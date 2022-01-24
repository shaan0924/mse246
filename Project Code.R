

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

#GrossApproval vs. Annual Default Rate
hist(raw_data$GrossApproval, breaks = seq(from=0, to=max(raw_data$GrossApproval), by=50000), main="Gross Approval Amounts")
raw_data = transform(raw_data, bin = cut(GrossApproval, breaks= c(0,100000, 200000, 500000, 10000000)))
temp = raw_data %>% group_by(ApprovalFiscalYear, bin) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = bin, color = bin)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Gross Approval Size") + xlab("Approval Year") + ylab("Annual Default Rate") 

#GrossApproval vs. Annual Default Rate
hist(raw_data$GrossApproval, breaks = seq(from=0, to=max(raw_data$GrossApproval), by=50000), main="Gross Approval Amounts")
raw_data = transform(raw_data, bin = cut(GrossApproval, breaks= c(0,100000, 250000, 500000, 750000, 10000000)))
raw_data$Size[raw_data$bin == 	"(0,1e+05]"] = "<$100k"
raw_data$Size[raw_data$bin == 	"(1e+05,2.5e+05]"] = "$100k-250K"
raw_data$Size[raw_data$bin == 	"(2.5e+05,5e+05]"] = "$250k-500k"
raw_data$Size[raw_data$bin == 	"(5e+05,7.5e+05]"] = "$500k-750k"
raw_data$Size[raw_data$bin == 	"(7.5e+05,1e+07]"] = "$750k+"

temp = raw_data %>% group_by(ApprovalFiscalYear, Size) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))

  
ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = Size, color = Size)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Gross Approval Size") + xlab("Approval Year") + ylab("Annual Default Rate") 

#GrossApproval vs. Annual Default Rate

raw_data$Region[raw_data$BorrState == "ME"| raw_data$BorrState == "VT"| raw_data$BorrState == "NH"|raw_data$BorrState == "MA"| raw_data$BorrState == "RI"| raw_data$BorrState == "NY"| raw_data$BorrState == "CT"| raw_data$BorrState == "NJ"|raw_data$BorrState == "PA"] = "Northeast"
raw_data$Region[raw_data$BorrState == "ND"| raw_data$BorrState == "SD"| raw_data$BorrState == "NE"|raw_data$BorrState == "KS"| raw_data$BorrState == "MN"| raw_data$BorrState == "IA"| raw_data$BorrState == "MO"| raw_data$BorrState == "WI" | raw_data$BorrState == "IL"| raw_data$BorrState == "IN"| raw_data$BorrState == "MI" | raw_data$BorrState == "OH"] = "Midwest"
raw_data$Region[raw_data$BorrState == "TX"| raw_data$BorrState == "OK"| raw_data$BorrState == "AR"|raw_data$BorrState == "LA"| raw_data$BorrState == "MS"| raw_data$BorrState == "AL"| raw_data$BorrState == "TN"| raw_data$BorrState == "KY"| raw_data$BorrState == "WV"| raw_data$BorrState == "VA"|raw_data$BorrState == "MD"| raw_data$BorrState == "DC"| raw_data$BorrState == "DE"| raw_data$BorrState == "NC"| raw_data$BorrState == "SC"| raw_data$BorrState == "GA" | raw_data$BorrState == "FL" | raw_data$BorrState == "PR" | raw_data$BorrState == "VI"] = "South"
raw_data$Region[raw_data$BorrState == "WA"| raw_data$BorrState == "OR"| raw_data$BorrState == "CA"|raw_data$BorrState == "NV"| raw_data$BorrState == "ID"| raw_data$BorrState == "MT"| raw_data$BorrState == "UT"| raw_data$BorrState == "WY"|raw_data$BorrState == "CO"| raw_data$BorrState == "NM"| raw_data$BorrState == "AZ"| raw_data$BorrState == "AK"| raw_data$BorrState == "HI" | raw_data$BorrState == "GU"] = "West"

temp = raw_data %>% group_by(Region) %>% summarise("Volume" = sum(GrossApproval))
ggplot(data=temp, aes(x=Region, y=Volume, color = Region)) + geom_bar(stat="identity")
unique(raw_data$BorrState)
temp = raw_data %>% group_by(ApprovalFiscalYear, Region) %>% summarise("Annual Default Rate" = sum(LoanStatus == "CHGOFF")/sum(LoanStatus == "CHGOFF" | LoanStatus == "PIF"))


ggplot(data=temp, aes(x=ApprovalFiscalYear, y= `Annual Default Rate`, group = Region, color = Region)) + geom_point() + geom_line() + ggtitle("Approval Year vs. Annual Default Rate by Region") + xlab("Approval Year") + ylab("Annual Default Rate")

#######################
#Additional Data Import
#######################

SP500 = getSymbols("^GSPC",from = "2005-01-01",to = "2009-12-31", periodicity = 'weekly', auto.assign = FALSE)
