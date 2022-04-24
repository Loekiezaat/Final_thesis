# load packages --------
library(tidyverse)
library(here)
library(janitor)
library(skimr)
library(reshape2)
library(plm)
library(ggplot2)
library(fixest)
library(margins)

library(mfx)
library(prediction)

library(marginaleffects)

# read and view data --------

# make separate datasets for each variable -----
# start with NONPBF
dataNONPBF<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")  #read data
dataNONPBF_turnover_melted<-melt(dataNONPBF[,1:10], id.vars = c("id"))    #select only the first 10 columns
dataNONPBF_turnover_melted$year<-as.numeric(substr(dataNONPBF_turnover_melted$variable,10,13))  #let R know what the year is
colnames(dataNONPBF_turnover_melted)[3]<-"turnover"                    #make the column name turnover
dataNONPBF_turnover_melted<-dataNONPBF_turnover_melted[,-2]            #remove column with variable name

#new dataset for each variable
dataNONPBF2<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF2<-dataNONPBF2[,-c(2:10)]                        # remove columns of previous variable
dataNONPBF_netincome_melted<-melt(dataNONPBF2[,1:10], id.vars = c("id"))
dataNONPBF_netincome_melted$year<-as.numeric(substr(dataNONPBF_netincome_melted$variable,11,14))
colnames(dataNONPBF_netincome_melted)[3]<-"netincome"
dataNONPBF_netincome_melted<-dataNONPBF_netincome_melted[,-2]

dataNONPBF3<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF3<-dataNONPBF3[,-c(2:19)]
dataNONPBF_cashflow_melted<-melt(dataNONPBF3[,1:10], id.vars = c("id"))
dataNONPBF_cashflow_melted$year<-as.numeric(substr(dataNONPBF_cashflow_melted$variable,10,13))
colnames(dataNONPBF_cashflow_melted)[3]<-"cashflow"
dataNONPBF_cashflow_melted<-dataNONPBF_cashflow_melted[,-2]

dataNONPBF4<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF4<-dataNONPBF4[,-c(2:28)]
dataNONPBF_totalassets_melted<-melt(dataNONPBF4[,1:10], id.vars = c("id"))
dataNONPBF_totalassets_melted$year<-as.numeric(substr(dataNONPBF_totalassets_melted$variable,13,16))
colnames(dataNONPBF_totalassets_melted)[3]<-"totalassets"
dataNONPBF_totalassets_melted<-dataNONPBF_totalassets_melted[,-2]

dataNONPBF5<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF5<-dataNONPBF5[,-c(2:37)]
dataNONPBF_currentratio_melted<-melt(dataNONPBF5[,1:10], id.vars = c("id"))
dataNONPBF_currentratio_melted$year<-as.numeric(substr(dataNONPBF_currentratio_melted$variable,14,17))
colnames(dataNONPBF_currentratio_melted)[3]<-"currentratio"
dataNONPBF_currentratio_melted<-dataNONPBF_currentratio_melted[,-2]

dataNONPBF6<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF6<-dataNONPBF6[,-c(2:46)]
dataNONPBF_profitmargin_melted<-melt(dataNONPBF6[,1:10], id.vars = c("id"))
dataNONPBF_profitmargin_melted$year<-as.numeric(substr(dataNONPBF_profitmargin_melted$variable,14,17))
colnames(dataNONPBF_profitmargin_melted)[3]<-"profitmargin"
dataNONPBF_profitmargin_melted<-dataNONPBF_profitmargin_melted[,-2]

dataNONPBF7<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF7<-dataNONPBF7[,-c(2:55)]
dataNONPBF_roe_melted<-melt(dataNONPBF7[,1:10], id.vars = c("id"))
dataNONPBF_roe_melted$year<-as.numeric(substr(dataNONPBF_roe_melted$variable,5,8))
colnames(dataNONPBF_roe_melted)[3]<-"roe"
dataNONPBF_roe_melted<-dataNONPBF_roe_melted[,-2]

dataNONPBF8<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF8<-dataNONPBF8[,-c(2:64)]
dataNONPBF_roce_melted<-melt(dataNONPBF8[,1:10], id.vars = c("id"))
dataNONPBF_roce_melted$year<-as.numeric(substr(dataNONPBF_roce_melted$variable,6,9))
colnames(dataNONPBF_roce_melted)[3]<-"roce"
dataNONPBF_roce_melted<-dataNONPBF_roce_melted[,-2]

dataNONPBF9<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF9<-dataNONPBF9[,-c(2:73)]
dataNONPBF_solvency_melted<-melt(dataNONPBF9[,1:10], id.vars = c("id"))
dataNONPBF_solvency_melted$year<-as.numeric(substr(dataNONPBF_solvency_melted$variable,10,13))
colnames(dataNONPBF_solvency_melted)[3]<-"solvency"
dataNONPBF_solvency_melted<-dataNONPBF_solvency_melted[,-2]

dataNONPBF10<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF10<-dataNONPBF10[,-c(2:82)]
dataNONPBF_employees_melted<-melt(dataNONPBF10[,1:10], id.vars = c("id"))
dataNONPBF_employees_melted$year<-as.numeric(substr(dataNONPBF_employees_melted$variable,11,14))
colnames(dataNONPBF_employees_melted)[3]<-"employees"
dataNONPBF_employees_melted<-dataNONPBF_employees_melted[,-2]

dataNONPBF11<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF11<-dataNONPBF11[,-c(2:91)]
dataNONPBF_costs_goodssold_melted<-melt(dataNONPBF11[,1:10], id.vars = c("id"))
dataNONPBF_costs_goodssold_melted$year<-as.numeric(substr(dataNONPBF_costs_goodssold_melted$variable,17,20))
colnames(dataNONPBF_costs_goodssold_melted)[3]<-"costs_goodssold"
dataNONPBF_costs_goodssold_melted<-dataNONPBF_costs_goodssold_melted[,-2]

dataNONPBF12<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF12<-dataNONPBF12[,-c(2:100)]
dataNONPBF_costs_employees_melted<-melt(dataNONPBF12[,1:10], id.vars = c("id"))
dataNONPBF_costs_employees_melted$year<-as.numeric(substr(dataNONPBF_costs_employees_melted$variable,17,20))
colnames(dataNONPBF_costs_employees_melted)[3]<-"costs_employees"
dataNONPBF_costs_employees_melted<-dataNONPBF_costs_employees_melted[,-2]

dataNONPBF13<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF13<-dataNONPBF13[,-c(2:109)]
dataNONPBF_costs_materials_melted<-melt(dataNONPBF13[,1:10], id.vars = c("id"))
dataNONPBF_costs_materials_melted$year<-as.numeric(substr(dataNONPBF_costs_materials_melted$variable,17,20))
colnames(dataNONPBF_costs_materials_melted)[3]<-"costs_materials"
dataNONPBF_costs_materials_melted<-dataNONPBF_costs_materials_melted[,-2]

dataNONPBF14<-read.csv2("orbis_NONPBF.csv",dec = ",", na.strings = "n.a.")
dataNONPBF14<-dataNONPBF14[,-c(2:119)]
dataNONPBF_age_melted<-melt(dataNONPBF14[,1:10], id.vars = c("id"))
dataNONPBF_age_melted$year<-as.numeric(substr(dataNONPBF_age_melted$variable,5,8))
colnames(dataNONPBF_age_melted)[3]<-"age"
dataNONPBF_age_melted<-dataNONPBF_age_melted[,-2]  

view(dataNONPBF_age_melted)

# now also for PBF separate datasets for each variable
dataPBF<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")  #read data
dataPBF_turnover_melted<-melt(dataPBF[,1:10], id.vars = c("id"))    #select only the first 10 columns
dataPBF_turnover_melted$year<-as.numeric(substr(dataPBF_turnover_melted$variable,10,13))  #let R know what the year is
colnames(dataPBF_turnover_melted)[3]<-"turnover"                    #make the column name turnover
dataPBF_turnover_melted<-dataPBF_turnover_melted[,-2]               #remove column with variable name

dataPBF2<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF2<-dataPBF2[,-c(2:10)]
dataPBF_netincome_melted<-melt(dataPBF2[,1:10], id.vars = c("id"))
dataPBF_netincome_melted$year<-as.numeric(substr(dataPBF_netincome_melted$variable,11,14))
colnames(dataPBF_netincome_melted)[3]<-"netincome"
dataPBF_netincome_melted<-dataPBF_netincome_melted[,-2]

dataPBF3<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF3<-dataPBF3[,-c(2:19)]
dataPBF_cashflow_melted<-melt(dataPBF3[,1:10], id.vars = c("id"))
dataPBF_cashflow_melted$year<-as.numeric(substr(dataPBF_cashflow_melted$variable,10,13))
colnames(dataPBF_cashflow_melted)[3]<-"cashflow"
dataPBF_cashflow_melted<-dataPBF_cashflow_melted[,-2]

dataPBF4<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF4<-dataPBF4[,-c(2:28)]
dataPBF_totalassets_melted<-melt(dataPBF4[,1:10], id.vars = c("id"))
dataPBF_totalassets_melted$year<-as.numeric(substr(dataPBF_totalassets_melted$variable,13,16))
colnames(dataPBF_totalassets_melted)[3]<-"totalassets"
dataPBF_totalassets_melted<-dataPBF_totalassets_melted[,-2]

dataPBF5<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF5<-dataPBF5[,-c(2:37)]
dataPBF_currentratio_melted<-melt(dataPBF5[,1:10], id.vars = c("id"))
dataPBF_currentratio_melted$year<-as.numeric(substr(dataPBF_currentratio_melted$variable,14,17))
colnames(dataPBF_currentratio_melted)[3]<-"currentratio"
dataPBF_currentratio_melted<-dataPBF_currentratio_melted[,-2]

dataPBF6<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF6<-dataPBF6[,-c(2:46)]
dataPBF_profitmargin_melted<-melt(dataPBF6[,1:10], id.vars = c("id"))
dataPBF_profitmargin_melted$year<-as.numeric(substr(dataPBF_profitmargin_melted$variable,14,17))
colnames(dataPBF_profitmargin_melted)[3]<-"profitmargin"
dataPBF_profitmargin_melted<-dataPBF_profitmargin_melted[,-2]

dataPBF7<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF7<-dataPBF7[,-c(2:55)]
dataPBF_roe_melted<-melt(dataPBF7[,1:10], id.vars = c("id"))
dataPBF_roe_melted$year<-as.numeric(substr(dataPBF_roe_melted$variable,5,8))
colnames(dataPBF_roe_melted)[3]<-"roe"
dataPBF_roe_melted<-dataPBF_roe_melted[,-2]

dataPBF8<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF8<-dataPBF8[,-c(2:64)]
dataPBF_roce_melted<-melt(dataPBF8[,1:10], id.vars = c("id"))
dataPBF_roce_melted$year<-as.numeric(substr(dataPBF_roce_melted$variable,6,9))
colnames(dataPBF_roce_melted)[3]<-"roce"
dataPBF_roce_melted<-dataPBF_roce_melted[,-2]

dataPBF9<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF9<-dataPBF9[,-c(2:73)]
dataPBF_solvency_melted<-melt(dataPBF9[,1:10], id.vars = c("id"))
dataPBF_solvency_melted$year<-as.numeric(substr(dataPBF_solvency_melted$variable,10,13))
colnames(dataPBF_solvency_melted)[3]<-"solvency"
dataPBF_solvency_melted<-dataPBF_solvency_melted[,-2]

dataPBF10<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF10<-dataPBF10[,-c(2:82)]
dataPBF_employees_melted<-melt(dataPBF10[,1:10], id.vars = c("id"))
dataPBF_employees_melted$year<-as.numeric(substr(dataPBF_employees_melted$variable,11,14))
colnames(dataPBF_employees_melted)[3]<-"employees"
dataPBF_employees_melted<-dataPBF_employees_melted[,-2]

dataPBF11<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF11<-dataPBF11[,-c(2:91)]
dataPBF_costs_goodssold_melted<-melt(dataPBF11[,1:10], id.vars = c("id"))
dataPBF_costs_goodssold_melted$year<-as.numeric(substr(dataPBF_costs_goodssold_melted$variable,17,20))
colnames(dataPBF_costs_goodssold_melted)[3]<-"costs_goodssold"
dataPBF_costs_goodssold_melted<-dataPBF_costs_goodssold_melted[,-2]

dataPBF12<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF12<-dataPBF12[,-c(2:100)]
dataPBF_costs_employees_melted<-melt(dataPBF12[,1:10], id.vars = c("id"))
dataPBF_costs_employees_melted$year<-as.numeric(substr(dataPBF_costs_employees_melted$variable,17,20))
colnames(dataPBF_costs_employees_melted)[3]<-"costs_employees"
dataPBF_costs_employees_melted<-dataPBF_costs_employees_melted[,-2]

dataPBF13<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF13<-dataPBF13[,-c(2:109)]
dataPBF_costs_materials_melted<-melt(dataPBF13[,1:10], id.vars = c("id"))
dataPBF_costs_materials_melted$year<-as.numeric(substr(dataPBF_costs_materials_melted$variable,17,20))
colnames(dataPBF_costs_materials_melted)[3]<-"costs_materials"
dataPBF_costs_materials_melted<-dataPBF_costs_materials_melted[,-2]

dataPBF14<-read.csv2("orbis_PBF.csv",dec = ",", na.strings = "n.a.")
dataPBF14<-dataPBF14[,-c(2:119)]
dataPBF_age_melted<-melt(dataPBF14[,1:10], id.vars = c("id"))
dataPBF_age_melted$year<-as.numeric(substr(dataPBF_age_melted$variable,5,8))
colnames(dataPBF_age_melted)[3]<-"age"
dataPBF_age_melted<-dataPBF_age_melted[,-2]

view(dataPBF_age_melted)

# merge NONPBF datasets together
mergedNONPBF1<-merge(dataNONPBF_turnover_melted,dataNONPBF_netincome_melted, all = TRUE)
mergedNONPBF2<-merge(dataNONPBF_cashflow_melted,dataNONPBF_totalassets_melted, all = TRUE)
mergedNONPBF3<-merge(dataNONPBF_currentratio_melted,dataNONPBF_profitmargin_melted, all = TRUE)
mergedNONPBF4<-merge(dataNONPBF_roe_melted,dataNONPBF_roce_melted, all = TRUE)
mergedNONPBF5<-merge(dataNONPBF_solvency_melted,dataNONPBF_employees_melted, all = TRUE)
mergedNONPBF6<-merge(dataNONPBF_costs_goodssold_melted,dataNONPBF_costs_employees_melted, all = TRUE)
mergedNONPBF7<-merge(dataNONPBF_costs_materials_melted, dataNONPBF_age_melted, all = TRUE)

# make sort of a pyramid of mergings, since can only merge 2 together and want to merge everything
mergedNONPBF8<-merge(mergedNONPBF1,mergedNONPBF2, all = TRUE)
mergedNONPBF9<-merge(mergedNONPBF3,mergedNONPBF4, all = TRUE)
mergedNONPBF10<-merge(mergedNONPBF5,mergedNONPBF6, all = TRUE)

mergedNONPBF11<-merge(mergedNONPBF8,mergedNONPBF9, all = TRUE)
mergedNONPBF12<-merge(mergedNONPBF7,mergedNONPBF10, all = TRUE)

mergedNONPBF_all<-merge(mergedNONPBF11,mergedNONPBF12, all = TRUE)
View(mergedNONPBF_all)


# now merge PBF datasets
mergedPBF1<-merge(dataPBF_turnover_melted,dataPBF_netincome_melted, all = TRUE)
mergedPBF2<-merge(dataPBF_cashflow_melted,dataPBF_totalassets_melted, all = TRUE)
mergedPBF3<-merge(dataPBF_currentratio_melted,dataPBF_profitmargin_melted, all = TRUE)
mergedPBF4<-merge(dataPBF_roe_melted,dataPBF_roce_melted, all = TRUE)
mergedPBF5<-merge(dataPBF_solvency_melted,dataPBF_employees_melted, all = TRUE)
mergedPBF6<-merge(dataPBF_costs_goodssold_melted,dataPBF_costs_employees_melted, all = TRUE)
mergedPBF7<-merge(dataPBF_costs_materials_melted,dataPBF_age_melted, all = TRUE)

mergedPBF8<-merge(mergedPBF1,mergedPBF2, all = TRUE)
mergedPBF9<-merge(mergedPBF3,mergedPBF4, all = TRUE)
mergedPBF10<-merge(mergedPBF5,mergedPBF6, all = TRUE)

mergedPBF11<-merge(mergedPBF8,mergedPBF9, all = TRUE)
mergedPBF12<-merge(mergedPBF7,mergedPBF10, all = TRUE)

mergedPBF_all<-merge(mergedPBF11,mergedPBF12, all = TRUE)
View(mergedPBF_all)

# make sure all variables are numeric
mergedPBF_all$turnover<-as.numeric(mergedPBF_all$turnover)
mergedPBF_all$netincome<-as.numeric(mergedPBF_all$netincome)
mergedPBF_all$cashflow<-as.numeric(mergedPBF_all$cashflow)
mergedPBF_all$totalassets<-as.numeric(mergedPBF_all$totalassets)
mergedPBF_all$employees<-as.numeric(mergedPBF_all$employees)
mergedPBF_all$costs_goodssold<-as.numeric(mergedPBF_all$costs_goodssold)
mergedPBF_all$costs_employees<-as.numeric(mergedPBF_all$costs_employees)
mergedPBF_all$costs_materials<-as.numeric(mergedPBF_all$costs_materials)
str(mergedPBF_all)

mergedNONPBF_all$turnover<-as.numeric(mergedNONPBF_all$turnover)
mergedNONPBF_all$netincome<-as.numeric(mergedNONPBF_all$netincome)
mergedNONPBF_all$cashflow<-as.numeric(mergedNONPBF_all$cashflow)
mergedNONPBF_all$totalassets<-as.numeric(mergedNONPBF_all$totalassets)
mergedNONPBF_all$employees<-as.numeric(mergedNONPBF_all$employees)
mergedNONPBF_all$costs_goodssold<-as.numeric(mergedNONPBF_all$costs_goodssold)
mergedNONPBF_all$costs_employees<-as.numeric(mergedNONPBF_all$costs_employees)
mergedNONPBF_all$costs_materials<-as.numeric(mergedNONPBF_all$costs_materials)
str(mergedNONPBF_all)


# Steps for regression --------
# 1. create all variable needed
# 2. create regression models 
# 2.1 google how to include interaction terms in R regession models (do with :)
# 2.2 include the interaction terms according to your model decscription
# 2.3 run the model and store the results (use the feols() function of the fixest  package)
# (2.3 add-on cluster error terms together with Tobias)
# 2.4 get the marginal effects using the margins() (i think) (google marginal effect R)
# do everything first for the mean effect, then svar

# make complete datasets --------
# use only the variables which will be used for the regression 
PBF_complete <- mergedPBF_all[complete.cases(mergedPBF_all[,c("id", "year","profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]),c("id","year", "profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]

NONPBF_complete <- mergedNONPBF_all[complete.cases(mergedNONPBF_all[,c("id","year","profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]),c("id","year","profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]

View(PBF_complete)
View(NONPBF_complete)

# linear regression ---------
# linear regression using feols function of fixest package
# use the complete datasets in the model
model_PBF_mean = feols(profitmargin ~ costs_employees + costs_materials + currentratio + solvency + costs_employees:costs_materials |id + year, cluster = c("year", "id"),data = PBF_complete)
summary(model_PBF_mean)

model_NONPBF_mean = feols(profitmargin ~ costs_employees + costs_materials + currentratio + solvency +  costs_employees:costs_materials |id + year, cluster = c("year", "id"), data = NONPBF_complete)
summary(model_NONPBF_mean)     


# marginal effects --------
# this gives information about the average marginal effects for each input variable
marginaleffects(model_PBF_mean, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_PBF_mean))

marginaleffects(model_NONPBF_mean, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_NONPBF_mean))


# create models for semi-variance --------
# use the complete df and add a column of residuals
# then add column of squared residuals
# finally make svar model - use squared residuals as dependent variable (instead of profit margin, which is y in mean model)
PBF_complete$residual <- resid(model_PBF_mean)
View(PBF_complete)

NONPBF_complete$residual <- resid(model_NONPBF_mean)
View(NONPBF_complete)

# squared residuals
PBF_complete$residualsquared <- resid(model_PBF_mean)^2
View(PBF_complete)

NONPBF_complete$residualsquared <- resid(model_NONPBF_mean)^2
View(NONPBF_complete)

# model semi variance
model_PBF_SV = feols(residualsquared ~ costs_employees + costs_materials + currentratio + solvency + costs_employees:costs_materials | id + year, cluster = c("year", "id"), data = PBF_complete[which(PBF_complete$residual<0),])
summary(model_PBF_SV)

model_NONPBF_SV = feols(residualsquared ~ costs_employees + costs_materials + currentratio + solvency + costs_employees:costs_materials | id + year, cluster = c("year", "id"), data = NONPBF_complete[which(NONPBF_complete$residual<0),])
summary(model_NONPBF_SV)

# marginal effects of the svariance --> interpret these for results
marginaleffects(model_PBF_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_PBF_SV))

marginaleffects(model_NONPBF_SV, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_NONPBF_SV))

# robustness check ----------
# change dependent variable from profit margin to return on equity to check whether this influences the results
PBF_complete_ROE <- mergedPBF_all[complete.cases(mergedPBF_all[,c("id", "year","profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]),c("id","year", "profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]
NONPBF_complete_ROE <- mergedNONPBF_all[complete.cases(mergedNONPBF_all[,c("id","year","profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]),c("id","year","profitmargin","costs_materials","costs_employees","currentratio","solvency","roe")]

model_PBF_mean_ROE = feols(roe ~ costs_employees + costs_materials + currentratio + solvency + costs_employees:costs_materials |id + year, cluster = c("year", "id"),data = PBF_complete_ROE)
summary(model_PBF_mean_ROE)

model_NONPBF_mean_ROE = feols(roe ~ costs_employees + costs_materials + currentratio + solvency + costs_employees:costs_materials |id + year, cluster = c("year", "id"), data = NONPBF_complete_ROE)
summary(model_NONPBF_mean_ROE)


marginaleffects(model_PBF_mean_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_PBF_mean_ROE))
marginaleffects(model_NONPBF_mean_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_NONPBF_mean_ROE))

# add residuals for ROE
PBF_complete_ROE$residualroe <- resid(model_PBF_mean_ROE)
View(PBF_complete_ROE) 

NONPBF_complete_ROE$residualroe <- resid(model_NONPBF_mean_ROE)
View(NONPBF_complete_ROE)

PBF_complete_ROE$residualsquaredroe <- resid(model_PBF_mean_ROE)^2
NONPBF_complete_ROE$residualsquaredroe <- resid(model_NONPBF_mean_ROE)^2

model_PBF_SV_ROE = feols(residualsquaredroe ~ costs_employees + costs_goodssold + costs_materials + currentratio + solvency + costs_employees:costs_goodssold + costs_employees:costs_materials + costs_goodssold:costs_materials | id + year, cluster = c("year", "id"), data = PBF_complete_ROE[which(PBF_complete_ROE$residualroe<0),])
summary(model_PBF_SV_ROE)

model_NONPBF_SV_ROE = feols(residualsquaredroe ~ costs_employees + costs_goodssold + costs_materials + currentratio + solvency + costs_employees:costs_goodssold + costs_employees:costs_materials + costs_goodssold:costs_materials | id + year, cluster = c("year", "id"), data = NONPBF_complete_ROE[which(NONPBF_complete_ROE$residualroe<0),])
summary(model_NONPBF_SV_ROE)

marginaleffects(model_PBF_SV_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_PBF_SV_ROE))

marginaleffects(model_NONPBF_SV_ROE, newdata=datagrid(), vcov=T)
summary(marginaleffects(model_NONPBF_SV_ROE))


# summaries to give quick overview about means ---------
# details per variable about the mean median max etc.

summary(PBF_complete$profitmargin)
summary(NONPBF_complete$profitmargin)

summary(PBF_complete$roe)
summary(NONPBF_complete$roe)

summary(PBF_complete$costs_materials)
summary(NONPBF_complete$costs_materials) 

summary(PBF_complete$costs_employees)
summary(NONPBF_complete$costs_employees)

summary(PBF_complete$costs_goodssold)
summary(NONPBF_complete$costs_goodssold) 

summary(PBF_complete$solvency)    
summary(NONPBF_complete$solvency) 

summary(PBF_complete$currentratio)
summary(NONPBF_complete$currentratio) 

