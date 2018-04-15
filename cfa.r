#install.packages("ltm") Usado para obener alpha de cronbach
#install.packages("lavaan") herramienta para CFA
#install.packages("readxl") paquuete para utilizar excel
library("ltm")
library("readxl")
library("lavaan")
require(ltm)

getwd()
#setwd()

#Import EO data
eodata<- read_excel("C:/Users/Admin/Desktop/R Memoria/memoria-uandes/XLSX/eodata.xlsx",sheet="EO")

#Se crean los data frame correspondientes a cada factor de EO
eo_error_competence <- data.frame(eodata$E2, eodata$E7, eodata$E31, eodata$E33)
eo_learning_from_errors <- data.frame(eodata$E21, eodata$E28, eodata$E22, eodata$E3)
eo_risk_taking <- data.frame(eodata$E34, eodata$E13, eodata$E6, eodata$E27)
eo_strain <- data.frame(eodata$E17, eodata$E18, eodata$E38, eodata$E10, eodata$E5)
eo_anticipation <- data.frame(eodata$E30, eodata$E37, eodata$E19, eodata$E25, eodata$E9)
eo_cover_error <- data.frame(eodata$E15, eodata$E8, eodata$E24, eodata$E26, eodata$E11, eodata$E14)
eo_communication <- data.frame(eodata$E36, eodata$E32, eodata$E29, eodata$E4)
eo_thinking_abot <- data.frame(eodata$E12, eodata$E16, eodata$E39, eodata$E1, eodata$E23)

# Compute Cronbach's alpha for each construct in the EO questionnaire
cronbach.alpha(eo_error_competence, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_learning_from_errors, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_risk_taking, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_strain, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_anticipation, standardized = FALSE, CI = FALSE) #Missing values
cronbach.alpha(eo_cover_error, standardized = FALSE, CI = FALSE) #Missing Values
cronbach.alpha(eo_communication, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_thinking_abot, standardized = FALSE, CI = FALSE) #Missing Values


#Import SGS data
sgsdata<- read_excel("C:/Users/Admin/Desktop/R Memoria/memoria-uandes/XLSX/sgs.xlsx",sheet="SGS")

#Se crean los data frame correspondientes a cada factor de SGS
sgs_consistency <- data.frame(sgsdata$S5, sgsdata$S2, sgsdata$S1, sgsdata$S4, sgsdata$S7, sgsdata$S13)
sgs_effort <- data.frame(sgsdata$S12, sgsdata$S11, sgsdata$S3, sgsdata$S8, sgsdata$S6, sgsdata$S9)

# Compute Cronbach's alpha for each construct in SGS
cronbach.alpha(sgs_consistency, standardized = FALSE, CI = FALSE) #Missing values
cronbach.alpha(sgs_effort, standardized = FALSE, CI = FALSE) #Missing values

