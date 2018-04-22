#install.packages("ltm") Usado para obener alpha de cronbach
#install.packages("lavaan") herramienta para CFA
#install.packages("readxl") paquuete para utilizar excel


library("ltm")
library("readxl")
library("lavaan")
library("mice")
require(ltm)

getwd()
#setwd()

#Import EO data
eodata<- read_excel("C:/Users/Admin/Desktop/R Memoria/memoria-uandes/XLSX/eodata.xlsx",sheet="EO")

#understand the missing value pattern
md.pattern(eodata)
#Pasando todo a factor para el analisis mediante el metodo polr
eodata_f= apply(eodata, 2, function(x) as.factor(as.numeric(x)))
mice_imputes = mice(eodata_f, m=5, method='polr',maxit = 40)
#codigo para observar los imputs elegidos para cada observacion en una variable dada (E15 en este caso)
mice_imputes$imp$E15
#Permite ver el metodo utilizado en cada variable
mice_imputes$method
#como los imputed data  are equals in each observation, we dont need to decide which imputed data set we will use
#si fuera otro el caso, se haria test de bondad de ajustes para poder decidir el mejor escenario
eodata_c = complete(mice_imputes,1)






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
#Missing values
cronbach.alpha(eo_anticipation, standardized = FALSE, CI = FALSE)
#Missing Values
cronbach.alpha(eo_cover_error, standardized = FALSE, CI = FALSE) 
cronbach.alpha(eo_communication, standardized = FALSE, CI = FALSE)
#Missing Values
cronbach.alpha(eo_thinking_abot, standardized = FALSE, CI = FALSE) 


#Import SGS data
sgsdata<- read_excel("C:/Users/Admin/Desktop/R Memoria/memoria-uandes/XLSX/sgs.xlsx",sheet="SGS")
sgsdata<- data.frame(sgsdata$S1,sgsdata$S2,sgsdata$S3,sgsdata$S4,sgsdata$S5,sgsdata$S6,sgsdata$S7,sgsdata$S8,sgsdata$S9,sgsdata$S10,sgsdata$S11,sgsdata$S12,sgsdata$S13)


#understand the missing value pattern
md.pattern(sgsdata)
#Pasando todo a factor para el analisis mediante el metodo polr
sgsdata_f= apply(sgsdata, 2, function(x) as.factor(as.numeric(x)))
mice_imputes = mice(sgsdata_f, m=5, method='polr',maxit = 40)
#codigo para observar los imputs elegidos para cada observacion en una variable dada (E15 en este caso)
mice_imputes$imp$sgsdata.S13
#Permite ver el metodo utilizado en cada variable
mice_imputes$method
#como los imputed data  are equals in each observation, we dont need to decide which imputed data set we will use
#si fuera otro el caso, se haria test de bondad de ajustes para poder decidir el mejor escenario
sgsdata_c = complete(mice_imputes,1)






#Se crean los data frame correspondientes a cada factor de SGS
sgs_consistency <- data.frame(sgsdata$S5, sgsdata$S2, sgsdata$S1, sgsdata$S4, sgsdata$S7, sgsdata$S13)
sgs_effort <- data.frame(sgsdata$S12, sgsdata$S11, sgsdata$S3, sgsdata$S8, sgsdata$S6, sgsdata$S9)
#sgs_consistency <- data.frame(sgsdata$sgsdata.S5, sgsdata$sgsdata.S2, sgsdata$sgsdata.S1, sgsdata$sgsdata.S4, sgsdata$sgsdata.S7, sgsdata$sgsdata.S13)
#sgs_effort <- data.frame(sgsdata$sgsdata.S12, sgsdata$sgsdata.S11, sgsdata$sgsdata.S3, sgsdata$sgsdata.S8, sgsdata$sgsdata.S6, sgsdata$sgsdata.S9)

# Compute Cronbach's alpha for each construct in SGS
cronbach.alpha(sgs_consistency, standardized = FALSE, CI = FALSE) #Missing values
cronbach.alpha(sgs_effort, standardized = FALSE, CI = FALSE) #Missing values

