#install.packages("ltm") Usado para obener alpha de cronbach
#install.packages("lavaan") herramienta para CFA
#install.packages("readxl") paquuete para utilizar excel
#install.packages("mice")

library("ltm")
library("readxl")
library("lavaan")
library("mice")
require(ltm)

# Análisis de adaptación al español de Error Orientation Questionnaire
# Rybowiak et al. (2009)

# Import EO data
eodata<- read_excel("./XLSX/eodata.xlsx",sheet="EO")

# Understand the missing value pattern
md.pattern(eodata)
# Pasando todo a factor para el analisis mediante el metodo polr
eodata_f= apply(eodata, 2, function(x) as.factor(as.numeric(x)))
mice_imputes = mice(eodata_f, m=5, method='polr',maxit = 40)
# Codigo para observar los imputs elegidos para cada observacion en una variable dada (E15 en este caso)
mice_imputes$imp$E15
# Permite ver el metodo utilizado en cada variable
mice_imputes$method
# Como los imputed data  are equals in each observation, we dont need to decide which imputed data set we will use
# si fuera otro el caso, se haria test de bondad de ajustes para poder decidir el mejor escenario
eodata_c = complete(mice_imputes,1)

# Se crean los data frame correspondientes a cada factor de EO
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
cronbach.alpha(eo_anticipation, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_cover_error, standardized = FALSE, CI = FALSE) 
cronbach.alpha(eo_communication, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_thinking_abot, standardized = FALSE, CI = FALSE) 

# Análisis de adaptación al español de Short Grit Scale
# Duckworth & Quinn (2009)

# Import SGS data
sgsdata<- read_excel("./XLSX/sgs.xlsx",sheet="SGS")
sgsdata<- data.frame(sgsdata$S1, sgsdata$S2, sgsdata$S3,
                    sgsdata$S4, sgsdata$S5, sgsdata$S6,
                    sgsdata$S7, sgsdata$S8, sgsdata$S9,
                    sgsdata$S10,sgsdata$S11,sgsdata$S12,
                    sgsdata$S13)


# Understand the missing value pattern
md.pattern(sgsdata)

# Pasando todo a factor para el analisis mediante el metodo polr
sgsdata_f= apply(sgsdata, 2, function(x) as.factor(as.numeric(x)))
mice_imputes = mice(sgsdata_f, m=5, method='polr', maxit = 40)

# Codigo para observar los imputs elegidos para cada observacion en una variable dada (S13 en este caso)
mice_imputes$imp$sgsdata.S13

# Permite ver el metodo utilizado en cada variable
mice_imputes$method

# Como los imputed data are equals in each observation, we dont need to decide which imputed data set we will use
# si fuera otro el caso, se haria test de bondad de ajustes para poder decidir el mejor escenario
sgsdata_c = complete(mice_imputes, 1)

# Se crean los data frame correspondientes a cada factor de SGS
#sgs_consistency <- data.frame(sgsdata$S5, sgsdata$S2, sgsdata$S1, sgsdata$S4, sgsdata$S7, sgsdata$S13)
#sgs_consistency <- data.frame(sgsdata$S1, sgsdata$S2, sgsdata$S5, sgsdata$S6)
sgs_consistency <- sgsdata_c[, c("sgsdata.S5", "sgsdata.S2", "sgsdata.S7", "sgsdata.S13")]
  
#sgs_effort <- data.frame(sgsdata$S12, sgsdata$S11, sgsdata$S3, sgsdata$S8, sgsdata$S6, sgsdata$S9)
#sgs_effort <- data.frame(sgsdata$S9, sgsdata$S10, sgsdata$S11, sgsdata$S12)
sgs_effort <- sgsdata_c[, c("sgsdata.S3", "sgsdata.S8", "sgsdata.S6", "sgsdata.S9")]

# Compute Cronbach's alpha for each construct in SGS
cronbach.alpha(sgs_consistency, standardized = FALSE, CI = FALSE)
cronbach.alpha(sgs_effort, standardized = FALSE, CI = FALSE)

# Análisis de adaptación al español de Revised Implicit Theories of Intelligence Scale
# De Castella & Byrne (2015)

# Import RITI data
ritidata<- read_excel("./XLSX/riti.xlsx",sheet="RITI")

# Understand the missing value pattern
md.pattern(ritidata)

# Pasando todo a factor para el analisis mediante el metodo polr
ritidata_f= apply(ritidata, 2, function(x) as.factor(as.numeric(x)))
mice_imputes = mice(ritidata_f, m=5, method='polr',maxit = 40)

# Codigo para observar los imputs elegidos para cada observacion en una variable dada (R06 en este caso)
mice_imputes$imp$R06

# Permite ver el metodo utilizado en cada variable
mice_imputes$method

# Como los imputed data are equals in each observation, we dont need to decide which imputed data set we will use
# si fuera otro el caso, se haria test de bondad de ajustes para poder decidir el mejor escenario
ritidata_c = complete(mice_imputes,1)

# Se crean los data frame correspondientes a cada factor de SGS
riti_EBG <- data.frame(ritidata$R04, ritidata$R10, ritidata$R01, ritidata$R16)
riti_IBG <- data.frame(ritidata$R02, ritidata$R07, ritidata$R13, ritidata$R09)
riti_EBS <- data.frame(ritidata$R12, ritidata$R05, ritidata$R11, ritidata$R06)
riti_IBS <- data.frame(ritidata$R17, ritidata$R08, ritidata$R14, ritidata$R03)

# Compute Cronbach's alpha for each construct in RITI
cronbach.alpha(riti_EBG, standardized = FALSE, CI = FALSE)
cronbach.alpha(riti_IBG, standardized = FALSE, CI = FALSE)
cronbach.alpha(riti_EBS, standardized = FALSE, CI = FALSE)
cronbach.alpha(riti_IBS, standardized = FALSE, CI = FALSE)


