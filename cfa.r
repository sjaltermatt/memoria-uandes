require(ltm)

eo_error_competence <- data.frame(eodata$E2, eodata$E7, eodata$E31, eodata$E33)
eo_learning_from_errors <- data.frame(eodata$E21, eodata$E28, eodata$E22, eodata$E3)

cronbach.alpha(eo_error_competence, standardized = FALSE, CI = FALSE)
cronbach.alpha(eo_learning_from_errors, standardized = FALSE, CI = FALSE)
