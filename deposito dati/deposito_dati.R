library(rio)
setwd("C:/Users/gasca/OneDrive - Università degli Studi di Milano-Bicocca/Dottorato/Papers and Chapters/Scaduto Negri 2024/deposito dati")

data = FFPT_def_pre_tidy
data$treatment1V = NULL
data$treatment2M = NULL
data$treatment3T=NULL
data$treatment4E=NULL
data$controlgroup=NULL
data$ipaddr=NULL

data$token=NULL
data[85:length(names(data))]=NULL

export(data, "dati_da_depositare.RDS")
