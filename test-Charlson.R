library(data.table)
DT <- readRDS("C:/Users/ms045/Desktop/Github/INESSS-QC_inesss1/data-fake/diagnostiques.rds")

dt = copy(DT)
ID = "ID"
DIAGN = "DIAGN"
DATE_DX = "DATE_DX"
SOURCE = "SOURCE"
indic = c("charlson", "elixhauser")
n1 = 30
n2 = 730
confirm_sourc = list("MED-ECHO" = 1, "BDCU" = 2, "SMOD" = 2)
