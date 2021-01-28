library(data.table)
conn <- SQL_connexion("ms045")

Cohort <- readRDS("V:/_MedPersAg/Charlson_test.rds")
setDT(Cohort)
Cohort <- unique(Cohort, by = "ID")
setkey(Cohort)

DT <- SQL_diagn(
  conn = conn,
  cohort = sunique(Cohort$ID),
  debut = as.character(min(Cohort$DAT_Index)-730),
  fin = as.character(max(Cohort$DAT_Index)),
  diagn_codes = Comorbidity_SQL_regex[1:5]
)

dt <- Cohort[ID %in% DT$ID, .(ID, DATE_INDEX = DAT_Index)]  # cohorte présente dans DT
dt <- DT[dt, on = .(ID)]  # ajouter les dates des ID
