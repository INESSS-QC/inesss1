library(inesss)
DT <- SQL_comorbidity(
  conn = SQL_connexion("ms045"),
  uid = NULL,
  pwd = NULL,
  dt = readRDS("V:/_MedPersAg/Charlson_test.rds"),
  ID = "ID",
  DATE_INDEX = "DAT_Index",
  method = c("Charlson", "Elixhauser"),
  CIM = c("CIM9", "CIM10"),
  scores = "CIM10",
  lookup = 2,
  n1 = 30,
  n2 = 730,
  dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MED-ECHO", V_SEJ_SERV_HOSP_CM = "MED-ECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU", I_SMOD_SERV_MD_CM = "SMOD"),
  confirm_sourc = list("MED-ECHO" = 1, "BDCU" = 2, "SMOD" = 2),
  obstetric_exclu = TRUE,
  verbose = TRUE
)
