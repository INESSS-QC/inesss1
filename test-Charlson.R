conn <- SQL_connexion("ms045")
Cohort <- readRDS("V:/_MedPersAg/Charlson_test.rds")
t1 <- Sys.time()
dt <- SQL_comorbidity(
  conn = conn,
  cohort = sunique(Cohort$ID),
  debut = as.character(min(Cohort$DAT_Index)-730),
  fin = as.character(max(Cohort$DAT_Index)),
  diagn_codes = Comorbidity_SQL_regex,
  dt_source = list(V_DIAGN_SEJ_HOSP_CM = 1L, V_SEJ_SERV_HOSP_CM = 1L,
                   V_EPISO_SOIN_DURG_CM = 2L, I_SMOD_SERV_MD_CM = 2L)
)
t2 <- Sys.time()
