library(inesss)
library(data.table)
conn <- SQL_connexion()  # connexion à Teradata

### Diagnostics à extraire
SQL_Dx <- list(
  AngPoitrine = list(CIM9 = "413%",
                     CIM10 = "I20%"),
  Infarctus = list(CIM9 = paste0(410:414, "%"),
                   CIM10 = paste0("I", 21:25, "%"))
)

### Extraction des Dx
dt_dx <- SQL_diagn(
  conn, cohort = NULL, debut = "2006-01-01", fin = "2021-04-31",  # voir dernière mise à jour des BD pour la fin
  Dx_table = SQL_Dx, CIM = c("CIM9", "CIM10"),
  dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU", I_SMOD_SERV_MD_CM = "SMOD"),
  date_dx_var = "depar", exclu_diagn = NULL, verbose = TRUE
)
dt_dx <- unique(dt_dx, by = c("ID", "DATE_DX"))  # valeurs unique par ID+DATE -> Dx et source pas importante

### Confirmation des Dx
dt_conf <- confirm_2Dx(
  dt = dt_dx, ID = "ID", DATE = "DATE_DX", DIAGN = NULL,
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730, reverse = FALSE
)

### Dernière date de Dx pour chaque ID
Dx_last <- dt_dx[
  dt_dx[, .I[.N], .(ID)]$V1,  # no de lignes du dernier Dx
  .(ID, LAST_DATE = DATE_DX)  # sélection des colonnes
]

### Conserver 1ère date confirmée
dt_conf_1st <- dt_conf[dt_conf[, .I[1], .(ID)]$V1]

### Ajouter la dernière date de Dx à chaque ID
coro_analyse <- Dx_last[dt_conf_1st, on = .(ID)][, .(ID, DATE_REP, LAST_DATE, DATE_CONF1)]
