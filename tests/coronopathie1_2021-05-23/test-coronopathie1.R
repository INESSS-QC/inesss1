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
  conn,
  cohort = NULL,  # mettre vecteur contenant les ID de l'étude
  debut = "2006-01-01",
  fin = "2021-04-30",  # ajuster la date de fin, ex: dernière mise à jour de la BD
  Dx_table = SQL_Dx,  # nom de la liste créée précédemment contenant les codes de Dx à extraire
  CIM = c("CIM9", "CIM10"),
  dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU", I_SMOD_SERV_MD_CM = "SMOD"),
  date_dx_var = "admis",  # sélectionner la date d'admission du Dx
  exclu_diagn = NULL, verbose = TRUE
)

### Étape 1 - 1re date de MEDECHO
dt_hospit <- dt_dx[
  dt_dx[SOURCE == "MEDECHO", .I[1], .(ID)]$V1,  # numeros de ligne à conserver
  .(ID, DI_Hospit = DATE_DX)  # sélection des colonnes
]

### Étape 2 - Confirmation des Dx en tenant compte des diagnostics
###     -> AngPoitrine confirme seulement AngPoitrine, idem pour Infarctus
dt_conf <- confirm_2Dx(
  dt = dt_dx, ID = "ID", DATE = "DATE_DX", DIAGN = "DIAGN",
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730, reverse = FALSE
)
setkey(dt_conf, ID, DATE_REP)
# Conserver la 1re date confirmée
dt_conf_1st <- dt_conf[
  dt_conf[, .I[1], .(ID)]$V1,  # numeros de lignes des 1re obs
  .(ID, DI_Acte = DATE_REP, DC_Acte = DATE_CONF1)  # sélection des colonnes
]

### Dernière date de Dx pour chaque ID
dt_last <- dt_dx[
  dt_dx[, .I[.N], .(ID)]$V1,  # no de lignes du dernier Dx
  .(ID, DI_Recent = DATE_DX)  # sélection des colonnes
]


### Combiner les datasets
dt_final <- unique(dt_dx[, .(ID)])  # liste des ID ayant au moins 1 Dx, sinon prendre la liste complète des ID à l'étude -> cohorte
dt_final <- dt_last[dt_final, on = .(ID)]  # ajout DI_Recent
dt_final <- dt_hospit[dt_final, on = .(ID)]  # ajout DI_Hospit
dt_final <- dt_conf_1st[, .(ID, DI_Acte)][dt_final, on = .(ID)]  # ajout DI_Acte
# Calculer DI_Finale - Date la plus ancienne entre DI_Acte et DI_Hospit
dt_final[, DI_Finale := DI_Acte]
dt_final[is.na(DI_Acte), DI_Finale := DI_Hospit]
dt_final[DI_Hospit < DI_Acte, DI_Finale := DI_Hospit]
