library(inesss)
library(data.table)
library(odbc)

conn <- SQL_connexion()  # connexion à Teradata
debut <- "2006-01-01"  # debut de la periode d'etude
fin <- "2010-12-31"  # fin de la periode d'etude

# CODES DIAGNOSTICS -------------------------------------------------------

# Important de mettre les '%'
SQL_Dx <- list(
  AngPoitrine = list(CIM9 = "413%", CIM10 = "I20%"),
  Infarctus = list(CIM9 = paste0(410:414, "%"), CIM10 = paste0("I", 21:25, "%"))
)


# ETAPE 1 -----------------------------------------------------------------

Dx_etape1 <- SQL_diagn(
  conn,
  cohort = NULL,  # conserver tous les ID
  debut = debut, fin = fin,  # du 2006-01-01 à aujourd'hui
  Dx_table = SQL_Dx,  # contient les codes de diagnostics
  CIM = c("CIM9", "CIM10"),
  dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM"),  # VUES à utiliser
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO"),
  date_dx_var = "admis",  # utiliser la date d'admission
  typ_diagn = c("P", "S", "D"),  # type de diagnostic à utiliser
  exclu_diagn = NULL,  # aucun Dx à exclure
  verbose = TRUE  # afficher où en est le processus
)
Dx_etape1_1er <- Dx_etape1[, .SD[1], .(ID)]  # Conserver 1re obs de chaque ID - peu importe le Dx
Dx_etape1_1er <- Dx_etape1_1er[, .(ID, DI_Hospit = DATE_DX)]  # sélectionner + renommer les colonnes


# ETAPE 2 -----------------------------------------------------------------

# Répéter l'étape 1, mais avec des paramètres différents
#   - différentes vues
#   - ajouter l'admission aux types de diagnostics
Dx_etape2 <- SQL_diagn(
  conn,
  cohort = NULL,
  debut = debut, fin = fin,
  Dx_table = SQL_Dx,
  CIM = c("CIM9", "CIM10"),
  dt_source = c("V_DIAGN_SEJ_HOSP_CM",
                "V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM",
                "I_SMOD_SERV_MD_CM"),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO",
                 V_SEJ_SERV_HOSP_CM = "MEDECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU",
                 I_SMOD_SERV_MD_CM = "SMOD"),
  date_dx_var = "admis",
  typ_diagn = c("A", "P", "S", "D"),
  exclu_diagn = NULL,
  verbose = TRUE
)
Dx_etape2_uniq <- unique(Dx_etape2[, .(ID, DATE_DX)])  # sélection des colonnes + combinaison unique ID+DATE
setkey(Dx_etape2_uniq, ID, DATE_DX)

### Confirmation des Dx
Dx_conf <- confirm_2Dx(
  dt = Dx_etape2_uniq, ID = "ID", DATE = "DATE_DX", DIAGN = NULL,
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730,
  keep_first = TRUE,
  reverse = FALSE
)
setnames(Dx_conf, c("DATE_REP", "DATE_CONF1"), c("DI_Acte", "DC_Acte"))

### Dernière date de Dx pour chaque ID
Dx_Tot <- rbind(Dx_etape1, Dx_etape2)
setkey(Dx_Tot, ID, DATE_DX)
Dx_Recent <- Dx_Tot[, .SD[.N], .(ID)]
Dx_Recent <- Dx_Recent[, .(ID, D_Recent = DATE_DX)]  # sélection + renommer les colonnes


# ETAPE FINALE ------------------------------------------------------------

### Combiner les datasets
# Creer un data contenant seulement les ID a l'etude
dt_final <- unique(Dx_Tot[, .(ID)])
# Ajouter les variables au dt final
dt_final <- Dx_Recent[dt_final, on = .(ID)]  # ajout D_Recent
dt_final <- Dx_etape1_1er[dt_final, on = .(ID)]  # ajout DI_Hospit
dt_final <- Dx_conf[, .(ID, DI_Acte)][dt_final, on = .(ID)]  # ajout DI_Acte
# Calculer DI_Finale - Date la plus ancienne entre DI_Acte et DI_Hospit
dt_final[, DI_Finale := DI_Acte]
dt_final[is.na(DI_Acte), DI_Finale := DI_Hospit]
dt_final[DI_Hospit < DI_Acte, DI_Finale := DI_Hospit]
