library(inesss)
library(data.table)
library(odbc)

conn <- SQL_connexion()  # connexion à Teradata
debut <- "2006-01-01"  # debut de la periode d'etude
fin <- Sys.Date()  # fin de la periode d'etude
IDs <- NULL  # vecteur de cohorte d'etude

# CODES DIAGNOSTICS -------------------------------------------------------

SQL_Dx <- list(
  AngPoitrine = list(CIM9 = "413%",
                     CIM10 = "I20%"),
  Infarctus = list(CIM9 = paste0(410:414, "%"),
                   CIM10 = paste0("I", 21:25, "%"))
)


# ETAPE 1 -----------------------------------------------------------------

# V_DIAGN_SEJ_HOSP_CM - Extraction des Dx
Dx_etape1.1 <- as.data.table(dbGetQuery(conn, statement = paste0(
  "select SHOP_NO_INDIV_BEN_BANLS as ID,\n",
  "       SHOP_DAT_ADMIS_SEJ_HOSP as DATE_DX\n",
  "from RES_SSS.V_DIAGN_SEJ_HOSP_CM\n",
  "where SHOP_COD_DIAGN_MDCAL_CLINQ like any (",paste(paste0("'",unlist(SQL_Dx),"'"), collapse = ", "),")\n",
  "    and SHOP_DAT_ADMIS_SEJ_HOSP between '",debut,"' and '",fin,"'\n",
  "    and SHOP_TYP_DIAGN_SEJ_HOSP in ('P', 'S', 'D');"
)))
# V_SEJ_SERV_HOSP_CM - Extraction des Dx
Dx_etape1.2 <- as.data.table(dbGetQuery(conn, statement = paste0(
  "select SHOP_NO_INDIV_BEN_BANLS as ID,\n",
  "       SHOP_DAT_ADMIS_SEJ_HOSP as DATE_DX\n",
  "from RES_SSS.V_SEJ_SERV_HOSP_CM\n",
  "where SHOP_COD_DIAGN_MDCAL_CLINQ like any (",paste(paste0("'",unlist(SQL_Dx),"'"), collapse = ", "),")\n",
  "and SHOP_DAT_ADMIS_SEJ_HOSP between '",debut,"' and '",fin,"';"
)))
Dx_etape1 <- rbind(Dx_etape1.1, Dx_etape1.2)  # joindre les deux tables ensemble
setkey(Dx_etape1)  # tri par ID + DATE_DX
Dx_etape1_1er <- Dx_etape1[, .SD[1], .(ID)]  # Conserver 1re obs de chaque ID
setnames(Dx_etape1_1er, "DATE_DX", "DI_Hospit")  # renommer la colonne


# ETAPE 2 -----------------------------------------------------------------

# V_DIAGN_SEJ_HOSP_CM - Extraction des Dx
Dx_etape2_medecho <- as.data.table(dbGetQuery(conn, statement = paste0(
  "select SHOP_NO_INDIV_BEN_BANLS as ID,\n",
  "       SHOP_DAT_ADMIS_SEJ_HOSP as DATE_DX\n",
  "from RES_SSS.V_DIAGN_SEJ_HOSP_CM\n",
  "where SHOP_COD_DIAGN_MDCAL_CLINQ like any (",paste(paste0("'",unlist(SQL_Dx),"'"), collapse = ", "),")\n",
  "    and SHOP_DAT_ADMIS_SEJ_HOSP between '",debut,"' and '",fin,"'\n",
  "    and SHOP_TYP_DIAGN_SEJ_HOSP in ('A', 'P', 'S', 'D');"
)))
# AUTRES VUES - Extraction des Dx
Dx_etape2_autre <- SQL_diagn(
  conn,
  cohort = NULL,
  debut = debut,
  fin = fin,
  Dx_table = SQL_Dx,  # nom de la liste creee precedemment contenant les codes de Dx a extraire
  CIM = c("CIM9", "CIM10"),
  dt_source = c("V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM",
                "I_SMOD_SERV_MD_CM"),
  dt_desc = list(V_SEJ_SERV_HOSP_CM = "MEDECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU",
                 I_SMOD_SERV_MD_CM = "SMOD"),
  date_dx_var = "admis",  # sélectionner la date d'admission du Dx
  exclu_diagn = NULL, verbose = TRUE
)
# Combiner les datasets de l'etape 2
Dx_etape2 <- rbind(Dx_etape2_medecho, Dx_etape2_autre[, .(ID, DATE_DX)])
setkey(Dx_etape2, ID)  # tri

### Confirmation des Dx
Dx_conf <- confirm_2Dx(
  dt = Dx_etape2, ID = "ID", DATE = "DATE_DX", DIAGN = NULL,
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730, reverse = FALSE
)
setkey(Dx_conf, ID, DATE_REP)
# Conserver la 1re date confirmee
Dx_conf_1st <- Dx_conf[, .SD[1], .(ID)]
setnames(Dx_conf_1st, c("DATE_REP", "DATE_CONF1"), c("DI_Acte", "DC_Acte"))

### Dernière date de Dx pour chaque ID
Dx_Tot <- rbind(Dx_etape1, Dx_etape2)
setkey(Dx_Tot)
Dx_Recent <- Dx_Tot[, .SD[.N], .(ID)]
setnames(Dx_Recent, "DATE_DX", "D_Recent")


# ETAPE FINALE ------------------------------------------------------------

### Combiner les datasets
# Creer un data contenant seulement les ID a l'etude
if (is.null(IDs)) {
  dt_final <- unique(Dx_Tot[, .(ID)])
} else {
  dt_final <- data.table(ID = IDs)
}
# Ajouter les variables au dt final
dt_final <- Dx_Recent[dt_final, on = .(ID)]  # ajout D_Recent
dt_final <- Dx_etape1_1er[dt_final, on = .(ID)]  # ajout DI_Hospit
dt_final <- Dx_conf_1st[, .(ID, DI_Acte)][dt_final, on = .(ID)]  # ajout DI_Acte
# Calculer DI_Finale - Date la plus ancienne entre DI_Acte et DI_Hospit
dt_final[, DI_Finale := DI_Acte]
dt_final[is.na(DI_Acte), DI_Finale := DI_Hospit]
dt_final[DI_Hospit < DI_Acte, DI_Finale := DI_Hospit]
