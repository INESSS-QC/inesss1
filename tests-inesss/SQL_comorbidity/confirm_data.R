library(inesss)
library(data.table)
library(odbc)


# 2021-06-03 --------------------------------------------------------------

conn <- SQL_connexion()  # connexion à Teradata

cohorte <- as.data.table(dbGetQuery(conn, statement = paste0(
  "select NO_INDIV_BEN_BANLS as ID,\n",
  "       DATE_INDEX as DATE_INDEX\n",
  "from DONNE_INESSS.COVID_COHORTE;"
)))
setkey(cohorte)
cohorte <- cohorte[, .SD[1], .(ID)]


verif <- SQL_comorbidity(
  conn = conn,
  dt = cohorte,
  ID = "ID",
  DATE_INDEX = "DATE_INDEX",
  Dx_table = "Combine_Dx_CCI_INSPQ18",
  CIM = c("CIM9", "CIM10"),
  scores = "CCI_INSPQ_2018_CIM10",
  lookup = 2, n1 = 30, n2 = 730,
  dt_source = c(
    "V_DIAGN_SEJ_HOSP_CM",
    "V_SEJ_SERV_HOSP_CM",
    "V_EPISO_SOIN_DURG_CM"
    #"I_SMOD_SERV_MD_CM"
  ),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO",
                 V_SEJ_SERV_HOSP_CM = "MEDECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU",
                 I_SMOD_SERV_MD_CM = "SMOD"),
  confirm_sourc = list(MEDECHO = 1,
                       BDCU = 2,
                       SMOD = 2),
  date_dx_var = "admis",
  obstetric_exclu = FALSE,
  exclu_diagn = NULL,
  verbose = TRUE,
  keep_confirm_data = TRUE
)

attributes(verif)
