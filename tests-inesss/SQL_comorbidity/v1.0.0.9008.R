library(inesss)
library(odbc)
conn <- SQL_connexion("ms045")
cohorte <- dbGetQuery(conn, statement = paste0(
  "select\n",
  "    no_indiv_ben_banls as ID,\n",
  "    min(date_index) as DATE_INDEX\n",
  "from DONNE_INESSS.COVID_COHORTE\n",
  "group by ID\n",
  "order by ID;"
))
CharlsonTest <- SQL_comorbidity(
  conn = conn,
  dt = cohorte, ID = "ID", DATE_INDEX = "DATE_INDEX",
  Dx_table = 'Charlson_Dx_CCI_INSPQ_Manitoba', CIM = c('CIM9', 'CIM10'), scores = 'CCI_INSPQ_Manitoba',
  lookup = 2, n1 = 30, n2 = 730,
  dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM', 'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
                 V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
  confirm_sourc = list(MEDECHO = 1, BDCU = 2, SMOD = 2),
  date_dx_var = "depar", obstetric_exclu = FALSE, exclu_diagn = NULL,
  verbose = TRUE, keep_confirm_data = FALSE
)




# library(inesss)
# library(odbc)
# conn <- SQL_connexion("ms045")
# cohorte <- dbGetQuery(conn, statement = paste0(
#   "select\n",
#   "    no_indiv_ben_banls as ID,\n",
#   "    min(date_index) as DATE_INDEX\n",
#   "from DONNE_INESSS.COVID_COHORTE\n",
#   "group by ID\n",
#   "order by ID;"
# ))
# conn = conn
# dt = cohorte
# ID = "ID"
# DATE_INDEX = "DATE_INDEX"
# Dx_table = 'Charlson_Dx_CCI_INSPQ_Manitoba'
# CIM = c('CIM9', 'CIM10')
# scores = 'CCI_INSPQ_Manitoba'
# lookup = 2
# n1 = 30
# n2 = 730
# dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM', 'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM')
# dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
#                V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD')
# confirm_sourc = list(MEDECHO = 1, BDCU = 2, SMOD = 2)
# date_dx_var = "depar"
# obstetric_exclu = TRUE
# exclu_diagn = NULL
# verbose = TRUE
# keep_confirm_data = FALSE
