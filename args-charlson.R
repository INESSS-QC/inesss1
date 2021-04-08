conn = SQL_connexion("ms045")
dt = readRDS("V:/GI-Data/_MedPersAg/Charlson_test.rds")
ID = "ID"
DATE_INDEX = "DAT_Index"
Dx_table = 'Combine_Dx_CCI_INSPQ18'
CIM = c('CIM9', 'CIM10')
scores = 'CCI_INSPQ_2018_CIM10'
lookup = 2
n1 = 30; n2 = 730
dt_source = c('V_DIAGN_SEJ_HOSP_CM',
              'V_SEJ_SERV_HOSP_CM',
              'V_EPISO_SOIN_DURG_CM',
              'I_SMOD_SERV_MD_CM')
dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO',
               V_SEJ_SERV_HOSP_CM = 'MEDECHO',
               V_EPISO_SOIN_DURG_CM = 'BDCU',
               I_SMOD_SERV_MD_CM = 'SMOD')
confirm_sourc = list(MEDECHO = 1,
                     BDCU = 2,
                     SMOD = 2)
obstetric_exclu = TRUE
exclu_diagn = c('drug', 'ld')
keep_confirm_data = TRUE
verbose = TRUE

DIAGN <- readRDS("V:/GI-Data/_MedPersAg/Charlson_test2.rds")
