################################################################################# #
############################### ATTENTION #########################################
# S'assurer d'installer la dernière version du package avant d'exécuter le code.
# Nécessaire pour analyser les nouveaux codes de médicaments qui apparaissent dans
# la base de données V_DEM_PAIMT_MED_CM (peut-être d'autres aussi)
# ############################################################################### #

library(inesss)

user <- askpass::askpass("User")
pwd <- askpass::askpass("Mot de passe")

files <- c(
  "CIM",
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM___DES_COURT_INDCN_RECNU",
  "V_CLA_AHF", "V_COD_STA_DECIS",
  "V_DEM_PAIMT_MED_CM",
  "V_DENOM_COMNE_MED",
  "V_DES_COD",
  "V_FORME_MED",
  "V_PARAM_SERV_MED",
  "V_PRODU_MED",
  "V_TENR_MED"
)
files <- paste0("data-raw/", files, ".R")

conn <- SQL_connexion(user, pwd)
if (is.null(conn)) {
  stop("User ou Mot de passe erroné.")
} else if (class(conn)[1] == "Teradata") {
  t1 <- Sys.time()
  for (f in files) {
    source(f, local = TRUE, encoding = "UTF-8")
  }
  t2 <- Sys.time()
  difftime(t2, t1)
}
