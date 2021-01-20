library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(lubridate)
# conn <- SQL_connexion(askpass("User"))

fct <- function(need_conn = FALSE) {

  ### Effectuer la connexion à Teradata
  if (need_conn) {
    conn <- sql_connexion(askpass("User"))
  }

  ### Extraction de la table
  query <-
    "select CODE_VAL_COD as CODE,
    	CODE_NOM_COD as TYPE_CODE,
    	CODE_DES as CODE_DESC,
    	CODE_DD_DES_COD as DATE_DEBUT,
    	CODE_DF_DES_COD as DATE_FIN
    from PROD.V_DES_COD;"
  DT <- as.data.table(dbGetQuery(conn, query))

  ### Arrangement des données
  DT[, `:=` (DATE_DEBUT = as_date(DATE_DEBUT),  # s'assurer que c'est au format date
             DATE_FIN = as_date(DATE_FIN))]
  setkey(DT, TYPE_CODE, CODE, DATE_DEBUT)  # tri

  attr(DT, "Date") <- Sys.Date()  # date de création
  return(DT)

}


V_DES_COD <- fct()
use_data(V_DES_COD, overwrite = TRUE)
