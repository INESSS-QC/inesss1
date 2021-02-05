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

  ### Tableau des codes DENOM
  query <-
    "select NMED_COD_DENOM_COMNE as DENOM,
  	  NMED_DD_DENOM_COMNE as DATE_DEBUT,
  	  NMED_DF_DENOM_COMNE as DATE_FIN,
  	  NMED_NOM_DENOM_COMNE as NOM_DENOM,
  	  NMED_NOM_DENOM_COMNE_SYNON as NOM_DENOM_SYNON,
  	  NMED_NOM_ANGL_DENOM_COMNE as NOM_DENOM_ANGLAIS,
  	  NMED_NOM_ANGL_DENOM_SYNON as NOM_DENOM_SYNON_ANGLAIS
    from PROD.V_DENOM_COMNE_MED;"
  DT <- as.data.table(dbGetQuery(conn, query))

  ### S'assurer que les dates sont au format Date
  DT[, `:=` (DATE_DEBUT = as_date(DATE_DEBUT),
             DATE_FIN = as_date(DATE_FIN))]

  setkey(DT, DENOM, DATE_DEBUT)
  attr(DT, "MaJ") <- Sys.Date()  # date de création
  return(DT)

}

V_DENOM_COMNE_MED <- fct()
use_data(V_DENOM_COMNE_MED, overwrite = TRUE)
