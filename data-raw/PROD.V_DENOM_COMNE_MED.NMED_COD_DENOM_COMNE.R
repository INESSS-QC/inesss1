library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
# conn <- sql_connexion("PEI_PRD", askpass("Identifiant ?"), askpass("Mot de passe?"), "latin1")

fct <- function(need_conn = FALSE) {
  if (need_conn) {
    conn <- sql_connexion("PEI_PRD", askpass("Identifiant ?"), askpass("Mot de passe?"), "latin1")
  }
  DT <- as.data.table(dbGetQuery(
    conn,
    "select NMED_COD_DENOM_COMNE as DENOM,
	  NMED_NOM_DENOM_COMNE as NOM_DENOM,
	  NMED_DD_DENOM_COMNE as DATE_DEBUT,
	  NMED_DF_DENOM_COMNE as DATE_FIN
  from PROD.V_DENOM_COMNE_MED;"
  ))
  setkey(DT, DENOM)
  attr(DT, "Date") <- Sys.Date()
  return(DT)
}

PROD.V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE <- fct()
use_data(PROD.V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE, overwrite = TRUE)
