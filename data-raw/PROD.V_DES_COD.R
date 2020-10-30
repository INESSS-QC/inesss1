library(usethis)
library(odbc)
library(data.table)
library(askpass)
# conn <- sql_connexion("PEI_PRD", askpass("Identifiant ?"), askpass("Mot de passe?"), "latin1")

fct <- function(need_conn = FALSE) {

  if (need_conn) {
    conn <- sql_connexion("PEI_PRD", askpass("Identifiant ?"), askpass("Mot de passe?"), "latin1")
  }

  DT <- as.data.table(dbGetQuery(
    conn,
    "select CODE_VAL_COD as CODE,
    	CODE_NOM_COD as TYPE_CODE,
    	CODE_DES as CODE_DESC,
    	CODE_DD_DES_COD as DATE_DEBUT,
    	CODE_DF_DES_COD as DATE_FIN
    from PROD.V_DES_COD
    order by CODE_NOM_COD asc, CODE_VAL_COD asc;"
  ))
  setkey(DT, TYPE_CODE, CODE)

  attr(DT, "Date") <- Sys.Date()
  return(DT)

}


PROD.V_DES_COD <- fct()
use_data(PROD.V_DES_COD, overwrite = TRUE)
