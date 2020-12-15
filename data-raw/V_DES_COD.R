library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
# conn <- sql_connexion(askpass("User"))

fct <- function(need_conn = FALSE) {

  if (need_conn) {
    conn <- sql_connexion(askpass("User"))
  }

  query <-
    "select CODE_VAL_COD as CODE,
    	CODE_NOM_COD as TYPE_CODE,
    	CODE_DES as CODE_DESC,
    	CODE_DD_DES_COD as DATE_DEBUT,
    	CODE_DF_DES_COD as DATE_FIN
    from PROD.V_DES_COD
    order by CODE_NOM_COD asc, CODE_VAL_COD asc;"

  DT <- as.data.table(dbGetQuery(conn, query))
  setkey(DT, TYPE_CODE, CODE)
  attr(DT, "Date") <- Sys.Date()
  return(DT)

}


V_DES_COD <- fct()
use_data(V_DES_COD, overwrite = TRUE)
