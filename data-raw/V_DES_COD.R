library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(lubridate)
color_text <- function(x) {
  return(crayon::italic(crayon::green(x)))
}
conn <- SQL_connexion(user, pwd)

fct <- function(need_conn = FALSE) {

  cat(color_text("V_DES_COD\n"))

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

  attr(DT, "verif_loop_var") <- NULL
  attr(DT, "name_loop_var") <- NULL
  return(DT)

}

V_DES_COD <- fct()
attr(V_DES_COD, "MaJ") <- Sys.Date()  # date de création
use_data(V_DES_COD, overwrite = TRUE)
rm(V_DES_COD)

# Fermer la connexion
conn <- odbc::dbDisconnect(conn)
