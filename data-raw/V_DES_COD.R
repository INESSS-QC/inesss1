library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(lubridate)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
if (!exists("user")) {
  user <- askpass::askpass("User")
}
if (!exists("pwd")) {
  pwd <- askpass::askpass()
}
conn <- SQL_connexion(user, pwd)

fct <- function() {

  cat(color_text("V_DES_COD\n"))

  ### Extraction de la table
  query <-
    "select
      CODE_NOM_COD as TYPE_CODE,
      CODE_VAL_COD as CODE,
    	CODE_DES as CODE_DESC,
    	CODE_DD_DES_COD as DATE_DEBUT,
    	CODE_DF_DES_COD as DATE_FIN
    from PROD.V_DES_COD;"
  DT <- as.data.table(dbGetQuery(conn, query))

  ### Arrangement des données
  DT[, `:=` (DATE_DEBUT = as_date(DATE_DEBUT),  # s'assurer que c'est au format date
             DATE_FIN = as_date(DATE_FIN))]
  setkey(DT, TYPE_CODE, CODE, CODE_DESC, DATE_DEBUT)  # tri

  DT[, diff := as.integer(DATE_DEBUT - shift(DATE_FIN)), .(TYPE_CODE, CODE, CODE_DESC)][is.na(diff), diff := 0L]
  DT[, per := 0L][diff > 1, per := 1L]
  DT[, per := cumsum(per) + 1L, .(TYPE_CODE, CODE, CODE_DESC)]
  DT <- DT[
    , .(DATE_DEBUT = min(DATE_DEBUT),
        DATE_FIN = max(DATE_FIN)),
    .(TYPE_CODE, CODE, CODE_DESC, per)
  ][, per := NULL]

  DT[intersect(
    DT[, .I[.N == 1], .(TYPE_CODE, CODE, CODE_DESC)]$V1,
    DATE_DEBUT > today()
  )]
  DT[  # Cas où une seule ligne et DATE_DEBUT="2078-12-31"
    intersect(DT[, .I[.N == 1], .(TYPE_CODE, CODE, CODE_DESC)]$V1,
              DATE_DEBUT > today()),
    DATE_DEBUT := as_date("1900-01-01")
  ]


  return(DT)

}

V_DES_COD <- fct()
attr(V_DES_COD, "MaJ") <- Sys.Date()

use_data(V_DES_COD, overwrite = TRUE)
rm(V_DES_COD)
