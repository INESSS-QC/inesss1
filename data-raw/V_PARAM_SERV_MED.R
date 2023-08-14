library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(lubridate)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
if (!exists("user") | is.null(user)) {
  user <- askpass::askpass("User")
}
if (!exists("pwd") | is.null("pwd")) {
  pwd <- askpass::askpass()
}
conn <- SQL_connexion(user, pwd)

fct <- function() {

  cat(color_text("V_PARAM_SERV_MED\n"))

  ### Extraction de la table
  query <-
    "select	NMED_COD_SERV_MED as COD_SERV,
		        NMED_DES_SERV_MED as COD_SERV_DESC,
		        NMED_DD_PARAM_SERV_MED as DATE_DEBUT,
		        NMED_DF_PARAM_SERV_MED as DATE_FIN
      from PROD.V_PARAM_SERV_MED;"
  DT <- as.data.table(dbGetQuery(conn, query))

  ### Arrangement des données
  DT[, `:=` (DATE_DEBUT = as_date(DATE_DEBUT),  # s'assurer que c'est au format date
             DATE_FIN = as_date(DATE_FIN))]
  setkey(DT, COD_SERV, DATE_DEBUT)  # tri

  DT[, diff := as.integer(DATE_DEBUT - shift(DATE_FIN)), .(COD_SERV, COD_SERV_DESC)][is.na(diff), diff := 0L]
  DT[, per := 0L][diff > 1, per := 1L]
  DT[, per := cumsum(per) + 1L, .(COD_SERV, COD_SERV_DESC)]
  DT <- DT[
    , .(DATE_DEBUT = min(DATE_DEBUT),
        DATE_FIN = max(DATE_FIN)),
    .(COD_SERV, COD_SERV_DESC, per)
  ][, per := NULL]

  DT[  # Cas où une seule ligne et DATE_DEBUT="2078-12-31"
    DT[, .I[.N == 1], .(COD_SERV, COD_SERV_DESC)]$V1 &
      DATE_DEBUT > today(),
    DATE_DEBUT := as_date("1900-01-01")
  ]

  return(DT)

}

V_PARAM_SERV_MED <- fct()
attr(V_PARAM_SERV_MED, "MaJ") <- Sys.Date()

use_data(V_PARAM_SERV_MED, overwrite = TRUE)
rm(V_PARAM_SERV_MED)
