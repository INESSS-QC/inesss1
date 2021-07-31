library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(lubridate)
conn <- SQL_connexion(user, pwd)

nom_marq_comrc <- function() {

  ### Extraction SQL
  query <-
    "select distinct NMED_COD_DENOM_COMNE as DENOM,
				NMED_COD_DIN as DIN,
				NMED_NOM_MARQ_COMRC as NOM_MARQ_COMRC,
				NMED_DD_PRODU_MED as DATE_DEBUT,
				NMED_DF_PRODU_MED as DATE_FIN
    from PROD.V_PRODU_MED;"
  DT <- as.data.table(dbGetQuery(conn, query))
  setkey(DT, DENOM, DIN, DATE_DEBUT)

  ### Arranger les données
  DT[, `:=` (DATE_DEBUT = as_date(DATE_DEBUT),  # s'assurer que c'est une Date
             DATE_FIN = as_date(DATE_FIN))]

  ### Regrouper les périodes qui se chevauchent
  DT[
    rmNA(DT[, .I[.N > 1], .(DENOM, DIN, NOM_MARQ_COMRC)]$V1),
    diff := as.integer(DATE_DEBUT - shift(DATE_FIN)),
    .(DENOM, DIN, NOM_MARQ_COMRC)
  ][is.na(diff), diff := 0L]
  DT[, per := 0L][diff > 1, per := 1L]
  DT[, per := cumsum(per) + 1L, .(DENOM, DIN, NOM_MARQ_COMRC)]
  DT <- DT[
    , .(DATE_DEBUT = min(DATE_DEBUT),
        DATE_FIN = max(DATE_FIN)),
    .(DENOM, DIN, NOM_MARQ_COMRC, per)
  ][, per := NULL]

  setkey(DT, DENOM, DIN)

  return(DT)

}

V_PRODU_MED <- list(
  NOM_MARQ_COMRC = nom_marq_comrc()
)
attr(V_PRODU_MED, "MaJ") <- Sys.Date()
use_data(V_PRODU_MED, overwrite = TRUE)
rm(V_PRODU_MED)

# Fermer la connexion
conn <- odbc::dbDisconnect(conn)
