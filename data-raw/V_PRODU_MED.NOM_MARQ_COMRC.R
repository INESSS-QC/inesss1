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
    "select distinct NMED_COD_DENOM_COMNE as DENOM,
				NMED_COD_DIN as DIN,
				NMED_NOM_MARQ_COMRC as NOM_MARQ_COMRC,
				NMED_DD_PRODU_MED as DATE_DEBUT,
				NMED_DF_PRODU_MED as DATE_FIN
    from PROD.V_PRODU_MED
    order by NMED_COD_DENOM_COMNE, NMED_COD_DIN, NMED_DD_PRODU_MED;"

  DT <- as.data.table(dbGetQuery(conn, query))
  setkey(DT, DENOM, DIN, DATE_DEBUT)

  # Regrouper les périodes qui se chevauche
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

  attr(DT, "Date") <- Sys.Date()
  return(DT)

}


V_PRODU_MED.NOM_MARQ_COMRC <- fct()
use_data(V_PRODU_MED.NOM_MARQ_COMRC, overwrite = TRUE)
