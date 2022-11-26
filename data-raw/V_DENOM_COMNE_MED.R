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

fct <- function() {

  cat(color_text("V_DENOM_COMNE_MED\n"))

  ### Tableau des codes DENOM
  denom_codes_query <-
    "select NMED_COD_DENOM_COMNE as DENOM,
  	  NMED_DD_DENOM_COMNE as DATE_DEBUT,
  	  NMED_DF_DENOM_COMNE as DATE_FIN,
  	  NMED_NOM_DENOM_COMNE as NOM_DENOM,
  	  NMED_NOM_DENOM_COMNE_SYNON as NOM_DENOM_SYNON,
  	  NMED_NOM_ANGL_DENOM_COMNE as NOM_DENOM_ANGLAIS,
  	  NMED_NOM_ANGL_DENOM_SYNON as NOM_DENOM_SYNON_ANGLAIS
    from PROD.V_DENOM_COMNE_MED;"
  DT <- as.data.table(dbGetQuery(conn, denom_codes_query))

  ### S'assurer que les dates sont au format Date
  DT[, `:=` (DATE_DEBUT = as_date(DATE_DEBUT),
             DATE_FIN = as_date(DATE_FIN))]

  setkey(DT, DENOM, DATE_DEBUT)
  attr(DT, "verif_loop_var") <- NULL
  attr(DT, "name_loop_var") <- NULL
  return(DT)

}

V_DENOM_COMNE_MED <- fct()
attr(V_DENOM_COMNE_MED, "MaJ") <- Sys.Date()
use_data(V_DENOM_COMNE_MED, overwrite = TRUE)
rm(V_DENOM_COMNE_MED)

# Fermer la connexion
conn <- odbc::dbDisconnect(conn)
