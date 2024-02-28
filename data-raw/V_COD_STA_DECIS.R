library(usethis)
library(data.table)
library(odbc)
library(inesss)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
if (!exists("user")) {
  user <- askpass::askpass("User")
}
if (!exists("pwd")) {
  pwd <- askpass::askpass()
}
conn <- SQL_connexion(user, pwd)

fct <- function() {

  cat(color_text("V_COD_STA_DECIS\n"))

  DT <- as.data.table(dbGetQuery(conn, statement =
    " select
        CODE_VAL_COD as COD_STA_DECIS,
        CODE_DES as NOM_COD_STA_DECIS,
        CODE_DD_DES_COD as DATE_DEBUT,
        CODE_DF_DES_COD as DATE_FIN
      from PROD.V_DES_COD
      where CODE_NOM_COD = 'COD_STA_DECIS'
      order by cod_sta_decis;"
  ))

  return(DT)

}

V_COD_STA_DECIS <- fct()
attr(V_COD_STA_DECIS, "MaJ") <- Sys.Date()

use_data(V_COD_STA_DECIS, overwrite = TRUE)
rm(V_COD_STA_DECIS)
