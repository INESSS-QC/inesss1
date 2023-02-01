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

  cat(color_text("V_CLA_AHF\n"))

  DT <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_CLA_AHF as AHFS_CLA,\n",
    "       NMED_COD_SCLA_AHF as AHFS_SCLA,\n",
    "       NMED_COD_SSCLA_AHF as AHFS_SSCLA,\n",
    "       NMED_NOM_CLA_AHF as NOM_AHFS\n",
    "from V_CLA_AHF;"
  )))

  setkey(DT, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)

  return(DT)

}

V_CLA_AHF <- fct()
use_data(V_CLA_AHF, overwrite = TRUE)
rm(V_CLA_AHF)

odbc <- odbc::dbDisconnect(conn)
