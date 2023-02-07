library(usethis)
library(odbc)
library(data.table)
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

  cat(color_text("V_FORME_MED\n"))

  DT <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select\n",
    "    NMED_COD_FORME_MED as COD_FORME,\n",
	  "    NMED_COD_TYP_FORME as COD_TYP_FORME,\n",
    "    NMED_NOM_FORME as NOM_FORME,\n",
    "    NMED_NOM_FORME_ABR as NOM_FORME_ABR,\n",
    "    NMED_NOM_ANGL_FORME as NOM_ANGL_FORME\n",
    "from PROD.V_FORME_MED\n",
    "order by COD_FORME;"
  )))

  setkey(DT, COD_FORME)

  return(DT)

}


V_FORME_MED <- fct()
attr(V_FORME_MED, "MaJ") <- Sys.Date()
use_data(V_FORME_MED, overwrite = TRUE)
rm(V_FORME_MED)

odbc <- odbc::dbDisconnect(conn)
