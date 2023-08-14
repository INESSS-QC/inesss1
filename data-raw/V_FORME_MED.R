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
    "    f.NMED_COD_FORME_MED as COD_FORME,\n",
    "    f.NMED_NOM_FORME as NOM_FORME,\n",
    "    f.NMED_NOM_FORME_ABR as NOM_FORME_ABR,\n",
    "    f.NMED_NOM_ANGL_FORME as NOM_ANGL_FORME,\n",
    "    f.NMED_COD_TYP_FORME as COD_TYP_FORME,\n",
    "    d.CODE_DES as NOM_TYPE_FORME\n",
    "from PROD.V_FORME_MED as f\n",
    "    left join PROD.V_DES_COD as d on d.CODE_VAL_COD = f.NMED_COD_TYP_FORME\n",
    "                                  and d.code_nom_cod = 'COD_TYP_FORME'\n",
    "order by COD_FORME;"
  )))

  setkey(DT, COD_FORME)

  return(DT)

}


V_FORME_MED <- fct()
attr(V_FORME_MED, "MaJ") <- Sys.Date()
use_data(V_FORME_MED, overwrite = TRUE)
rm(V_FORME_MED)
