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

  cat(color_text("V_TENR_MED\n"))

  DT <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select\n",
    "    NMED_COD_TENR_MED as COD_TENR,\n",
    "    NMED_NOM_TENR as NOM_TENR,\n",
    "    NMED_NOM_ANGL_TENR\n",
    "from PROD.V_TENR_MED\n",
    "order by COD_TENR;"
  )))

  setkey(DT, COD_TENR)

  return(DT)

}


V_TENR_MED <- fct()
attr(V_TENR_MED, "MaJ") <- Sys.Date()
use_data(V_TENR_MED, overwrite = TRUE)
rm(V_TENR_MED)
