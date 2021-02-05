library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(lubridate)
library(stringr)
# conn <- SQL_connexion(askpass("User"))

fct <- function(need_con = FALSE) {

  ### Effectuer la connexion à Teradata
  if (need_con) {
    conn <- sql_connexion(askpass("User"))
  }

  ### Trouver la liste des codes pour chaque année
  to_year <- 1996:year(Sys.Date())  # année à analyser
  DT <- vector("list", length(to_year) * 12)  # contiendra les tableaux des requêtes
  i <- 1L
  for (yr in to_year) {  # pour chaque année
    for (mth in 1:12) {  # pour chaque mois
      dt <- as.data.table(dbGetQuery(  # liste unique des codes de service pour le mois
        conn, paste0(
          "select distinct SMED_COD_DIN as DIN\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",
          paste0(yr,"-",str_pad(mth, width = 2, pad = "0"),"-01"),
          "' and '",
          as.Date(ifelse(
            mth == 12, paste0(yr,"-12-31"),
            as.Date(paste0(yr,"-",str_pad(mth+1, width = 2, pad = "0"),"-01"))-1),
            origin = "1970-01-01"
          ),
          "';"
        )
      ))
      if (nrow(dt)) {
        dt[, ANNEE := yr]  # indiquer l'année
        DT[[i]] <- dt
      } else {
        DT[[i]] <- NULL
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)  # un seul tableau
  setkey(DT, DIN, ANNEE)  # trier par DIN + ANNEE

  ### Indiquer les années où le code est présent
  DT <- DT[
    ,.(DEBUT = min(ANNEE), FIN = max(ANNEE)),
    .(DIN)
  ]

  attr(DT, "MaJ") <- Sys.Date()  # date de création
  return(DT)

}

V_DEM_PAIMT_MED_CM.SMED_COD_DIN <- fct()
use_data(V_DEM_PAIMT_MED_CM.SMED_COD_DIN, overwrite = TRUE)
