library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(stringr)
library(inesss)
# conn <- sql_connexion(askpass("User"))

fct <- function(need_con = FALSE) {

  if (need_con) {
    conn <- sql_connexion(askpass("User"))
  }

  DT <- data.table()
  for (yr in 1996:year(Sys.Date())) {

    for (mth in 1:12) {

      dt <- as.data.table(dbGetQuery(  # liste unique des codes de service
        conn, paste0(
          "select distinct SMED_COD_DIN as COD_DIN\n",
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
        dt[, ANNEE := yr]
        DT <- rbind(DT, dt)
      }

    }

  }

  DT <- unique(DT)
  setkey(DT)

  # Indiquer les années où le code est présent
  dt <- DT[
    ,.(DEBUT = min(ANNEE), FIN = max(ANNEE)),
    .(COD_DIN)
  ]

  attr(dt, "Date") <- Sys.Date()
  return(dt)

}

V_DEM_PAIMT_MED_CM.SMED_COD_DIN <- fct()
use_data(V_DEM_PAIMT_MED_CM.SMED_COD_DIN, overwrite = TRUE)
