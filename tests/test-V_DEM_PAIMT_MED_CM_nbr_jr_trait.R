library(inesss)
library(data.table)

conn <- SQL_connexion("ms045")

DENOM <- rmNA(sunique(V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$DENOM))
DT <- vector("list", length(DENOM))
i <- 1L
for (denom in DENOM) {
  dt <- as.data.table(odbc::dbGetQuery(
    conn, statement = paste0(
      "select SMED_COD_SERV_1 as SMED_COD_SERV,\n",
      "       SMED_NBR_JR_DUREE_TRAIT as DUREE_TX\n",
      "from V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV between '2020-01-01' and '2020-12-31'\n",
      "    and SMED_COD_DENOM_COMNE = '",denom,"';"
    )
  ))
  if (nrow(dt)) {
    DT[[i]] <- dt
  }
  i <- i + 1L
}
DT <- rbindlist(DT)

DT <- dt[
  , .(MIN = min(DUREE_TX),
      MEAN = mean(DUREE_TX),
      MEDIAN = median(DUREE_TX),
      MAX = max(DUREE_TX)),
  .(SMED_COD_SERV)
]


