library(usethis)
library(data.table)
library(odbc)
library(stringr)
library(lubridate)
library(inesss)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
if (!exists("user")) {
  user <- askpass::askpass("User")
}
if (!exists("pwd")) {
  pwd <- askpass::askpass()
}
conn <- SQL_connexion(user, pwd)



# Fonctions ---------------------------------------------------------------

v_dem_paimt_med_cm <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    years <- 1996:year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"  # en théorie la date est en 1996
    ))$min_date
    years <- min_year:year(Sys.Date())
  }

  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct\n",
        "    smed_cod_denom_comne as DENOM,\n",
        "    smed_cod_din as DIN,\n",
        "    smed_cod_cla_ahf as AHFS_CLA,\n",
        "    smed_cod_scla_ahf as AHFS_SCLA,\n",
        "    smed_cod_sscla_ahf as AHFS_SSCLA,\n",
        "    smed_cod_forme_med as FORME,\n",
        "    smed_cod_tenr_med as TENEUR,\n",
        "    smed_cod_serv_1 as COD_SERV_1,\n",
        "    smed_cod_serv_2 as COD_SERV_2,\n",
        "    smed_cod_serv_3 as COD_SERV_3,\n",
        "    smed_cod_sta_decis as COD_STA_DECIS\n",
        "from prod.v_dem_paimt_med_cm\n",
        "where smed_dat_serv between '",date_ymd(yr, mth, 1L),"' and '",date_ymd(yr, mth, "last"),"';"
      )))
      if (nrow(DT[[i]])) {
        DT[[i]][, `:=` (DATE_DEBUT = make_date(yr, mth, 1),
                        DATE_FIN = make_date(yr, mth, 1) %m+% months(1, FALSE) - 1)]
      } else {
        DT[[i]] <- NULL
      }
      if (yr == year(Sys.Date()) && mth == month(Sys.Date())) {
        break
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)

  DT[!is.na(AHFS_CLA), AHFS := paste0(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]
  DT[, `:=` (COD_SERV_1 = str_remove_all(COD_SERV_1, " "),
             COD_SERV_2 = str_remove_all(COD_SERV_2, " "),
             COD_SERV_3 = str_remove_all(COD_SERV_3, " "))]

  cols <- c(
    "DENOM", "DIN", "AHFS", "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA",
    "FORME", "TENEUR",
    "COD_SERV_1", "COD_SERV_2", "COD_SERV_3",
    "COD_STA_DECIS",
    "DATE_DEBUT", "DATE_FIN"
  )
  cols_by <- cols[!cols %in% c("DATE_DEBUT", "DATE_FIN")]
  setcolorder(DT, cols)
  setkey(DT)

  DT[, diff := as.integer(DATE_DEBUT - shift(DATE_FIN) - 1L), by = cols_by][is.na(diff), diff := 0L]
  DT[, per := 0L][diff > 0L, per := 1L]
  DT[, per := cumsum(per) + 1L, by = cols_by]
  DT <- DT[
    , .(DATE_DEBUT = min(DATE_DEBUT),
        DATE_FIN = max(DATE_FIN)),
    keyby = c(cols_by, "per")
  ][, per := NULL]

  setorderv(DT, names(DT), na.last = TRUE)

  return(DT)

}

# Créer dataset -----------------------------------------------------------

V_DEM_PAIMT_MED_CM <- v_dem_paimt_med_cm()
attr(V_DEM_PAIMT_MED_CM, "MaJ") <- Sys.Date()

use_data(V_DEM_PAIMT_MED_CM, overwrite = TRUE)
rm(V_DEM_PAIMT_MED_CM)
