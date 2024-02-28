library(usethis)
library(data.table)
library(odbc)
library(stringr)
library(lubridate)
library(inesss)
library(writexl)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
if (!exists("user")) {
  user <- askpass::askpass("User")
}
if (!exists("pwd")) {
  pwd <- askpass::askpass()
}
conn <- SQL_connexion(user, pwd)

# Arguments ####
loop_dates <- sort(c(
  # Dates de début pour l'extraction SQL
  # À modifier en fonction de l'utilisateur qui exécute et son SPOOL disponible.
  paste0(seq(1996, 2000, 2), "-01-01"),  # deux ans
  paste0(2002:2007, "-01-01"),  # une année
  c(  # 6 mois - deux extractions par année
    paste0(2008:year(Sys.Date()), "-01-01"),
    paste0(2008:year(Sys.Date()), "-07-01")
  ),
  as.character(Sys.Date())
))
loop_dates <- loop_dates[loop_dates <= as.character(Sys.Date())]  # Supprimer les dates qui sont plus grande qu'aujourd'hui


# Fonctions ---------------------------------------------------------------

v_dem_paimt_med_cm <- function(loop_dates) {

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
  } else {
    verif_loop_var <- TRUE
  }

  DT <- vector("list", length(loop_dates) - 1)
  for (i in 1:length(DT)) {
    DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
      "select distinct\n",
      "    smed_cod_denom_comne as DENOM,\n",
      "    smed_cod_din as DIN,\n",
      "    smed_cod_forme_med as FORME,\n",
      "    smed_cod_tenr_med as TENEUR,\n",
      "    smed_cod_indcn_thera as INDCN_THERA,\n",
      "    smed_cod_cla_ahf as AHFS_CLA,\n",
      "    smed_cod_scla_ahf as AHFS_SCLA,\n",
      "    smed_cod_sscla_ahf as AHFS_SSCLA,\n",
      "    smed_cod_serv_1 as COD_SERV_1,\n",
      "    smed_cod_serv_2 as COD_SERV_2,\n",
      "    smed_cod_serv_3 as COD_SERV_3,\n",
      "    smed_cod_sta_decis as COD_STA_DECIS,\n",
      "    year(smed_dat_serv) as ANNEE,\n",
      "    month(smed_dat_serv) as MOIS\n",
      "from prod.v_dem_paimt_med_cm\n",
      "where smed_dat_serv between '",loop_dates[i],"' and '",as_date(loop_dates[i+1])-1,"';"
    )))
  }
  DT <- rbindlist(DT)
  DT[, `:=` (DATE_DEBUT = make_date(ANNEE, MOIS, 1),
             DATE_FIN = make_date(ANNEE, MOIS, 1) %m+% months(1, FALSE) - 1)]
  DT[, INDCN_THERA := str_trim(INDCN_THERA, side = "both")]
  DT[!is.na(AHFS_CLA), AHFS := paste0(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]
  DT[, `:=` (COD_SERV_1 = str_remove_all(COD_SERV_1, " "),
             COD_SERV_2 = str_remove_all(COD_SERV_2, " "),
             COD_SERV_3 = str_remove_all(COD_SERV_3, " "))]

  cols <- c(
    "DENOM", "DIN",
    "FORME", "TENEUR", "INDCN_THERA",
    "AHFS", "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA",
    "COD_SERV_1", "COD_SERV_2", "COD_SERV_3",
    "COD_STA_DECIS",
    "DATE_DEBUT", "DATE_FIN"
  )
  cols_by <- cols[!cols %in% c("DATE_DEBUT", "DATE_FIN")]
  DT <- DT[, ..cols]
  setkey(DT)

  DT[, diff := as.integer(DATE_DEBUT - shift(DATE_FIN) - 1L), by = cols_by][is.na(diff), diff := 0L]
  DT[, per := 0L][diff > 0L, per := 1L]
  DT[, per := cumsum(per) + 1L, by = cols_by]
  DT <- DT[
    , .(DATE_DEBUT = min(DATE_DEBUT),
        DATE_FIN = max(DATE_FIN)),
    keyby = c(cols_by, "per")
  ][, per := NULL]


  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}


# V_DEM_PAIMT_MED_CM ------------------------------------------------------

V_DEM_PAIMT_MED_CM <- v_dem_paimt_med_cm(loop_dates)
attr(V_DEM_PAIMT_MED_CM, "MaJ") <- Sys.Date()
attr(V_DEM_PAIMT_MED_CM, "nouveau_denom") <- attr(inesss::V_DEM_PAIMT_MED_CM, "nouveau_denom")
attr(V_DEM_PAIMT_MED_CM, "nouveau_din") <- attr(inesss::V_DEM_PAIMT_MED_CM, "nouveau_din")



# Nouveaux DENOM et DIN ---------------------------------------------------

new_tab <- list()
new_codes <- FALSE

new_denom <- V_DEM_PAIMT_MED_CM[
  !DENOM %in% sort(unique(inesss::V_DEM_PAIMT_MED_CM$DENOM)) & !is.na(DENOM),
  .(MIN_DAT_SERV = format(min(DATE_DEBUT), "%Y-%m")),
  keyby = .(DENOM)
]
if (nrow(new_denom)) {
  attr_new_denom <- attr(V_DEM_PAIMT_MED_CM, "nouveau_denom")
  attr_new_denom[[paste0("MaJ_", str_remove_all(Sys.Date(), "-"))]] <- new_denom
  attr(V_DEM_PAIMT_MED_CM, "nouveau_denom") <- attr_new_denom
  new_codes <- TRUE
  new_tab[["DENOM"]] <- new_denom
}

new_din <- V_DEM_PAIMT_MED_CM[
  !DIN %in% sort(unique(inesss::V_DEM_PAIMT_MED_CM$DIN)) & !is.na(DIN),
  .(MIN_DAT_SERV = format(min(DATE_DEBUT), "%Y-%m")),
  keyby =  .(DIN)
]
if (nrow(new_din)) {
  attr_new_din <- attr(V_DEM_PAIMT_MED_CM, "nouveau_din")
  attr_new_din[[paste0("MaJ_", str_remove_all(Sys.Date(), "-"))]] <- new_din
  attr(V_DEM_PAIMT_MED_CM, "nouveau_din") <- attr_new_din
  new_codes <- TRUE
  new_tab[["DIN"]] <- new_din
}

if (new_codes) {
  write_xlsx(new_tab, paste0("inst/v_dem_paimt_med_cm/Nouveaux Codes/",paste0("MaJ_", Sys.Date()),".xlsx"))
}


# SAVE --------------------------------------------------------------------

use_data(V_DEM_PAIMT_MED_CM, overwrite = TRUE)
rm(V_DEM_PAIMT_MED_CM)






