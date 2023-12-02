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

# Arguments ####
loop_dates <- sort(c(
  # Dates de début pour l'extraction SQL
  # À modifier en fonction de l'utilisateur qui exécute et son SPOOL disponible.
  "1996-01-01",  # 1996 à 2000 les descriptions sont vides
  "2001-01-01",
  paste0(seq(2016, year(Sys.Date()), 3), "-01-01"),
  as.character(Sys.Date())
))
loop_dates <- loop_dates[loop_dates <= as.character(Sys.Date())]  # Supprimer les dates qui sont plus grande qu'aujourd'hui


# FONCTIONS -----------------------------------------------------------------------------------

des_court_indcn_recnu <- function(loop_dates) {

  cat(color_text("I_APME_DEM_AUTOR_CRITR_ETEN_CM - DES_COURT_INDCN_RECNU\n"))

  # Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "APME_DAT_STA_DEM_PME"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct APME_DAT_STA_DEM_PME\n",
    "from PROD.I_APME_DEM_AUTOR_CRITR_ETEN_CM\n",
    "where APME_DAT_STA_DEM_PME is null;"
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
      "    apme_cod_denom_comne_dem as DENOM_DEM,\n",
      "    apme_cod_din_dem as DIN_DEM,\n",
      "    npme_des_court_indcn_recnu as DESC_COURT_INDCN_RECNU,\n",
      "    year(apme_dat_sta_dem_pme) as ANNEE,\n",
      "    month(apme_dat_sta_dem_pme) as MOIS\n",
      "from prod.i_apme_dem_autor_critr_eten_cm\n",
      "where apme_dat_sta_dem_pme between '",loop_dates[i],"' and '",as_date(loop_dates[i+1])-1,"'\n",
      "    and npme_des_court_indcn_recnu is not null;"
    )))
  }
  DT <- rbindlist(DT)
  DT[, `:=` (DATE_DEBUT = make_date(ANNEE, MOIS, 1),
             DATE_FIN = make_date(ANNEE, MOIS, 1) %m+% months(1, FALSE) - 1)]

  cols <- c(
    "DENOM_DEM", "DIN_DEM", "DESC_COURT_INDCN_RECNU",
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


# SAVE DATASET --------------------------------------------------------------------------------

DES_COURT_INDCN_RECNU = des_court_indcn_recnu(loop_dates)
attr(DES_COURT_INDCN_RECNU, "MaJ") <- Sys.Date()

use_data(DES_COURT_INDCN_RECNU, overwrite = TRUE)
rm(DES_COURT_INDCN_RECNU)
