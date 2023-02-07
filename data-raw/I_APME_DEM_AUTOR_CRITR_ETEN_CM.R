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



# FONCTIONS -----------------------------------------------------------------------------------

des_court_indcn_recnu <- function() {

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
    years <- 1996:data.table::year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from APME_DAT_STA_DEM_PME)) as min_date\n",
      "from PROD.I_APME_DEM_AUTOR_CRITR_ETEN_CM;"
    ))$min_date
    years <- min_year:data.table::year(Sys.Date())
  }

  ### Extraction

  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct\n",
        "    APME_COD_DENOM_COMNE_DEM as DENOM_DEM,\n",
        "    APME_COD_DIN_DEM as DIN_DEM,\n",
        "    NPME_DES_COURT_INDCN_RECNU as DES_COURT_INDCN_RECNU,\n",
        "    extract(year from APME_DAT_STA_DEM_PME) as ANNEE,\n",
        "    extract(month from APME_DAT_STA_DEM_PME) as MOIS\n",
        "from PROD.I_APME_DEM_AUTOR_CRITR_ETEN_CM\n",
        "where APME_DAT_STA_DEM_PME between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"'\n",
        "    and NPME_DES_COURT_INDCN_RECNU is not null\n",
        "order by DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU, ANNEE, MOIS;"
      )))
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  DT[
    , ANNEEMOIS := fcase(
      MOIS < 10, as.integer(paste0(ANNEE, "0", MOIS)),
      MOIS >= 10, as.integer(paste0(ANNEE, MOIS))
    )
  ]
  DT[
    , diff := ANNEEMOIS - shift(ANNEEMOIS),
    .(DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU)
  ]
  DT[is.na(diff), diff := 0L]  # 1er d'un groupe, inscrire 0
  DT[diff == 89, diff := 1L]  # 89 = Janvier - Décembre année précédente
  DT[, per := fcase(diff > 1, 1L, default = 0L)]
  DT[, per := cumsum(per) + 1L, .(DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU)]
  DT <- DT[
    , .(DebPeriodeDesc = min(ANNEEMOIS),
        FinPeriodeDesc = max(ANNEEMOIS)),
    keyby = .(DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU, per)
  ][, per := NULL]
  DT[
    , DebPeriodeDesc := as_date(paste0(
      str_sub(DebPeriodeDesc, 1, 4), "-",
      str_pad(str_sub(DebPeriodeDesc, 5, 6), width = 2, side = "left", pad = "0"), "-",
      "01"
    ))
  ]
  DT[
    , FinPeriodeDesc := as_date(paste0(
      str_sub(FinPeriodeDesc, 1, 4), "-",
      str_pad(str_sub(FinPeriodeDesc, 5, 6), width = 2, side = "left", pad = "0"), "-",
      "01"
    ))
  ][, FinPeriodeDesc := FinPeriodeDesc %m+% months(1, FALSE) - 1L]
  DT[
    , `:=` (DebPeriodeDesc = format(DebPeriodeDesc, "%Y-%m"),
            FinPeriodeDesc = format(FinPeriodeDesc, "%Y-%m"))
  ]

  setkey(DT, DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}


# SAVE DATASET --------------------------------------------------------------------------------

I_APME_DEM_AUTOR_CRITR_ETEN_CM <- list(
  DES_COURT_INDCN_RECNU = des_court_indcn_recnu()
)
attr(I_APME_DEM_AUTOR_CRITR_ETEN_CM, "MaJ") <- Sys.Date()

use_data(I_APME_DEM_AUTOR_CRITR_ETEN_CM, overwrite = TRUE)
rm(I_APME_DEM_AUTOR_CRITR_ETEN_CM)
