library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(stringr)
library(lubridate)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
conn <- SQL_connexion(user, pwd)


# Fonctions ---------------------------------------------------------------

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
no_seq_indcn_recnu <- function() {

  cat(color_text("I_APME_DEM_AUTOR_CRITR_ETEN_CM - NO_SEQ_INDCN_RECNU_PME\n"))

  ### Extraction data
  DT <- dbGetQuery(
    conn, statement = paste0(
      "select distinct(NPME_NO_SEQ_INDCN_RECNU_PME) as NO_SEQ_INDCN_RECNU,\n",
      "       APME_DD_TRAIT_DEM_PME as DD_TRAIT_DEM,\n",
      "       APME_DF_TRAIT_DEM_PME as DF_TRAIT_DEM,\n",
      "       APME_DD_AUTOR_PME as DD_AUTOR,\n",
      "       APME_DF_AUTOR_PME as DF_AUTOR,\n",
      "       APME_DD_APLIC_AUTOR_PME as DD_APLIC_AUTOR,\n",
      "       APME_DF_APLIC_AUTOR_PME as DF_APLIC_AUTOR,\n",
      "       APME_DAT_STA_DEM_PME as DAT_STA_DEM\n",
      "from I_APME_DEM_AUTOR_CRITR_ETEN_CM;"
    )
  )
  setDT(DT)
  setkey(DT, NO_SEQ_INDCN_RECNU, DD_TRAIT_DEM, DD_AUTOR, DD_APLIC_AUTOR, DAT_STA_DEM)
  DT[, `:=` (DD_TRAIT_DEM = year(DD_TRAIT_DEM),
             DF_TRAIT_DEM = year(DF_TRAIT_DEM),
             DD_AUTOR = year(DD_AUTOR),
             DF_AUTOR = year(DF_AUTOR),
             DD_APLIC_AUTOR = year(DD_APLIC_AUTOR),
             DF_APLIC_AUTOR = year(DF_APLIC_AUTOR),
             DAT_STA_DEM = year(DAT_STA_DEM))]

  ### Afficher la valeur min et la valeur max des dates pour chaque code
  DT <- DT[
    , .(DD_TRAIT_DEM = paste0(min(DD_TRAIT_DEM, na.rm = TRUE),"-",max(DD_TRAIT_DEM, na.rm = TRUE)),
        DF_TRAIT_DEM = paste0(min(DF_TRAIT_DEM, na.rm = TRUE),"-",max(DF_TRAIT_DEM, na.rm = TRUE)),
        DD_AUTOR = paste0(min(DD_AUTOR, na.rm = TRUE),"-",max(DD_AUTOR, na.rm = TRUE)),
        DF_AUTOR = paste0(min(DF_AUTOR, na.rm = TRUE),"-",max(DF_AUTOR, na.rm = TRUE)),
        DD_APLIC_AUTOR = paste0(min(DD_APLIC_AUTOR, na.rm = TRUE),"-",max(DD_APLIC_AUTOR, na.rm = TRUE)),
        DF_APLIC_AUTOR = paste0(min(DF_APLIC_AUTOR, na.rm = TRUE),"-",max(DF_APLIC_AUTOR, na.rm = TRUE)),
        DAT_STA_DEM = paste0(min(DAT_STA_DEM, na.rm = TRUE),"-",max(DAT_STA_DEM, na.rm = TRUE))),
    .(NO_SEQ_INDCN_RECNU)
  ]

  attr(DT, "verif_loop_var") <- NULL
  attr(DT, "name_loop_var") <- NULL
  return(DT)

}


# Créer dataset ----------------------------------------------------------

I_APME_DEM_AUTOR_CRITR_ETEN_CM <- list(
  DES_COURT_INDCN_RECNU = des_court_indcn_recnu(),
  NO_SEQ_INDCN_RECNU_PME = no_seq_indcn_recnu()
)
attr(I_APME_DEM_AUTOR_CRITR_ETEN_CM, "MaJ") <- Sys.Date()


# Fermer la connexion
conn <- odbc::dbDisconnect(conn)

# Save data pour package --------------------------------------------------

use_data(I_APME_DEM_AUTOR_CRITR_ETEN_CM, overwrite = TRUE)
rm(I_APME_DEM_AUTOR_CRITR_ETEN_CM)
