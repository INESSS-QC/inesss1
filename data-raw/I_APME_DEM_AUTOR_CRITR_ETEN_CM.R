library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(stringr)
# conn <- SQL_connexion(askpass("User"))

des_court_indcn_recnu <- function() {

  ### Vérifier si la variable d'itération contient une valeur nulle
  verif_iterateur <- dbGetQuery(conn, statement = paste0(
    "select APME_DAT_STA_DEM_PME as DAT_STA_DEM_PME\n",
    "from I_APME_DEM_AUTOR_CRITR_ETEN_CM\n",
    "where APME_DAT_STA_DEM_PME is null;"
  ))
  if (nrow(verif_iterateur)) {
    stop("I_APME_DEM_AUTOR_CRITR_ETEN_CM.des_court_indcn_recnu(): itérateur nulle.")
  } else {
    ### Années à utiliser pour les itérations
    iter_vars <- dbGetQuery(conn, statement = paste0(
      "select min(APME_DAT_STA_DEM_PME) as MIN_DAT,\n",
      "       max(APME_DAT_STA_DEM_PME) as MAX_DAT\n",
      "from I_APME_DEM_AUTOR_CRITR_ETEN_CM;"
    ))

    ### Extraction
    years <- (year(iter_vars$MIN_DAT)):(year(iter_vars$MAX_DAT))
    DT <- vector("list", length(years))
    i <- 1L
    for (yr in years) {
      DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct(NPME_DES_COURT_INDCN_RECNU) as DES_COURT_INDCN_RECNU\n",
        "from I_APME_DEM_AUTOR_CRITR_ETEN_CM\n",
        "where APME_DAT_STA_DEM_PME between '",date_ymd(yr, 1, 1),"' and '",date_ymd(yr, 12, 31),"';"
      )))
      DT[[i]][, ANNEE := yr]
      i <- i + 1L
    }
    DT <- rbindlist(DT)
    setorder(DT, DES_COURT_INDCN_RECNU, ANNEE)
    DT <- DT[
      , .(DEBUT = min(ANNEE),
          FIN = max(ANNEE)),
      .(DES_COURT_INDCN_RECNU)
    ]

    return(DT)
  }

}
no_seq_indcn_recnu <- function() {

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

  return(DT)

}

I_APME_DEM_AUTOR_CRITR_ETEN_CM <- list(
  DES_COURT_INDCN_RECNU = des_court_indcn_recnu(),
  NO_SEQ_INDCN_RECNU_PME = no_seq_indcn_recnu()
)
attr(I_APME_DEM_AUTOR_CRITR_ETEN_CM, "MaJ") <- Sys.Date()

use_data(I_APME_DEM_AUTOR_CRITR_ETEN_CM, overwrite = TRUE)

rm(I_APME_DEM_AUTOR_CRITR_ETEN_CM)
