library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
library(stringr)
library(writexl)
# conn <- SQL_connexion()


# Fonctions ---------------------------------------------------------------

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


# Créer dataset ----------------------------------------------------------

I_APME_DEM_AUTOR_CRITR_ETEN_CM <- list(
  DES_COURT_INDCN_RECNU = des_court_indcn_recnu(),
  NO_SEQ_INDCN_RECNU_PME = no_seq_indcn_recnu()
)
attr(I_APME_DEM_AUTOR_CRITR_ETEN_CM, "MaJ") <- Sys.Date()


# Nouvelles Valeurs -------------------------------------------------------

new_indcn <- I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU[
  !DES_COURT_INDCN_RECNU %in% inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$DES_COURT_INDCN_RECNU,
  .(DES_COURT_INDCN_RECNU)
]
old_date <- attr(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM, "MaJ")
new_indcn[, Du := old_date]
new_indcn[, Au := Sys.Date()]
if (nrow(new_indcn)) {
  write_xlsx(new_indcn, paste0("C:/Users/ms045/Desktop/saveAuto/new_indcn_",Sys.Date(),".xlsx"))
}

# Envoyer courriel --------------------------------------------------------

if (send_mail && is.character(mail_to) && length(mail_to) >= 1) {
  # des_court_indcn_recnu

  if (nrow(new_indcn)) {
    # Envoyer un courriel
    outlook_mail(to = mail_to,
                 subject = "new_values_INDCN_RECNU",
                 body = paste0("Ceci est un message automatisé.\n\n",
                               "DES_COURT_INDCN_RECNU - Description courte des indications reconnus\n",
                               "Il y a eu des nouvelles valeurs entre le ",
                               old_date," et le ",Sys.Date(),".\n",
                               "Voir le fichier en pièce jointe."),
                 attachments = paste0("C:/Users/ms045/Desktop/saveAuto/new_indcn_",Sys.Date(),".xlsx"))
  } else {
    outlook_mail(to = mail_to,
                 subject = "new_values_INDCN_RECNU",
                 body = paste0("Ceci est un message automatisé.\n\n",
                               "DES_COURT_INDCN_RECNU - Description courte des indications reconnus\n",
                               "Il n'y a pas eu de nouvelle valeurs entre le ",
                               old_date," et le ",Sys.Date(),"."))
  }
}

# Save data pour package --------------------------------------------------

use_data(I_APME_DEM_AUTOR_CRITR_ETEN_CM, overwrite = TRUE)
rm(I_APME_DEM_AUTOR_CRITR_ETEN_CM)
