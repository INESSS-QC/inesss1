library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(inesss)
# conn <- SQL_connexion(askpass("User"))

no_seq_indcn_recnu <- function(conn) {

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

  ### Arranger les dates
  for (col in names(DT)[names(DT) != "NO_SEQ_INDCN_RECNU"]) {
    DT[get(col) > Sys.Date(), (col) := Sys.Date()]
    DT[, (col) := year(get(col))]
  }

  ### Afficher la valeur min et la valeur max des dates pour chaque code
  DT <- DT[
    , .(DD_TRAIT_DEM = paste0(min(DD_TRAIT_DEM),"-",max(DD_TRAIT_DEM)),
        DF_TRAIT_DEM = paste0(min(DF_TRAIT_DEM),"-",max(DF_TRAIT_DEM)),
        DD_AUTOR = paste0(min(DD_AUTOR),"-",max(DD_AUTOR)),
        DF_AUTOR = paste0(min(DF_AUTOR),"-",max(DF_AUTOR)),
        DD_APLIC_AUTOR = paste0(min(DD_APLIC_AUTOR),"-",max(DD_APLIC_AUTOR)),
        DF_APLIC_AUTOR = paste0(min(DF_APLIC_AUTOR),"-",max(DF_APLIC_AUTOR)),
        DAT_STA_DEM = paste0(min(DAT_STA_DEM),"-",max(DAT_STA_DEM))),
    .(NO_SEQ_INDCN_RECNU)
  ]

}

I_APME_DEM_AUTOR_CRITR_ETEN_CM <- list(
  NPME_NO_SEQ_INDCN_RECNU_PME = no_seq_indcn_recnu(conn)  # no séquence indication reconnue - PME
)

use_data(I_APME_DEM_AUTOR_CRITR_ETEN_CM, overwrite = TRUE)
