library(stringr)

# DER.inesss ----------------------------------------------------------------------------------

## Description ####
file.copy("DESCRIPTION", "DER.inesss/DESCRIPTION", overwrite = TRUE)
desc_file <- readLines("DER.inesss/DESCRIPTION")
desc_file[[1]] <- str_replace(desc_file[[1]], "inesss", "DER.inesss")  # nom package
desc_file[[9]] <- str_replace(desc_file[[9]], "BDCA", "DER")  # description
writeLines(desc_file, "DER.inesss/DESCRIPTION")

## Fonction ####
DER.inesss.fcts <- paste0(c(
  "abrev_mois", "ArgCheck",
  "date_ymd", "DER_inesss_update", "DER_SQL_generateur", "domaine_valeurs",
  "from_bd.vue", "fusion_periodes",
  "indent",
  "qu",
  "rmNA",
  "SQL_connexion",
  "stat_generales_1",
  "unaccent"
),".R")
for (fct in DER.inesss.fcts) {
  file.copy(paste0("R/",fct), paste0("DER.inesss/R/",fct), overwrite = TRUE)
}
# Remplacer les appels {inesss::, inesss:::} par {DER.inesss::, DER.inesss:::}
for (fil in list.files("DER.inesss/R")) {
  script <- readLines(paste0("DER.inesss/R/",fil))
  if (any(str_detect(script, "inesss\\:\\:"))) {
    for (i in 1:length(script)) {
      script[[i]] <- str_replace_all(script[[i]], "inesss\\:\\:", "DER.inesss\\:\\:")
      script[[i]] <- str_replace_all(script[[i]], "inesss\\:\\:\\:", "DER.inesss\\:\\:\\:")
    }
    writeLines(script, paste0("DER.inesss/R/",fil))
  }
}


## Datas ####
DER.inesss.datas <- paste0(c(
  "CIM",
  "DES_COURT_INDCN_RECNU", "domaine_valeurs_fiche_technique",
  "V_CLA_AHF", "V_COD_STA_DECIS",
  "V_DEM_PAIMT_MED_CM", "V_DENOM_COMNE_MED", "V_DES_COD",
  "V_FORME_MED",
  "V_PARAM_SERV_MED", "V_PRODU_MED",
  "V_TENR_MED"
), ".rda")
for (tab in DER.inesss.datas) {
  file.copy(paste0("data/",tab), paste0("DER.inesss/data/",tab), overwrite = TRUE)
}

## Documentation ####
file.copy(
  "Documentation/Installations/DER - Installation librairie R.pdf",
  "DER.inesss/Documentation", overwrite = TRUE
)

## Addins ####
file.copy(
  "inst/rstudio/addins.dcf", "DER.inesss/inst/rstudio",
  overwrite = TRUE
)

