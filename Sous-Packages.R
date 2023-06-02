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
  "date_ymd", "DER_inesss_update", "domaine_valeurs",
  "rmNA",
  "SQL_connexion",
  "unaccent"
),".R")
for (fct in DER.inesss.fcts) {
  file.copy(paste0("R/",fct), paste0("DER.inesss/R/",fct), overwrite = TRUE)
}
# domaine_valeurs
domaine_valeurs_script <- readLines("DER.inesss/R/domaine_valeurs.R")
for (i in 1:length(domaine_valeurs_script)) {
  domaine_valeurs_script[[i]] <- str_replace_all(
    domaine_valeurs_script[[i]],
    "inesss\\:\\:",
    "DER.inesss\\:\\:"
  )
}
writeLines(domaine_valeurs_script, "DER.inesss/R/domaine_valeurs.R")


## Datas ####
DER.inesss.datas <- paste0(c(
  "CIM",
  "domaine_valeurs_fiche_technique",
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM",
  "V_CLA_AHF",
  "V_DEM_PAIMT_MED_CM", "V_DENOM_COMNE_MED",
  "V_FORME_MED",
  "V_PRODU_MED",
  "V_TENR_MED"
), ".rda")
for (tab in DER.inesss.datas) {
  file.copy(paste0("data/",tab), paste0("DER.inesss/data/",tab), overwrite = TRUE)
}

## Documentation ####
file.copy("Documentation/Installations/DER - Installation librairie R.pdf", "DER.inesss/Documentation",
          overwrite = TRUE)
