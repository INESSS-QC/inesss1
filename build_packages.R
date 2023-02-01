
# DER -----------------------------------------------------------------------------------------

if (file.exists("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/fileExist")) {
  devtools::build(path = "J:/GRP/A/5/A/Commun/0 Outils/Librairies R/inesss_DER.tar.gz")
  DER_status <- TRUE
} else {
  DER_status <- FALSE
}



# Statut des installations --------------------------------------------------------------------

if (DER_status) {
  crayon::green("inesss_DER installé sur poste local.\n")
} else {
  crayon::yellow("inesss_DER n'a pas été installé sur le poste local.")
}
