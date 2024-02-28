# Répertoire commun - INESSS local
if (file.exists("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/fileExist")) {
  devtools::build(path = "J:/GRP/A/5/A/Commun/0 Outils/Librairies R/DER.inesss.tar.gz")
}

# Supprimer le fichier .Rhistory s'il existe
if (file.exists("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/.Rhistory")) {
  file.remove("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/.Rhistory")
}
