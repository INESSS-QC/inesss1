library(usethis)
library(data.table)
library(readxl)

# Importation du data
DT <- read_excel("inst/extdata/EstimationProjectionComparable_1996_2041_20200424.xlsx",
                 sheet = "Âge", skip = 4)
DT <- setDT(DT)  # convertir data.table

# Modifier le nom des colonnes
setnames(
  DT,
  c("Niveau géographique", "Code du territoire", "Année", "Type de données",
    "Statut", "Sexe", "Moins un an", "90 ans ou plus"),
  c("GEO", "CODE", "AN", "TYPE", "STATUT", "SEXE", "0", "90")
)

# Conserver les colonnes nécessaires
cols <- c(  # colonnes à conserver
  "GEO", "CODE", "AN", "TYPE", "STATUT", "SEXE",
  0:90  # âges de 0 à 90+
)
dt <- DT[, ..cols]  # sélection des colonnes

# Arrangement des données
# SEXE = Total -> à supprimer
dt <- dt[SEXE != "Total"]
# Code GEO Quebec = 99 -> supprimer code
dt[GEO == "Québec", CODE := NA]
# Statut des données
dt[STATUT == "r", STATUT := "Révisée"][STATUT == "p", STATUT := "Provisoire"]
# M=Masculin, F=Féminin
dt[SEXE == "Masculin", SEXE := "M"][SEXE == "Féminin", SEXE := "F"]

# Colonnes des âges en lignes
dt <- melt(
  dt, id.vars = c("GEO", "CODE", "AN", "TYPE", "STATUT", "SEXE"),
  variable.name = "AGE", value.name = "POP"
)

# Class des colonnes
dt[, `:=` (CODE = as.integer(CODE),
           AN = as.integer(AN),
           AGE = as.integer(levels(AGE)[AGE]),
           POP = as.integer(POP))]

setkey(dt, GEO, CODE, AN, SEXE, AGE)

Pop_QC <- copy(dt)
attr(Pop_QC, "MaJ") <- Sys.Date()
use_data(Pop_QC, overwrite = TRUE)

rm(Pop_QC)
