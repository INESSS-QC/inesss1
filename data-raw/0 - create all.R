library(inesss)

user <- askpass::askpass("User")
pwd <- askpass::askpass("Mot de passe")

files <- paste0(c(
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM",
  "V_CLA_AHF",
  "V_DEM_PAIMT_MED_CM",
  "V_DENOM_COMNE_MED",
  "V_FORME_MED",
  "V_PRODU_MED",
  "V_TENR_MED"
), ".R")
files <- paste0("data-raw/", files)

conn <- SQL_connexion(user, pwd)
if (is.null(conn)) {
  stop("User ou Mot de passe erroné.")
} else if (class(conn)[1] == "Teradata") {
  rm(conn)  # suppprimer conn parce qu'on le recrée dans les scripts
  t1 <- Sys.time()
  for (f in files) {
    source(f, local = TRUE, encoding = "UTF-8")
  }
  t2 <- Sys.time()
  difftime(t2, t1)
}
