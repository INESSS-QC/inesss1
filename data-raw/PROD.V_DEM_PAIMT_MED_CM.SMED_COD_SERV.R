library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(stringr)
# conn <- sql_connexion("PEI_PRD", askpass("Identifiant ?"), askpass("Mot de passe?"), "latin1")

fct <- function(need_conn = FALSE) {
  if (need_conn) {
    conn <- sql_connexion("PEI_PRD", askpass("Identifiant ?"), askpass("Mot de passe?"), "latin1")
  }

  # Extraire les valeurs de COD_SERV_X pour chaque année de 1996 à aujourd'hui
  DT <- data.table()
  for (i in 1:3) {  # codes de service 1 à 3
    for (yr in 1996:year(Sys.Date())) {  # mininum DAT_SERV de V_DEM_PAIMT_MED_CM = 1996-01-01
      dt <- as.data.table(dbGetQuery(  # liste unique des codes de service
        conn, paste0(
          "select distinct SMED_COD_SERV_",i," as COD_SERV\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",paste0(yr,"-01-01"),"' and '",paste0(yr,"-12-31"),"';"
        )
      ))
      dt <- dt[!is.na(COD_SERV)]

      if (nrow(dt)) {
        dt[, `:=` (SERV = i, ANNEE = yr)]  # indiquer numéro de service et année
        DT <- rbind(DT, dt)
      }
    }
  }
  setkey(DT)

  # Indiquer la période que le code a été utilisé : min(ANNEE)-max(ANNEE)
  COD_SERV <- DT[
    , .(PERIODE = paste0(min(ANNEE),"-",max(ANNEE))),
    by = .(COD_SERV, SERV)
  ]
  COD_SERV <- dcast(COD_SERV, COD_SERV ~ SERV, value.var = "PERIODE")  # lignes en colonnes
  for (col in 2:3) {  # Créer la colonne du code de service si absente
    if (!paste(col) %in% names(COD_SERV)) {
      COD_SERV[, paste(col) := NA]
    }
  }

  # Dataset avec la description de chaque Code de service
  COD_SERV_DESC <- as.data.table(dbGetQuery(
    conn,
    "select distinct NMED_COD_SERV_MED as COD_SERV, NMED_DES_SERV_MED as COD_SERV_DESC
   from PROD.V_PARAM_SERV_MED;"
  ))
  setkey(COD_SERV_DESC, COD_SERV)

  COD_SERV <- merge(COD_SERV, COD_SERV_DESC,  # ajouter les descriptif
                    by = "COD_SERV", all = TRUE)
  setnames(COD_SERV, paste(1:3), paste0("SERV_",1:3))
  COD_SERV[, COD_SERV := str_remove_all(COD_SERV, " ")]
  attr(COD_SERV, "Date") <- Sys.Date()

  return(COD_SERV)
}


PROD.V_DEM_PAIMT_MED_CM.SMED_COD_SERV <- fct()
use_data(PROD.V_DEM_PAIMT_MED_CM.SMED_COD_SERV, overwrite = TRUE)
