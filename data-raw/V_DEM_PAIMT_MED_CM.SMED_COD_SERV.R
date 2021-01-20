library(usethis)
library(odbc)
library(data.table)
library(askpass)
library(stringr)
library(inesss)
# conn <- SQL_connexion(askpass("User"))

fct <- function(need_conn = FALSE) {

  ### Effectuer la connexion à Teradata
  if (need_conn) {
    conn <- SQL_connexion(askpass("User"))
  }

  ### Liste des codes de service dans chaque colonne (1 à 3) de SMED_COD_SERV_
  to_year <- 1996:year(Sys.Date())  # années à analyser
  DT <- vector("list", length(to_year) * 3)  # contiendra les tableaux des requêtes
  i <- 1L
  for (c_serv in 1:3) {  # codes de service 1 à 3
    for (yr in 1996:year(Sys.Date())) {  # mininum DAT_SERV de V_DEM_PAIMT_MED_CM = 1996-01-01
      dt <- as.data.table(dbGetQuery(  # liste unique des codes de service
        conn, paste0(
          "select distinct SMED_COD_SERV_",c_serv," as COD_SERV\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",paste0(yr,"-01-01"),"' and '",paste0(yr,"-12-31"),"';"
        )
      ))
      dt <- dt[!is.na(COD_SERV)]
      if (nrow(dt)) {
        dt[, `:=` (SERV = c_serv, ANNEE = yr)]  # indiquer numéro de service et année
        DT[[i]] <- dt
      } else {
        DT[[i]] <- NULL
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  setkey(DT)

  ### Indiquer la période que le code a été utilisé : min(ANNEE)-max(ANNEE)
  DT <- DT[
    , .(PERIODE = paste0(min(ANNEE),"-",max(ANNEE))),
    by = .(COD_SERV, SERV)
  ]
  DT <- dcast(DT, COD_SERV ~ SERV, value.var = "PERIODE")  # lignes en colonnes
  for (col in 2:3) {  # Créer la colonne du code de service si absente
    if (!paste(col) %in% names(DT)) {
      DT[, paste(col) := NA]
    }
  }

  ### Dataset avec la description de chaque Code de service et ajouter à DT
  COD_SERV_DESC <- as.data.table(dbGetQuery(
    conn,
    "select distinct NMED_COD_SERV_MED as COD_SERV, NMED_DES_SERV_MED as COD_SERV_DESC
   from PROD.V_PARAM_SERV_MED;"
  ))
  setkey(COD_SERV_DESC, COD_SERV)
  DT <- merge(DT, COD_SERV_DESC,  # ajouter les descriptif
              by = "COD_SERV", all = TRUE)

  ### Arrangements finaux
  setnames(DT, paste(1:3), paste0("SERV_",1:3))  # nom des colonnes
  DT[, COD_SERV := str_remove_all(COD_SERV, " ")]  # supprimer espaces
  attr(DT, "Date") <- Sys.Date()  # date de création

  return(DT)

}


V_DEM_PAIMT_MED_CM.SMED_COD_SERV <- fct()
use_data(V_DEM_PAIMT_MED_CM.SMED_COD_SERV, overwrite = TRUE)
