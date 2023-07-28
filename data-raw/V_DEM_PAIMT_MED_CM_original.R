library(usethis)
library(data.table)
library(odbc)
library(stringr)
library(lubridate)
library(inesss)
color_text <- function(x) {return(crayon::italic(crayon::green(x)))}
if (!exists("user")) {
  user <- askpass::askpass("User")
}
if (!exists("pwd")) {
  pwd <- askpass::askpass()
}
conn <- SQL_connexion(user, pwd)


# Fonctions ---------------------------------------------------------------

cod_ahfs <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - COD_AHFS\n"))

  ### Descriptif des codes AHFS
  ahfs_desc <- as.data.table(dbGetQuery(conn, paste0(
    "select NMED_COD_CLA_AHF as AHFS_CLA,\n",
    "       NMED_COD_SCLA_AHF as AHFS_SCLA,\n",
    "       NMED_COD_SSCLA_AHF as AHFS_SSCLA,\n",
    "       NMED_NOM_CLA_AHF as AHFS_NOM_CLA\n",
    "from V_CLA_AHF;"
  )))
  setkey(ahfs_desc)

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    years <- 1996:year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
    years <- min_year:data.table::year(Sys.Date())
  }

  ### Extraction des codes AHFS
  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      dt <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct(SMED_COD_CLA_AHF) as AHFS_CLA,\n",
        "       SMED_COD_SCLA_AHF as AHFS_SCLA,\n",
        "       SMED_COD_SSCLA_AHF as AHFS_SSCLA\n",
        "from V_DEM_PAIMT_MED_CM\n",
        "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
      )))
      dt[, ANNEE := yr]
      DT[[i]] <- copy(dt)
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  setkey(DT, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)
  DT <- DT[
    , .(PremierePrescription = min(ANNEE),
        DernierePrescription = max(ANNEE)),
    .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)
  ]

  ### Ajouter la description
  DT <- ahfs_desc[DT, on = .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]

  setorder(DT, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA,
           na.last = TRUE)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}
cod_denom <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - COD_DENOM\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    years <- 1996:year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
    years <- min_year:data.table::year(Sys.Date())
  }

  ### Descriptions des Denom à ajouter
  denom_desc_query <- paste0(
    "select NMED_COD_DENOM_COMNE as DENOM,\n",
    "       NMED_DD_DENOM_COMNE as DATE_DEBUT,\n",
    "       NMED_DF_DENOM_COMNE as DATE_FIN,\n",
    "       NMED_NOM_DENOM_COMNE as NOM_DENOM\n",
    "from PROD.V_DENOM_COMNE_MED\n",
    "order by DENOM, DATE_DEBUT;"
  )
  DT_desc <- as.data.table(dbGetQuery(conn, denom_desc_query))
  DT_desc[
    , `:=` (DATE_DEBUT = year(DATE_DEBUT),
            DATE_FIN = year(DATE_FIN))
  ]
  # Ceux qui ont juste une ligne : mettre de 1996 à aujourd'hui
  # Permet d'avoir une description pour des codes où les dates ne seraient pas valides
  # en utilisant la seule description possible
  idx <- DT_desc[, .I[.N == 1], .(DENOM)]$V1
  if (length(idx)) {
    DT_desc[idx, `:=` (DATE_DEBUT = 1996, DATE_FIN = year(Sys.Date()))]
  }
  DT_desc <- DT_desc[, .(ANNEE = DATE_DEBUT:DATE_FIN), .(DENOM, NOM_DENOM)]

  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      dt <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct(SMED_COD_DENOM_COMNE) as DENOM\n",
        "from V_DEM_PAIMT_MED_CM\n",
        "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
      )))
      dt[, ANNEE := yr]
      dt <- DT_desc[, .(DENOM, NOM_DENOM, ANNEE)][dt, on = .(DENOM, ANNEE)]
      DT[[i]] <- dt
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  setkey(DT, DENOM, ANNEE)
  DT <- DT[
    , .(PremierePrescription = min(ANNEE),
        DernierePrescription = max(ANNEE)),
    .(DENOM, NOM_DENOM)
  ]

  setorder(DT, DENOM, PremierePrescription, na.last = TRUE)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}
cod_din <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - COD_DIN\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    years <- 1996:year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
    years <- min_year:data.table::year(Sys.Date())
  }

  ### Description des DIN
  DIN_desc <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_DIN as DIN,\n",
    "       NMED_DD_PRODU_MED as DATE_DEBUT,\n",
    "       NMED_DF_PRODU_MED as DATE_FIN,\n",
    "       NMED_NOM_MARQ_COMRC as MARQ_COMRC\n",
    "from V_PRODU_MED;"
  )))
  setkey(DIN_desc, DIN, DATE_DEBUT, DATE_FIN)
  DIN_desc <- DIN_desc[  # conserver le dernier nom, le plus récent
    DIN_desc[, .I[.N], .(DIN)]$V1,
    .(DIN, MARQ_COMRC)
  ]

  ### Trouver la liste des codes pour chaque année
  DT <- vector("list", length(years) * 12)  # contiendra les tableaux des requêtes
  i <- 1L
  for (yr in years) {  # pour chaque année
    for (mth in 1:12) {  # pour chaque mois
      dt <- as.data.table(dbGetQuery(  # liste unique des codes de service pour le mois
        conn, paste0(
          "select distinct SMED_COD_DIN as DIN\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
        )
      ))
      if (nrow(dt)) {
        dt[, ANNEE := yr]  # indiquer l'année
        DT[[i]] <- dt
      } else {
        DT[[i]] <- NULL
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)  # un seul tableau

  ### Indiquer les années où le code est présent
  DT <- DT[
    , .(PremierePrescription = min(ANNEE),
        DernierePrescription = max(ANNEE)),
    .(DIN)
  ]

  ### Ajouter marque commerciale
  DT <- DIN_desc[DT, on = .(DIN)]
  setkey(DT, DIN)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}
cod_serv <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - COD_SERV\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    min_year <- 1996L
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
  }

  ### Description des codes de service
  DT_desc <- as.data.table(dbGetQuery(
    conn, statement = paste0(
      "select	NMED_COD_SERV_MED as COD_SERV,
		          NMED_DES_SERV_MED as COD_SERV_DESC,
		          NMED_DD_PARAM_SERV_MED as DEBUT,
		          NMED_DF_PARAM_SERV_MED as FIN
      from PROD.V_PARAM_SERV_MED;"
    )
  ))
  DT_desc[, `:=` (DEBUT = year(DEBUT), FIN = year(FIN))]
  setkey(DT_desc, COD_SERV, DEBUT, FIN)
  # Joindre les cod+cod_desc qui se chevauchent dans le temps
  DT_desc[, diff := DEBUT - shift(FIN), .(COD_SERV, COD_SERV_DESC)][is.na(diff), diff := 0L]
  DT_desc[, per := 0L][diff > 1, per := 1L][, per := cumsum(per) + 1L]
  DT_desc <- DT_desc[
    , .(DEBUT = min(DEBUT), FIN = max(FIN)),
    .(COD_SERV, COD_SERV_DESC, per)
  ][, per := NULL]
  DT_desc[, diff := DEBUT - shift(FIN), .(COD_SERV, COD_SERV_DESC)]
  idx <- DT_desc[, .I[diff <= 1], .(COD_SERV, COD_SERV_DESC)][["V1"]]
  idx <- idx[!is.na(idx)]
  while (length(idx)) {
    DT_desc[is.na(diff), diff := 0L]
    DT_desc[, per := 0L][diff > 1, per := 1L][, per := cumsum(per) + 1L]
    DT_desc <- DT_desc[
      , .(DEBUT = min(DEBUT), FIN = max(FIN)),
      .(COD_SERV, COD_SERV_DESC, per)
    ][, per := NULL]
    DT_desc[, diff := DEBUT - shift(FIN), .(COD_SERV, COD_SERV_DESC)]
    idx <- DT_desc[, .I[diff <= 1], .(COD_SERV, COD_SERV_DESC)]$V1
    idx <- idx[!is.na(idx)]
  }
  DT_desc[, diff := NULL]

  # Modifier les valeurs uniques pour avoir des descriptions avant 2003
  DT_desc[DT_desc[, .I[1], .(COD_SERV)]$V1, DEBUT := min_year]

  ### Liste des codes de service dans chaque colonne (1 à 3) de SMED_COD_SERV_
  years <- min_year:data.table::year(Sys.Date())
  DT <- vector("list", length(years) * 3)  # contiendra les tableaux des requêtes
  i <- 1L
  for (c_serv in 1:3) {  # codes de service 1 à 3
    for (yr in years) {  # mininum DAT_SERV de V_DEM_PAIMT_MED_CM = 1996-01-01
      dt <- as.data.table(dbGetQuery(  # liste unique des codes de service
        conn, paste0(
          "select distinct SMED_COD_SERV_",c_serv," as COD_SERV\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",paste0(yr,"-01-01"),"' and '",paste0(yr,"-12-31"),"';"
        )
      ))
      if (nrow(dt)) {
        dt <- DT_desc[DEBUT <= yr & yr <= FIN, .(COD_SERV, COD_SERV_DESC)][dt, on = .(COD_SERV)]
        dt[, `:=` (SERV = c_serv, ANNEE = yr)]  # indiquer numéro de service et année
        DT[[i]] <- dt
      } else {
        DT[[i]] <- NULL
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)

  ### Indiquer la période que le code a été utilisé : min(ANNEE)-max(ANNEE)
  DT <- DT[
    , .(PERIODE = paste0(min(ANNEE),"-",max(ANNEE))),
    by = .(COD_SERV, COD_SERV_DESC, SERV)
  ]
  DT <- dcast(DT, COD_SERV + COD_SERV_DESC ~ SERV, value.var = "PERIODE")  # lignes en colonnes
  for (col in 2:3) {  # Créer la colonne du code de service si absente
    if (!paste(col) %in% names(DT)) {
      DT[, paste(col) := NA]
    }
  }

  ### Arrangements finaux
  setnames(DT, paste(1:3), paste0("COD_SERV_",1:3))  # nom des colonnes
  DT[, COD_SERV := str_remove_all(COD_SERV, " ")]  # supprimer espaces

  setorder(DT, COD_SERV, COD_SERV_1, na.last = TRUE)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}
cod_sta_decis <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - COD_STA_DECIS\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    min_year <- 1996L
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
  }


  ### Valeurs uniques
  years <- min_year:year(Sys.Date())
  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      DT[[i]] <- as.data.table(dbGetQuery(
        conn, statement = paste0(
          "select distinct(P.SMED_COD_STA_DECIS) as COD_STA_DECIS,\n",
          "       D.CODE_DES as COD_STA_DESC\n",
          "from V_DEM_PAIMT_MED_CM as P left join V_DES_COD as D\n",
          "    on P.SMED_COD_STA_DECIS = D.CODE_VAL_COD\n",
          "where P.SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"'\n",
          "    and D.CODE_NOM_COD = 'COD_STA_DECIS';"
        )
      ))
      DT[[i]][, ANNEE := yr]
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  setkey(DT, COD_STA_DECIS, ANNEE)

  DT <- DT[
    , .(PremierePrescription = min(ANNEE),
        DernierePrescription = max(ANNEE)),
    .(COD_STA_DECIS, COD_STA_DESC)
  ]

  setkey(DT, COD_STA_DECIS)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}
denom_din_ahfs <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - DENOM_DIN_AHFS\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    years <- 1996:year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
    years <- min_year:data.table::year(Sys.Date())
  }

  ### Description
  # DENOM
  DENOM_desc <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_DENOM_COMNE as DENOM,\n",
    "       NMED_DD_DENOM_COMNE as DATE_DEBUT,\n",
    "       NMED_DF_DENOM_COMNE as DATE_FIN,\n",
    "       NMED_NOM_DENOM_COMNE as NOM_DENOM\n",
    "from PROD.V_DENOM_COMNE_MED\n",
    "order by DENOM, DATE_DEBUT;"
  )))
  setkey(DENOM_desc, DENOM, DATE_DEBUT)
  DENOM_desc <- DENOM_desc[  # conserver le dernier nom, le plus récent
    DENOM_desc[, .I[.N], .(DENOM)]$V1,
    .(DENOM, NOM_DENOM)
  ]

  # DIN
  DIN_desc <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_DIN as DIN,\n",
    "       NMED_DD_PRODU_MED as DATE_DEBUT,\n",
    "       NMED_DF_PRODU_MED as DATE_FIN,\n",
    "       NMED_NOM_MARQ_COMRC as MARQ_COMRC\n",
    "from V_PRODU_MED;"
  )))
  setkey(DIN_desc, DIN, DATE_DEBUT, DATE_FIN)
  DIN_desc <- DIN_desc[  # conserver le dernier nom, le plus récent
    DIN_desc[, .I[.N], .(DIN)]$V1,
    .(DIN, MARQ_COMRC)
  ]

  # AHFS
  AHFS_desc <- as.data.table(dbGetQuery(conn, paste0(
    "select NMED_COD_CLA_AHF as AHFS_CLA,\n",
    "       NMED_COD_SCLA_AHF as AHFS_SCLA,\n",
    "       NMED_COD_SSCLA_AHF as AHFS_SSCLA,\n",
    "       NMED_NOM_CLA_AHF as AHFS_NOM_CLA\n",
    "from V_CLA_AHF;"
  )))
  setkey(AHFS_desc)

  ### Extraction
  years <- min_year:year(Sys.Date())
  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      dt <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select	 distinct(SMED_COD_DENOM_COMNE) as DENOM,\n",
        "        SMED_COD_DIN as DIN,\n",
        "        SMED_COD_CLA_AHF as AHFS_CLA,\n",
        "        SMED_COD_SCLA_AHF as AHFS_SCLA,\n",
        "        SMED_COD_SSCLA_AHF as AHFS_SSCLA\n",
        "from	   V_DEM_PAIMT_MED_CM\n",
        "where 	 SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
      )))
      dt[, ANNEE := yr]
      DT[[i]] <- dt
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  DT <- DT[
    , .(PremierePrescription = min(ANNEE),
        DernierePrescription = max(ANNEE)),
    .(DENOM, DIN, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)
  ]
  setorder(DT, DENOM, DIN, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA,
           na.last = TRUE)

  ### Ajouter les noms (les plus récents)
  DT <- DENOM_desc[DT, on = .(DENOM)]
  DT <- DIN_desc[DT, on = .(DIN)]
  DT <- AHFS_desc[DT, on = .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]

  setcolorder(DT, c("DENOM", "DIN", "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA",
                    "NOM_DENOM", "MARQ_COMRC", "AHFS_NOM_CLA",
                    "PremierePrescription", "DernierePrescription"))
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}
denom_din_teneur_forme <- function() {

  cat(color_text("V_DEM_PAIMT_MED_CM - DENOM_DIN_TENEUR_FORME\n"))

  ### Indiquer si la variable d'itération donne accès à la table complète ou partielle
  name_loop_var <- "SMED_DAT_SERV"
  verif_loop_var <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select distinct SMED_DAT_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  )))
  if (nrow(verif_loop_var)) {
    verif_loop_var <- FALSE
    years <- 1996:year(Sys.Date())
  } else {
    verif_loop_var <- TRUE
    min_year <- dbGetQuery(conn, statement = paste0(
      "select min(extract(year from SMED_DAT_SERV)) as min_date\n",
      "from PROD.V_DEM_PAIMT_MED_CM\n",
      "where SMED_DAT_SERV < '1997-12-31';"
    ))$min_date
    years <- min_year:data.table::year(Sys.Date())
  }

  # Nom des forme
  nom_forme <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_FORME_MED as FORME,\n",
    "   	  NMED_NOM_FORME as NOM_FORME\n",
    "from V_FORME_MED\n",
    "order by FORME;"
  )))
  nom_forme[, FORME := as.integer(FORME)]
  setkey(nom_forme, FORME)

  # Forme des médicaments
  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct\n",
        "    SMED_COD_DENOM_COMNE as DENOM,\n",
        "    SMED_COD_DIN as DIN,\n",
        "    SMED_COD_FORME_MED as FORME\n",
        "from PROD.V_DEM_PAIMT_MED_CM\n",
        "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
      )))
      if (nrow(DT[[i]])) {
        DT[[i]][, ANNEE := yr]
      } else {
        DT[[i]] <- NULL
      }
      i <- i + 1L
    }
  }
  forme <- rbindlist(DT)
  setkey(forme, DENOM, DIN, FORME, ANNEE)
  forme <- unique(forme)
  forme[, FORME := as.integer(FORME)]

  # Ajouter le nom des formes
  forme <- nom_forme[forme, on = .(FORME)]

  # Teneur du médicament
  nom_teneur <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_TENR_MED as TENEUR,\n",
    "       NMED_NOM_TENR as NOM_TENEUR\n",
    "from PROD.V_TENR_MED;"
  )))
  nom_teneur[, TENEUR := as.integer(TENEUR)]
  setkey(nom_teneur, TENEUR)

  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct\n",
        "    SMED_COD_DENOM_COMNE as DENOM,\n",
        "    SMED_COD_DIN as DIN,\n",
        "    SMED_COD_TENR_MED as TENEUR\n",
        "from PROD.V_DEM_PAIMT_MED_CM\n",
        "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
      )))
      if (nrow(DT[[i]])) {
        DT[[i]][, ANNEE := yr]
      } else {
        DT[[i]] <- NULL
      }
      i <- i + 1L
    }
  }
  teneur <- rbindlist(DT)
  teneur[, TENEUR := as.integer(TENEUR)]
  teneur <- unique(teneur)
  setkey(teneur, DENOM, DIN, TENEUR, ANNEE)

  # Ajouter le nom des teneur
  teneur <- nom_teneur[teneur, on = .(TENEUR)]

  ### Merge des formats et des teneur
  DT <- merge(
    forme, teneur,
    by = c("DENOM", "DIN", "ANNEE"),
    all = TRUE
  )
  DT <- DT[
    , .(PremierePrescription = min(ANNEE),
        DernierePrescription = max(ANNEE)),
    .(DENOM, DIN, TENEUR, NOM_TENEUR, FORME, NOM_FORME)
  ]
  setorder(DT, DENOM, DIN, TENEUR, FORME, na.last = TRUE)
  attr(DT, "verif_loop_var") <- verif_loop_var
  attr(DT, "name_loop_var") <- name_loop_var
  return(DT)

}


# Créer dataset -----------------------------------------------------------

V_DEM_PAIMT_MED_CM <- list()
# DENOM_DIN_TENEUR_FORME
V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME <- denom_din_teneur_forme()
# DENOM_DIN_AHFS
V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS <- denom_din_ahfs()
# COD_DENOM_COMNE
V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE <- cod_denom()
# COD_DIN
V_DEM_PAIMT_MED_CM$COD_DIN <- cod_din()
# COD_AHFS
V_DEM_PAIMT_MED_CM$COD_AHFS <- cod_ahfs()
# COD_SERV
V_DEM_PAIMT_MED_CM$COD_SERV <- cod_serv()
# COD_STA_DECIS
V_DEM_PAIMT_MED_CM$COD_STA_DECIS <- cod_sta_decis()

attr(V_DEM_PAIMT_MED_CM, "MaJ") <- Sys.Date()  # date de création


# Save data pour package --------------------------------------------------

use_data(V_DEM_PAIMT_MED_CM, overwrite = TRUE)
rm(V_DEM_PAIMT_MED_CM)
