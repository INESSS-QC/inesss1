library(usethis)
library(data.table)
library(odbc)
library(inesss)
library(askpass)
library(stringr)
library(lubridate)
# conn <- SQL_connexion(askpass("User"))

denom_din_ahfs <- function() {
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
  DENOM_desc[, `:=` (DATE_DEBUT = year(DATE_DEBUT), DATE_FIN = year(DATE_FIN))]
  # Ceux qui ont juste une ligne : mettre de 1996 à aujourd'hui
  # Permet d'avoir une description pour des codes où les dates ne seraient pas valides
  # en utilisant la seule description possible
  idx <- DENOM_desc[, .I[.N == 1], .(DENOM)]$V1
  if (length(idx)) {
    DENOM_desc[idx, `:=` (DATE_DEBUT = 1996, DATE_FIN = year(Sys.Date()))]
  }

  # DIN
  DIN_desc <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_DIN as DIN,\n",
    "       NMED_DD_PRODU_MED as DATE_DEBUT,\n",
    "       NMED_DF_PRODU_MED as DATE_FIN,\n",
    "       NMED_NOM_MARQ_COMRC as MARQ_COMRC\n",
    "from V_PRODU_MED;"
  )))
  DIN_desc[, `:=` (DATE_DEBUT = year(DATE_DEBUT), DATE_FIN = year(DATE_FIN))]
  setkey(DIN_desc, DIN, DATE_DEBUT, DATE_FIN)
  # Supprimer les doublons descriptif
  DIN_desc <- unique(DIN_desc, by = c("DIN", "MARQ_COMRC"))
  # Regrouper les années qui se chevauchent avec la même description
  idx <- DIN_desc[, .I[.N >= 2], .(DIN, MARQ_COMRC)]$V1
  if (length(idx)) {
    DIN_desc[
      idx,
      diff := DATE_DEBUT - shift(DATE_FIN),
      .(DIN, MARQ_COMRC)
    ][is.na(diff), diff := 0L]
    DIN_desc[, period := 0L][diff > 1, period := 1L]
    DIN_desc[, period := cumsum(period) + 1L, .(DIN, MARQ_COMRC)]
    DIN_desc <- DIN_desc[
      , .(DATE_DEBUT = min(DATE_DEBUT),
          DATE_FIN = max(DATE_FIN)),
      .( DIN, MARQ_COMRC, period)
    ][, period := NULL]
  }
  # Arranger les années de ceux qui ont seulement une observation pour associer
  # cette unique descriptio à tous
  idx <- DIN_desc[, .I[.N == 1], .(DIN, MARQ_COMRC)]$V1
  if (length(idx)) {
    DIN_desc[idx, `:=` (DATE_DEBUT = 1996, DATE_FIN = year(Sys.Date()))]
  }
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
  to_year <- 1996:year(Sys.Date())
  DT <- vector("list", length(to_year) * 12)
  i <- 1L
  for (yr in to_year) {
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
      dt <- DENOM_desc[DATE_DEBUT <= yr & yr <= DATE_FIN, .(DENOM, NOM_DENOM)][dt, on = .(DENOM)]  # ajout DENOM desc
      dt <- DIN_desc[DATE_DEBUT <= yr & yr <= DATE_FIN, .(DIN, MARQ_COMRC)][dt, on = .(DIN)]  # ajouty DIN desc
      dt <- AHFS_desc[dt, on = .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]
      DT[[i]] <- copy(dt)
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  DT <- DT[
    , .(DEBUT = min(ANNEE),
        FIN = max(ANNEE)),
    .(DENOM, DIN, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA, NOM_DENOM, MARQ_COMRC, AHFS_NOM_CLA)
  ]
  setkey(DT, DENOM, DIN, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)

  return(DT)

}
cod_ahfs <- function() {
  ### Descriptif des codes AHFS
  ahfs_desc <- as.data.table(dbGetQuery(conn, paste0(
    "select NMED_COD_CLA_AHF as AHFS_CLA,\n",
    "       NMED_COD_SCLA_AHF as AHFS_SCLA,\n",
    "       NMED_COD_SSCLA_AHF as AHFS_SSCLA,\n",
    "       NMED_NOM_CLA_AHF as AHFS_NOM_CLA\n",
    "from V_CLA_AHF;"
  )))
  setkey(ahfs_desc)

  ### Extraction des codes AHFS
  to_year <- 1996:year(Sys.Date())
  DT <- vector("list", length(to_year) * 12)
  i <- 1L
  for (yr in to_year) {
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
    , .(DEBUT = min(ANNEE),
        FIN = max(ANNEE)),
    .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)
  ]

  ### Ajouter la description
  DT <- ahfs_desc[DT, on = .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]

  setkey(DT, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)

  return(DT)

}
cod_denom <- function() {

  ### Vérifier si la variable d'itération contient une valeur nulle
  verif_iter <- dbGetQuery(conn, statement = paste0(
    "select distinct(smed_dat_serv) as DATE_SERV\n",
    "from V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  ))
  if (nrow(verif_iter)) {
    stop("V_DEM_PAIMT_MED_CM.cod_denom(): iter nulle.")
  } else {
    ### Descriptions des Denom à ajouter
    query <- paste0(
      "select NMED_COD_DENOM_COMNE as DENOM,\n",
      "       NMED_DD_DENOM_COMNE as DATE_DEBUT,\n",
      "       NMED_DF_DENOM_COMNE as DATE_FIN,\n",
      "       NMED_NOM_DENOM_COMNE as NOM_DENOM\n",
      "from PROD.V_DENOM_COMNE_MED\n",
      "order by DENOM, DATE_DEBUT;"
    )
    DT_desc <- as.data.table(dbGetQuery(conn, query))
    DT_desc[, `:=` (DATE_DEBUT = year(DATE_DEBUT), DATE_FIN = year(DATE_FIN))]
    # Ceux qui ont juste une ligne : mettre de 1996 à aujourd'hui
    # Permet d'avoir une description pour des codes où les dates ne seraient pas valides
    # en utilisant la seule description possible
    idx <- DT_desc[, .I[.N == 1], .(DENOM)]$V1
    if (length(idx)) {
      DT_desc[idx, `:=` (DATE_DEBUT = 1996, DATE_FIN = year(Sys.Date()))]
    }

    to_year <- 1996:year(Sys.Date())
    DT <- vector("list", length(to_year) * 12)
    i <- 1L
    for (yr in to_year) {
      for (mth in 1:12) {
        dt <- as.data.table(dbGetQuery(conn, statement = paste0(
          "select distinct(SMED_COD_DENOM_COMNE) as DENOM\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"';"
        )))
        dt <- DT_desc[DATE_DEBUT <= yr & yr <= DATE_FIN, .(DENOM, NOM_DENOM)][dt, on = .(DENOM)]
        dt[, ANNEE := yr]
        DT[[i]] <- dt
        i <- i + 1L
      }
    }
    DT <- rbindlist(DT)
    setkey(DT, DENOM, ANNEE)
    DT <- DT[
      , .(DEBUT = min(ANNEE),
          FIN = max(ANNEE)),
      .(DENOM, NOM_DENOM)
    ]

    setkey(DT, DENOM)
    return(DT)
  }

}
cod_din <- function() {
  ### Vérifier si la variable d'itération contient une valeur nulle
  verif_iter <- dbGetQuery(conn, statement = paste0(
    "select distinct(smed_dat_serv) as DATE_SERV\n",
    "from V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV is null;"
  ))
  if (nrow(verif_iter)) {
    stop("V_DEM_PAIMT_MED_CM.cod_denom(): iter nulle.")
  } else {
    ### Description des DENOM
    query <- paste0(
      "select NMED_COD_DENOM_COMNE as DENOM,\n",
      "       NMED_DD_DENOM_COMNE as DATE_DEBUT,\n",
      "       NMED_DF_DENOM_COMNE as DATE_FIN,\n",
      "       NMED_NOM_DENOM_COMNE as NOM_DENOM\n",
      "from PROD.V_DENOM_COMNE_MED\n",
      "order by DENOM, DATE_DEBUT;"
    )
    DENOM_desc <- as.data.table(dbGetQuery(conn, query))
    setkey(DENOM_desc, DENOM, DATE_DEBUT)
    DENOM_desc[, `:=` (DATE_DEBUT = year(DATE_DEBUT), DATE_FIN = year(DATE_FIN))]
    # Ceux qui ont juste une ligne : mettre de 1996 à aujourd'hui
    # Permet d'avoir une description pour des codes où les dates ne seraient pas valides
    # en utilisant la seule description possible
    idx <- DENOM_desc[, .I[.N == 1], .(DENOM)]$V1
    if (length(idx)) {
      DENOM_desc[idx, `:=` (DATE_DEBUT = 1996, DATE_FIN = year(Sys.Date()))]
    }
    ### Description des DIN
    DIN_desc <- as.data.table(dbGetQuery(conn, statement = paste0(
      "select NMED_COD_DENOM_COMNE as DENOM,\n",
      "       NMED_COD_DIN as DIN,\n",
      "       NMED_DD_PRODU_MED as DATE_DEBUT,\n",
      "       NMED_DF_PRODU_MED as DATE_FIN,\n",
      "       NMED_NOM_MARQ_COMRC as MARQ_COMRC\n",
      "from V_PRODU_MED;"
    )))
    DIN_desc[, `:=` (DATE_DEBUT = year(DATE_DEBUT), DATE_FIN = year(DATE_FIN))]
    setkey(DIN_desc, DENOM, DIN, DATE_DEBUT, DATE_FIN)
    # Supprimer les doublons descriptif
    DIN_desc <- unique(DIN_desc, by = c("DENOM", "DIN", "MARQ_COMRC"))
    # Regrouper les années qui se chevauchent avec la même description
    idx <- DIN_desc[, .I[.N >= 2], .(DENOM, DIN, MARQ_COMRC)]$V1
    if (length(idx)) {
      DIN_desc[
        idx,
        diff := DATE_DEBUT - shift(DATE_FIN),
        .(DENOM, DIN, MARQ_COMRC)
      ][is.na(diff), diff := 0L]
      DIN_desc[, period := 0L][diff > 1, period := 1L]
      DIN_desc[, period := cumsum(period) + 1L, .(DENOM, DIN, MARQ_COMRC)]
      DIN_desc <- DIN_desc[
        , .(DATE_DEBUT = min(DATE_DEBUT),
            DATE_FIN = max(DATE_FIN)),
        .(DENOM, DIN, MARQ_COMRC, period)
      ][, period := NULL]
    }
    # Arranger les années de ceux qui ont seulement une observation pour associer
    # cette unique descriptio à tous
    idx <- DIN_desc[, .I[.N == 1], .(DENOM, DIN, MARQ_COMRC)]$V1
    if (length(idx)) {
      DIN_desc[idx, `:=` (DATE_DEBUT = 1996, DATE_FIN = year(Sys.Date()))]
    }

    ### Description des codes
    DT_desc <- DENOM_desc[, .(DENOM, NOM_DENOM)][DIN_desc, on = .(DENOM)]
    DT_desc <- DT_desc[, .(DIN, NOM_DENOM, MARQ_COMRC, DEBUT = DATE_DEBUT, FIN = DATE_FIN)]
    rm(DENOM_desc, DIN_desc)


    ### Trouver la liste des codes pour chaque année
    to_year <- 1996:year(Sys.Date())  # année à analyser
    DT <- vector("list", length(to_year) * 12)  # contiendra les tableaux des requêtes
    i <- 1L
    for (yr in to_year) {  # pour chaque année
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
          dt <- DT_desc[DEBUT <= yr & yr <= FIN, .(DIN, NOM_DENOM, MARQ_COMRC)][dt, on = .(DIN)]
          DT[[i]] <- dt
        } else {
          DT[[i]] <- NULL
        }
        i <- i + 1L
      }
    }
    DT <- rbindlist(DT)  # un seul tableau
    setkey(DT, DIN, ANNEE)  # trier par DIN + ANNEE

    ### Indiquer les années où le code est présent
    DT <- DT[
      ,.(DEBUT = min(ANNEE), FIN = max(ANNEE)),
      .(DIN, NOM_DENOM, MARQ_COMRC)
    ]

    setkey(DT, DIN)

    return(DT)
  }

}
cod_serv <- function() {

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
  # Modifier les valeurs pour avoir des descriptions avant 2003
  DT_desc[DT_desc[, .I[1], .(COD_SERV)]$V1, DEBUT := 1996L]

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
  setnames(DT, paste(1:3), paste0("SERV_",1:3))  # nom des colonnes
  DT[, COD_SERV := str_remove_all(COD_SERV, " ")]  # supprimer espaces

  setkey(DT, COD_SERV)

  return(DT)

}
cod_sta_decis <- function() {
  ### Valeurs uniques

  years <- 1996:year(Sys.Date())
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
    , .(DEBUT = min(ANNEE),
        FIN = max(ANNEE)),
    .(COD_STA_DECIS, COD_STA_DESC)
  ]
  setkey(DT, COD_STA_DECIS)

  return(DT)

}


# SAVE --------------------------------------------------------------------

V_DEM_PAIMT_MED_CM <- list()
V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS <- denom_din_ahfs()
V_DEM_PAIMT_MED_CM$COD_AHFS <- cod_ahfs()
V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE <- cod_denom()
V_DEM_PAIMT_MED_CM$COD_DIN <- cod_din()
V_DEM_PAIMT_MED_CM$COD_SERV <- cod_serv()
V_DEM_PAIMT_MED_CM$COD_STA_DECIS <- cod_sta_decis()

attr(V_DEM_PAIMT_MED_CM, "MaJ") <- Sys.Date()  # date de création

use_data(V_DEM_PAIMT_MED_CM, overwrite = TRUE)
