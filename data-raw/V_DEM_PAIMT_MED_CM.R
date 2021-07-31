library(usethis)
library(data.table)
library(odbc)
library(inesss)
library(askpass)
library(stringr)
library(lubridate)


# Fonctions ---------------------------------------------------------------

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
      DT[[i]] <- dt
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  DT <- DT[
    , .(DEBUT = min(ANNEE),
        FIN = max(ANNEE)),
    .(DENOM, DIN, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)
  ]
  setkey(DT, DENOM, DIN, AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)

  ### Ajouter les noms (les plus récents)
  colorder <- names(DT)
  DT <- DENOM_desc[DT, on = .(DENOM)]
  DT <- DIN_desc[DT, on = .(DIN)]
  DT <- AHFS_desc[DT, on = .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]
  setcolorder(DT, c(colorder, "NOM_DENOM", "MARQ_COMRC", "AHFS_NOM_CLA"))

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
    DT_desc <- DT_desc[, .(ANNEE = DATE_DEBUT:DATE_FIN), .(DENOM, NOM_DENOM)]

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
        dt[, ANNEE := yr]
        dt <- DT_desc[, .(DENOM, NOM_DENOM, ANNEE)][dt, on = .(DENOM, ANNEE)]
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

    setkey(DT, DENOM, DEBUT)
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
      , .(DEBUT = min(ANNEE), FIN = max(ANNEE)),
      .(DIN)
    ]

    ### Ajouter marque commerciale
    DT <- DIN_desc[DT, on = .(DIN)]
    setkey(DT, DIN)

    return(DT)
  }

}
denom_din_teneur <- function() {

  # Nom des teneurs
  nom_teneur <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select NMED_COD_TENR_MED as TENEUR,\n",
    "       NMED_NOM_TENR as NOM_TENEUR\n",
    "from PROD.V_TENR_MED;"
  )))
  setkey(nom_teneur, TENEUR)

  # Table d'analyse
  years <- 1996:year(Sys.Date())
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
        "where SMED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"'\n",
        "    and SMED_COD_TENR_MED is not null;"
      )))
      if (nrow(DT[[i]])) {
        DT[[i]][, ANNEE := yr]
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  DT <- DT[  # Indiquer la 1re et la dernière année inscrite
    , .(DEBUT = min(ANNEE),
        FIN = max(ANNEE)),
    keyby = .(DENOM, DIN, TENEUR)
  ]

  # Ajouter le nom de la teneur
  DT <- nom_teneur[DT, on = .(TENEUR)]
  setcolorder(DT, c("DENOM", "DIN", "TENEUR", "NOM_TENEUR", "DEBUT", "FIN"))
  setkey(DT, DENOM, DIN, TENEUR)

  return(DT)

}
denom_din_format <- function() {

  years <- 1996:year(Sys.Date())
  DT <- vector("list", length(years) * 12)
  i <- 1L
  for (yr in years) {
    for (mth in 1:12) {
      DT[[i]] <- as.data.table(dbGetQuery(conn, statement = paste0(
        "select distinct\n",
        "    SMED_COD_DENOM_COMNE as DENOM,\n",
        "    SMED_COD_DIN as DIN,\n",
        "    SMED_COD_FORMA_ACQ_MED as FORMAT_ACQ\n",
        "from PROD.V_DEM_PAIMT_MED_CM\n",
        "where MED_DAT_SERV between '",date_ymd(yr, mth, 1),"' and '",date_ymd(yr, mth, "last"),"'\n",
        "    and SMED_COD_FORMA_ACQ_MED is not null;"
      )))
      if (nrow(DT[[i]])) {
        DT[[i]][, ANNEE := yr]
      }
      i <- i + 1L
    }
  }
  DT <- rbindlist(DT)
  DT <- DT[
    , .(DEBUT = min(ANNEE),
        FIN = max(ANNEE)),
    .(DENOM, DIN, FORMAT_ACQ)
  ]
  setkey(DT, DENOM, DIN, FORMAT_ACQ)

  return(DT)

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

  setkey(DT, COD_SERV, SERV_1)

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


# Créer dataset -----------------------------------------------------------

V_DEM_PAIMT_MED_CM <- list()
# DENOM_DIN_AHFS
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS <- denom_din_ahfs()
conn <- odbc::dbDisconnect(conn)
# COD_AHFS
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$COD_AHFS <- cod_ahfs()()
conn <- odbc::dbDisconnect(conn)
# COD_DENOM_COMNE
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE <- cod_denom()
conn <- odbc::dbDisconnect(conn)
# COD_DIN
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$COD_DIN <- cod_din()
conn <- odbc::dbDisconnect(conn)
# DENOM_DIN_TENEUR
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR <- denom_din_teneur()
conn <- odbc::dbDisconnect(conn)
# DENOM_DIN_FORMAT
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$DENOM_DIN_FORMAT <- denom_din_format()
conn <- odbc::dbDisconnect(conn)
# COD_SERV
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$COD_SERV <- cod_serv()
conn <- odbc::dbDisconnect(conn)
# COD_STA_DECIS
conn <- SQL_connexion(user, pwd)
V_DEM_PAIMT_MED_CM$COD_STA_DECIS <- cod_sta_decis()
conn <- odbc::dbDisconnect(conn)

attr(V_DEM_PAIMT_MED_CM, "MaJ") <- Sys.Date()  # date de création


# Nouvelles valeurs -------------------------------------------------------

conn <- SQL_connexion(user, pwd)
# Liste des DENOM qui n'étaient pas présents
new_denom <- V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE[, .(DENOM)][
  !inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE[, .(DENOM)], on = .(DENOM)
]
new_denom <- data.table(DENOM = c('00046', '00062', '00067', '46278'))
if (nrow(new_denom)) {
  # Ajouter la 1re date inscrite dans la base de données pour les nouveaux new_denom
  new_denom <- as.data.table(dbGetQuery(conn, statement = paste0(
    "select SMED_COD_DENOM_COMNE as DENOM,\n",
    "       min(SMED_DAT_SERV) as MIN_DATE_SERV\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_COD_DENOM_COMNE in (",qu(new_denom$DENOM),")\n",
    "group by DENOM\n",
    "order by DENOM;"
  )))
  # Ajouter la description des DENOM
  DENOM_desc <- V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE[
    DENOM %in% new_denom$DENOM, .SD[.N], .(DENOM)
  ][, .(DENOM, NOM_DENOM)]
  new_denom <- DENOM_desc[new_denom, on = .(DENOM)][, .(DENOM, MIN_DATE_SERV, NOM_DENOM)]

  old_date <- attr(inesss::V_DEM_PAIMT_MED_CM, "MaJ")
  new_denom[, Du := old_date]
  new_denom[, Au := Sys.Date()]
  write_xlsx(new_denom, paste0("C:/Users/ms045/Desktop/saveAuto/new_denom_",Sys.Date(),".xlsx"))
}

conn <- odbc::dbDisconnect(conn)


# Envoyer courriel --------------------------------------------------------

if (send_mail && is.character(mail_to) && length(mail_to) >= 1) {
  # des_court_indcn_recnu

  if (nrow(new_denom)) {
    # Envoyer un courriel
    outlook_mail(to = mail_to,
                 subject = "new_values_DENOM",
                 body = paste0("Ceci est un message automatisé.\n\n",
                               "DENOM - Codes de dénomination commune\n",
                               "Il y a eu des nouvelles valeurs entre le ",
                               old_date," et le ",Sys.Date(),".\n",
                               "Voir le fichier en pièce jointe."),
                 attachments = paste0("C:/Users/ms045/Desktop/saveAuto/new_denom_",Sys.Date(),".xlsx"))
  } else {
    outlook_mail(to = mail_to,
                 subject = "new_values_DENOM",
                 body = paste0("Ceci est un message automatisé.\n\n",
                               "DENOM - Codes de dénomination commune\n",
                               "Il n'y a pas eu de nouvelle valeurs entre le ",
                               old_date," et le ",Sys.Date(),"."))
  }
}



# Save data pour package --------------------------------------------------

use_data(V_DEM_PAIMT_MED_CM, overwrite = TRUE)
rm(V_DEM_PAIMT_MED_CM)
