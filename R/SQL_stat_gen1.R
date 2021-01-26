#' Statistiques générales
#'
#' Tableau indiquant les statistiques générales d'un ou de plusieurs codes de médicaments selon certains critères.
#'
#' Pour se connecter à Teradata, utiliser `conn` ou la combinaison `uid` et `pwd`.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}. Évite d'utiliser les arguments `uid` et `pwd`.
#' @param uid Nom de l'identifiant pour la connexion SQL Teradata.
#' @param pwd Mot de passe associé à l'identifiant. Si `NULL`, le programme demande le mot passe. Cela permet de ne pas afficher le mot de passe dans un script.
#' @inheritParams query_stat_gen1
#'
#' @encoding UTF-8
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' ### GROUP_BY
#' # Aucun group by
#' dt1 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"), fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(39, 47092, 48222), group_by = NULL
#' )
#' # Codes - DENOM
#' dt2 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(39, 47092, 48222), group_by = "Codes"
#' )
#' # Codes - DIN
#' dt3 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DIN", codes = c(30848, 585092), group_by = "Codes"
#' )
#' # Codes et Format
#' dt4 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(39, 47092, 48222), group_by = c("Codes", "Format")
#' )
#' # Teneur
#' dt5 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(39, 47092, 48222), group_by = "Teneur"
#' )
#'
#' ### Exclusion & Inclusion
#' dt6 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(39, 47092, 48222), group_by = "Codes",
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = c("03", "40", "41"), code_list_filtre = "Inclusion"
#' )
#' }
SQL_stat_gen1 <- function(
  conn, uid, pwd,
  debut, fin,
  type_Rx = "DENOM", codes,
  group_by = "Codes",
  code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
  code_list = NULL, code_list_filtre = "Inclusion",
  ...
) {

  if (missing(conn)) {
    conn <- NULL
  }
  if (missing(uid)) {
    uid <- NULL
  }
  if (missing(pwd)) {
    pwd <- NULL
  }

  ### Vérification des arguments
  ### Seulement conn & uid - les autres sont vérifiés dans la fonction query_stat_gen1()
  dot_args <- list(...)
  if (!"verif" %in% names(dot_args)) {
    dot_args$verif <- TRUE  # vérification par défaut
  }
  if (dot_args$verif) {
    SQL_stat_gen1.verif_args(conn, uid, pwd)
  }

  ### Demander le mot de passe si pas inscrit
  if (is.null(conn) && is.null(pwd)) {
    pwd <- askpass::askpass("Quel est votre mot de passe?")
  }

  ### Arranger les arguments
  # codes
  if (type_Rx == "DENOM") {
    # DENOM est une chaîne de caractères de longueur 5
    codes <- stringr::str_pad(codes, width = 5, side = "left", pad = "0")
  }
  # code_list
  if (!is.null(code_list)) {
    # code_list est une chaîne de caractères de longueur 2
    code_list <- stringr::str_pad(code_list, width = 2, side = "left", pad = "0")
  }

  ### Effectuer la connexion si nécessaire
  if (is.null(conn)) {
    conn <- SQL_connexion(uid, pwd)
  }

  ### Effectuer la requête à partir des arguments
  if (is.null(conn)) {
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {
    DT <- vector("list", length(debut))  # contiendra les tableaux résultats
    for (i in 1:length(debut)) {

      # Tableau résultat pour la période i
      dt <- as.data.table(odbc::dbGetQuery(
        conn = conn,
        statement = query_stat_gen1(debut[i], fin[i], type_Rx, codes, group_by,
                                    code_serv, code_serv_filtre,
                                    code_list, code_list_filtre)
      ))

      # Ajouter informations selon le group_by
      dt <- SQL_stat_gen1.infos_query(conn, dt, debut[i], fin[i], type_Rx, codes, group_by)

      # Ajouter nom des médicaments
      dt <- SQL_stat_gen1.nom_med(dt, debut[i], fin[i], type_Rx, group_by)

      # Ajouter le tableau à la liste des résultats
      DT[[i]] <- dt
    }
  }
  DT <- rbindlist(DT)

  ### Ordre des lignes et des colonnes
  DT <- SQL_stat_gen1.cols_order(DT, type_Rx, group_by)
  DT <- SQL_stat_gen1.obs_order(DT, group_by)

  ### Format des colonnes
  if ("DENOM" %in% names(DT)) {
    DT[, DENOM := as.integer(DENOM)]
  }
  if ("CODES_RX" %in% names(DT)) {
    setnames(DT, "CODES_RX", type_Rx)
  }

  return(DT)

}

SQL_stat_gen1.verif_args <- function(conn, uid, pwd) {
  ### Vérification des arguments
  # Seulement les arguments conn, uid et pwd. Les autres sont vérifiés lors de
  # la fonction query_stat_gen1()

  check <- newArgCheck()

  # conn & uid & pwd
  if ((is.null(conn) & is.null(uid)) || (!is.null(conn) & !is.null(uid))) {
    addError("Utiliser la variable conn OU la combinaison uid et pwd.", check)
  }

  finishArgCheck(check)

}
SQL_stat_gen1.infos_query <- function(conn, dt, deb, fin, type_Rx, codes, group_by) {
  ### Statement de la requêtes à exécuter pour la colonne qui indique quels sont
  ### les codes analysés pour chaque ligne de 'group_by'.

  if (!"Codes" %in% group_by) {

    ### Requête à exécuter selon le group_by
    query <- paste0("select distinct(",SQL_stat_gen1.select_type_rx_var(type_Rx),") as CODES,\n")
    if ("Teneur" %in% group_by) {
      query <- paste0(query, indent("select"), "SMED_COD_TENR_MED as TENEUR,\n")
    }
    if ("Format" %in% group_by) {
      query <- paste0(query, indent("select"), "SMED_COD_FORMA_ACQ_MED as FORMAT_ACQ,\n")
    }
    query <- paste0(
      query,
      from_bd.vue("PROD", "V_DEM_PAIMT_MED_CM"),"\n",
      "where SMED_DAT_SERV between '",deb,"' and '",fin,"'\n",
      indent(),"and ",SQL_stat_gen1.select_type_rx_var(type_Rx)," in (",ifelse(type_Rx == "DENOM",qu(codes), paste(codes, collapse = ", ")),")\n",
      "order by CODES;"
    )

    ### Supprimer la virgule au dernier select avant le from
    if (stringr::str_detect(query, ",\nfrom")) {
      stringr::str_sub(query, stringr::str_locate(query, ",\nfrom")[1,][[1]], stringr::str_locate(query, ",\nfrom")[1,][[1]]) <- ""
    }

    ### Requête SQL - codes analysés pour cette période et selon group_by
    infos <- as.data.table(dbGetQuery(conn, query))

    ### Regrouper les codes dans une cellule par période et group_by
    if (type_Rx == "DENOM") {
      infos[, CODES := as.integer(CODES)]  # convertir les DENOM en integer
    }
    if (is.null(group_by)) {
      dt[, CODES_RX := paste(infos$CODES, collapse = "; ")]
    } else {
      dt_by <- names(infos)[!names(infos) %in% "CODES"]  # colonnes servant au merge
      infos <- infos[, .(CODES_RX = paste(CODES, collapse = "; ")), keyby = dt_by]
      dt <- infos[dt, on = dt_by]
    }

  }

  return(dt)

}
SQL_stat_gen1.select_type_rx_var <- function(type_Rx) {
  ### Nom de la variable selon le type Rx

  if (type_Rx == "DENOM") {
    return("SMED_COD_DENOM_COMNE")
  } else if (type_Rx == "DIN") {
    return("SMED_COD_DIN")
  } else {
    stop("SQL_stat_gen1.select_type_rx_var() : type_Rx non permi.")
  }

}
SQL_stat_gen1.nom_med <- function(dt, deb, fin, type_Rx, group_by) {
  ### Ajouter le nom des médicaments

  if ("Codes" %in% group_by) {

    # Base de données avec les nom à inscrire
    if (type_Rx == "DENOM") {
      dt_noms <- inesss::V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE[get(type_Rx) %in% dt[[type_Rx]]]
    } else if (type_Rx == "DIN") {
      dt_noms <- inesss::V_PRODU_MED.NOM_MARQ_COMRC[get(type_Rx) %in% dt[[type_Rx]]]
    }

    # Modifier les dates pour être certain d'avoir un nom aux périodes demandées
    idx <- rmNA(dt_noms[, .I[1], type_Rx]$V1)
    if (length(idx)) {
      dt_noms[idx, DATE_DEBUT := lubridate::as_date(paste0(year(DATE_DEBUT),"-01-01"))]
    }
    idx <- rmNA(dt_noms[, .I[.N], .(DENOM)]$V1)
    if (length(idx)) {
      dt_noms[idx, DATE_FIN := lubridate::as_date(paste0(year(DATE_FIN),"-12-31"))]
    }

    # Conserver la ligne selon les dates de périodes
    dt_noms <- dt_noms[
      DATE_DEBUT <= deb & deb <= DATE_FIN,  # le bon nom selon la période d'étude
      c(type_Rx, nom_type_rx(type_Rx)), with = FALSE  # sélection des colonnes
    ]

    # Ajouter les noms
    dt <- dt_noms[dt, on = type_Rx]

  }

  return(dt)

}
SQL_stat_gen1.cols_order <- function(DT, type_Rx, group_by) {
  ### Ordre des colonnes

  cols <- rmNA(c(
    "DATE_DEBUT", "DATE_FIN",  # période d'étude
    {if ("Codes" %in% group_by) c(type_Rx, nom_type_rx(type_Rx)) else NA},  # médicament + nom
    {if ("Teneur" %in% group_by) "TENEUR" else NA},
    {if ("Format" %in% group_by) "FORMAT_ACQ" else NA},
    "MNT_MED", "MNT_SERV", "MNT_TOT",
    "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX",
    {if (!"Codes" %in% group_by) "CODES_RX" else NA}
  ))
  data.table::setcolorder(DT, cols)

  return(DT)

}
SQL_stat_gen1.obs_order <- function(DT, group_by) {
  ### Ordre des données de DT

  orderv <- c(`1` = "DATE_DEBUT", `-1` = "DATE_FIN")
  if ("Teneur" %in% group_by) {
    orderv <- c(orderv, `1` = "TENEUR")
  }
  if ("Format" %in% group_by) {
    orderv <- c(orderv, `1` = "FORMAT_ACQ")
  }
  setorderv(DT, orderv, order = as.integer(names(orderv)))

  return(DT)

}
