#' Statistiques générales
#'
#' Statistiques d'un ou de plusieurs codes de médicaments selon certains critères.\cr
#' Vue utilisée : \code{\href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}}.
#'
#' \strong{\code{debut, fin} :}\cr
#' `debut` et `fin` doivent contenir le même nombre de valeurs.\cr\cr
#' \strong{\code{codes} :}\cr
#' Si `type_Rx='AHFS'` : codes sous la forme de 6 caractères où les deux premiers caractères représente la classe AHFS, les deux du milieu la sous-classe AHFS et les deux derniers la sous-sous-classe AHFS. Il est possible de remplacer une paire de caractères (\{1, 2\}, \{3, 4\} ou \{5, 6\}) par `'--'` pour rechercher toutes les types de classes. Par exemple, `'04--12'` indique qu'on recherche la classe AHFS 04, toutes les sous-classes AHFS et la sous-sous-classe 12.\cr
#' Sinon : inscrire les codes sous la forme d'un nombre entier.\cr\cr
#' \strong{\code{code_serv_filtre, code_list_filtre} :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.\cr\cr
#' \strong{Nom des médicaments :}\cr
#' Que ce soit pour les codes AHFS (`NOM_AHFS`), les DENOM (`NOM_DENOM`) ou les DIN (`NOM_MARQ_COMRC`), **le nom inscrit est toujours celui le plus récent**.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param debut Vecteur contenant la date de début de la ou des périodes d'étude au format `AAAA-MM-JJ`.
#' @param fin Vecteur contenant la date de fin de la ou des périodes d'étude au format `AAAA-MM-JJ`.
#' @param type_Rx Type de code à analyser. Une valeur parmi :
#' * `'AHFS'` : Code identifiant la classe de médicaments telle que déterminée par l'\emph{American Hospital Formulary Service}.
#' * `'DENOM'` : Code de dénomination commune.
#' * `'DIN'` : Code d'identification du médicament.
#' @param codes Le ou les codes à analyser. Voir *Details*.
#' @param group_by Équivalent du *group by* SQL. Regrouper (aggréger) les résultats par :
#' * `'AHFS'` : Code identifiant la classe de médicaments telle que déterminée par l'\emph{American Hospital Formulary Service}.
#' * `'DENOM'` : Code de dénomination commune.
#' * `'DIN'` : Code d'identification du médicament.
#' * `'CodeList'` : Code de catégorie de liste de médicament.
#' * `'CodeServ'` : Code de service.
#' * `'Teneur'` : Teneur du médicament.
#' * `'Format'` : Format d'acquisition du médicament.
#' * `'Age'` : Âge à une date précise. Combiner avec l'argument `age_date`.
#' @param code_serv Le ou les codes de services à exclure ou inclure, sinon inscrire `NULL`. `character`.
#' @param code_serv_filtre `'Exclusion'` ou `'Inclusion'` des codes de services.
#' @param code_list Le ou les codes de catégorie de listes de médicaments à exclure ou inclure, sinon inscrire `NULL`. `character`.
#' @param code_list_filtre `'Exclusion'` ou `'Inclusion'` des codes de catégories de listes de médicaments.
#' @param age_date Date à laquelle on calcul l'âge des individus. À utiliser seulement si `group_by` contient `'Age'`.
#'
#' @return `data.table`
#' @encoding UTF-8
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' conn <- SQL_connexion(askpass::askpass('Utilisateur :'), askpass::askpass('Mot de passe :'))
#'
#' ### Plusieurs periodes d'etude
#' ex01 <- SQL_stat_gen1(conn, debut = c('2016-01-01', '2017-01-01', '2018-01-01'),
#'                       fin = c('2016-12-31', '2017-12-31', '2018-12-31'),
#'                       type_Rx = 'DENOM', codes = 47092)
#'
#' ### Plusieurs codes dans une meme periode
#' ex02 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DENOM', codes = c(39, 47092, 47135))
#' ### Grouper par codes
#' # AHFS
#' ex03 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'AHFS', codes = c(040412, '08----'),
#'                      group_by = 'AHFS')
#' # DENOM
#' ex04 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DENOM', codes = c(39, 47092, 47135),
#'                      group_by = 'DENOM')
#' # DIN
#' ex05 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DIN', codes = c(30848, 585092),
#'                      group_by = 'DIN')
#' # DENOM & DIN
#' ex06 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DENOM', codes = c(47092, 47135),
#'                      group_by = c('DENOM', 'DIN'))
#'
#' ### Grouper par - Autres
#' # Codes de services
#' ex07 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DENOM', codes = 47092,
#'                      group_by = c('DENOM', 'CodeServ'))
#' # Codes de categories de listes de medicaments
#' ex08 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DENOM', codes = 47092,
#'                      group_by = c('DENOM', 'CodeList'))
#' # Teneur et Format d'acquisition du medicament
#' ex09 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                      type_Rx = 'DENOM', codes = 47092,
#'                      group_by = c('DENOM', 'Teneur', 'Format'))
#' # Age au 2018-01-01
#' ex10 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                       type_Rx = 'DENOM', codes = 47092,
#'                       group_by = c('DENOM', 'Age'),
#'                       age_date = '2018-01-01')
#'
#' ### Exclusion VS Inclusion
#' # Codes de services
#' ex11 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                       type_Rx = 'DENOM', codes = 47092,
#'                       code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion')
#' ex12 <- SQL_stat_gen1(conn, debut = '2018-01-01', fin = '2018-12-31',
#'                       type_Rx = 'DENOM', codes = 47092,
#'                       code_serv = c('1', 'AD'), code_serv_filtre = 'Inclusion')
#' }
SQL_stat_gen1 <- function(
  conn, debut, fin,
  type_Rx = 'DENOM', codes,
  group_by = NULL,
  code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
  code_list = NULL, code_list_filtre = 'Inclusion',
  age_date = NULL
) {

  ### Arranger les arguments
  # codes
  if (type_Rx == "DENOM") {
    # DENOM est une chaîne de caractères de longueur 5
    codes <- stringr::str_pad(codes, width = 5, side = "left", pad = "0")
  } else if (type_Rx == "AHFS") {
    codes <- stringr::str_pad(codes, width = 6, side = "left", pad = "0")
  }
  # code_list
  if (!is.null(code_list)) {
    # code_list est une chaîne de caractères de longueur 2
    code_list <- stringr::str_pad(code_list, width = 2, side = "left", pad = "0")
  }

  ### Effectuer la requête à partir des arguments
  if (is.null(attr(conn, "info")) || is.null(conn)) {
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {
    DT <- vector("list", length(debut))  # contiendra les tableaux résultats
    for (i in 1:length(debut)) {
      if (type_Rx == "AHFS") {
        dt <- vector("list", length(codes))
        for (j in 1:length(codes)) {
          sd <- as.data.table(odbc::dbGetQuery(
            conn = conn,
            statement = query_stat_gen1(debut[i], fin[i], type_Rx, codes[j], group_by,
                                        code_serv, code_serv_filtre,
                                        code_list, code_list_filtre,
                                        age_date)
          ))
          if (nrow(sd)) {
            dt[[j]] <- sd
          } else {
            dt[[j]] <- NULL
          }
        }
        dt <- rbindlist(dt)
      } else {
        dt <- as.data.table(odbc::dbGetQuery(
          conn = conn,
          statement = query_stat_gen1(debut[i], fin[i], type_Rx, codes, group_by,
                                      code_serv, code_serv_filtre,
                                      code_list, code_list_filtre,
                                      age_date)
        ))
      }
      # Ajouter le tableau à la liste des résultats
      if (nrow(dt)) {
        DT[[i]] <- dt
      } else {
        DT[[i]] <- NULL
      }
    }

    DT <- rbindlist(DT)

    ### Ajouter les noms des médicaments
    DT <- SQL_stat_gen1.ajout_nom_codes(DT, group_by)

    ### Ordre des lignes et des colonnes
    DT <- SQL_stat_gen1.cols_order(DT, group_by)
    DT <- SQL_stat_gen1.obs_order(DT, group_by)

    ### Format des colonnes
    if (any(names(DT) == "DENOM")) {
      DT[, DENOM := as.integer(DENOM)]
    }

    return(DT)

  }

}

SQL_stat_gen1.ajout_nom_codes <- function(DT, group_by) {
  ### Ajouter le nom des médicaments selon le group_by

  if (any(group_by == "AHFS")) {
    DT <- inesss::V_CLA_AHF[DT, on = .(AHFS_CLA, AHFS_SCLA, AHFS_SSCLA)]
  }
  if (any(group_by == "DENOM")) {
    desc <- inesss::V_DENOM_COMNE_MED[
      inesss::V_DENOM_COMNE_MED[, .I[.N], .(DENOM, NOM_DENOM)]$V1,  # sélectionner la description la plus à jour : dernière dispo
      .(DENOM, NOM_DENOM)
    ]
    DT <- desc[DT, on = .(DENOM)]
  }
  if (any(group_by == "DIN")) {
    desc <- inesss::V_PRODU_MED$NOM_MARQ_COMRC[, .(DIN, NOM_MARQ_COMRC, DATE_DEBUT, DATE_FIN)]
    setkey(desc, DIN, DATE_DEBUT)
    desc <- desc[desc[, .I[.N], .(DIN)]$V1, .(DIN, NOM_MARQ_COMRC)]  # conserver dernière description disponible, la plus à jour
    DT <- desc[DT, on = .(DIN)]
  }

  return(DT)
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
      dt_noms <- inesss::V_DENOM_COMNE_MED[get(type_Rx) %in% dt[[type_Rx]]]
    } else if (type_Rx == "DIN") {
      dt_noms <- inesss::V_PRODU_MED$NOM_MARQ_COMRC[get(type_Rx) %in% dt[[type_Rx]]]
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

    # Conserver la ligne selon les dates de périodes et les colonnes Code+Nom
    dt_noms <- dt_noms[
      DATE_DEBUT <= deb & deb <= DATE_FIN,  # le bon nom selon la période d'étude
      c(type_Rx, nom_type_rx(type_Rx)), with = FALSE  # sélection des colonnes
    ]

    # Ajouter les noms
    dt <- dt_noms[dt, on = type_Rx]

  }

  return(dt)

}
SQL_stat_gen1.cols_order <- function(DT, group_by) {
  ### Ordre des colonnes

  cols <- rmNA(c(
    "DATE_DEBUT", "DATE_FIN",  # période d'étude
    {if (any(group_by == "AHFS")) c("AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA", "NOM_AHFS") else NA},  # AHFS
    {if (any(group_by == "DENOM")) c("DENOM", "NOM_DENOM") else NA},
    {if (any(group_by == "DIN")) c("DIN", "NOM_MARQ_COMRC") else NA},
    {if (any(group_by == "CodeServ")) "CODE_SERV" else NA},
    {if (any(group_by == "CodeList")) "CODE_LIST" else NA},
    {if (any(group_by == "Teneur")) "TENEUR" else NA},
    {if (any(group_by == "Format")) "FORMAT_ACQ" else NA},
    {if (any(group_by == "Age")) "AGE" else NA},
    "MNT_MED", "MNT_SERV", "MNT_TOT",
    "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX",
    {if (any(names(DT) == "CODES_RX")) "CODES_RX" else NA}
  ))
  data.table::setcolorder(DT, cols)

  return(DT)

}
SQL_stat_gen1.obs_order <- function(DT, group_by) {
  ### Ordre des données de DT

  orderv <- c(`1` = "DATE_DEBUT", `-1` = "DATE_FIN")
  if (any(group_by == "AHFS")) {
    orderv <- c(orderv,
                `1` = "AHFS_CLA", `1` = "AHFS_SCLA", `1` = "AHFS_SSCLA")
  }
  if (any(group_by == "DENOM")) {
    orderv <- c(orderv, `1` = "DENOM")
  }
  if (any(group_by == "DIN")) {
    orderv <- c(orderv, `1` = "DIN")
  }
  if (any(group_by == "CodeServ")) {
    orderv <- c(orderv, `1` = "CODE_SERV")
  }
  if (any(group_by == "CodeList")) {
    orderv <- c(orderv, `1` = "CODE_LIST")
  }
  if (any(group_by == "Teneur")) {
    orderv <- c(orderv, `1` = "TENEUR")
  }
  if (any(group_by == "Format")) {
    orderv <- c(orderv, `1` = "FORMAT_ACQ")
  }
  if (any(group_by == "Age")) {
    orderv <- c(orderv, `1` = "AGE")
  }
  setorderv(DT, orderv, order = as.integer(names(orderv)))

  return(DT)

}
