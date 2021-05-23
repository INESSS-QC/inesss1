#' Statistiques générales
#'
#' Statistiques d'un ou de plusieurs codes de médicaments selon certains critères.\cr
#' Vue utilisée : \code{\href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}}.
#'
#' \strong{debut, fin :}\cr
#' `debut` et `fin` doivent contenir le même nombre de valeurs.\cr\cr
#' \strong{codes :}\cr
#' Si `type_Rx='AHFS'` : codes sous la forme de 6 caractères où les deux premiers caractères représente la classe AHFS, les deux du milieu la sous-classe AHFS et les deux derniers la sous-sous-classe AHFS. Il est possible de remplacer une paire de caractères (\{1, 2\}, \{3, 4\} ou \{5, 6\}) par `'--'` pour rechercher toutes les types de classes. Par exemple, `'04--12'` indique qu'on recherche la classe AHFS 04, toutes les sous-classes AHFS et la sous-sous-classe 12.\cr
#' Sinon : inscrire les codes sous la forme d'un nombre entier.\cr\cr
#' \strong{code_serv_filtre, code_list_filtre :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.\cr\cr
#' \strong{Nom des médicaments :}\cr
#' Que ce soit pour les codes AHFS (`NOM_AHFS`), les DENOM (`NOM_DENOM`) ou les DIN (`NOM_MARQ_COMRC`), **le nom inscrit est toujours celui le plus récent**.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param debut Vecteur contenant la ou les dates de début des périodes d'étude au format `AAAA-MM-JJ`.
#' @param fin Vecteur contenant la ou les dates de fin des périodes d'étude au format `AAAA-MM-JJ`.
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
#' ### group_by
#' # Aucun
#' ex01 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135), group_by = NULL
#' )
#' # Tous les group_by
#' ex02 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135),
#'   group_by = c('AHFS', 'DENOM', 'DIN', 'CodeList', 'CodeServ', 'Teneur', 'Format', 'Age')
#' )
#'
#' ### AHFS
#' ex03 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'AHFS', codes = c('04----', '08--12', '122426'), group_by = 'AHFS'
#' )
#'
#' ### DENOM
#' ex04 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135), group_by = c('DENOM', 'DIN')
#' )
#'
#' ### DIN
#' ex05 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DIN', codes = c(30848, 585092), group_by = 'DIN'
#' )
#'
#' ### Age
#' ex06 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DIN', codes = c(30848, 585092), group_by = c('DIN', 'Age'), age_date = '2018-01-01'
#' )
#'
#' ### Exclusion et Inclusion code_serv et code_list
#' ex07 <- SQL_stat_gen1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135), group_by = 'DENOM',
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
#'   code_list = c('40', '41'), code_list_filtre = 'Inclusion'
#' )
#' }
SQL_stat_gen1 <- function(
  conn = NULL, debut, fin,
  type_Rx = 'DENOM', codes, group_by = 'DENOM',
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

  ### Effectuer la connexion si nécessaire
  if (is.null(conn)) {
    conn <- SQL_connexion(uid = askpass::askpass("Identifiant SQL :"))
  }

  ### Effectuer la requête à partir des arguments
  if (is.null(attr(conn, "info")) || is.null(conn)) {
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {
    DT <- vector("list", length(debut))  # contiendra les tableaux résultats
    for (i in 1:length(debut)) {
      dt <- as.data.table(odbc::dbGetQuery(
        conn = conn,
        statement = query_stat_gen1(debut[i], fin[i], type_Rx, codes, group_by,
                                    code_serv, code_serv_filtre,
                                    code_list, code_list_filtre,
                                    age_date)
      ))
      # Ajouter le tableau à la liste des résultats
      if (nrow(dt)) {
        DT[[i]] <- dt
      }
    }

    DT <- rbindlist(DT)


    if (nrow(DT)) {
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
    } else {
      return(NULL)
    }

  }

}

#' @title SQL_stat_gen1
#' @description Ajouter le nom des médicaments selon le group_by. Voir *Details*.
#' @details DENOM fait afficher le nom du DENOM; DIN fait afficher le nom de la marque commerciale; AHFS fait afficher le nom de la classe AHFS.
#' @keywords internal
#' @encoding UTF-8
#' @import data.table
SQL_stat_gen1.ajout_nom_codes <- function(DT, group_by) {

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
#' @title SQL_stat_gen1
#' @description Nom de la variable selon le type Rx.
#' @keywords internal
#' @encoding UTF-8
SQL_stat_gen1.select_type_rx_var <- function(type_Rx) {

  if (type_Rx == "DENOM") {
    return("SMED_COD_DENOM_COMNE")
  } else if (type_Rx == "DIN") {
    return("SMED_COD_DIN")
  } else {
    stop("SQL_stat_gen1.select_type_rx_var() : type_Rx non permi.")
  }

}
#' @title SQL_stat_gen1
#' @description Ordre des colonnes.
#' @keywords internal
#' @encoding UTF-8
SQL_stat_gen1.cols_order <- function(DT, group_by) {

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
#' @title SQL_stat_gen1
#' @description Ordre des données. 1 = croissant; -1 = décroissant.
#' @keywords internal
#' @encoding UTF-8
SQL_stat_gen1.obs_order <- function(DT, group_by) {

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

  data.table::setorderv(DT, orderv, order = as.integer(names(orderv)))

  return(DT)

}
