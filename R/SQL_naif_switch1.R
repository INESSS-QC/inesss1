#' Naïfs et Switchs
#'
#' Statistiques générales pour un ou des médicaments à partir d'une cohorte consommant ce(s) médicament(s) pour la première fois.\cr
#' Un individu est considéré *naïf* lorsqu'il a un traitement pour la première fois et qu'il n'a jamais eu d'autres traitements *de la même famille*.\cr
#' Un individu est considéré *switch* lorsqu'il a un traitement pour la première fois, mais qu'il a eu un autre traitement dans le passé appartenant *à la même famille*.\cr
#' Vue utilisée : \code{\href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}}.
#'
#' \strong{rx_retrospect_a_exclure :}\cr
#' La période rétrospective est construite à partir des dates de références (index) et de l'argument `njours_sans_conso` : \code{[INDEX - njours_sans_conso; INDEX - 1]}.\cr\cr
#' \strong{code_serv_filtre, code_list_filtre :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @inheritParams query_naif_switch1
#' @return `data.table`
#' * **`DATE_DEBUT` :** Indique la ou les dates de début de la période d'étude.
#' * **`DATE_FIN` :** Indique la ou les dates de fin de la période d'étude.
#' * **`AHFS_CLA` :** Seulement si `group_by` contient `'AHFS'`. Code de la classe AHFS.
#' * **`AHFS_SCLA` :** Seulement si `group_by` contient `'AHFS'`. Code de la sous-classe AHFS.
#' * **`AHFS_SSCLA` :** Seulement si `group_by` contient `'AHFS'`. Code de la sous-sous-classe AHFS.
#' * **`NOM_AHFS` :** Seulement si `group_by` contient `'AHFS'`. Nom de la classe AHFS.
#' * **`DENOM` :** Seulement si `group_by` contient `'DENOM'`. Code de dénomination commune.
#' * **`NOM_DENOM` :** Seulement si `group_by` contient `'DENOM'`. Nom de la dénomination commune.
#' * **`DIN` :** Seulement si `group_by` contient `'DIN'`. Code d'identification du médicament.
#' * **`NOM_MARQ_COMRC` :** Seulement si `group_by` contient `'DIN'`. Nom de la marque commerciale.
#' * **`CODE_SERV` :** Seulement si `group_by` contient `'CodeServ'`. Code de service,
#' * **`CODE_LIST` : ** Seulement si `group_by` contient `'CodeList'`. Code de catégorie de listes de médicaments.
#' * **`TENEUR` :** Seulement si `group_by` contient `'Teneur'`. Teneur du médicament.
#' * **`FORMAT_ACQ` :** Seulement si `group_by` contient `'Format'`. Format d'acquisition du médicament.
#' * **`AGE` :** Seulement si `group_by` contient `'Age'`. Age de l'individu à la date `age_date`.
#' * **`MNT_MED` :** Montant autorisé par la RAMQ pour le médicament ou le produit. Il comprend la part du grossiste (s'il y a lieu) et la part du manufacturier. Voir la variable `SMED_MNT_AUTOR_MED`.
#' * **`MNT_SERV` :** Montant de frais de service autorisé par la RAMQ à la date du service. Voir la variable `SMED_MNT_AUTOR_FRAIS_SERV`.
#' * **`MNT_TOT` :** Somme des variables `MNT_MED` et `MNT_SERV`.
#' * **`COHORTE` :** Nombre d'individus unique.
#' * **`NBRE_RX` :** Nombre de demandes de paiement.
#' * **`QTE_MED` :** Quantité totale des médicaments ou des fournitures dispensés. Voir la variable `SMED_QTE_MED`.
#' * **`DUREE_TX` :** Durée de traitement totale des prescriptions en jours. Voir la variable `SMED_NBR_JR_DUREE_TRAIT`.
#' @encoding UTF-8
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' conn <- SQL_connexion(askpass::askpass('Utilisateur :'), askpass::askpass('Mot de passe :'))
#'
#' ### group_by
#' # Aucun
#' ex01 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135), group_by = NULL
#' )
#' # Tous les group_by
#' ex02 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135),
#'   group_by = c('AHFS', 'DENOM', 'DIN', 'CodeList', 'CodeServ', 'Teneur', 'Format', 'Age')
#' )
#'
#' ### DENOM
#' ex03 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135), group_by = 'DENOM'
#' )
#'
#' ### DIN
#' ex04 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DIN', codes = c(30848, 585092), group_by = 'DIN'
#' )
#'
#' ### Exclusions Rx retrospectif
#' # AHFS
#' ex05 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(47092, 47135), group_by = 'DENOM',
#'   type_Rx_retro = 'AHFS', rx_retrospect_a_exclure = c('04----', '08--16', '122436')
#' )
#'
#' # DENOM
#' ex06 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(47092, 47135), group_by = 'DENOM',
#'   type_Rx_retro = 'DENOM', rx_retrospect_a_exclure = c(47092, 47135, 47136)
#' )
#' # DIN
#' ex07 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = 47092, group_by = c('DENOM', 'DIN'),
#'   type_Rx_retro = 'DIN', rx_retrospect_a_exclure = c(2083523, 2084082, 2240331, 2453312)
#' )
#'
#' ### Age
#' ex08 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DIN', codes = c(30848, 585092), group_by = c('DIN', 'Age'), age_date = '2018-06-05'
#' )
#'
#' ### Exclusion VS Inclusion
#' ex09 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_Rx = 'DENOM', codes = c(39, 47092, 47135), group_by = 'DENOM',
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
#'   code_list = c('40', '41'), code_list_filtre = 'Inclusion'
#' )
#' }
SQL_naif_switch1 <- function(
  conn = NULL, debut, fin,
  type_Rx = 'DENOM', codes, group_by = 'DENOM',
  type_Rx_retro = NULL, rx_retrospect_a_exclure = NULL,
  njours_sans_conso = 365,
  code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
  code_list = NULL, code_list_filtre = 'Inclusion',
  age_date = NULL,
  ...
) {

  ### Arranger les arguments
  # codes
  codes <- sunique(codes)
  if (type_Rx == "DENOM") {
    codes <- stringr::str_pad(codes, width = 5, side = "left", pad = "0")
  }
  # type_Rx_retro
  if (is.null(type_Rx_retro)) {
    type_Rx_retro <- type_Rx
  }
  # rx_retrospect_a_exclure
  if (is.null(rx_retrospect_a_exclure)) {
    rx_retrospect_a_exclure <- codes
  } else {
    rx_retrospect_a_exclure <- sunique(rx_retrospect_a_exclure)
    if (type_Rx_retro == "DENOM") {
      rx_retrospect_a_exclure <- stringr::str_pad(rx_retrospect_a_exclure, width = 5,
                                                  side = "left", pad = "0")
    } else if (type_Rx_retro == "AHFS") {
      rx_retrospect_a_exclure <- stringr::str_pad(rx_retrospect_a_exclure, width = 6,
                                                  side = "left", pad = "0")
    }
  }
  # code_list
  if (!is.null(code_list)) {
    code_list <- stringr::str_pad(code_list, width = 2, side = "left", pad = "0")
  }

  ### Effectuer la connexion si nécessaire
  if (is.null(conn)) {
    conn <- SQL_connexion(uid = askpass::askpass("Identifiant SQL :"))
  }

  ### Requêtes
  if (is.null(conn) || is.null(attr(conn, "info"))) {
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {

    DT <- vector("list", length(debut))
    for (i in 1:length(debut)) {
      dt <- as.data.table(odbc::dbGetQuery(
        conn = conn,
        statement = query_naif_switch1(debut[i], fin[i], type_Rx, codes, group_by,
                                       type_Rx_retro, rx_retrospect_a_exclure, njours_sans_conso,
                                       code_serv, code_serv_filtre, code_list, code_list_filtre,
                                       age_date)
      ))
      if (nrow(dt)) {
        DT[[i]] <- dt
      }
    }

    DT <- rbindlist(DT)

    ### Ajouter le nom des médicaments
    if (nrow(DT)) {
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

#' @title SQL_naif_switch1
#' @description Vérification des arguments.
#' @keywords internal
#' @encoding UTF-8
SQL_naif_switch1.verif_args <- function(
  debut, fin, type_Rx, codes, group_by,
  rx_retrospect_a_exclure, njours_sans_conso,
  code_serv, code_serv_filtre, code_list, code_list_filtre
) {

  vals <- inesss:::fct_values$query_naif_switch1
  check <- newArgCheck()

  # debut, fin
  if (anyNA(debut)) {
    addError("debut ne peut contenir de NA.", check)
  } else {
    if (anyNA(lubridate::as_date(debut))) {
      addError("debut contient au moins une date qui n'est pas au format 'AAAA-MM-JJ'.", check)
    }
  }
  if (anyNA(fin)) {
    addError("fin ne peut contenir de NA.", check)
  } else {
    if (anyNA(lubridate::as_date(fin))) {
      addError("fin contient au moins une date qui n'est pas au format 'AAAA-MM-JJ'.", check)
    }
  }
  if (length(debut) != length(fin)) {
    addError("debut et fin doivent contenir le même nombre de valeurs.", check)
  }

  # type_Rx
  if (length(type_Rx) != 1) {
    addError("type_Rx doit contenir une seule valeur.", check)
  }
  if (!type_Rx %in% vals$type_Rx) {
    addError("type_Rx ne contient pas une valeur permise.", check)
  }

  # codes
  if (length(codes) < 1) {
    addError("codes doit contenir au moins une valeur.", check)
  }

  # group_by
  if (!is.null(group_by) && !all(group_by %in% vals$group_by)) {
    addError("group_by contient au moins une valeur non permise.", check)
  }

  # njours_sans_conso
  if (!is.numeric(njours_sans_conso)) {
    addError("njours_sans_conso doit contenit une valeur numérique.", check)
  }

  # code_serv
  if (!is.null(code_serv) && !code_serv_filtre %in% c("Exclusion", "Inclusion")) {
    addError("code_serv_filtre doit spécifier si c'est une 'Exclusion' ou une 'Inclusion'.", check)
  }

  # code_list
  if (!is.null(code_list) && !code_list_filtre %in% c("Exclusion", "Inclusion")) {
    addError("code_list_filtre doit spécifier si c'est une 'Exclusion' ou un 'Inclusion'.", check)
  }

  finishArgCheck(check)

}
