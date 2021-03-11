#' Naïfs et Switchs
#'
#' Statistiques générales pour un ou des médicaments à partir d'une cohorte consommant ce(s) médicament(s) pour la première fois.\cr
#' Un individu est considéré *naïf* lorsqu'il a un traitement pour la première fois et qu'il n'a jamais eu d'autres traitements *de la même famille*.\cr
#' Un individu est considéré *switch* lorsqu'il a un traitement pour la première fois, mais qu'il a eu un autre traitement dans le passé appartenant *à la même famille*.\cr
#' Vue utilisée : \code{\href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}}.
#'
#' \strong{\code{rx_retrospect_a_exclure} :}\cr
#' La période rétrospective est construite à partir des dates de références (index) et de l'argument `njours_sans_conso` : \code{[INDEX - njours_sans_conso; INDEX - 1]}.\cr\cr
#' \strong{`code_serv_filtre`, `code_list_filtre` :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param grouper_par Regrouper (aggréger) les résultats par :
#' * `'Codes'` : Résultats par code analysé.
#' @inheritParams query_naif_switch1
#'
#' @return `data.table` contenant certaines de ces colonnes selon les cas :
#' * `DATE_DEBUT` : Date(s) de début de la période d'étude.
#' * `DATE_FIN` : Date(s) de fin de la période d'étude.
#' * `MNT_MED` : Montant autorisé par la RAMQ pour le médicament ou le produit. Il comprend la part du grossiste (s’il y a lieu) et la part du manufacturier.
#' * `MNT_SERV` : Montant de frais de service autorisé par la RAMQ à la date du service.
#' * `MNT_TOT` : Somme des variables `MNT_MED` et `MNT_SERV`.
#' * `COHORTE` : Nombre d'individus unique.
#' * `NBRE_RX` : Nombre de prescriptions/paiements.
#' * `QTE_MED` : Quantité totale des médicaments ou des fournitures dispensés.
#' * `DUREE_TX` : Durée de traitement totale des prescriptions en jours.
#' * `DENOM` ou `DIN` : Code(s) analysé(s) à l’intérieur de la période d’étude.
#' * `RX_RETROSPECT_A_EXCLURE` : Code(s) de médicament(s) qui n'ont *jamais* été consommé(s) durant la période rétrospective.
#' * `NJOURS_SANS_CONSO` : Nombre de jours qu'un individu ne doit pas avoir reçu de traitements avant sa date de référence (index) pour être considéré comme *naïf* ou *switch*.
#' @encoding UTF-8
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' ### DENOM
#' dt1 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_rx = 'DENOM', codes = c(47873, 47958, 48213), grouper_par = 'Codes',
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' )
#' ### GROUPER_PAR = NULL - Plusieurs codes pour un meme type de traitement
#' dt2 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_rx = 'DENOM', codes = c(47873, 47958, 48213), grouper_par = NULL,
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' )
#' ### DIN
#' dt3 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_rx = 'DIN', codes = 2241927, grouper_par = NULL,
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' )
#' ### RX_RETROSPECT_A_EXCLURE conteant plus de valeurs que CODES (avec ou sans les memes valeurs)
#' dt4 <- SQL_naif_switch1(
#'   conn, debut = c('2018-01-01', '2019-01-01'), fin = c('2018-12-31', '2019-12-31'),
#'   type_rx = 'DIN', codes = c(2257238, 2272903), grouper_par = NULL,
#'   rx_retrospect_a_exclure = c(2042479, 2042487, 2257238, 2272903, 2317192, 2317206),
#'   njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' )
#' }
SQL_naif_switch1 <- function(
  conn, debut, fin,
  type_rx = 'DENOM', codes, grouper_par = NULL,
  rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
  code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
  code_list = NULL, code_list_filtre = 'Inclusion',
  ...
) {

  if (missing(conn)) {
    conn <- NULL
  }
  dot_args <- list(...)

  ### Vérification des arguments
  if (!"verif" %in% names(dot_args)) {
    dot_args$verif <- TRUE  # vérification par défaut
  }
  if (dot_args$verif) {
    SQL_naif_switch1.verif_args(
      debut, fin, type_rx, codes, grouper_par,
      rx_retrospect_a_exclure, njours_sans_conso,
      code_serv, code_serv_filtre, code_list, code_list_filtre
    )
  }

  ### Arranger les arguments
  # codes
  codes <- sunique(codes)
  if (type_rx == "DENOM") {
    codes <- stringr::str_pad(codes, width = 5, side = "left", pad = "0")
  }
  # rx_retrospect_a_exclure
  if (is.null(rx_retrospect_a_exclure)) {
    rx_retrospect_a_exclure <- codes
  } else {
    rx_retrospect_a_exclure <- sunique(rx_retrospect_a_exclure)
    if (type_rx == "DENOM") {
      rx_retrospect_a_exclure <- stringr::str_pad(rx_retrospect_a_exclure, width = 5,
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
    if (!is.null(grouper_par) && grouper_par == "Codes") {
      DT <- vector("list", length(debut) * length(codes))
      k <- 1L
      for (i in 1:length(debut)) {
        for (j in 1:length(codes)) {
          DT[[k]] <- as.data.table(odbc::dbGetQuery(
            conn = conn,
            statement = query_naif_switch1(
              debut = debut[i], fin = fin[i],
              type_rx = type_rx, codes = codes[j],
              rx_retrospect_a_exclure = rx_retrospect_a_exclure,
              njours_sans_conso = njours_sans_conso,
              code_serv = code_serv, code_serv_filtre = code_serv_filtre,
              code_list = code_list, code_list_filtre = code_list_filtre
            )
          ))
          # Ajouter les colonnes des codes analysés
          DT[[k]][, (type_rx) := paste(codes[j], collapse = "; ")]
          DT[[k]][, RX_RETROSPECT_A_EXCLURE := paste(rx_retrospect_a_exclure, collapse = "; ")]
          k <- k + 1L
        }
      }
    } else {
      DT <- vector("list", length(debut))
      for (i in 1:length(debut)) {
        DT[[i]] <- as.data.table(odbc::dbGetQuery(
          conn = conn,
          statement = query_naif_switch1(
            debut = debut[i], fin = fin[i],
            type_rx = type_rx, codes = codes,
            rx_retrospect_a_exclure = rx_retrospect_a_exclure,
            njours_sans_conso = njours_sans_conso,
            code_serv = code_serv, code_serv_filtre = code_serv_filtre,
            code_list = code_list, code_list_filtre = code_list_filtre
          )
        ))
        # Ajouter les colonnes des codes analysés
        DT[[i]][, (type_rx) := paste(codes, collapse = "; ")]
        DT[[i]][, RX_RETROSPECT_A_EXCLURE := paste(rx_retrospect_a_exclure, collapse = "; ")]
        DT[[i]][, NJOURS_SANS_CONSO := njours_sans_conso]
      }
    }
    DT <- rbindlist(DT)
    setorderv(DT, c("DATE_DEBUT", "DATE_FIN", type_rx), order = c(1, -1, 1))

    return(DT)

  }


}

#' @title SQL_naif_switch1
#' @description Vérification des arguments. Les arguments manquants sont vérifiés dans la fonction \code{\link{query_naif_switch1}}.
#' @encoding UTF-8
#' @keywords internal
SQL_naif_switch1.verif_args <- function(
  debut, fin, type_rx, codes, grouper_par,
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

  # type_rx
  if (length(type_rx) != 1) {
    addError("type_rx doit contenir une seule valeur.", check)
  }
  if (!type_rx %in% vals$type_rx) {
    addError("type_rx ne contient pas une valeur permise.", check)
  }

  # codes
  if (length(codes) < 1) {
    addError("codes doit contenir au moins une valeur.", check)
  }
  if (!is.numeric(codes)) {
    addError("codes doit contenir des valeurs numériques.", check)
  }

  # grouper_par
  if (!is.null(grouper_par) && !all(grouper_par %in% vals$grouper_par)) {
    addError("grouper_par contient au moins une valeur non permise.", check)
  }

  # rx_retrospect_a_exclure
  if (!is.null(rx_retrospect_a_exclure) && !is.numeric(rx_retrospect_a_exclure)) {
    addError("rx_retrospect_a_exclure doit contenir une valeur numérique.", check)
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
