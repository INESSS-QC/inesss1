#' Code SQL
#'
#' Générateur de code SQL pour la méthode `naif_switch1`.
#'
#' \strong{Méthode `naif_switch1` :}\cr
#' Statistiques descriptives tirées de la vue `V_DEM_PAIMT_MED_CM`.\cr\cr
#' \strong{\code{codes} :}\cr
#' Inscrire les codes sous la forme d'un nombre entier.\cr\cr
#' \strong{\code{rx_retrospect_a_exclure} :}\cr
#' La période rétrospective est construite à partir des dates de références (date index) et de l’argument `njours_sans_conso` : [INDEX -njours_sans_conso; INDEX -1].\cr\cr
#' \strong{`code_serv_filtre`, `code_list_filtre` :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.
#'
#' @param type_Rx_retro Type de code à exclure. Une valeur parmi :
#' * `'AHFS'` : Code identifiant la classe de médicaments telle que déterminée par l'\emph{American Hospital Formulary Service}.
#' * `'DENOM'` : Code de dénomination commune (`SMED_COD_DENOM_COMNE`).
#' * `'DIN'` : Code d'identification du médicament (`SMED_COD_DIN`).
#' @param rx_retrospect_a_exclure Traitement(s) à inclure dans la période rétrospective. Voir Details. Un individu qui a au moins un traitement durant la période rétrospective ne sera pas considéré comme *naïf* ou *switch*.
#' @param njours_sans_conso Nombre de jours qu’un individu ne doit pas avoir reçu de traitements avant sa date de référence (date index) pour être considéré *naïf* ou *switch*.
#' @inheritParams query_stat_gen1
#'
#' @return Chaîne de caractères à utiliser dans une requête SQL.
#' @encoding UTF-8
#' @export
#' @examples
#' ### Avantages d'utiliser cat()
#' # Sans cat()
#' query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                    type_Rx = 'DIN', codes = c(707503, 707600),
#'                    group_by = 'DIN')
#' # Avec cat()
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DIN', codes = c(707503, 707600),
#'                        group_by = 'DIN'))
#'
#' ### group_by
#' # Aucun
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DIN', codes = c(707503, 707600),
#'                        group_by = NULL))
#' # Tous les group_by
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DIN', codes = c(707503, 707600),
#'                        group_by = c("DENOM", "DIN", "CodeList", "CodeServ", "Teneur", "Format", "Age")))
query_naif_switch1 <- function(
  debut, fin,
  type_Rx = 'DENOM', codes, group_by = 'DENOM',
  type_Rx_retro = NULL, rx_retrospect_a_exclure = NULL,
  njours_sans_conso = 365,
  code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
  code_list = NULL, code_list_filtre = 'Inclusion',
  age_date = NULL,
  ...
) {

  dot_args <- list(...)

  ### Vérification des arguments
  if (!any(names(dot_args) == "verif")) {
    dot_args$verif <- TRUE
  }
  if (dot_args$verif) {
    query_naif_switch1.verif_args(debut, fin, type_Rx, codes, group_by,
                                  type_Rx_retro,
                                  code_serv, code_serv_filtre,
                                  code_list, code_list_filtre,
                                  age_date)
  }

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
    if (type_Rx == "DENOM") {
      rx_retrospect_a_exclure <- stringr::str_pad(rx_retrospect_a_exclure, width = 5,
                                                  side = "left", pad = "0")
    }
  }
  # code_list
  if (!is.null(code_list)) {
    code_list <- stringr::str_pad(code_list, width = 2, side = "left", pad = "0")
  }
  # age_date
  if (any(group_by == "Age") && is.null(age_date)) {
    age_date <- debut
  }

  ### Query
  query <- paste0(
    "with ALL_USER as (\n",
    indent(1),"select\n",
    indent(2),"SMED_NO_INDIV_BEN_BANLS as ID,\n",
    indent(2),"min(SMED_DAT_SERV) as DATE_INDEX\n",
    indent(1),"from V_DEM_PAIMT_MED_CM\n",
    indent(1),"where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
    query_stat_gen1.where_codes(type_Rx, codes, lvl = 2),
    query_stat_gen1.where_code_serv(code_serv, code_serv_filtre, lvl = 2),
    query_stat_gen1.where_code_list(code_list, code_list_filtre, lvl = 2),
    indent(2),"and SMED_NBR_JR_DUREE_TRAIT > 0\n",
    indent(1),"group by ID\n",
    "),\n",
    "OLD_USER as (\n",
    indent(1),"select distinct ID\n",
    indent(1),"from ALL_USER as A left join V_DEM_PAIMT_MED_CM as V\n",
    indent(2),"on A.ID = V.SMED_NO_INDIV_BEN_BANLS\n",
    indent(1),"where SMED_DAT_SERV between (DATE_INDEX - ",njours_sans_conso,") and (DATE_INDEX - 1)\n",
    query_stat_gen1.where_codes(type_Rx, rx_retrospect_a_exclure, lvl = 2),
    query_stat_gen1.where_code_serv(code_serv, code_serv_filtre, lvl = 2),
    query_stat_gen1.where_code_list(code_list, code_list_filtre, lvl = 2),
    indent(2),"and SMED_NBR_JR_DUREE_TRAIT > 0\n",
    "),\n",
    "NAIF_SWITCH_COHORT as (\n",
    indent(1),"select ID from ALL_USER\n",
    indent(1),"except\n",
    indent(1),"select ID from OLD_USER\n",
    ")\n",
    "select\n",
    indent(1),query_stat_gen1.sel_debut(debut),
    indent(1),query_stat_gen1.sel_fin(fin),
    query_stat_gen1.sel_group_by(group_by, age_date),
    indent(1),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",  # montant autorisé pour le médicament
    indent(1),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",  # frais de service autorisé
    indent(1),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",  # somme des montants
    indent(1),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",  # cohorte d'étude (nombre personnes)
    indent(1),"count(*) as NBRE_RX,\n",  # nombre de services/prescriptions
    indent(1),"sum(SMED_QTE_MED) as QTE_MED,\n",  # quantité du médicament ou de la fourniture dispensé
    indent(1),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",  # durée de traitement en jours
    "from V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
    indent(1),"and SMED_NO_INDIV_BEN_BANLS in (select ID from NAIF_SWITCH_COHORT)\n",
    query_stat_gen1.where_codes(type_Rx, rx_retrospect_a_exclure),
    query_stat_gen1.where_code_serv(code_serv, code_serv_filtre),
    query_stat_gen1.where_code_list(code_list, code_list_filtre),
    indent(1),"and SMED_NBR_JR_DUREE_TRAIT > 0\n",
    query_stat_gen1.group_order_by(type_Rx, group_by),
    ";"
  )

  ### Arranger le code au besoin
  if (stringr::str_detect(query, "\n;")) {
    # Supprimer le changement de ligne si le code se termine "\n;"
    stringr::str_sub(query, nchar(query) - 1, nchar(query) - 1) <- ""
  }

  return(query)

}

query_naif_switch1.verif_args <- function(debut, fin, type_Rx, codes, group_by,
                                          type_Rx_retro,
                                          code_serv, code_serv_filtre,
                                          code_list, code_list_filtre,
                                          age_date) {
  ### Vérifier les arguments d'entrée

  check <- newArgCheck()
  vals <- inesss:::fct_values$query_naif_switch1  # Possible values

  # debut & fin
  for (val in c("debut", "fin")) {
    if (length(get(val)) != 1) {
      addError(paste(val, "doit être de longueur 1."), check)
    }
    if (is.na(suppressWarnings(lubridate::as_date(get(val))))) {
      addError(paste(val, "n'est pas au format 'AAAA-MM-JJ'."), check)
    }
  }

  # type_Rx
  if (length(type_Rx) == 1) {
    if (!type_Rx %in% vals$type_Rx) {
      addError("type_Rx ne contient pas une valeur permise.", check)
    }
  } else {
    addError("type_Rx doit être de longueur 1 (une valeur).", check)
  }

  # codes
  if (anyNA(codes)) {
    addError("codes ne peut contenir de NA.", check)
  } else if (type_Rx == "AHFS") {
    if (length(codes) > 1) {
      addError("Un seul code AHFS est permis.", check)
    }
    if (any(nchar(codes) > 6)) {
      addError("codes ne peut contenir plus de 6 caractères lorsque c'est un code AHFS.", check)
    }
  } else {
    suppressWarnings({codes <- as.numeric(codes)})
    if (anyNA(codes)) {
      addError("codes doit contenir des valeurs numériques.", check)
    }
  }

  # type_Rx_retro
  if (!is.null(type_Rx_retro)) {
    if (length(type_Rx_retro) == 1) {
      if (!any(vals$type_Rx_retro == type_Rx_retro)) {
        addError("type_Rx_retro ne contient pas une valeur permise.", check)
      }
    } else {
      addError("type_Rx_retro doit être de longueur 1 (une valeur).", check)
    }
  }

  # group_by
  if (!is.null(group_by) && !all(group_by %in% vals$group_by)) {
    if (length(group_by) == 1) {
      addError("group_by ne contient pas une valeur permise.", check)
    } else {
      addError("group_by contient au moins une valeur non permise.", check)
    }
  }

  # code_serv & code_list
  for (val in c("code_serv", "code_list")) {
    if (anyNA(get(val))) {
      addError(paste(val, "ne peut contenir de NA."), check)
    }
  }

  # code_serv_filtre & code_list_filtre
  for (val in c("code_serv_filtre", "code_list_filtre")) {
    if (length(get(val)) != 1) {
      addError(paste(val, "doit être de longueur 1."), check)
    }
    if (!get(val) %in% vals[[val]]) {
      addError(paste(val, "ne contient pas une valeur permise."), check)
    }
  }

  # age_date
  if (!is.null(age_date)) {
    if (length(age_date) > 1) {
      addError("age_date doit être de longueur 1.", check)
    } else if (is.na(lubridate::as_date(age_date))) {
      addError("age_date n'est pas une date au format 'AAAA-MM-JJ'.", check)
    }
  }

  finishArgCheck(check)

}
