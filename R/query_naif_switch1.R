#' Code SQL
#'
#' Générateur de code SQL pour la méthode `naif_switch1`.
#'
#' \strong{\code{rx_retrospect_a_exclure} :}\cr
#' La période rétrospective est construite à partir des dates de références (date index) et de l’argument `njours_sans_conso` : \[INDEX\ifelse{html}{-}{\eqn{-}}njours_sans_conso; INDEX\ifelse{html}{-}{\eqn{-}}1\].\cr
#' Inscrire les codes AHFS sous la forme de six (6) caractères. Par exemple, inscrire `040812` revient à chercher la classe 04, la sous-classe 08 et la sous-sous-classe 12. Il est aussi possible de chercher seulement la classe AHFS 04 en inscrivant `04----`. Puisque les deux premiers caractères indique la classe, ceux du milieu la sous-classe et les deux derniers la sous-sous-classe, remplacer une paire de caractères revient à rechercher toutes les classes (ou sous-classe, ou sous-sous-classe).\cr\cr
#' \strong{`code_serv_filtre`, `code_list_filtre` :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.
#'
#' @param debut Date de début de la période d'étude au format `AAAA-MM-JJ` (une seule valeur).
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ` (une seule valeur).
#' @param type_Rx Type de code à analyser. Une valeur parmi :
#' * `'DENOM'` : Code de dénomination commune (`SMED_COD_DENOM_COMNE`).
#' * `'DIN'` : Code d'identification du médicament (`SMED_COD_DIN`).
#' @param codes Le ou les codes à analyser. Voir *Details*.
#' @param group_by Regrouper (aggréger) les résultats par :
#' * `'AHFS'` : Résultats par code de classe AHFS.
#' * `'DENOM'` : Résultats par code de dénomination commune.
#' * `'DIN'` : Résultats par code d'identification du médicament.
#' * `'CodeList'` : Résultats par code de catégories de liste de médicaments.
#' * `'CodeServ'` : Résultats par code de service.
#' * `'Teneur'` : Résultats par teneur du médicament.
#' * `'Format'` : Résultats par format d'acquisition du médicament.
#' * `'Age'` : Résultats par âge à une date précise. Voir argument `age_date`. L'âge est calculé à partir de la date de naissance disponible dans la vue `V_FICH_ID_BEN_CM`.
#' @param type_Rx_retro Type de code à exclure. Si `NULL`, prend la valeur de `type_Rx`. Une valeur parmi :
#' * `'AHFS'` : Code identifiant la classe de médicaments telle que déterminée par l'\emph{American Hospital Formulary Service}.
#' * `'DENOM'` : Code de dénomination commune (`SMED_COD_DENOM_COMNE`).
#' * `'DIN'` : Code d'identification du médicament (`SMED_COD_DIN`).
#' @param rx_retrospect_a_exclure Traitement(s) à inclure dans la période rétrospective. Voir *Details*. Un individu qui a au moins un traitement durant la période rétrospective ne sera pas considéré comme *naïf* ou *switch*.
#' @param njours_sans_conso Nombre de jours qu’un individu ne doit pas avoir reçu de traitements avant sa date de référence (date index) pour être considéré *naïf* ou *switch*.
#' @param code_serv Vecteur de type `character` comprenant le ou les codes de service (`SMED_COD_SERV_1`) à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_serv_filtre `'Inclusion'` ou `'Exclusion'` des codes de service `code_serv`. Inscrire `code_serv = NULL` s'il n'y a pas de filtre à appliquer.
#' @param code_list Vecteur de type `character` comprenant le ou les codes de catégories de listes de médicaments (`SMED_COD_CATG_LISTE_MED`) à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_list_filtre `'Inclusion'` ou `'Exclusion'` des codes de catégories de liste de médicaments `code_list`. Inscrire `code_list = NULL` s'il n'y a pas de filtre à appliquer.
#' @param age_date Date à laquelle on calcule l'âge si `group_by` contient `'Age'`. Si `NULL`, aura pour valeur `debut`.
#'
#' @return Chaîne de caractères à utiliser dans une requête SQL.
#' @keywords internal
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
#' # Age a une date autre que debut
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DIN', codes = c(707503, 707600),
#'                        group_by = c('DENOM', 'Age'),
#'                        age_date = '2018-06-15'))
#'
#' ### DENOM
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = c(39, 47092, 47135),
#'                        group_by = 'DENOM'))
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = c(39, 47092, 47135),
#'                        group_by = c('DENOM', 'DIN')))
#'
#' ### DIN
#' ### Voir sections plus haut 1) Avantages d'utiliser cat() ou 2) group_by
#'
#' ### Exclusions Rx retrospectif
#' # AHFS
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = c(47092, 47135),
#'                        group_by = 'DENOM',
#'                        type_Rx_retro = 'AHFS', rx_retrospect_a_exclure = '040408'))
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = c(47092, 47135),
#'                        group_by = 'DENOM',
#'                        type_Rx_retro = 'AHFS',
#'                        rx_retrospect_a_exclure = c('04----', '08--16', '122436')))
#' # DENOM
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = c(47092, 47135),
#'                        group_by = 'DENOM',
#'                        type_Rx_retro = 'DENOM',
#'                        rx_retrospect_a_exclure = c(47092, 47135, 47136)))
#' # DIN
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = 47092,
#'                        group_by = 'DENOM',
#'                        type_Rx_retro = 'DIN',
#'                        rx_retrospect_a_exclure = c(2083523, 2084082, 2240331, 2453312))
#'
#' ### Age
#' cat(query_naif_switch1(debut = '2018-01-01', fin = '2018-12-31',
#'                        type_Rx = 'DENOM', codes = c(39, 47092, 47135),
#'                        group_by = c('DENOM', 'DIN', 'Age'), age_date = '2018-01-01'))
#'
#' ### Exclusion VS Inclusion
#' cat(query_stat_gen1(debut = '2018-01-01', fin = '2018-12-31',
#'                     type_Rx = 'DENOM', codes = c(47092, 47135), group_by = 'DENOM',
#'                     code_serv_filtre = 'Exclusion', code_serv = c('1', 'AD')))
#' cat(query_stat_gen1(debut = '2018-01-01', fin = '2018-12-31',
#'                     type_Rx = 'DENOM', codes = c(47092, 47135), group_by = 'DENOM',
#'                     code_serv_filtre = 'Inclusion', code_serv = c('1', 'AD')))
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
    } else if (type_Rx == "AHFS") {
      rx_retrospect_a_exclure <- stringr::str_pad(rx_retrospect_a_exclure, width = 6,
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
    query_naif_switch.old_user_where_codes(type_Rx_retro, rx_retrospect_a_exclure),
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
    query_stat_gen1.from(group_by),
    "where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
    indent(1),"and SMED_NO_INDIV_BEN_BANLS in (select ID from NAIF_SWITCH_COHORT)\n",
    query_stat_gen1.where_codes(type_Rx, codes),
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


#' @title query_naif_switch1
#' @description Vérification des arguments
#' @keywords internal
#' @encoding UTF-8
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
    if (!any(vals$type_Rx == type_Rx)) {
      addError("type_Rx ne contient pas une valeur permise.", check)
    }
  } else {
    addError("type_Rx doit être de longueur 1 (une valeur).", check)
  }

  # codes
  if (anyNA(codes)) {
    addError("codes ne peut contenir de NA.", check)
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
#' @title query_naif_switch1
#' @description Générateur de code SQL pour la section du WITH *OLD_USER*, *where*, *codes*.
#' @keywords internal
#' @encoding UTF-8
query_naif_switch.old_user_where_codes <- function(type_Rx_retro, rx_retrospect_a_exclure) {

  if (type_Rx_retro == "DENOM") {
    return(paste0(
      indent(2),
      "and SMED_COD_DENOM_COMNE in (",qu(rx_retrospect_a_exclure),")\n"
    ))
  } else if (type_Rx_retro == "DIN") {
    return(paste0(
      indent(2),
      "and SMED_COD_DIN in (",paste(rx_retrospect_a_exclure, collapse = ", "),")\n"
    ))
  } else if (type_Rx_retro == "AHFS") {
    ### Séparer les codes AHFS selon les colonnes des bases de données
    ahfs_cla <- stringr::str_sub(rx_retrospect_a_exclure, 1, 2)
    ahfs_scla <- stringr::str_sub(rx_retrospect_a_exclure, 3, 4)
    ahfs_sscla <- stringr::str_sub(rx_retrospect_a_exclure, 5, 6)

    ### Construire le code selon les codes demandés
    criteres <- vector("character", length(ahfs_cla))
    for (i in 1:length(ahfs_cla)) {
      if (ahfs_cla[i] == "--") {
        sql_cols <- NULL
        codes_exclus <- NULL
      } else {
        sql_cols <- "SMED_COD_CLA_AHF"
        codes_exclus <- ahfs_cla[i]
      }
      if (ahfs_scla[i] != "--") {
        sql_cols <- c(sql_cols, "SMED_COD_SCLA_AHF")
        codes_exclus <- c(codes_exclus, ahfs_scla[i])
      }
      if (ahfs_sscla[i] != "--") {
        sql_cols <- c(sql_cols, "SMED_COD_SSCLA_AHF")
        codes_exclus <- c(codes_exclus, ahfs_sscla[i])
      }
      sql_cols <- paste(sql_cols, collapse = " || ")
      codes_exclus <- paste(codes_exclus, collapse = "")

      if (length(ahfs_cla) == 1) {
        criteres[i] <- paste0(
          indent(2),"and ",sql_cols," = '",codes_exclus,"'\n"
        )
      } else {
        if (i == 1) {
          criteres[i] <- paste0(
            indent(2),"and (",sql_cols," = '",codes_exclus,"'\n"
          )
        } else if (i == length(ahfs_cla)) {
          criteres[i] <- paste0(
            indent(2),"     ","or ",sql_cols," = '",codes_exclus,"')\n"
          )
        } else {
          criteres[i] <- paste0(
            indent(2),"     ","or ",sql_cols," = '",codes_exclus,"'\n"
          )
        }
      }
    }

    return(paste(criteres, collapse = ""))
  } else {
    stop("query_naif_switch.old_user_where_codes() : type_Rx_retro valeur non permise.")
  }

}
