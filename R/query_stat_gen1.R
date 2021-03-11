#' Code SQL
#'
#' Générateur de code SQL pour la méthode `stat_gen1`.
#'
#' \strong{Méthode `stat_gen1` :}\cr
#' Statistiques descriptives tirées de la vue `V_DEM_PAIMT_MED_CM`.\cr\cr
#' \strong{`group_by` :} Revient à utiliser la commande `group by` dans le code SQL.\cr\cr
#' \strong{`code_serv_filtre`, `code_list_filtre` :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.
#'
#' @param debut Date de début de la période d'étude au format `AAAA-MM-JJ` (une seule valeur).
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ` (une seule valeur).
#' @param type_Rx Indique le type de code analysé :
#' * `'DENOM'` : Code de dénomination commune (`SMED_COD_DENOM_COMNE`).
#' * `'DIN'` : Code d'identification du médicament (`SMED_COD_DIN`).
#' @param codes Vecteur comprenant le ou les codes d'analyse au format numérique, sans zéros.
#' @param group_by Regrouper (aggréger) les résultats par :
#' * `'Codes'` : Résultats par code analysé.
#' * `'Teneur'` : Résultats par teneur de médicament (`SMED_COD_TENR_MED`) incluant les valeurs absentes.
#' * `'Format'` : Résultats par format d'acquisition du médicament (`SMED_COD_FORMA_ACQ_MED`) inclouant les valeurs absentes.
#' @param code_serv Vecteur de type `character` comprenant le ou les codes de service (`SMED_COD_SERV_1`) à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_serv_filtre `'Inclusion'` ou `'Exclusion'` des codes de service `code_serv`. Inscrire `code_serv = NULL` s'il n'y a pas de filtre à appliquer.
#' @param code_list Vecteur de type `character` comprenant le ou les codes de catégories de liste de médicaments (`SMED_COD_CATG_LISTE_MED`) à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_list_filtre `'Inclusion'` ou `'Exclusion'` des codes de catégories de liste de médicaments `code_list`. Inscrire `code_list = NULL` s'il n'y a pas de filtre à appliquer.
#'
#' @return Chaîne de caractères à utiliser dans une requête SQL.
#' @encoding UTF-8
#' @export
#' @examples
#' ### Avantages d'utiliser cat() si c'est pour afficher du code dans la console.
#' # Avec cat()
#' cat(query_stat_gen1(
#'   debut = '2020-01-01', fin = '2020-12-31',
#'   type_Rx = 'DENOM', codes = c(47092, 47135, 48222), group_by = 'Codes',
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion'
#' ))
#' # Sans cat()
#' query_stat_gen1(
#'   debut = '2020-01-01', fin = '2020-12-31',
#'   type_Rx = 'DENOM', codes = c(47092, 47135, 48222), group_by = 'Codes',
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion'
#' )
#'
#' ### GROUP_BY
#' # group_by = NULL : resultats par periode d'etude.
#' cat(query_stat_gen1(
#'   debut = '2020-01-01', fin = '2020-12-31',
#'   type_Rx = 'DENOM', codes = c(47092, 47135, 48222),
#'   group_by = NULL,
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion'
#' ))
#' # group_by = 'Codes' : resultats par code d'analyse.
#' cat(query_stat_gen1(
#'   debut = '2020-01-01', fin = '2020-12-31',
#'   type_Rx = 'DENOM', codes = c(47092, 47135, 48222),
#'   group_by = 'Codes',
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion'
#' ))
#' # group_by = c('Teneur', 'Format') : resultats par teneur et format.
#' cat(query_stat_gen1(
#'   debut = '2020-01-01', fin = '2020-12-31',
#'   type_Rx = 'DENOM', codes = c(47092, 47135, 48222),
#'   group_by = c('Teneur', 'Format'),
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion'
#' ))
#'
#' ### Exclusion VS Inclusion
#' cat(query_stat_gen1(
#'   debut = '2020-01-01', fin = '2020-12-31',
#'   type_Rx = 'DENOM', codes = c(47092, 47135, 48222),
#'   group_by = NULL,
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
#'   code_list = c('40', '41'), code_list_filtre = 'Inclusion'
#' ))
query_stat_gen1 <- function(
  debut, fin,
  type_Rx = 'DENOM', codes,
  group_by = 'Codes',
  code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
  code_list = NULL, code_list_filtre = 'Inclusion',
  ...
) {

  ### Vérification des arguments
  dot_args <- list(...)
  if (!"verif" %in% names(dot_args)) {  # créer dot_args$verif si n'existe pas
    dot_args$verif <- TRUE  # vérification par défaut
  }
  if (dot_args$verif) {  # effectuer la vérification
    query_stat_gen1.verif_args(debut, fin, type_Rx, codes, group_by,
                               code_serv, code_serv_filtre,
                               code_list, code_list_filtre)
  }

  ### Arranger les arguments
  # codes
  codes <- (sunique(codes))  # tri croissant + valeurs uniques
  if (type_Rx == "DENOM") {
    # Code Denom doit être une chaine de caractères de longueur 5
    codes <- stringr::str_pad(codes, width = 5, side = "left", pad = "0")
  }

  ### Arranger les codes de liste de catégorie de médicaments
  if (!is.null(code_list)) {
    # Code List doit être une chaine de caractères de longueur 2
    code_list <- stringr::str_pad(code_list, width = 2, side = "left", pad = "0")
  }

  ### Code Query à générer selon résultat
  query <- paste0(
    # SELECT
    "select ",query_stat_gen1.sel_debut(debut),  # date de début
    indent("select"),query_stat_gen1.sel_fin(fin),  # date de fin
    query_stat_gen1.sel_code(group_by, type_Rx),  # si par code
    query_stat_gen1.sel_teneur(group_by),  # si par teneur
    query_stat_gen1.sel_format(group_by),  # si par format
    indent("select"),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",  # montant autorisé pour le médicament
    indent("select"),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",  # frais de service autorisé
    indent("select"),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",  # somme des montants
    indent("select"),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",  # cohorte d'étude (nombre personnes)
    indent("select"),"count(*) as NBRE_RX,\n",  # nombre de services/prescriptions
    indent("select"),"sum(SMED_QTE_MED) as QTE_MED,\n",  # quantité du médicament ou de la fourniture dispensé
    indent("select"),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",  # durée de traitement en jours
    # FROM
    from_bd.vue("PROD","V_DEM_PAIMT_MED_CM"),"\n",  # nom de la BD à utiliser
    # WHERE
    query_stat_gen1.where_dates(debut, fin),  # filtre les dates de service
    query_stat_gen1.where_codes(type_Rx, codes),  # filtre les codes à analyser
    query_stat_gen1.where_code_serv(code_serv, code_serv_filtre),  # filtre les codes de service
    query_stat_gen1.where_code_list(code_list, code_list_filtre),  # filtre les codes de catégorie de liste de médicament
    query_stat_gen1.group_order_by(type_Rx, group_by),  # grouper et trier les résultats
    ";"
  )

  ### Arranger le code au besoin
  if (stringr::str_detect(query, "\n;")) {
    # Supprimer le changement de ligne si le code se termine "\n;"
    stringr::str_sub(query, nchar(query) - 1, nchar(query) - 1) <- ""
  }

  return(query)

}

query_stat_gen1.verif_args <- function(debut, fin, type_Rx, codes, group_by,
                                       code_serv, code_serv_filtre,
                                       code_list, code_list_filtre) {
  ### Vérifier les arguments d'entrée

  check <- newArgCheck()
  vals <- inesss:::fct_values$query_stat_gen1  # Possible values

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
      addError("type_Rx n'est pas une valeur permise.", check)
    }
  } else {
    addError("type_Rx doit être de longueur 1.", check)
  }

  # codes
  if (anyNA(codes)) {
    addError("codes ne peut contenir de NA.", check)
  } else {
    suppressWarnings({codes <- as.numeric(codes)})
    if (anyNA(codes)) {
      addError("codes doit contenir des valeurs numériques.", check)
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

  finishArgCheck(check)

}
query_stat_gen1.sel_debut <- function(debut) {
  ### Sélection de la date de début dans le code SQL.

  return(paste0("'",debut,"' as DATE_DEBUT,\n"))

}
query_stat_gen1.sel_fin <- function(fin) {
  ### Sélection de la date de fin dans le code SQL.

  return(paste0("'",fin,"' as DATE_FIN,\n"))

}
query_stat_gen1.sel_code <- function(group_by, type_Rx) {
  ### Sélection des codes dans le code SQL

  if ("Codes" %in% group_by) {
    if (type_Rx == "DENOM") {
      return(paste0(indent("select"),"SMED_COD_DENOM_COMNE as DENOM,\n"))
    } else if (type_Rx == "DIN") {
      return(paste0(indent("select"),"SMED_COD_DIN as DIN,\n"))
    }
  } else {
    return("")
  }

}
query_stat_gen1.sel_teneur <- function(group_by) {
  ### Sélection de la teneur du médicament dans le code SQL.

  if ("Teneur" %in% group_by) {
    return(paste0(indent("select"),"SMED_COD_TENR_MED as TENEUR,\n"))
  } else {
    return("")
  }

}
query_stat_gen1.sel_format <- function(group_by) {
  ### Sélection du format du médicament dans le code SQL.

  if ("Format" %in% group_by) {
    return(paste0(indent("select"),"SMED_COD_FORMA_ACQ_MED as FORMAT_ACQ,\n"))
  } else {
    return("")
  }

}
query_stat_gen1.where_dates <- function(debut, fin) {
  ### Filtre les dates de services.

  return(paste0("where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n"))

}
query_stat_gen1.where_codes <- function(type_Rx, codes) {
  ### Sélection du format du médicament dans le code SQL.

  if (type_Rx == "DENOM") {
    return(paste0(indent(),"and SMED_COD_DENOM_COMNE in (",qu(codes),")\n"))
  } else if (type_Rx == "DIN") {
    return(paste0(indent(),"and SMED_COD_DIN in (",paste(codes, collapse = ", "),")\n"))
  } else {
    stop("query_stat_gen1.where_codes(): erreur valeur type_Rx.")
  }

}
query_stat_gen1.where_code_serv <- function(code_serv, code_serv_filtre) {
  ### Sélection des codes de service (SMED_COD_SERV_1).

  if (is.null(code_serv)) {
    return("")
  } else if (code_serv_filtre == "Exclusion") {
    return(paste0(indent(),"and (SMED_COD_SERV_1 not in (",qu(code_serv),") or SMED_COD_SERV_1 is null)\n"))
  } else if (code_serv_filtre == "Inclusion") {
    return(paste0(indent(),"and SMED_COD_SERV_1 in (",qu(code_serv),")\n"))
  } else {
    stop("query_stat_gen1.where_code_serv(): erreur valeur code_serv_filtre.")
  }

}
query_stat_gen1.where_code_list <- function(code_list, code_list_filtre) {
  ### Sélection des codes de catégorie de liste de médicaments (SMED_COD_CATG_LISTE_MED).

  if (is.null(code_list)) {
    return("")
  } else if (code_list_filtre == "Exclusion") {
    return(paste0(indent(),"and (SMED_COD_CATG_LISTE_MED not in (",qu(code_list),") or SMED_COD_CATG_LISTE_MED is null)\n"))
  } else if (code_list_filtre == "Inclusion") {
    return(paste0(indent(),"and SMED_COD_CATG_LISTE_MED in (",qu(code_list),")\n"))
  } else {
    stop("stat_gen1_txt_query_1period.where_code_list() code_list_filtre valeur non permise.")
  }

}
query_stat_gen1.group_order_by <- function(type_Rx, group_by) {
  ### Grouper les résultats par...

  if (is.null(group_by)) {
    return("")
  } else {
    grp_by <- NULL
    # Ajouter les colonnes au 'group by' selon la demande
    if ("Codes" %in% group_by) {
      grp_by <- c(type_Rx, grp_by)
    }
    if ("Teneur" %in% group_by) {
      grp_by <- c(grp_by, "TENEUR")
    }
    if ("Format" %in% group_by) {
      grp_by <- c(grp_by, "FORMAT_ACQ")
    }
    return(paste0(
      "group by ", paste(grp_by, collapse = ", "),"\n",
      "order by ", paste(grp_by, collapse = ", ")
    ))
  }

}
