#' Code SQL
#'
#' Générateur de code SQL pour la méthode `naif_switch1`.
#'
#' \strong{\code{rx_retrospect_a_exclure} :}\cr
#' La période rétrospective est construite à partir des dates de références (index) et de l'argument `njours_sans_conso` : \code{[INDEX - njours_sans_conso; INDEX - 1]}.\cr\cr
#' \strong{`code_serv_filtre`, `code_list_filtre` :}\cr
#' `'Exclusion'` inclus les `NULL`\cr
#' `'Inclusion'` exclus les `NULL`.
#'
#' @param debut Date de début de la période d'étude au format `AAAA-MM-JJ` (une seule valeur).
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ` (une seule valeur).
#' @param type_Rx Indique le type de code analysé :
#' * `'DENOM'` : Code de dénomination commune (`SMED_COD_DENOM_COMNE`).
#' * `'DIN'` : Code d'identification du médicament (`SMED_COD_DIN`).
#' @param codes Traitement(s) d'intérêt(s). Vecteur comprenant le ou les codes d'analyse au format numérique, sans zéros.
#' @param rx_retrospect_a_exclure Traitement(s) à inclure dans la période rétrospective. Voir *Details*. Un individu qui a au moins un traitement durant la période rétrospective ne sera pas considéré comme *naïf* ou *switch*.
#' @param njours_sans_conso Nombre de jours qu'un individu ne doit pas avoir reçu de traitements avant sa date de référence (index) pour être considéré *naïf* ou *switch*.
#' @param code_serv Vecteur de type `character` comprenant le ou les codes de service (`SMED_COD_SERV_1`) à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_serv_filtre `'Inclusion'` ou `'Exclusion'` des codes de service `code_serv`. Inscrire `code_serv = NULL` s'il n'y a pas de filtre à appliquer.
#' @param code_list Vecteur de type `character` comprenant le ou les codes de catégories de liste de médicaments (`SMED_COD_CATG_LISTE_MED`) à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_list_filtre `'Inclusion'` ou `'Exclusion'` des codes de catégories de liste de médicaments `code_list`. Inscrire `code_list = NULL` s'il n'y a pas de filtre à appliquer.
#'
#' @return Chaîne de caractères à utiliser dans une requête SQL.
#' @encoding UTF-8
#' @export
#' @examples
#' ### DENOM
#' cat(query_naif_switch1(
#'   debut = '2019-01-01', fin = '2019-12-31',
#'   type_rx = 'DENOM', codes = 47946,
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' ))
#'
#' ### DIN
#' cat(query_naif_switch1(
#'   debut = '2019-01-01', fin = '2019-12-31',
#'   type_rx = 'DIN', codes = 2257238,
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' ))
#'
#' ### Plusieurs codes
#' cat(query_naif_switch1(
#'   debut = '2019-01-01', fin = '2019-12-31',
#'   type_rx = 'DENOM', codes = c(39, 47135, 48222),
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' ))
#'
#' ### rx_retrospect_a_exclure
#' # Plusieurs, dont celui inclus dans 'codes'
#' cat(query_naif_switch1(
#'   debut = '2019-01-01', fin = '2019-12-31',
#'   type_rx = 'DENOM', codes = 47946,
#'   rx_retrospect_a_exclure = c(47946, 48222, 48259), njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' ))
#' # Plusieurs, sans celui inclus dans 'codes'
#' cat(query_naif_switch1(
#'   debut = '2019-01-01', fin = '2019-12-31',
#'   type_rx = 'DENOM', codes = 47946,
#'   rx_retrospect_a_exclure = c(48222, 48259, 43012), njours_sans_conso = 365,
#'   code_serv = '1', code_serv_filtre = 'Exclusion',
#'   code_list = NULL, code_list_filtre = 'Inclusion'
#' ))
#'
#' ### Exclusion vs Inclusion
#' cat(query_naif_switch1(
#'   debut = '2019-01-01', fin = '2019-12-31',
#'   type_rx = 'DENOM', codes = 47946,
#'   rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
#'   code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
#'   code_list = c('40', '41'), code_list_filtre = 'Inclusion'
#' ))
query_naif_switch1 <- function(
  debut, fin,
  type_rx = 'DENOM', codes,
  rx_retrospect_a_exclure = NULL, njours_sans_conso = 365,
  code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
  code_list = NULL, code_list_filtre = 'Inclusion',
  ...
) {

  dot_args <- list(...)

  ### Vérification des arguments
  if (!"verif" %in% names(dot_args)) {
    dot_args$verif <- TRUE
  }
  if (dot_args$verif) {
    query_naif_switch1.verif_args(
      debut, fin, type_rx, codes, rx_retrospect_a_exclure, njours_sans_conso,
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

  ### Query
  query <- paste0(
    # Tous les utilisateurs qui consomment le(s) médicament(s)
    "with ALL_USER as (\n",
    "    select SMED_NO_INDIV_BEN_BANLS as ID,\n",
    "           min(SMED_DAT_SERV) as DATE_INDEX\n",
    "    from PROD.V_DEM_PAIMT_MED_CM\n",
    "    where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
    "        and ",query_naif_switch1.var_type_rx(type_rx)," in (",
    {  # Sélection des codes selon le type de Rx
      if (type_rx == "DENOM") {
        qu(codes)  # entre guillemet
      } else if (type_rx == "DIN") {
        paste(codes, collapse = ", ")  # numeric = pas de guillemet
      }
    },")\n",
    {  # Codes de services
      if (is.null(code_serv)) {
        ""
      } else {
        paste0(indent(), query_naif_switch1.where_code_serv(code_serv, code_serv_filtre))
      }
    },
    {  # Code de catégorie de liste de médicament
      if (is.null(code_list)) {
        ""
      } else {
        paste0(indent(), query_naif_switch1.where_code_list(code_list, code_list_filtre))
      }
    },
    "        and SMED_NBR_JR_DUREE_TRAIT > 0\n",
    "    group by ID\n",
    "),\n",
    "OLD_USER as (\n",
    "    select distinct(A.ID)\n",
    "    from ALL_USER As A left join PROD.V_DEM_PAIMT_MED_CM As V\n",
    "        on A.ID = V.SMED_NO_INDIV_BEN_BANLS\n",
    "    where V.SMED_DAT_SERV between (A.DATE_INDEX - ",njours_sans_conso,") and (A.DATE_INDEX - 1)\n",
    "        and V.",query_naif_switch1.var_type_rx(type_rx)," in (",
    {  # Sélection des codes selon le type de Rx
      if (type_rx == "DENOM") {
        qu(rx_retrospect_a_exclure)  # entre guillemet
      } else if (type_rx == "DIN") {
        paste(rx_retrospect_a_exclure, collapse = ", ")  # numeric = pas de guillemet
      }
    },")\n",
    {  # Codes de services
      if (is.null(code_serv)) {
        ""
      } else {
        paste0(indent(), query_naif_switch1.where_code_serv(code_serv, code_serv_filtre, cte = "V."))
      }
    },
    {  # Codes de catégorie de liste de médicament
      if (is.null(code_list)) {
        ""
      } else {
        paste0(indent(), query_naif_switch1.where_code_list(code_list, code_list_filtre, cte = "V."))
      }
    },
    "        and V.SMED_NBR_JR_DUREE_TRAIT > 0\n",
    "),\n",
    "NAIF_SWITCH as (\n",
    "    select ID from ALL_USER\n",
    "    except\n",
    "    select ID from OLD_USER\n",
    ")\n",
    "select '",debut,"' as DATE_DEBUT,\n",
    indent("select"),"'",fin,"' as DATE_FIN,\n",
    indent("select"),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",  # montant autorisé pour le médicament
    indent("select"),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",  # frais de service autorisé
    indent("select"),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",  # somme des montants
    indent("select"),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",  # cohorte d'étude (nombre personnes)
    indent("select"),"count(*) as NBRE_RX,\n",  # nombre de services/prescriptions
    indent("select"),"sum(SMED_QTE_MED) as QTE_MED,\n",  # quantité du médicament ou de la fourniture dispensé
    indent("select"),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",  # durée de traitement en jours
    "from NAIF_SWITCH As N left join PROD.V_DEM_PAIMT_MED_CM as V\n",
    "    on N.ID = V.SMED_NO_INDIV_BEN_BANLS\n",
    "where V.SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
    "    and V.",query_naif_switch1.var_type_rx(type_rx)," in (",
    {  # sélection des codes selon le type de Rx
      if (type_rx == "DENOM") {
        qu(codes)  # entre guillemet
      } else if (type_rx == "DIN") {
        paste(codes, collapse = ", ")  # numeric = pas de guillemet
      }
    },")\n",
    query_naif_switch1.where_code_serv(code_serv, code_serv_filtre, cte = "V."),
    query_naif_switch1.where_code_list(code_list, code_list_filtre, cte = "V."),
    "    and V.SMED_NBR_JR_DUREE_TRAIT > 0;"
  )

  return(query)

}

#' @title query_naif_switch1
#' @description Vérification des arguments pour la fonction `query_naif_switch1`.
#' @encoding UTF-8
#' @keywords internal
query_naif_switch1.verif_args <- function(
  debut, fin, type_rx, codes, rx_retrospect_a_exclure, njours_sans_conso,
  code_serv, code_serv_filtre, code_list, code_list_filtre
) {

  check <- newArgCheck()
  vals <- inesss:::fct_values$query_naif_switch1

  # debut & fin
  for (val in c("debut", "fin")) {
    if (length(get(val)) != 1) {
      addError(paste(val, "doit être de longueur 1."), check)
    }
    if (is.na(suppressWarnings(lubridate::as_date(get(val))))) {
      addError(paste(val, "n'est pas au format 'AAAA-MM-JJ'."), check)
    }
  }

  # type_rx
  if (length(type_rx) == 1) {
    if (!type_rx %in% vals$type_rx) {
      addError("type_rx n'est pas une valeur permise.", check)
    }
  } else {
    addError("type_rx doit être de longueur 1.", check)
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

  # rx_retrospect_a_exclure
  if (!is.null(rx_retrospect_a_exclure)) {
    if (anyNA(rx_retrospect_a_exclure)) {
      addError("rx_retrospect_a_exclure ne peut contenir de NA.", check)
    } else {
      suppressWarnings({rx_retrospect_a_exclure <- as.numeric(rx_retrospect_a_exclure)})
      if (anyNA(rx_retrospect_a_exclure)) {
        addError("rx_retrospect_a_exclure doit contenir des valeurs numériques.", check)
      }
    }
  }

  # njours_sans_conso
  if (length(njours_sans_conso) != 1) {
    addError("njours_sans_conso doit être de longueur 1.", check)
  }
  if (!is.numeric(njours_sans_conso)) {
    addError("njours_sans_conso doit contenir une valeur numérique.", check)
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
#' @title query_naif_switch1
#' @description Déterminer le nom de la variable de la vue *V_DEM_PAIMT_MED_CM* selon `type_rx`.
#' @encoding UTF-8
#' @keywords internal
query_naif_switch1.var_type_rx <- function(type_rx) {
  if (type_rx == "DENOM") {
    return("SMED_COD_DENOM_COMNE")
  } else if (type_rx == "DIN") {
    return("SMED_COD_DIN")
  } else {
    stop("query_naif_switch1.var_type_rx() : valeur non permise")
  }
}
#' @title query_naif_switch1
#' @description Commande SQL du `where` pour les codes de catégorie de liste de médicament.
#' @encoding UTF-8
#' @keywords internal
query_naif_switch1.where_code_list <- function(code_list, code_list_filtre, cte = "") {
  if (is.null(code_list)) {
    return("")
  } else if (code_list_filtre == "Exclusion") {
    return(paste0(
      indent(),"and (",
      paste0(cte,"SMED_COD_CATG_LISTE_MED")," not in (",qu(code_list),") or ",
      paste0(cte,"SMED_COD_CATG_LISTE_MED")," is null)\n"
    ))
  } else if (code_list_filtre == "Inclusion") {
    return(paste0(
      indent(),"and ",
      paste0(cte,"SMED_COD_CATG_LISTE_MED")," in (",qu(code_list),")\n"
    ))
  } else {
    stop("query_naif_switch1.where_code_list() code_list_filtre valeur non permise.")
  }

}
#' @title query_naif_switch1
#' @description Commande SQL du `where` pour les codes de services.
#' @encoding UTF-8
#' @keywords internal
query_naif_switch1.where_code_serv <- function(code_serv, code_serv_filtre, cte = "") {
  if (is.null(code_serv)) {
    return("")
  } else if (code_serv_filtre == "Exclusion") {
    return(paste0(
      indent(),"and (",
      paste0(cte,"SMED_COD_SERV_1")," not in (",qu(code_serv),") or ",
      paste0(cte,"SMED_COD_SERV_1")," is null)\n"
    ))
  } else if (code_serv_filtre == "Inclusion") {
    return(paste0(
      indent(),"and ",
      paste0(cte,"SMED_COD_SERV_1")," in (",qu(code_serv),")\n"
    ))
  } else {
    stop("query_naif_switch1.where_code_serv(): erreur valeur code_serv_filtre.")
  }
}
