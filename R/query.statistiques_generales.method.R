#' SQL requête
#'
#' Statistiques générales
#'
#' @param debut Date de début au format AAAA-MM-JJ. `string`.
#' @param fin Date de fin au format AAAA-MM-JJ. `string`.
#' @param type_Rx Type de médicament : {"DENOM", "DIN"}. `string`.
#' @param codes Codes à analyser. `string` ou `numeric`.
#' @param code_serv Liste des codes de services à inclure ou exclure selon `code_serv_filtre`. `string`.
#' @param code_serv_filtre "Exclusion" ou "Inclusion". `string`.
#' @param group_by Variables dont on doit aggréger les résultats. `string`.
#' @param cohort Vrai ou Faux. Si la requête est dépendante d'une cohorte. `logical`.
#'
#' @importFrom stringr str_pad
#'
#' @return `string`
#' @encoding UTF-8
#' @keywords internal
#' @export
query.statistiques_generales.method <- function(
    debut, fin,
    type_Rx, codes = NULL,
    code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
    group_by = NULL,
    cohort = FALSE
) {

  # SELECT
  select.group_by.type_Rx <- function(group_by) {
    if (is.null(group_by)) {
      return("")
    } else if (group_by == "DENOM") {
      return("    SMED_COD_DENOM_COMNE as DENOM,\n")
    } else if (group_by == "DIN") {
      return("    SMED_COD_DIN as DIN,\n")
    }
  }

  # WHERE
  where.codes <- function(type_Rx, codes) {
    if (is.null(codes)) {
      return("")
    } else if (type_Rx == "DENOM") {
      return(paste0("    and SMED_COD_DENOM_COMNE in (",qu(str_pad(codes, 5, "left", "0")),")\n"))
    } else if (type_Rx == "DIN") {
      return(paste0("    and SMED_COD_DIN in (",paste(codes, collapse = ", "),")\n"))
    }
  }
  where.code_serv <- function(code_serv, code_serv_filtre) {
    if (is.null(code_serv)) {
      return("")
    } else if (code_serv_filtre == "Exclusion") {
      return(paste0("    and (SMED_COD_SERV_1 not in (",qu(code_serv),") or SMED_COD_SERV_1 is null)\n"))
    } else if (code_serv_filtre == "Inclusion") {
      return(paste0("and SMED_COD_SERV_1 in (",qu(code_serv),")\n"))
    }
  }

  # GROUP BY
  group_by.type_Rx <- function(group_by) {
    if (is.null(group_by)) {
      return("")
    } else if (group_by == "DENOM") {
      return("group by DENOM\n")
    } else if (group_by == "DIN") {
      return("group by DIN\n")
    }
  }

  # ORDER BY
  order_by <- function(group_by) {
    if (is.null(group_by)) {
      return("order by DATE_DEBUT;")
    } else if (group_by == "DENOM") {
      return("order by DATE_DEBUT, DENOM;")
    } else if (group_by == "DIN") {
      return("order by DATE_DEBUT, DIN;")
    }
  }


  # REQUÊTE
  query <- paste0(
    "select\n",
    "    '",debut,"' as DATE_DEBUT,\n",
    "    '",fin,"' as DATE_FIN,\n",
    select.group_by.type_Rx(group_by),
    "    sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",
    "    sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",
    "    sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",
    "    count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",
    "    count(*) as NBRE_RX,\n",
    "    sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",
    "from PROD.V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
    where.codes(type_Rx, codes),
    where.code_serv(code_serv, code_serv_filtre),
    group_by.type_Rx(group_by),
    order_by(group_by)
  )

  return(query)

}
