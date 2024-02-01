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

  # PREFIX
  if (cohort) {
    p.cohort <- "C."
    p.v_dem <- "V."
  } else {
    p.cohort <- ""
    p.v_dem <- ""
  }

  # WITH
  with.cohort <- function(cohort, debut, fin, codes) {
    if (cohort) {
      return(paste0(
        "with COHORT_STATGEN1 as (\n",
        "    select SMED_NO_INDIV_BEN_BANLS\n",
        "    from PROD.V_DEM_PAIMT_MED_CM\n",
        "    where SMED_DAT_SERV between '",debut,"' and '",fin,"'\n",
        paste0("        and SMED_COD_DENOM_COMNE in (",qu(str_pad(codes, 5, "left", "0")),")\n"),
        "),\n"
      ))
    } else {
      return("")
    }
  }

  # SELECT
  select.group_by.type_Rx <- function(group_by, p.v_dem) {
    if (is.null(group_by)) {
      return("")
    } else if (group_by == "DENOM") {
      return(paste0("    ",p.v_dem,"SMED_COD_DENOM_COMNE as DENOM,\n"))
    } else if (group_by == "DIN") {
      return(paste0("    ",p.v_dem,"SMED_COD_DIN as DIN,\n"))
    }
  }

  # FROM
  from <- function(cohort, p.cohort, p.v_dem) {
    if (cohort) {
      return(paste0(
        "from PROD.V_DEM_PAIMT_MED_CM as ",p.v_dem,"\n",
        "    inner join COHORT_STATGEN1 as ",p.cohort,"\n",
        "        on ",p.cohort,".SMED_NO_INDIV_BEN_BANLS = ",p.v_dem,".SMED_NO_INDIV_BEN_BANLS\n"
      ))
    } else {
      return("from PROD.V_DEM_PAIMT_MED_CM\n")
    }
  }

  # WHERE
  where.codes <- function(type_Rx, codes, p.v_dem) {
    if (is.null(codes)) {
      return("")
    } else if (type_Rx == "DENOM") {
      return(paste0("    and ",p.v_dem,"SMED_COD_DENOM_COMNE in (",qu(str_pad(codes, 5, "left", "0")),")\n"))
    } else if (type_Rx == "DIN") {
      return(paste0("    and ",p.v_dem,"SMED_COD_DIN in (",paste(codes, collapse = ", "),")\n"))
    }
  }
  where.code_serv <- function(code_serv, code_serv_filtre, p.v_dem) {
    if (is.null(code_serv)) {
      return("")
    } else if (code_serv_filtre == "Exclusion") {
      return(paste0("    and (",p.v_dem,"SMED_COD_SERV_1 not in (",qu(code_serv),") or ",p.v_dem,"SMED_COD_SERV_1 is null)\n"))
    } else if (code_serv_filtre == "Inclusion") {
      return(paste0("and ",p.v_dem,"SMED_COD_SERV_1 in (",qu(code_serv),")\n"))
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
    with.cohort(cohort, debut, fin, codes),
    "select\n",
    "    '",debut,"' as DATE_DEBUT,\n",
    "    '",fin,"' as DATE_FIN,\n",
    select.group_by.type_Rx(group_by, p.v_dem),
    paste0("    sum(",p.v_dem,"SMED_MNT_AUTOR_MED) as MNT_MED,\n"),
    paste0("    sum(",p.v_dem,"SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n"),
    paste0("    sum(",p.v_dem,"SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n"),
    paste0("    count(distinct ",p.v_dem,"SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n"),
    paste0("    count(",p.v_dem,"*) as NBRE_RX,\n"),
    paste0("    sum(",p.v_dem,"SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n"),
    from(cohort, p.cohort, p.v_dem),
    paste0("where ",p.v_dem,"SMED_DAT_SERV between '",debut,"' and '",fin,"'\n"),
    where.codes(type_Rx, codes, p.v_dem),
    where.code_serv(code_serv, code_serv_filtre, p.v_dem),
    group_by.type_Rx(group_by),
    order_by(group_by)
  )

  return(query)

}
