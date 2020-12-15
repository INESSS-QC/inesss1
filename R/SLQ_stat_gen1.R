#' Title
#'
#' @param user Identifiant SQL Teradata.
#' @param pwd Mot de passe associé au compte (`user`) Teradata.
#' @param date_debut,date_fin Dates au format `"AAAA-MM-JJ"`. Dates de début et de fin de la période d'analyse, de la période d'étude.
#' @param type_rx Type des codes Rx.
#' @param code_rx Codes Rx à analyser.
#' @param grouper_par
#' @param code_serv_filtre
#' @param code_serv
#' @param code_list_filtre
#' @param code_list
#'
#' @return
#' @import data.table
#' @importFrom askpass askpass
#' @export
#'
#' @examples
SQL_stat_gen1 <- function(
  user, pwd = NULL,
  date_debut, date_fin,
  type_rx, code_rx, grouper_par,
  code_serv_filtre, code_serv,
  code_list_filtre, code_list
) {

  # Demander le mot de passe s'il n'a pas été inscrit
  if (is.null(pwd)) {
    pwd <- askpass("Mot de passe :")
  }




}

#' Statistiques générales
#'
#' Code de la requête SQL - chaîne de caractères.
#'
#' @keywords internal
#' @importFrom stringr str_locate str_sub str_detect
#' @export
stat_gen1_txt_query_1period <- function(
  debut, fin,
  type_Rx, codes,
  groupby,
  code_serv, code_serv_filtre,
  code_list, code_list_filtre
) {

# Internal FCTS -----------------------------------------------------------

  select_type_rx <- function(groupby, type_Rx) {
    if (is.null(groupby)) {
      if (type_Rx == "DENOM") {
        return(paste0(indent(),"SMED_COD_DENOM_COMNE as DENOM,\n"))
      } else if (type_Rx == "DIN") {
        return(paste0(indent(),"SMED_COD_DIN as DIN,\n"))
      } else {
        stop("stat_gen1_txt_query_1period.select_CODES() valeur non permise.")
      }
    } else {
      return("")
    }
  }
  where_code_rx <- function(type_Rx, codes) {
    if (type_Rx == "DENOM") {
      return(paste0("and SMED_COD_DENOM_COMNE in (",qu(codes),")\n"))
    } else if (type_Rx == "DIN") {
      return(paste0("and SMED_COD_DIN in (",paste(codes, collapse = ", "),")\n"))
    } else {
      stop("stat_gen1_txt_query_1period.where_type_rx() type_Rx valeur non permise.")
    }
  }
  where_code_serv <- function(code_serv_filtre, code_serv) {
    if (is.null(code_serv)) {
      return("")
    } else if (code_serv_filtre == "Exclusion") {
      return(paste0(indent(),"and (SMED_COD_SERV_1 not in (",qu(code_serv),") or SMED_COD_SERV_1 is null)\n"))
    } else if (code_serv_filtre == "Inclusion") {
      return(paste0(indent(),"and SMED_COD_SERV_1 in (",qu(code_serv),")\n"))
    } else {
      stop("stat_gen1_txt_query_1period.where_code_serv() code_serv_filtre valeur non permise")
    }
  }
  where_code_list <- function(code_list_filtre, code_list) {
    if (is.null(code_list)) {
      return("")
    } else if (code_list_filtre == "Exclusion") {
      return(paste0(indent(),"and (SMED_COD_CATG_LISTE_MED not in (",qu(code_list),") or SMED_COD_CATG_LISTE_MED is null)\n"))
    } else if (code_list_filtre == "Inclusion") {
      return(paste0(indent(),"and SMED_COD_CATG_LISTE_MED in (",qu(code_list),")\n"))
    } else {
      stop("stat_gen1_txt_query_1period.where_code_list() code_list_filtre valeur non permise")
    }
  }
  group_by <- function(groupby, type_Rx) {
    if (is.null(groupby)) {
      txt_grpby <- "group by "
      if (type_Rx == "DENOM") {
        txt_grpby <- paste0(txt_grpby, "DENOM")
      } else if (type_Rx == "DIN") {
        txt_grpby <- paste0(txt_grpby, "DIN")
      }
    } else if ("Périodes" %in% groupby) {
      txt_grpby <- ""
    }
    return(txt_grpby)
  }

# Principal FCT -----------------------------------------------------------

  query <- paste0(
    "select ",qu(debut)," as DATE_DEBUT,\n",
    indent(),qu(fin)," as DATE_FIN,\n",
             select_type_rx(groupby, type_Rx),
    indent(),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",
    indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",
    indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",
    indent(),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",
    indent(),"count(*) as NBRE_RX,\n",
    indent(),"sum(SMED_QTE_MED) as QTE_MED,\n",
    indent(),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",
    from_bd.vue("PROD","V_DEM_PAIMT_MED_CM"),"\n",
    "where SMED_DAT_SERV between ",qu(debut)," and ",qu(fin),"\n",
    indent(),where_code_rx(type_Rx, codes),
    where_code_serv(code_serv_filtre, code_serv),
    where_code_list(code_list_filtre, code_list),
    group_by(groupby, type_Rx),
    ";"
  )

  if (str_detect(query, "\n;")) {  # supprimer retour de ligne (\n) si la commande est terminée (;)
    substr(query, nchar(query) - 1, nchar(query) - 1) <- ""  # supprime '\n' qui est compté comme 1 seul char
    # *** NOTE : stringr::str_sub() ne fonctionne pas, la fonction n'était pas
    #            disponible malgré le importFrom stringr str_sub.
  }

  return(query)

}
