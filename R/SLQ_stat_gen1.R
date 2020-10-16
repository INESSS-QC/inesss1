#' Statistique générale
#'
#' Code de la requête SQL (châine de caractères)
#'
#' @param DateDebut Date de début d'étude.
#' @param DateFin Date de fin d'étude
#' @param Variable 'DENOM' ou 'DIN'.
#' @param Codes Codes à l'étude selon `Variable`.
#' @param Stats Vecteur indiquant les statistiques à afficher.
#' @param GroupBy  Vecteur indiquant les *group by*, par exemple Années, Code, ...
#' @param ExcluCodeServ Codes de service à exclure
#' @param CategorieListe Categorie de liste à analyser (filtre)
#'
#' @keywords internal
#' @importFrom stringr str_locate str_sub
#' @export
stat_gen1_txt_query_1period <- function(
  DateDebut, DateFin,
  Variable, Codes, Stats, GroupBy,
  ExcluCodeServ, CategorieListe
) {


# Internal FCTS -----------------------------------------------------------

  cols_dates <- function(DateDebut, DateFin) {
    return(paste0(
      indent(),qu(DateDebut)," as DEBUT,\n",
      indent(),qu(DateFin)," as FIN,\n"
    ))
  }
  select_ANNEE <- function(GroupBy) {
    if (!is.null(GroupBy) && "ANNEE" %in% GroupBy) {
      return(paste0(indent(),"extract(year from SMED_DAT_SERV) as ANNEE,\n"))
    } else {
      return("")
    }
  }
  select_CODES <- function(Variable) {
    if (Variable == "DENOM") {
      return(paste0(indent(),"SMED_COD_DENOM_COMNE as DENOM,\n"))
    } else if (Variable == "DIN") {
      return(paste0(indent(),"SMED_COD_DIN as DIN,\n"))
    }
  }
  stats_COHORTE <- function(Stats) {
    if (!is.null(Stats) && "COHORTE" %in% Stats) {
      return(paste0(indent(),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n"))
    } else {
      return("")
    }
  }
  stats_DUREE_TX <- function(Stats) {
    if (!is.null(Stats) && "DUREE_TX" %in% Stats) {
      return(paste0(indent(),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX,\n"))
    } else {
      return("")
    }
  }
  stats_MNT_MED <- function(Stats) {
    if (!is.null(Stats) && "MNT_MED" %in% Stats) {
      return(paste0(indent(),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n"))
    } else {
      return("")
    }
  }
  stats_MNT_SERV <- function(Stats) {
    if (!is.null(Stats) && "MNT_SERV" %in% Stats) {
      return(paste0(indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n"))
    } else {
      return("")
    }
  }
  stats_MNT_TOT <- function(Stats) {
    if (!is.null(Stats) && "MNT_TOT" %in% Stats) {
      return(paste0(indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n"))
    } else {
      return("")
    }
  }
  stats_NBRE_RX <- function(Stats) {
    if (!is.null(Stats) && "NBRE_RX" %in% Stats) {
      return(paste0(indent(),"count(*) as NBRE_RX,\n"))
    } else {
      return("")
    }
  }
  stats_QTE_MED <- function(Stats) {
    if (!is.null(Stats) && "QTE_MED" %in% Stats) {
      return(paste0(indent(),"sum(SMED_QTE_MED) as QTE_MED,\n"))
    } else {
      return("")
    }
  }
  where_codes <- function(Codes, Variable) {
    if (!is.null(Codes)) {
      if (Variable == "DENOM") {
        return(paste0(indent(),"and SMED_COD_DENOM_COMNE in (",paste(Codes, collapse = ", "),")\n"))
      } else if (Variable == "DIN") {
        return(paste0(indent(),"and SMED_COD_DIN in (",paste(Codes, collapse = ", "),")\n"))
      }
    } else {
      return("")
    }
  }
  where_dates_etude <- function(DateDebut, DateFin) {
    # Inverser les valeurs de debut et fin si l'utilisateur a mis la fin avant
    # le debut
    if (DateDebut > DateFin) {
      DateFin_0 <- DateFin
      DateFin <- DateDebut
      DateDebut <- DateFin_0
    }
    return(paste0("SMED_DAT_SERV between '",DateDebut,"' and '",DateFin,"'\n"))
  }
  where_codes_serv <- function(ExcluCodeServ) {
    if (is.null(ExcluCodeServ)) {
      return(paste0(indent(),"and SMED_COD_SERV_1 is null\n"))
    } else {
      return(paste0(indent(),"and (SMED_COD_SERV_1 not in (",qu(ExcluCodeServ),") or SMED_COD_SERV_1 is null)\n"))
    }
  }
  groupby_orderby <- function(GroupBy, Variable) {

    # Vérifier s'il y a une année
    if (!is.null(GroupBy) && "ANNEE" %in% GroupBy) {
      an <- "ANNEE, "
    } else {
      an <- ""
    }
    # Déterminer le type de variable
    if (Variable == "DENOM") {
      by_var <- "DENOM"
    } else if (Variable == "DIN") {
      by_var <- "DIN"
    }

    return(paste0(
      "group by ", an, by_var,"\n",
      "order by ", an, by_var
    ))

  }



# Principal FCT -----------------------------------------------------------

  query <- paste0(
    "select", nl(),
    cols_dates(DateDebut, DateFin),
    select_ANNEE(GroupBy),
    select_CODES(Variable),
    stats_MNT_MED(Stats),
    stats_MNT_SERV(Stats),
    stats_MNT_TOT(Stats),
    stats_COHORTE(Stats),
    stats_NBRE_RX(Stats),
    stats_QTE_MED(Stats),
    stats_DUREE_TX(Stats),
    from_bd.vue("PROD", "V_DEM_PAIMT_MED_CM"),nl(),
    "where ",
    where_dates_etude(DateDebut, DateFin),
    where_codes(Codes, Variable),
    where_codes_serv(ExcluCodeServ),
    groupby_orderby(GroupBy, Variable)
  )

  # Supprimer la virgule du dernier SELECT
  str_to_remove <- str_locate(query, ",\nfrom ")[1,][[1]]  # identifier la position
  str_sub(query, str_to_remove, str_to_remove) <- ""  # supprimer la virgule

  return(query)

}
#
DateDebut = "2020-10-06"
DateFin = "2020-10-06"
Variable = "DENOM"
Codes = "65"
Stats = c("MNT_MED", "MNT_SERV", "MNT_TOT", "COHORTE", "NBRE_RX", "DUREE_TX", "QTE_MED")
GroupBy = c("ANNEE")
ExcluCodeServ = "1"
CategorieListe = NULL
