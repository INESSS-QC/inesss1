#' Statistique générale
#'
#' Code de la requête SQL (châine de caractères)
#'
#' @param DateDebut Date de début d'étude.
#' @param DateFin Date de fin d'étude
#' @param TypeVar 'DENOM' ou 'DIN'.
#' @param Codes Codes à l'étude selon `TypeVar`.
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
  TypeVar, Codes, Stats, GroupBy,
  ExcluCodeServ, CategorieListe
) {


# Internal FCTS -----------------------------------------------------------

  groupby_ANNEE <- function(GroupBy) {
    if ("ANNEE" %in% GroupBy) {
      return(paste0(indent(),"extract(year from SMED_DAT_SERV) as ANNEE,\n"))
    } else {
      return("")
    }
  }
  groupby_CODE <- function(GroupBy, TypeVar) {
    if ("CODES" %in% GroupBy) {
      if (TypeVar == "DENOM") {
        return(paste0(indent(),"SMED_COD_DENOM_COMNE as DENOM,\n"))
      } else if (TypeVar == "DIN") {
        return(paste0(indent(),"SMED_COD_DIN as DIN,\n"))
      }
    } else {
      return("")
    }
  }
  stats_COUT <- function(Stats) {
    if ("COUT" %in% Stats) {
      return(paste0(indent(),"sum(SMED_MNT_AUTOR_MED) as COUT,\n"))
    } else {
      return("")
    }
  }
  stats_COUT_TOT <- function(Stats) {
    if ("COUT_TOT" %in% Stats) {
      return(paste0(indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as COUT_TOT,\n"))
    } else {
      return("")
    }
  }
  stats_HONORAIRE <- function(Stats) {
    if ("HONORAIRE" %in% Stats) {
      return(paste0(indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as HONORAIRE,\n"))
    } else {
      return("")
    }
  }
  stats_ID_UNIQUE <- function(Stats) {
    if ("ID_UNIQUE" %in% Stats) {
      return(paste0(indent(),"count(distinct SMED_NO_INDIV_BEN_BANLS) as ID_UNIQUE,\n"))
    } else {
      return("")
    }
  }
  where_codes <- function(Codes, TypeVar) {
    if (TypeVar == "DENOM") {
      return(paste0(indent(),"and SMED_COD_DENOM_COMNE in (",paste(Codes, collapse = ", "),")\n"))
    } else if (TypeVar == "DIN") {
      return(paste0(indent(),"and SMED_COD_DIN in (",paste(Codes, collapse = ", "),")\n"))
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


# Principal FCT -----------------------------------------------------------

  query <- paste0(
    "select", nl(),
    groupby_ANNEE(GroupBy),
    groupby_CODE(GroupBy, TypeVar),
    stats_ID_UNIQUE(Stats),
    stats_COUT(Stats),
    stats_HONORAIRE(Stats),
    stats_COUT_TOT(Stats),
    from_bd.vue("PROD", "V_DEM_PAIMT_MED_CM"),nl(),
    "where ",
    where_dates_etude(DateDebut, DateFin),
    where_codes(Codes, TypeVar),
    where_codes_serv(ExcluCodeServ)
  )

  # Supprimer la virgule du dernier SELECT
  str_to_remove <- str_locate(query, ",\nfrom ")[1,][[1]]  # identifier la position
  str_sub(query, str_to_remove, str_to_remove) <- ""  # supprimer la virgule

  return(query)

}

DateDebut = "2020-10-06"
DateFin = "2020-10-06"
TypeVar = "DENOM"
Codes = "65"
Stats = c("COUT", "HONORAIRE", "COUT_TOT", "ID_UNIQUE", "DUREE_TX", "RX_NOMBRE", "RX_QTE")
GroupBy = c("ANNEE", "CODES")
ExcluCodeServ = "1"
CategorieListe = NULL
