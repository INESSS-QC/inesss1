#' SQL requête
#'
#' Statistiques générales - Méthode 1
#'
#' @param debut
#' @param typeRx
#' @param codesRx
#' @param cat_list_med
#' @param code_serv
#' @param code_serv_filtre
#'
#' @return `string`
#' @encoding UTF-8
#' @keywords internal
#' @export
statgen1 <- function(
    debut = NULL,
    typeRx = NULL,
    codesRx = NULL,
    cat_list_med = c(3, 40, 41),
    code_serv = "1", code_serv_filtre = "Exclusion",
    grp_age = "Mineur-Majeur"
) {

  statgen1_functions <- list(
    with.date = function(debut) {
      return(paste0(
        "with DAT as (\n",
        indent(),"select\n",
        indent(2),"date '",debut,"' as MAX_DATE\n",
        indent(2),"-interval '12' month + (MAX_DATE + interval '1' day) as MIN_DATE\n",
        "),\n"
      ))
    },
    with.codesRx = function(typeRx, codesRx) {
      return(paste0(
        typeRx," as (\n",
        indent(),"select distinct(VAL_COD) as ",typeRx,"\n",
        indent(),"from COD_DENOM_COMNE\n",
        indent(),"where VAL_COD in (",paste(codesRx, collapse = ", "),")\n",
        "),\n"
      ))
    },
    with.cat_liste_med = function(cat_list_med) {
      if (is.null(cat_list_med)) {
        return(NULL)
      } else {
        select_from <- paste0("select ",cat_list_med[1]," as LISTE from (select 1 as T) as T")
        if (length(cat_list_med) > 1) {
          for (i in 2:length(cat_list_med)) {
            select_from <- paste0(
              select_from," union\n",
              indent(4),"select ",cat_list_med[i]," as LISTE from (select 1 as T) as T"
            )
          }
        }
        return(paste0(
          "CAT as (\n",
          indent(),"select LISTE\n",
          indent(),"from    (   ",select_from,"\n",
          indent(3),") as T\n",
          "),"
        ))
      }
    },
    with.temp.select.groupe_age = function(grp_age) {
      if (is.null(grp_age)) {
        return(paste0(indent(2),"SMED_AGE_BEN_AN_SERV as GROUP_AGE\n"))
      } else if (grp_age == "Mineur-Majeur") {
        return(paste0(
          indent(2),"case when SMED_AGE_BEN_AN_SERV between 0 and 17 then '0-17'\n",
          indent(2),"     else '18+'\n",
          indent(2),"     end as GROUP_AGE,\n"
        ))
      }
    },
    with.temp.select.periode_ref = function() {
      return(paste0(
        indent(2),"case when (extract month from SMED_DAT_SERV) >= (extract month from DAT.MIN_DATE) then SMED_AN_CIVIL_DAT + 1\n",
        indent(2),"     else SMED_AN_CIVIL_DAT\n",
        indent(2),"     end as PERIODE_REF"
      ))
    },
    with.temp.from = function(typeRx, cat_list_med) {
      from <- paste0(indent(),"from V_DEM_PAIMT_MED_CM, DAT, ",typeRx)
      if (!is.null(cat_list_med)) {
        from <- paste0(from,", CAT")
      }
      return(paste0(from,"\n"))
    },
    with.temp.where.typeRx = function(typeRx) {
      if (typeRx == "DENOM") {
        varname <- "SMED_COD_DENOM_COMNE"
      }
      return(paste0(
        indent(2),"and ",varname," in ",typeRx,".",typeRx,"\n"
      ))
    },
    with.temp.where.cod_serv = function(code_serv, code_serv_filtre) {
      if (code_serv_filtre == "Exclusion") {
        return(paste0(
          indent(2),"and (SMED_COD_SERV_1 not in (",qu(code_serv),") or SMED_COD_SERV_1 is null)\n"
        ))
      } else if (code_serv_filtre == "Inclusion"){
        return(paste0(
          indent(2),"and SMED_COD_SERV_1 in (",qu(code_serv),")\n"
        ))
      }
    },
    with.temp.where.cat_liste_med = function(cat_list_med) {
      if (is.null(cat_list_med)) {
        return(NULL)
      } else {
        return(paste0(indent(2),"and SMED_COD_CATG_LISTE_MED in CAT.LISTE\n"))
      }
    },
    with.temp.groupby = function(cat_list_med) {
      grpby <- vector("character", 7)
      grpby[1:4] <- c("GROUPE_AGE", "PERIODE_REF", "ANNEE_CIVILE", "MOIS")
      grpby[7] <- "SMED_COD_TENR_MED"
      if (!is.null(cat_list_med)) {
        grpby[5] <- "SMED_COD_CATG_LISTE_MED"
      }
      if (typeRx == "DENOM") {
        grpby[6] <- "SMED_COD_DENOM_COMNE"
      } else if (typeRx == "DIN") {
        grpby[6] <- "SMED_COD_DIN"
      }

      return(paste(grpby[grpby != ""], collapse = ", "))
    }
  )
  statgen1_functions <- c(statgen1_functions, list(
    with.ben.select.groupe_age = statgen1_functions$with.temp.select.groupe_age,
    with.ben.select.periode_ref = statgen1_functions$with.temp.select.periode_ref,
    with.ben.from = statgen1_functions$with.temp.from,
    with.ben.where.typeRx = statgen1_functions$with.temp.where.typeRx,
    with.ben.where.cod_serv = statgen1_functions$with.temp.where.cod_serv,
    with.ben.where.cat_liste_med =  statgen1_functions$with.temp.where.cat_liste_med,
  ))

  query <- paste0(
    statgen1_functions$with.date(debut),
    statgen1_functions$with.codesRx(typeRx, codesRx)
  )

}
























