#' SQL requête
#'
#' Statistiques générales - Méthode 1
#'
#' @param debut
#' @param typeRx
#' @param codesRx
#' @param catg_liste_med
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
    catg_liste_med = c(3, 40, 41),
    code_serv = "1", code_serv_filtre = "Exclusion",
    grp_age = "Mineur-Majeur"
) {

  sg1_fcts <- list(
    with.date = function(debut) {
      return(paste0(
        "with DAT as (\n",
        indent(),"select\n",
        indent(2),"date '",debut,"' as MAX_DATE,\n",
        indent(2),"-interval '12' month + (MAX_DATE + interval '1' day) as MIN_DATE\n",
        "),\n"
      ))
    },
    with.codesRx = function(typeRx, codesRx) {
      if (typeRx == "DENOM") {
        return(paste0(
          typeRx," as (\n",
          indent(),"select distinct(VAL_COD) as COD_PROD\n",
          indent(),"from COD_DENOM_COMNE\n",
          indent(),"where VAL_COD in (",paste(codesRx, collapse = ", "),")\n",
          "),\n"
        ))
      }
    },
    with.catg_liste_med = function(catg_liste_med) {
      if (is.null(catg_liste_med)) {
        return(NULL)
      } else {
        select_from <- paste0("select ",catg_liste_med[1]," as LISTE from (select 1 as T) as T")
        if (length(catg_liste_med) > 1) {
          for (i in 2:length(catg_liste_med)) {
            select_from <- paste0(
              select_from," union\n",
              indent(4),"select ",catg_liste_med[i]," as LISTE from (select 1 as T) as T"
            )
          }
        }
        return(paste0(
          "CATG as (\n",
          indent(),"select LISTE\n",
          indent(),"from    (   ",select_from,"\n",
          indent(3),") as T\n",
          "),\n"
        ))
      }
    },
    with.temp.as = function() {
      return("TEMP as (\n")
    },
    with.temp.select = function() {
      return(paste0(indent(),"select\n"))
    },
    with.temp.select.groupe_age = function(grp_age) {
      if (is.null(grp_age)) {
        return(paste0(indent(2),"SMED_AGE_BEN_AN_SERV as GROUP_AGE\n"))
      } else if (grp_age == "Mineur-Majeur") {
        return(paste0(
          indent(2),"case when SMED_AGE_BEN_AN_SERV between 0 and 17 then '0-17'\n",
          indent(2),"     else '18+'\n",
          indent(2),"     end as GROUPE_AGE,\n"
        ))
      }
    },
    with.temp.select.periode_ref = function() {
      return(paste0(
        indent(2),"case when month(SMED_DAT_SERV) >= month(DAT.MIN_DATE) then SMED_AN_CIVIL_DAT + 1\n",
        indent(2),"     else SMED_AN_CIVIL_DAT\n",
        indent(2),"     end as PERIODE_REF,\n"
      ))
    },
    with.temp.select.annee_civile = function() {
      return(paste0(indent(2),"year(SMED_DAT_SERV) as ANNEE_CIVILE,\n"))
    },
    with.temp.select.mois = function() {
      return(paste0(indent(2),"month(SMED_DAT_SERV) as MOIS,\n"))
    },
    with.temp.select.catg_liste_med = function(catg_liste_med) {
      if (is.null(catg_liste_med)) {
        return(NULL)
      } else {
        return(paste0(indent(2),"SMED_COD_CATG_LISTE_MED as COD_CATG_LISTE,\n"))
      }
    },
    with.temp.select.typeRx = function(typeRx) {
      if (typeRx == "DENOM") {
        select_var <- "SMED_COD_DENOM_COMNE"
      }
      return(paste0(indent(2),select_var," as COD_PRODUIT,\n"))
    },
    with.temp.select.tenr_med = function() {
      return(paste0(indent(2),"SMED_COD_TENR_MED as COD_TENR,\n"))
    },
    with.temp.select.ben = function() {
      return(paste0(indent(2),"count(distinct SMED_NO_INDIV_BEN_BANLS) as BEN,\n"))
    },
    with.temp.select.qte_med = function() {
      return(paste0(indent(2),"sum(SMED_QTE_MED) as QTE_MED,\n"))
    },
    with.temp.select.couts = function() {
      return(paste0(indent(2),"sum(SMED_MNT_AUTOR_MED) as COUTS,\n"))
    },
    with.temp.select.rx = function() {
      return(paste0(indent(2),"count(*) as RX,\n"))
    },
    with.temp.select.honoraire = function() {
      return(paste0(indent(2),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as HONORAIRE,\n"))
    },
    with.temp.select.duree_trait = function() {
      return(paste0(indent(2),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TRAIT,\n"))
    },
    with.temp.select.cout_tot = function() {
      return(paste0(indent(2),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as COUT_TOT\n"))
    },
    with.temp.from = function(typeRx, catg_liste_med) {
      from <- paste0(indent(),"from V_DEM_PAIMT_MED_CM, DAT, PRODUIT")
      if (!is.null(catg_liste_med)) {
        from <- paste0(from,", CATG")
      }
      return(paste0(from,"\n"))
    },
    with.temp.where.dat_serv = function() {
      return(paste0(indent(),"where SMED_DAT_SERV between DAT.MIN_DATE and DAT.MAX_DATE\n"))
    },
    with.temp.where.typeRx = function(typeRx) {
      if (typeRx == "DENOM") {
        varname <- "SMED_COD_DENOM_COMNE"
      }
      return(paste0(
        indent(2),"and ",varname," in PRODUIT.COD_PROD\n"
      ))
    },
    with.temp.where.catg_liste_med = function(catg_liste_med) {
      if (is.null(catg_liste_med)) {
        return(NULL)
      } else {
        return(paste0(indent(2),"and SMED_COD_CATG_LISTE_MED in CATG.LISTE\n"))
      }
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
    with.temp.where.duree_trait = function() {
      return(paste0(indent(2),"and SMED_NBR_JR_DUREE_TRAIT > 0\n"))
    },
    with.temp.where.sta_decis = function() {
      return(paste0(indent(2),"and SMED_COD_STA_DECIS not in ('ANN', 'REF')\n"))
    },
    with.temp.groupby = function(catg_liste_med) {
      grpby <- vector("character", 7)
      grpby[1:4] <- c("GROUPE_AGE", "PERIODE_REF", "ANNEE_CIVILE", "MOIS")
      grpby[6] <- "COD_PRODUIT"
      grpby[7] <- "COD_TENR"
      if (!is.null(catg_liste_med)) {
        grpby[5] <- "COD_CATG_LISTE"
      }

      return(paste0(
        indent(1),"group by ", paste(grpby[grpby != ""], collapse = ", "),"\n",
        "),\n"
      ))
    },
    with.ben.as = function() {
      return("BEN as (\n")
    },
    with.ben.select = function() {
      return(paste0(indent(),"select\n"))
    }
  )
  sg1_fcts <- c(sg1_fcts, list(
    with.ben.select.groupe_age = sg1_fcts$with.temp.select.groupe_age,
    with.ben.select.periode_ref = sg1_fcts$with.temp.select.periode_ref,
    with.ben.select.annee_civile = sg1_fcts$with.temp.select.annee_civile,
    with.ben.select.mois = sg1_fcts$with.temp.select.mois,
    with.ben.select.ben = function() {
      return(paste0(indent(2),"count(distinct SMED_NO_INDIV_BEN_BANLS) as BEN_UNIQUE_MOIS,\n"))
    },
    with.ben.from = sg1_fcts$with.temp.from,
    with.ben.where.dat_serv = sg1_fcts$with.temp.where.dat_serv,
    with.ben.where.catg_liste_med =  sg1_fcts$with.temp.where.catg_liste_med,
    with.ben.where.typeRx = sg1_fcts$with.temp.where.typeRx,
    with.ben.where.cod_serv = sg1_fcts$with.temp.where.cod_serv,
    with.ben.where.duree_trait = sg1_fcts$with.temp.where.duree_trait,
    with.ben.where.sta_decis = sg1_fcts$with.temp.where.sta_decis,
    with.ben.groupby = function() {
      return(paste0(
        indent(),"group by GROUPE_AGE, PERIODE_REF, ANNEE_CIVILE, MOIS\n",
        "),\n"
      ))
    },
    select = function() {
      return("select\n")
    }
  ))

  query <- paste0(
    sg1_fcts$with.date(debut),
    sg1_fcts$with.codesRx(typeRx, codesRx),
    sg1_fcts$with.catg_liste_med(catg_liste_med),
    sg1_fcts$with.temp.as(),
    sg1_fcts$with.temp.select(),
    sg1_fcts$with.temp.select.groupe_age(grp_age),
    sg1_fcts$with.temp.select.periode_ref(),
    sg1_fcts$with.temp.select.annee_civile(),
    sg1_fcts$with.temp.select.mois(),
    sg1_fcts$with.temp.select.catg_liste_med(catg_liste_med),
    sg1_fcts$with.temp.select.typeRx(typeRx),
    sg1_fcts$with.temp.select.tenr_med(),
    sg1_fcts$with.temp.select.ben(),
    sg1_fcts$with.temp.select.qte_med(),
    sg1_fcts$with.temp.select.couts(),
    sg1_fcts$with.temp.select.rx(),
    sg1_fcts$with.temp.select.honoraire(),
    sg1_fcts$with.temp.select.duree_trait(),
    sg1_fcts$with.temp.select.cout_tot(),
    sg1_fcts$with.temp.from(typeRx, catg_liste_med),
    sg1_fcts$with.temp.where.dat_serv(),
    sg1_fcts$with.temp.where.catg_liste_med(catg_liste_med),
    sg1_fcts$with.temp.where.typeRx(typeRx),
    sg1_fcts$with.temp.where.cod_serv(code_serv, code_serv_filtre),
    sg1_fcts$with.temp.where.duree_trait(),
    sg1_fcts$with.temp.where.sta_decis(),
    sg1_fcts$with.temp.groupby(catg_liste_med),
    sg1_fcts$with.ben.as(),
    sg1_fcts$with.ben.select(),
    sg1_fcts$with.ben.select.groupe_age(grp_age),
    sg1_fcts$with.ben.select.periode_ref(),
    sg1_fcts$with.ben.select.annee_civile(),
    sg1_fcts$with.ben.select.mois(),
    sg1_fcts$with.ben.select.ben(),
    sg1_fcts$with.ben.from(typeRx, catg_liste_med),
    sg1_fcts$with.ben.where.dat_serv(),
    sg1_fcts$with.ben.where.catg_liste_med(catg_liste_med),
    sg1_fcts$with.ben.where.typeRx(typeRx),
    sg1_fcts$with.ben.where.cod_serv(code_serv, code_serv_filtre),
    sg1_fcts$with.ben.where.duree_trait(),
    sg1_fcts$with.ben.where.sta_decis(),
    sg1_fcts$with.ben.groupby()
  )
  attr(query, "internal_fcts") <- sg1_fcts

  cat(query)
  debut = "2022-01-01"
  typeRx = "DENOM"
  codesRx = c(39, 48135)
  catg_liste_med = c(3, 40, 41)
  grp_age = "Mineur-Majeur"
  code_serv = "1"
  code_serv_filtre = "Exclusion"

}























