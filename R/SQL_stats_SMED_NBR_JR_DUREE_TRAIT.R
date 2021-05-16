#' Statistiques
#'
#' Statistiques descriptives de la variable `SMED_NBR_JR_DUREE_TRAIT` de la vue `V_DEM_PAIMT_MED_CM`.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param debut Date de début de la période d'étute au format `AAAA-MM-JJ`.
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ`.
#' @param by_code_serv `TRUE` ou `FALSE`. Grouper les résultats par code de services. Par défaut `TRUE`.
#' @param include_dureeTx_0 `TRUE` ou `FALSE`. Inclure les durées de traitements égale à zéro. Par défaut `FALSE`.
#'
#' @import data.table
#' @encoding UTF-8
#' @export
SQL_stats_SMED_NBR_JR_DUREE_TRAIT <- function(conn, debut, fin, by_code_serv = TRUE, include_dureeTx_0 = FALSE) {

  # Vérification des arguments ----------------------------------------------

  check <- newArgCheck()
  debut <- lubridate::as_date(debut)
  if (is.na(debut)) {
    addError("debut n'est pas au format AAAA-MM-JJ.", check)
  } else if (year(debut) < 1996) {
    addError(paste0("debut (",debut,") doit être le 1996-01-01 ou après."), check)
  }
  fin <- lubridate::as_date(fin)
  if (is.na(fin)) {
    addError("fin n'est pas au format AAAA-MM-JJ.", check)
  } else if (year(fin) < 1996) {
    addError(paste0("fin (",fin,") doit être le 1996-01-01 ou après."), check)
  }
  if (!is.logical(by_code_serv)) {
    addError("by_code_serv doit être de type logical.", check)
  }
  if (!is.logical(include_dureeTx_0)) {
    addError("include_dureeTx_0 doit être de type logical", check)
  }
  finishArgCheck(check)

  ### Corriger les dates si debut > fin
  if (debut > fin) {
    fin_copy <- fin
    fin <- debut
    debut <- fin_copy
  }


  # Extraction des durées ---------------------------------------------------

  deb_yr <- year(debut)
  deb_mth <- month(debut)
  yr <- year(debut)
  mth <- month(debut)
  fin_yr <- year(fin)
  fin_mth <- month(fin)
  DT <- vector("list", (fin_yr - deb_yr + 1) * 12 - (deb_mth - 1) - (12 - fin_mth))
  i <- 1L
  loop <- TRUE
  while (loop) {
    if (i == 1) {
      if (deb_yr == fin_yr && deb_mth == fin_mth) {
        DT[[i]] <- as.data.table(odbc::dbGetQuery(
          conn, statement = SQL_stats_SMED_NBR_JR_DUREE_TRAIT.query(
            deb_yr, deb_mth, mday(debut), mday(fin),
            by_code_serv, include_dureeTx_0
          )
        ))
        loop <- FALSE
      } else {
        DT[[i]] <- as.data.table(odbc::dbGetQuery(
          conn, statement = SQL_stats_SMED_NBR_JR_DUREE_TRAIT.query(
            deb_yr, deb_mth, mday(debut), "last",
            by_code_serv, include_dureeTx_0
          )
        ))
        i <- i + 1L
        mth <- mth + 1L  # appliquer pour le mois suivant
        if (mth == 13) {  # prochaine année
          yr <- yr + 1L
          mth <- 1L
        }
      }
    } else if (yr == fin_yr && mth == fin_mth) {
      DT[[i]] <- as.data.table(odbc::dbGetQuery(
        conn, statement = SQL_stats_SMED_NBR_JR_DUREE_TRAIT.query(
          yr, mth, 1, mday(fin),
          by_code_serv, include_dureeTx_0
        )
      ))
      loop <- FALSE
    } else {
      DT[[i]] <- as.data.table(odbc::dbGetQuery(
        conn, statement = SQL_stats_SMED_NBR_JR_DUREE_TRAIT.query(
          yr, mth, 1, "last",
          by_code_serv, include_dureeTx_0
        )
      ))
      i <- i + 1L
      mth <- mth + 1L  # appliquer pour le mois suivant
      if (mth == 13) {  # prochaine année
        yr <- yr + 1L
        mth <- 1L
      }
    }
  }
  DT <- rbindlist(DT)


# Statistiques --------------------------------------------------------------------------------



}

#' @title SQL_stats_SMED_NBR_JR_DUREE_TRAIT
#' @description Code SQL à exécuter pour chaque mois à l'intérieur de la période d'étude.
#' @param yr Année.
#' @param mth Mois
#' @param deb_day Première journée.
#' @param fin_day Dernière journée.
#' @param by_code_serv `TRUE` ou `FALSE`. Si on veut les codes de services (pour résultats par code de service).
#' @param include_dureeTx_0 `TRUE` ou `FALSE`. Inclure les durées de traitement égale à zéro (0).
#' @keywords internal
#' @encoding UTF-8
SQL_stats_SMED_NBR_JR_DUREE_TRAIT.query <- function(yr, mth, deb_day, fin_day,
                                                    by_code_serv = TRUE, include_dureeTx_0 = FALSE) {
  query <- paste0(
    "select\n",
    "    SMED_COD_DENOM_COMNE as DENOM,\n"
  )
  if (by_code_serv) {
    query <- paste0(
      query,
      "    SMED_COD_SERV_1 as CODE_SERV,\n"
    )
  }
  query <- paste0(
    query,
    "    SMED_NBR_JR_DUREE_TRAIT as DUREE_TRAIT\n",
    "from V_DEM_PAIMT_MED_CM\n",
    "where SMED_DAT_SERV between '",date_ymd(yr, mth, deb_day),"' and '",date_ymd(yr, mth, fin_day),"'\n"
  )
  if (include_dureeTx_0) {
    query <- paste0(
      query,
      "    and SMED_NBR_JR_DUREE_TRAIT >= 0;"
    )
  } else {
    query <- paste0(
      query,
      "    and SMED_NBR_JR_DUREE_TRAIT > 0;"
    )
  }
  return(query)

}
