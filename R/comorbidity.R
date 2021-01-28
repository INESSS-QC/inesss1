comorbidity <- function(
  dt, ID, DIAGN, DATE_DX, SOURCE,
  indic = c("charlson", "elixhauser"),
  n1 = 30, n2 = 730,
  confirm_sourc = list("MED-ECHO" = 1, "BDCU" = 2, "SMOD" = 2)
) {

  ### Arranger dataset à analyser
  if (!is.data.table(dt)) {  # convertir data.table au besoin
    dt <- as.data.table(dt)
  }
  dt <- dt[, c(ID, DIAGN, DATE_DX,  SOURCE), with = FALSE]  # sélection des colonnes
  setnames(dt, names(dt), c("ID", "DIAGN", "DATE_DX", "SOURCE"))  # renommer les colonnes
  # DATE_DX est une date
  if (!lubridate::is.Date(dt$DATE_DX)) {
    dt[, DATE_DX := lubridate::as_date(DATE_DX)]  # convertir au format date au besoin
    if (anyNA(dt$DATE_DX)) {  # erreur s'il y a des NA après convertion
      stop("DATE_DX contient au moins une valeur qui n'est pas une date au format 'AAAA-MM-JJ'.")
    }
  }
  dt[, DATE_DX := as.integer(DATE_DX)]  # convertir en integer -> memory efficient

  ### Confirmation des codes de diagnostiques
  dt <- confirm_diagn(dt, n1, n2, confirm_sources)

  ### Mettre une colonne par diagn
  dt <- dcast(dt[, .(ID, DIAGN, val = 1L)], ID ~ DIAGN, value.var = "val")
  dt <- replace_NA_in_dt(dt, 0L)

}

#' Comorbidity
#'
#' @encoding UTF-8
#' @keywords internal
#' @return `data.table` de 6 colonnes :
#' * ID : Numéro d'identification de l'usager.
#' * DIAGN : Code de diagnostique.
#' * DATE_REP : Première date de diagnostique confirmée.
#' * SOURCE_REP : Provenance de `DATE_REP`.
#' * DATE_CONF : Date qui confirme `DATE_REP`.
#' * SOURCE_CONF : Provenance de `SOURCE_CONF`.
comorbidity.confirm_diagn <- function(dt, n1, n2, confirm_sources) {
  ### Confirmation des codes s'il y a au moins un 2e code qui le suit dans
  ### l'intervale [n1, n2].

  ### Trier les données selon l'importance des sources
  for (desc in names(confirm_sourc)) {
    dt[SOURCE == desc, tri := confirm_sourc[[desc]]]
  }
  setkey(dt, ID, DIAGN, DATE_DX, tri, SOURCE)  # tri croissant
  dt[, tri := NULL]

  ### Conserver un data pour les informations
  infos <- unique(dt[, .(ID, DIAGN, DATE_DX, SOURCE)])
  infos <- infos[, .SD[1], .(ID, DIAGN, DATE_DX)]

  ### Confirmer les diagn
  idx <- rmNA(dt[, .I[.N > 1], .(ID, DIAGN)]$V1)  # no lignes ayant 2 obs ou plus
  if (length(idx)) {
    dt[  # nombres de jours entre DATE_DX et la valeur qui la précède
      idx,
      diff := DATE_DX - shift(DATE_DX),
      .(ID, DIAGN)
    ][is.na(diff), diff := 0L]
  } else {
    dt[, diff := 0L]
  }
  dt[  # somme cumulative de diff qui recommence à 0 si dépasse n2
    , cdiff := Reduce(function (x, y) {
      z <- x + y
      if (z > n2) return(0L) else return(z)
    }, x = diff, accumulate = TRUE),
    .(ID, DIAGN)
  ]
  # Indiquer si les dates confirment une date précédente
  dt[, `:=` (diff_conf = FALSE, cdiff_conf = FALSE)]
  sourc1 <- comorbidity.confirm_sourc_names(confirm_sourc, 1)  # sources n'ayant pas besoin de confirmation
  if (length(sourc1)) {
    dt[  # confirmation automatique -> TRUE
      SOURCE %in% sourc1,
      `:=` (diff_conf = TRUE,
            cdiff_conf = TRUE)
    ]
  }
  sourc2 <- comorbidity.confirm_sourc_names(confirm_sourc, 2)  # sources ayant besoin d'une confirmation dans le future [n1, n2]
  if (length(sourc2)) {
    dt[  # vérifier le nombre de jours à partir de diff
      SOURCE %in% sourc2 &
        n1 <= diff & diff <= n2,
      diff_conf := TRUE
    ][  # vérifier le nombre de jours à partir de cdiff
      SOURCE %in% sourc2 &
        n1 <= cdiff & cdiff <= n2,
      cdiff_conf := TRUE
    ]
  }
  # Conserver les confirmations de diagnostiques seulement
  dt <- dt[diff_conf == TRUE | cdiff_conf == TRUE]
  dt <- dt[, .SD[1], .(ID, DIAGN)]  # conserver la première confirmation seulement
  # Indiquer le nombre de jours à conserver entre diff ou cdiff pour retrouver la date à utiliser
  dt[
    diff_conf == TRUE & cdiff_conf == TRUE,
    njours := min(diff, cdiff),
    .(ID, DIAGN)
  ]
  dt[
    diff_conf == TRUE & cdiff_conf == FALSE,
    njours := diff
  ]
  dt[
    diff_conf == FALSE & cdiff_conf == TRUE,
    njours := cdiff
  ]
  dt <- dt[, .(ID, DIAGN, DATE_CONF = DATE_DX, SOURCE_CONF = SOURCE, DATE_REP = DATE_DX - njours)]

  ### Arranger le dataset pour présentation finale
  infos <- infos[ID %in% dt$ID]  # conserver les ID existant dans dt
  dt <- infos[, .(ID, DIAGN, DATE_REP = DATE_DX, SOURCE_REP = SOURCE)][dt, on = .(ID, DIAGN, DATE_REP)]

  ### Dates au format AAAA-MM-JJ
  dt[, `:=` (DATE_REP = lubridate::as_date(DATE_REP),
             DATE_CONF = lubridate::as_date(DATE_CONF))]

  return(dt)

}
comorbidity.confirm_sourc_names <- function(confirm_sourc, val) {
  ### Indique le ou les noms des sources qui ont la valeur 'val'.

  return(unlist(lapply(names(confirm_sourc), function(x) {
    if (confirm_sourc[[x]] == val) {
      return(x)
    } else {
      return(NULL)
    }
  })))

}
