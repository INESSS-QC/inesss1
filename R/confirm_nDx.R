#' Astuce
#'
#' Confirmation d'un diagnostic par d'autres diagnostics lorsque ceux-ci se retrouvent dans un intervalle précis.
#'
#' @param dt Table contenant les dates de diagnostics des individus.
#' @param ID Nom de la colonne contenant le numéro d'identification unique des individus.
#' @param DATE Nom de la colonne contenant la date du diagnostic.
#' @param DIAGN Facultatif. Nom de la colonne indiquant les codes de diagnostics.
#' @param study_start Date de début de la période d'étude **contenant les dates de repérage**. Si `NULL`, aura pour valeur la première date de `dt`, la plus ancienne.
#' @param study_end Date de fin de la période d'étude **contenant les dates de repérage**. Si `NULL`, aura pour valeur la dernière date de `dt`, la plus récente.
#' @param n1,n2 Nombre de jours permettant de construire l'intervalle \[n1; n2\] où un code de diagnostic peut en confirmer un autre.
#' @param keep_first `TRUE` ou `FALSE`. Permet d'arrêter le processus si on veut conserver la première date qui est confirmée par une autre dans l'intervalle \[n1; n2\]. Accélère le processus en évitant de confirmer d'autres dates inutilement.
#' @param reverse `TRUE` ou `FALSE`. Si on doit faire la vérification en prenant la date la plus récente et en reculant dans le temps.
#'
#' @name confirm_nDx
#' @encoding UTF-8
#' @examples
#' dt_ex <- data.frame(
#'   id = 1L,
#'   dates = c('2020-01-01', '2020-01-09', '2020-01-10', '2020-01-15', '2020-01-16',
#'             '2020-01-20', '2020-01-26', '2020-01-31')
#' )
#' ex_2dx <- confirm_2Dx(dt = dt_ex, ID = 'id', DATE = 'dates', DIAGN = NULL,
#'                       n1 = 10, n2 = 20, reverse = FALSE)
#' ex_2dx_reverse <- confirm_2Dx(dt = dt_ex, ID = 'id', DATE = 'dates', DIAGN = NULL,
#'                               n1 = 10, n2 = 20, reverse = TRUE)
#' ex_3dx <- confirm_3Dx(dt = dt_ex, ID = 'id', DATE = 'dates',
#'                       n1 = 10, n2 = 20, reverse = FALSE)
#' ex_3dx_reverse <- confirm_3Dx(dt = dt_ex, ID = 'id', DATE = 'dates', DIAGN = NULL,
#'                               n1 = 10, n2 = 20, reverse = TRUE)
#'
#' ### Avec argument DIAGN
#' dt_ex_dx <- data.frame(
#'   id = 1L,
#'   dates = c('2020-01-01', '2020-01-09', '2020-01-10', '2020-01-15', '2020-01-16',
#'             '2020-01-20', '2020-01-26', '2020-01-31'),
#'   dx = c(rep('diab', 4), rep('canc', 4))
#' )
#' ex_2dx_diagn <- confirm_2Dx(dt = dt_ex_dx, ID = 'id', DATE = 'dates', DIAGN = 'dx',
#'                             n1 = 10, n2 = 20, reverse = FALSE)
#'
#' ### study_start & study_end
#' ex_studydates <- confirm_2Dx(dt = dt_ex, ID = 'id', DATE = 'dates', DIAGN = NULL,
#'                              study_start = '2020-01-10', study_end = '2020-01-20',
#'                              n1 = 10, n2 = 20, reverse = FALSE)
#' ex_studydates_rev <- confirm_2Dx(dt = dt_ex, ID = 'id', DATE = 'dates', DIAGN = NULL,
#'                                  study_start = '2020-01-10', study_end = '2020-01-20',
#'                                  n1 = 10, n2 = 20, reverse = TRUE)


#' @rdname confirm_nDx
#' @import data.table
#' @export
confirm_2Dx <- function(
  dt, ID, DATE, DIAGN = NULL,
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730,
  keep_first = FALSE,
  reverse = FALSE
) {

  ### Arranger data
  # Convertir data.table au besoin
  if (!is.data.table(dt)) {
    setDT(dt)
  }
  # Créer colonne facultative DIAGN
  if (is.null(DIAGN)) {
    remove_DIAGN <- TRUE  # indiquera plus tard si on la supprime
    DIAGN <- "DIAGN"
    dt <- copy(dt)
    dt[, DIAGN := 0L]
  } else {
    remove_DIAGN <- FALSE
  }
  dt <- dt[, c(ID, DATE, DIAGN), with = FALSE]  # sélection colonnes
  setnames(dt, names(dt), c("ID", "DATE", "DIAGN"))  # renommer colonnes pour la fonction
  dt <- unique(dt)  # valeurs uniques
  # Convertir les dates en integer pour meilleure performance
  if (!lubridate::is.Date(dt$DATE)) {
    dt[, DATE := as_date(DATE)]
  }
  if (!is.integer(dt$DATE)) {
    dt[, DATE := as.integer(DATE)]
  }

  ### Arranger arguments
  # study_dates
  if (is.null(study_start)) {
    study_start <- min(dt$DATE)
  } else if (!lubridate::is.Date(study_start)) {
    study_start <- lubridate::as_date(study_start)
  }
  if (is.null(study_end)) {
    study_end <- max(dt$DATE)
  } else if (!lubridate::is.Date(study_end)) {
    study_end <- lubridate::as_date(study_end)
  }

  ### Nombre d'itérations nécessaires au maximum
  dt[, n_iter := 1:.N, .(ID, DIAGN)]
  n_iter <- max(dt$n_iter)
  dt[, n_iter := NULL]

  ### Confirmation des codes
  confirm_tab <- vector("list", n_iter)  # où stocker les dates confirmées
  if (reverse) {
    setorder(dt, ID, DIAGN, -DATE)
  } else {
    setkey(dt, ID, DIAGN, DATE)
  }
  for (i in 1:n_iter) {
    idx <- dt[, .I[.N >= 2], .(ID, DIAGN)]$V1  # lignes où les ID ont 2 obs ou plus
    if (length(idx)) {
      sd <- dt[idx]  # subset data
      # Trouver 1ère date où la différence = [n1, n2]
      sd[, diff := as.integer(DATE - shift(DATE)), .(ID, DIAGN)][is.na(diff), diff := 0L]
      if (reverse) {
        sd[, diff := -diff]
      }
      sd[, diff := cumsum(diff), .(ID, DIAGN)]  # nombre de jours cumulés
      # Conserver les ID où la 1ere ligne est confirmé par deux dates
      sd[
        , keep := fcase(any(diff %in% n1:n2), TRUE,
                        default = FALSE),
        .(ID, DIAGN)
      ]
      # sd[, keep := FALSE]
      # sd[any(diff %in% n1:n2), keep := TRUE, .(ID, DIAGN)]
      sd <- sd[keep == TRUE]
      if (nrow(sd)) {
        if (reverse) {
          sd[, DATE_REP := max(DATE), .(ID, DIAGN)]  # date de repérage
          sd[diff >= n1, DATE_CONF1 := max(DATE), .(ID, DIAGN)]  # date qui confirme DATE_REP
        } else {
          sd[, DATE_REP := min(DATE), .(ID, DIAGN)]  # date de repérage
          sd[diff >= n1, DATE_CONF1 := min(DATE), .(ID, DIAGN)]  # date qui confirme DATE_REP
        }
        sd <- sd[
          !is.na(DATE_CONF1),
          .(DATE_REP = unique(DATE_REP),
            DATE_CONF1 = unique(DATE_CONF1, na.rm = TRUE)),
          .(ID, DIAGN)
        ]
        confirm_tab[[i]] <- sd
        if (keep_first) {
          # Supprimer les ID qui ont déjà une date confirmée
          dt <- dt[!ID %in% sunique(sd$ID)]
        }
      }
    } else {
      break
    }
    if (nrow(dt)) {
      dt <- dt[!dt[, .I[1], .(ID, DIAGN)]$V1]  # supprimer la 1ere ligne
    } else {
      break
    }
  }

  ### Arrangement data final
  confirm_tab <- rbindlist(confirm_tab)
  if (nrow(confirm_tab)) {
    confirm_tab <- confirm_tab[between(DATE_REP, study_start, study_end)]  # dates de repérage dans la période d'étude
    setkey(confirm_tab, ID, DIAGN, DATE_REP)  # trier
    # Arranger le data final
    confirm_tab[, `:=` (DATE_REP = lubridate::as_date(DATE_REP),  # colonnes au format DATE
                        DATE_CONF1 = lubridate::as_date(DATE_CONF1))]
    if (remove_DIAGN) {
      confirm_tab[, DIAGN := NULL]
    }
    if (reverse) {
      if (remove_DIAGN) {
        setorder(confirm_tab, ID, -DATE_REP)
      } else {
        setorder(confirm_tab, ID, DIAGN, -DATE_REP)
      }
    } else {
      if (remove_DIAGN) {
        setkey(confirm_tab, ID, DATE_REP)
      } else {
        setkey(confirm_tab, ID, DIAGN, DATE_REP)
      }
    }
    setnames(confirm_tab, "ID", ID)
    if (!remove_DIAGN) {
      setnames(confirm_tab, "DIAGN", DIAGN)
    }

    return(confirm_tab)

  } else {
    return(NULL)
  }

}

#' @rdname confirm_nDx
#' @import data.table
#' @export
confirm_3Dx <- function(
  dt, ID, DATE, DIAGN = NULL,
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730,
  keep_first = FALSE,
  reverse = FALSE
) {

  ### Arranger data
  # Convertir data.table au besoin
  if (!is.data.table(dt)) {
    setDT(dt)
  }
  # Créer colonne facultative DIAGN
  if (is.null(DIAGN)) {
    remove_DIAGN <- TRUE  # indiquera plus tard si on la supprime
    DIAGN <- "DIAGN"
    dt <- copy(dt)
    dt[, DIAGN := 0L]
  } else {
    remove_DIAGN <- FALSE
  }
  dt <- dt[, c(ID, DATE, DIAGN), with = FALSE]  # sélection colonnes
  setnames(dt, names(dt), c("ID", "DATE", "DIAGN"))  # renommer colonnes pour la fonction
  dt <- unique(dt)  # valeurs uniques
  # Convertir les dates en integer pour meilleure performance
  if (!lubridate::is.Date(dt$DATE)) {
    dt[, DATE := lubridate::as_date(DATE)]
  }
  if (!is.integer(dt$DATE)) {
    dt[, DATE := as.integer(DATE)]
  }

  ### Arranger arguments
  # study_dates
  if (is.null(study_start)) {
    study_start <- min(dt$DATE)
  } else if (!lubridate::is.Date(study_start)) {
    study_start <- lubridate::as_date(study_start)
    # Supprimer les dates qui ne concordent pas avec study_start
    if (!reverse) {
      dt <- dt[DATE >= study_start]
    }
  }
  if (is.null(study_end)) {
    study_end <- max(dt$DATE)
  } else if (!lubridate::is.Date(study_end)) {
    study_end <- lubridate::as_date(study_end)
    # Supprimer les dates qui ne concordent pas avec study_end
    if (reverse) {
      dt <- dt[DATE <= study_end]
    }
  }

  ### Nombre d'itérations nécessaires
  dt[, n_iter := 1:.N, .(ID, DIAGN)]
  n_iter <- max(dt$n_iter)
  dt[, n_iter := NULL]

  ### Confirmation des codes
  confirm_tab <- vector("list", n_iter)  # où stocker les dates confirmées
  if (reverse) {
    setorder(dt, ID, DIAGN, -DATE)
  } else {
    setkey(dt, ID, DIAGN, DATE)
  }
  for (i in 1:5) {
    idx <- dt[, .I[.N >= 3], .(ID, DIAGN)]$V1  # lignes où les ID ont 3 obs ou plus
    if (length(idx)) {
      sd <- dt[idx]  # subset data
      # Trouver 1ère date où la différence = [n1, n2]
      sd[, diff := as.integer(DATE - shift(DATE)), .(ID, DIAGN)][is.na(diff), diff := 0L]
      if (reverse) {
        sd[, diff := -diff]
      }
      sd[, diff := cumsum(diff), .(ID, DIAGN)]  # nombre de jours cumulés
      # Répéter étape précédente, mais à partir de la date trouvée précédemment
      sd[, DATE2 := DATE][diff < n1, DATE2 := 0L]
      suppressWarnings({  # supprimer message d'avis si aucun DATE2>0 : Dans min(DATE2) : aucun argument trouvé pour min ; Inf est renvoyé
        if (reverse) {
          sd[DATE2 > 0, diff2 := -(DATE2 - max(DATE2)), .(ID, DIAGN)][is.na(diff2), diff2 := 0L]
        } else {
          sd[DATE2 > 0, diff2 := DATE2 - min(DATE2), .(ID, DIAGN)][is.na(diff2), diff2 := 0L]
        }
      })
      # Conserver les ID où la 1ere ligne est confirmé par deux dates
      sd[, keep := FALSE]
      sd[
        , keep := fcase(any(diff %in% n1:n2) & any(diff2 %in% n1:n2), TRUE,
                        default = FALSE),
        .(ID, DIAGN)
      ]
      sd <- sd[keep == TRUE]
      if (nrow(sd)) {
        if (reverse) {
          sd[, DATE_REP := max(DATE), .(ID, DIAGN)]  # date de repérage
          sd[diff >= n1, DATE_CONF1 := max(DATE), .(ID, DIAGN)]  # date qui confirme DATE_REP
          sd[diff2 >= n1, DATE_CONF2 := max(DATE), .(ID, DIAGN)]  # date qui confirme DATE_CONF1
        } else {
          sd[, DATE_REP := min(DATE), .(ID, DIAGN)]  # date de repérage
          sd[diff >= n1, DATE_CONF1 := min(DATE), .(ID, DIAGN)]  # date qui confirme DATE_REP
          sd[diff2 >= n1, DATE_CONF2 := min(DATE), .(ID, DIAGN)]  # date qui confirme DATE_CONF1
        }
        sd <- sd[
          !is.na(DATE_CONF2),
          .(DATE_REP = unique(DATE_REP),
            DATE_CONF1 = unique(DATE_CONF1, na.rm = TRUE),
            DATE_CONF2 = unique(DATE_CONF2, na.rm = TRUE)),
          .(ID, DIAGN)
        ]
        confirm_tab[[i]] <- sd
        if (keep_first) {
          # Supprimer les ID qui ont déjà une date confirmée
          dt <- dt[!ID %in% sunique(sd$ID)]
        }
      }
    } else {
      break
    }
    if (nrow(dt)) {
      dt <- dt[!dt[, .I[1], .(ID, DIAGN)]$V1]  # supprimer la 1ere ligne
    } else {
      break
    }
  }

  ### Arrangement data final
  confirm_tab <- rbindlist(confirm_tab)
  if (nrow(confirm_tab)) {
    confirm_tab <- confirm_tab[between(DATE_REP, study_start, study_end)]  # dates de repérage dans la période d'étude
    setkey(confirm_tab, ID, DIAGN, DATE_REP)  # trier
    # Arranger le data final
    confirm_tab[, `:=` (DATE_REP = lubridate::as_date(DATE_REP),
                        DATE_CONF1 = lubridate::as_date(DATE_CONF1),
                        DATE_CONF2 = lubridate::as_date(DATE_CONF2))]
    if (remove_DIAGN) {
      confirm_tab[, DIAGN := NULL]
    }
    if (reverse) {
      if (remove_DIAGN) {
        setorder(confirm_tab, ID, -DATE_REP)
      } else {
        setorder(confirm_tab, ID, DIAGN, -DATE_REP)
      }
    } else {
      if (remove_DIAGN) {
        setkey(confirm_tab, ID, DATE_REP)
      } else {
        setkey(confirm_tab, ID, DIAGN, DATE_REP)
      }
    }
    setnames(confirm_tab, "ID", ID)
    if (!remove_DIAGN) {
      setnames(confirm_tab, "DIAGN", DIAGN)
    }

    return(confirm_tab)

  } else {
    return(NULL)
  }

}
