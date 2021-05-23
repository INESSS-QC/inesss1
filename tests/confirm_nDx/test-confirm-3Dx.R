library(data.table)
library(lubridate)


# Data test -----------------------------------------------------------------------------------

set.seed(42)
nID <- 10000L
DT <- vector("list", nID)
for (i in 1:nID) {  # chaque ID
  DT[[i]] <- data.table(
    Id = i,
    Date = as_date(sample(x = 16072:18261,  # dates entre 2014-01-02 et 2019-12-31
                          size = sample(1:30, 1)))  # nombre aléatoire de valeurs
  )
}
DT <- rbindlist(DT)


# Fonction confirmation 3 Dx ------------------------------------------------------------------

#' Confirmation Code
#'
#' @param dt Table contenant les dates de diagnostics des individus.
#' @param ID Nom de la colonne contenant le numéro d'identification unique des individus.
#' @param DATE Nom de la colonne contenant la date du diagnostic
#' @param DIAGN Facultatif. Nom de la colonne indiquant les codes de diagnostics
#' @param study_start
#' @param study_end
#' @param n1
#' @param n2
dt = copy(DT)
ID = "Id"
DATE = "Date"
DIAGN = NULL
n1 = 30
n2 = 730
study_start = NULL
study_end = NULL
reverse = FALSE
confirme_3_Dx <- function(dt, ID, DATE, DIAGN = NULL,
                          study_start = NULL, study_end = NULL, n1 = 30, n2 = 730,
                          reverse = FALSE) {

  ### Arranger data
  # Convertir data.table au besoin
  if (is.data.table(dt)) {
    setDT(dt)
  }
  # Créer colonne facultative DIAGN
  if (is.null(DIAGN)) {
    remove_DIAGN <- TRUE  # indiquera plus tard si on la supprime
    DIAGN <- "DIAGN"
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
  for (i in 1:n_iter) {
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
      if (reverse) {
        sd[DATE2 > 0, diff2 := -(DATE2 - max(DATE2)), .(ID, DIAGN)][is.na(diff2), diff2 := 0L]
      } else {
        sd[DATE2 > 0, diff2 := DATE2 - min(DATE2), .(ID, DIAGN)][is.na(diff2), diff2 := 0L]
      }
      # Conserver les ID où la 1ere ligne est confirmé par deux dates
      sd[, keep := FALSE]
      sd[
        any(diff %in% n1:n2) & any(diff2 %in% n1:n2),
        keep := TRUE,
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
      }
    } else {
      break
    }
    dt <- dt[!dt[, .I[1], .(ID, DIAGN)]$V1]  # supprimer la 1ere ligne
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
    return(confirm_tab)
  } else {
    return(NULL)
  }

}


# TEST ----------------------------------------------------------------------------------------

test <- confirme_3_Dx(
  dt = DT, ID = "Id", DATE = "Date", DIAGN = NULL,
  study_start = "2019-01-01", study_end = "2019-12-31",
  n1 = 30, n2 = 730, reverse = TRUE
)

test2 <- confirme_3_Dx(
  dt = DT, ID = "Id", DATE = "Date", DIAGN = NULL,
  study_start = NULL, study_end = NULL,
  n1 = 30, n2 = 730, reverse = FALSE
)

