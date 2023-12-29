#' Arrangement périodes
#'
#' Fusionne les périodes qui se chevauchent dans le temps.
#'
#' La variable `cols` ne peut contenir les valeurs "debut" ou "fin".
#'
#' @param dt Data à arranger.
#' @param debut Nom de la colonne indiquant le début de la période.
#' @param fin Nom de la colonne indiquant la fin de la période.
#' @param cols Nom de la ou des colonnes indiquant en quoi consiste la période (codes d'identification d'individus ou de produits).
#' @param njours Nombre de jours entre le début et la fin précédente (debut{i} - fin{i-1}). Voir examples. Par défaut 1.
#'
#' @import data.table
#'
#' @return `data.table`
#' @export
#' @examples
#' require(data.table)
#' ex1 <- data.frame(
#'   ID = c(1, 1, 1, 2, 2),
#'   DATE_DEBUT = as.Date(c("2020-03-01", "2020-04-01", "2020-05-02",
#'                          "2020-06-15", "2020-06-22")),
#'   DATE_FIN = as.Date(c("2020-03-31", "2020-04-30", "2020-05-31",
#'                        "2020-06-25", "2020-06-30"))
#' )
#' ex1_1 <- fusion_periodes(ex1, debut = "DATE_DEBUT", fin = "DATE_FIN", cols = "ID", njours = 1L)
fusion_periodes <- function(dt, debut, fin, cols, njours = 1L) {

  if (!is.data.table(dt)) {
    setDT(dt)
  }

  select_cols <- c(cols, debut, fin)
  dt <- dt[, ..select_cols]
  setnames(dt, c(debut, fin), c("debut", "fin"))

  dt[, diff := as.integer(debut - shift(fin)), by = cols]
  idx <- rmNA(dt[, .I[diff <= njours], by = cols]$V1)
  while(length(idx)) {
    dt[is.na(diff), diff := 0L]
    dt[, per := 0L][diff > njours, per := 1L]
    dt[, per := cumsum(per) + 1L, by = cols]
    dt <- dt[
      , .(debut = min(debut),
          fin = max(fin)),
      by = c(cols, "per")
    ]
    dt[, diff := as.integer(debut - shift(fin)), by = cols]
    idx <- rmNA(dt[, .I[diff <= njours], by = cols]$V1)
  }
  dt[, `:=` (per = NULL, diff = NULL)]

  setnames(dt, c("debut", "fin"), c(debut, fin))
  return(dt)

}
