#' Utils
#'
#' Transforme une `list` en `data.table` sans répéter les valeurs, ajoute des `NA`s.
#'
#' @param List Liste à convertir en data.table
#'
#' @keywords internal
#' @encoding UTF-8
list_as_dt_no_recycled <- function(List) {

  nval_tot <- max(sapply(List, length))  # nombre de valeurs max

  ### Ajouter des NA aux éléments qui ont moins de nval valeurs
  for (l in names(List)) {
    nval <- length(List[[l]])
    if (nval < nval_tot) {
      List[[l]] <- c(List[[l]], rep(NA, nval_tot - nval))
    }
  }

  return(data.table::as.data.table(List))

}
