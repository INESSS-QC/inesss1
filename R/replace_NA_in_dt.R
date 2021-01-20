#' Utils
#'
#' Remplace les `NA`s dans un tableau par `by`.
#'
#' @param dt Tableau contenant des `NA`s.
#' @param by Valeur de remplacement.
#'
#' @keywords internal
#' @encoding UTF-8
replace_NA_in_dt <- function(dt, by) {

  if (!data.table::is.data.table(dt)) {
    dt <- data.table::as.data.table(dt)
  }

  if (is.character(by)) {
    to_char <- TRUE
  }

  for (j in 1:ncol(dt)) {
    if (to_char && !is.character(dt[[j]])) {
      dt[[j]] <- as.character(dt[[j]])
    }
    data.table::set(dt, which(is.na(dt[[j]])), j, by)
  }

  return(dt)

}
