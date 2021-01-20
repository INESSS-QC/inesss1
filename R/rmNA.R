#' Utils
#'
#' Supprime les `NA`s du vecteur. Renvoie `NULL` si aucune valeur.
#'
#' @param x Vecteur.
#'
#' @keywords internal
#' @encoding UTF-8
rmNA <- function(x) {

  if (anyNA(x)) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NULL)
    }
  }

  return(x)

}
