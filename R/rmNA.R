#' Astuce
#'
#' Supprime les `NA`s du vecteur. Renvoie `NULL` si aucune valeur.
#'
#' @param x Vecteur.
#'
#' @encoding UTF-8
#' @export
#'
#' @examples
#' rmNA(c(4, 6, 8, NA, 78, 4, NaN))
rmNA <- function(x) {

  if (anyNA(x)) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NULL)
    }
  }

  return(x)

}
