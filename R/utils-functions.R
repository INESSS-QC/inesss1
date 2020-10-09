#' Utils
#'
#' Force les deux décimales, car `x` est un prix/cout.
#'
#' @param x Vecteur prix/cout
#' @keywords internal
#' @export
as_price <- function(x) {
  if (!is.numeric(x))
    x <- as.numeric(x)
  return(round(x, 2))
}


#' Utils
#'
#' @return '\\n'
#' @export
nl <- function() {
  ### nl = New line = '\n'
  return("\n")
}


#' Utils
#'
#' Supprime les NA du vecteur. Renvoie NULL si aucune valeur.
#'
#' @param x Vecteur
#'
#' @keywords internal
#' @export
rmNA <- function(x) {
  if (anyNA(x)) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NULL)
    }
  }
  return(x)
}
