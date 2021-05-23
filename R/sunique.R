#' Utils
#'
#' Combinaison de `sort()` et `unique()`.
#'
#' @param x Vecteur à trier et supprimer doublons.
#' @param decreasing Ordre décroissant = `TRUE`, sinon `FALSE`.
#' @param na.last Afficher les `NA` à la fin = `TRUE`, sinon `FALSE`. `NA` n'affiche pas les valeurs `NA`.
#'
#' @encoding UTF-8
#' @export
#' @examples
#' x <- sample(c(1:10, NA, NaN))
#' x
#'
#' sunique(x)
#' sunique(x, na.last = TRUE)
#' sunique(x, decreasing = TRUE, na.last = NA)
sunique <- function(x, decreasing = FALSE, na.last = FALSE) {

  return(sort(unique(x), decreasing = decreasing, na.last = na.last))

}
