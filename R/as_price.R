#' Utils
#'
#' Force les deux décimales, car `x` est un prix/cout.
#'
#' @param x Vecteur prix/cout
#' @encoding UTF-8
#' @keywords internal
as_price <- function(x) {

  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }

  return(round(x, 2))

}
