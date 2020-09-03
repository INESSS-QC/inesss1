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
#' Converti un vecteur R en code SQL
#'
#' @param x Vecteur
#' @keywords internal
#' @return c("x", "y") -> "'x','y'"
#' @export
sql_quote <- function(x) {
  return(paste(paste0("'",x,"'"), collapse = ","))
}
