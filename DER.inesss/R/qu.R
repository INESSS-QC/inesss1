#' Utils
#'
#' Converti un vecteur R en code SQL
#'
#' @param x Vecteur
#'
#' @return c("x", "y") -> "'x','y'"
#' @encoding UTF-8
#' @keywords internal
#'
#' @export
qu <- function(x) {
  return(paste(paste0("'",x,"'"), collapse = ", "))
}
