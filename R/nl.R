#' Utils
#'
#' @param x Nombre de fois que l'on répète le retour de ligne.
#' @return '\\n'
#' @keywords internal
#' @encoding UTF-8
nl <- function(x = 1) {

  ### nl = New line = '\n'
  return(paste(rep("\n", x), collapse = ""))

}
