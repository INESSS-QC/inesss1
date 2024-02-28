#' Utils
#'
#' Indentation du code
#'
#' @param niv Niveau d'indentation.
#'
#' @return Quatre (4) espaces répétés `niv` fois.
#' @encoding UTF-8
#' @keywords internal
indent <- function(niv = 1) {
  if (niv < 0) {
    stop("indent() : niv doit être plus grand ou égal à zéro.")
  } else if (niv == "select") {
    return("       ")
  } else if (niv == 0) {
    return("")
  } else {
    return(paste0(rep("    ", niv), collapse = ""))
  }
}
