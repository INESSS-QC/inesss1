#' Utils
#'
#' FROM clause de SQL
#'
#' @param bd Nom de la base de données. Souvent `PROD`.
#' @param vue Nom de la vue.
#'
#' @return "from `bd`.`vue`"
#' @encoding UTF-8
#' @keywords internal
from_bd.vue <- function(bd = "PROD", vue) {
  ### Provenance des données
  return(paste0("from ",bd,".",vue))
}


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
