#' Utils
#'
#' FROM clause de SQL
#'
#' @param bd Nom de la base de données. Souvent `PROD`.
#' @param vue Nom de la vue.
#'
#' @encoding UTF-8
#' @return "from `bd`.`vue`"
#' @keywords internal
#' @export
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
#' @encoding UTF-8
#' @return Quatre (4) espaces répétés `niv` fois.
#' @keywords internal
#' @export
indent <- function(niv = 1) {
  if (niv == "select") {
    return("       ")
  } else {
    return(paste0(rep("    ", niv)))
  }
}


#' Utils
#'
#' Converti un vecteur R en code SQL
#'
#' @param x Vecteur
#' @keywords internal
#' @encoding UTF-8
#' @return c("x", "y") -> "'x','y'"
#' @export
qu <- function(x) {
  return(paste(paste0("'",x,"'"), collapse = ", "))
}
