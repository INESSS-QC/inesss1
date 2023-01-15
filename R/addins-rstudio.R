#' Démarrer domaine de valeur
#'
#' @return \link{domaine_valeurs}
#' @keywords internal
#' @encoding UTF-8
domaine_valeurs_addin <- function() {
  return(inesss:::domaine_valeurs())
}

#' Démarrer le formulaire
#'
#' @return \link{formulaire}
#' @keywords internal
#' @encoding UTF-8
formulaire_addin <- function() {
  return(inesss:::formulaire())
}

#' Update inesss package
#'
#' @return \link{install_local_inesss}
#' @keywords internal
#' @encoding UTF-8
DER_update_package <- function() {
  return(install_local_inesss(direction = "DER"))
}
