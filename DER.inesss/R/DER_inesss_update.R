#' Install package inesss
#'
#' Installation ou mise à jour de la librairie selon la direction indiquée.
#'
#' @param direction Nom de la direction.
#'
#' @encoding UTF-8
#' @keywords internal
#' @export
DER_inesss_update.addins <- function() {

  if (any("DER.inesss" == installed.packages()[,1])) {
    return(DER.DER.inesss::DER_inesss_update())
  } else if (any("inesss" == installed.packages()[,1])) {
    return(DER.inesss::DER_inesss_update())
  }

}

#' Install package inesss
#'
#' Installation ou mise à jour de la librairie selon la direction indiquée.
#'
#' @param direction Nom de la direction.
#'
#' @encoding UTF-8
#'
#' @export
DER_inesss_update <- function() {

  ### Unload package s'il est en function
  if (any("DER.inesss" == .packages())) {
    cat("DER.inesss est en fonction. Il sera désélectionné pour l'installation.\n")
    detach("package:DER.inesss", unload = TRUE)
  }

  ### Installer le package selon l'équipe
  if (file.exists("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/DER.inesss.tar.gz")) {
    cat("Installation - J:/GRP/A/5/A/Commun/0 Outils/Librairies R/DER.inesss.tar.gz\n")
    remotes::install_local("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/DER.inesss.tar.gz",
                           upgrade = "never",
                           force = TRUE)
  } else {
    cat("Fichier d'installation indisponible. Vérifier accès du répertoire d'installation.\n")
  }

}
