#' Install package inesss
#'
#' Installation ou mise à jour de la librairie selon la direction indiquée.
#'
#' @param direction Nom de la direction.
#'
#' @export
DER_inesss_update.addins <- function() {

  if (any("DER.inesss" == installed.packages()[,1])) {
    return(DER.inesss::DER_inesss_update())
  } else if (any("inesss" == installed.packages()[,1])) {
    return(inesss::DER_inesss_update())
  }

}

#' Install package inesss
#'
#' Installation ou mise à jour de la librairie selon la direction indiquée.
#'
#' @param direction Nom de la direction.
#'
#' @export
DER_inesss_update <- function() {

  ### Unload package s'il est en function
  if (any("DER.inesss" == .packages())) {
    detach("package:DER.inesss", unload = TRUE)
  }

  ### Installer le package selon l'équipe
  if (file.exists("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/DER.inesss.tar.gz")) {
    remotes::install_local("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/DER.inesss.tar.gz",
                           upgrade = "never")
  }

}
