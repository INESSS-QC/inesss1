#' Install package inesss
#'
#' Installation ou mise à jour de la librairie selon la direction indiquée.
#'
#' @param direction Nom de la direction.
#'
#' @export
install_local_inesss <- function(direction) {

  ### Unload package s'il est en function
  if (any("inesss" == .packages())) {
    detach("package:inesss", unload = TRUE)
  }

  ### Installer le package selon l'équipe
  if (direction == "DER") {
    if (file.exists("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/inesss_DER.tar.gz")) {
      remotes::install_local("J:/GRP/A/5/A/Commun/0 Outils/Librairies R/inesss_DER.tar.gz",
                             upgrade = "never")
    }
  }

}
