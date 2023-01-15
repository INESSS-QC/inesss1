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
    if (file.exists("C:/Users/ms045/Desktop/Github/inesss_1.1.0.9000.tar.gz")) {
      remotes::install_local("C:/Users/ms045/Desktop/Github/inesss_1.1.0.9000.tar.gz",
                             upgrade = "never")
    } else if (file.exists("C:/Users/bogu5550/Documents/GitHub/inesss_1.1.0.9000.tar.gz")) {
      remotes::install_local("C:/Users/bogu5550/Documents/GitHub/inesss_1.1.0.9000.tar.gz",
                             upgrade = "never")
    }
  }

}
