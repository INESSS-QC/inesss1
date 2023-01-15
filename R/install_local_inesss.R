#' Install package inesss
#'
#' Installation ou mise à jour de la librairie selon la direction indiquée.
#'
#' @param direction Nom de la direction.
#'
#' @export
install_local_inesss <- function(direction) {

  ### À modifier quand

  if (team == "DER") {
    if (length(list.files("C:/Users/ms045/Desktop/Github/"))) {
      remotes::install_local("C:/Users/ms045/Desktop/Github/inesss_1.1.0.9000.tar.gz",
                             upgrade = "never")
    }
  }

}
