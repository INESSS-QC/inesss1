#' Utils
#'
#' Modifier le répertoire des packages utilisés entre les environnements de PROD ou DEV.
#'
#' @param x \code{"PROD"} ou \code{"DEV"}. Par défaut \code{"PROD"}.
#'
#' @returns \code{.libPaths()}
#' @encoding UTF-8
#' @importFrom crayon green yellow
#' @export
#'
#' @examples
#' \dontrun{
#' rlibs("PROD")
#' rlibs("DEV")
#' }
rlibs <- function(x = "PROD") {

  if (toupper(x) == "PROD") {
    .libPaths("U:/INSPQ/libs-PROD")
    cat("Répertoire des librairies en PROD : ", green(.libPaths()[1]), ".", sep = "")
  } else if (toupper(x) == "DEV") {
    .libPaths(Sys.getenv("R_LIBS_USER"))
    cat("Répertoire des librairies en DEV : ", yellow(.libPaths()[1]), ".", sep = "")
  }

}
