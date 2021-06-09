#' Package - Installation
#'
#' Installation de la librairie \href{https://github.com/omegahat/RDCOMClient}{RDCOMCLIENT}. Si elle est déjà installée, le programme demande si on veut la mettre à jour.
#'
#' @export
#' @encoding UTF-8
install_RDCOMClient <- function(msg = TRUE) {

  ### Vérifier si le package est déjà installé et le mettre à jour, sinon l'installer
  if ("RDCOMClient" %in% installed.packages()[, 1]) {
    answ <- utils::askYesNo("RDCOMClient est déjà installé. Voulez-vous le mettre à jour?")
  } else {
    answ <- TRUE
  }

  if (answ) {
    isError <- testthat::capture_condition({
      install.packages("RDCOMClient", repos = "http://www.omegahat.net/R")
    })
    if (is.null(isError)) {
      message("RDCOMClient a bien été installé.")
    } else {
      message(paste0("ERREUR. ",isError$message,"."))
    }
  }

}
