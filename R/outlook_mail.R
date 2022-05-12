#' Astuce
#'
#' Envoyer un courriel à partir de Outlook.\cr
#' **ATTENTION** Vérifier l'adresse utilisée s'il y a plusieurs comptes.\cr
#' La librairie \href{https://github.com/omegahat/RDCOMClient}{RDCOMClient} doit être installée. Voir la fonction \code{\link{install_RDCOMClient}}.
#'
#' @param to Destinataire(s). Section `From` / `À`.
#' @param cc Destinaire(s). Section `Cc`.
#' @param subject Objet du courriel.
#' @param body Message du courriel. Utiliser `\n` pour un retour de ligne.
#' @param attachments Répertoire du ou des fichiers à mettre en pièce jointe.
#'
#' @export
#' @encoding UTF-8
#'
#' @examples
#' \donttest{
#' outlook_mail(to = "Mon.Directeur@inesss.qc.ca",
#'              cc = "Mon.Collegue@inesss.qc.ca",
#'              subject = "Projet1 - Finalisation",
#'              body = "Bonjour,\n Il faudrait se rencontrer pour en discuter.\n
#'                      Merci\n\n Mon Nom")
#' }
outlook_mail <- function(to = NULL, cc = NULL, subject = NULL, body = NULL,
                         attachments = NULL) {

  ### Erreur s'il n'y a pas de destinataires
  if (is.null(to) && is.null(cc)) {
    stop("Il n'y a aucune adresse d'envoi.")
  }

  ### Installer la librairie au besoin
  if (!"RDCOMClient" %in% installed.packages()[, 1]) {
    install_RDCOMClient()
  }

  if (!"RDCOMClient" %in% installed.packages()[, 1]) {

    message("La librairie RDCOMCLIENT ne s'est pas installée. Envoi du courriel impossible.")

  } else {

    library(RDCOMClient)
    outlook <- COMCreate("Outlook.Application")
    email <- outlook$CreateItem(0)

    ### Ajout des informations
    for (varia in c("to", "cc", "subject", "body")) {
      if (!is.null(get(varia))) {
        email[[varia]] <- get(varia)
      }
    }

    ### Pièce jointe
    if (!is.null(attachments)) {
      for (pj in attachments) {
        email[["attachments"]]$Add(pj)
      }
    }

    ### Envoyer le courriel
    email$Send()

  }

}
