#' Macros VBA
#'
#' Télécharge le fichier Excel contenant les macros VBA et l'enregistre au répertoire suivant :\cr
#' \code{C:\\Users\\NomUtilisateur\\AppData\\Roaming\\Microsoft\\Excel\\XLSTART}.\cr
#' L'enregistrer à cet endroit permet de rendre disponible les macros VBA à partir de n'importe quelle classeur Excel.
#'
#' @param filename Nom du fichier contenant les macros VBA. Si le fichier existe déjà, on demande si on veut le remplacer. Par défaut `'PERSONAL.XLSB'`.
#'
#' @encoding UTF-8
#' @export
#' @examples
#' \dontrun{
#' inesss_excel_macros()  # nom = 'PERSONAL.XLSB'
#' inesss_excel_macros(filename = 'LeNomQueJeVeux.xlsb')
#' }
inesss_excel_macros <- function(filename = 'PERSONAL.XLSB') {

  ### Arguments nécessaires
  user <- Sys.info()[["user"]]  # nom de l'utilisateur de l'ordinateur
  dir_to_have <- list("AppData", "Roaming", "Microsoft", "Excel", "XLSTART")  # dossiers qui doivent exister (en ordre)
  dir <- paste0("C:/Users/",user)  # répertoire initial qui existe assurément

  ### Créer les répertoires pour le fichier PERSONAL.XLSB s'ils n'existent pas
  for (folder in dir_to_have) {
    dir <- paste0(dir, "/", folder)  # prochain dossier à vérifier
    if (!dir.exists(dir)) {
      dir.create(dir)  # créer le dossier si le répertoire n'existe pas
    }
  }

  ### Télécharger le fichier dans le bon répertoire
  # Demander si on écrase le fichier s'il existe déjà, sinon rien
  if (filename %in% list.files(dir)) {
    # Oui = TRUE, Non = Faut, Annuler = NA
    answ <- askYesNo("Le fichier existe déjà. Voulez-vous le remplacer?")
  } else {
    answ <- TRUE
  }
  if (answ) {
    download.file(
      url = "https://github.com/INESSS-QC/inesss1/raw/devel/Documentation/Gabarits%20et%20Outils%20EXCEL/PERSONAL.XLSB",
      destfile = paste0(dir,"/",filename), mode = "wb"
    )
  }

}
