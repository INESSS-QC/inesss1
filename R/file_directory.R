#' Astuce
#'
#' Emplacement sur le disque dur où le script est sauvegardé.
#'
#' Si le script n'est pas sauvegardé, retourne `NULL`.
#'
#' @return CHR. Emplacement du dossier qui contient le script R.
#'
#' @encoding UTF-8
#' @export
file_directory <- function(){

  path <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if(path == ""){
    return(NULL)
  } else {
    return(dir)
  }

}
