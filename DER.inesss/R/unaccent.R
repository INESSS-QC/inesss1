#' Supprimer les accents
#'
#' Permet de convertir des caractères avec accents sans accent.
#'
#' @param text Chaîne de caractères à convertir, où l'on veut supprimer les accents.
#'
#' @importFrom stringr str_replace_all
#'
#' @return \code{text} sans accent.
#' @encoding UTF-8
#'
#' @export
#' @examples
#' unaccent("âàéèïì")
unaccent <- function(text) {
  text <- str_replace_all(text, "['`^~\"]", " ")
  text <- iconv(text, to = "ASCII//TRANSLIT//IGNORE")
  text <- str_replace_all(text, "['`^~\"]", "")
  return(text)
}
