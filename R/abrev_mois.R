#' Utils
#'
#' @param mois Nombre entier entre 1 et 12 en référence aux 12 mois de l'année.
#'
#' @return `string`
#' @encoding UTF-8
#' @keywords internal
#' @export
abrev_mois <- function(mois) {
  if (mois == 1) {
    return("JAN")
  } else if (mois == 2) {
    return("FEV")
  } else if (mois == 3) {
    return("MAR")
  } else if (mois == 4) {
    return("AVR")
  } else if (mois == 5) {
    return("MAI")
  } else if (mois == 6) {
    return("JUN")
  } else if (mois == 7) {
    return("JUI")
  } else if (mois == 8) {
    return("AOU")
  } else if (mois == 9) {
    return("SEP")
  } else if (mois == 10) {
    return("OCT")
  } else if (mois == 11) {
    return("NOV")
  } else if (mois == 12) {
    return("DEC")
  } else {
    stop("abrev_mois(): mois doit être entre 1 et 12")
  }
}
