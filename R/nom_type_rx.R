#' Utils
#'
#' Indique le nom de la colonne indiquant le nom du type de Rx dans un data
#'
#' @param type_rx Type de code, "DENOM", "DIN", ...
#'
#' @keywords internal
#' @encoding UTF-8
#' @return `"DENOM"` : `"NOM_DENOM"`.\cr
#' `"DIN"` = `"NOM_MARQ_COMRC`.
nom_type_rx <- function(type_rx) {

  if (type_rx == "DENOM") {
    return("NOM_DENOM")
  } else if (type_rx == "DIN") {
    return("NOM_MARQ_COMRC")
  } else {
    stop("nom_type_rx() valeur non permise.")
  }

}
