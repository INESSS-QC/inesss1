from_bd.vue <- function(bd = "Prod", vue = "V_DEM_PAIMT_MED_CM") {
  ### Provenance des données
  return(paste0("from ",bd,".",vue))
}


#' Utils
#'
#' Connexion entre R et SQL Teradata
#'
#' @param dsn **D**ata **S**ource **N**ame.
#' @param uid User Identifier.
#' @param pwd Password to use.
#'
#' @importFrom odbc odbc dbConnect
#' @export
conn <- function(dsn, uid, pwd) {
  return(dbConnect(odbc(), dsn, uid = uid, pwd = pwd))
}


#' Utils
#'
#' Indentation du code
#'
#' @param niv Niveau d'indentation.
#'
#' @return Quatre (4) espaces répétés `niv` fois.
#' @export
indent <- function(niv = 1) {
  #### Indentation du code
  return(paste0(rep("    ", niv)))
}


#' Utils
#'
#' Converti un vecteur R en code SQL
#'
#' @param x Vecteur
#' @keywords internal
#' @return c("x", "y") -> "'x','y'"
#' @export
q <- function(x) {
  return(paste(paste0("'",x,"'"), collapse = ","))
}
