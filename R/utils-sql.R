#' Utils
#'
#' FROM clause de SQL
#'
#' @param bd Nom de la base de données. Souvent `PROD`.
#' @param vue Nom de la vue.
#'
#' @return "from `bd`.`vue`"
#' @keywords internal
#' @export
from_bd.vue <- function(bd = "PROD", vue) {
  ### Provenance des données
  return(paste0("from ",bd,".",vue))
}


#' Utils
#'
#' Connexion entre R et SQL Teradata.
#'
#' @param dsn **D**ata **S**ource **N**ame.
#' @param uid User Identifier.
#' @param pwd Password to use.
#'
#' @return Connexion Teradata, sinon `NULL`.
#' @importFrom odbc odbc dbConnect
#' @importFrom testthat capture_error
#' @export
sql_connexion <- function(dsn, uid, pwd, encoding = "latin1") {
  is_error <- capture_error({  # effectuer la connexion et conserver message erreur s'il y en a
    sql_conn <- dbConnect(odbc(), dsn, uid = uid, pwd = pwd, encoding = encoding)
  })
  if (is.null(is_error)) {
    return(sql_conn)
  } else {
    return(NULL)
  }
}


#' Utils
#'
#' Indentation du code
#'
#' @param niv Niveau d'indentation.
#'
#' @return Quatre (4) espaces répétés `niv` fois.
#' @keywords internal
#' @export
indent <- function(niv = 1) {
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
qu <- function(x) {
  return(paste(paste0("'",x,"'"), collapse = ", "))
}
