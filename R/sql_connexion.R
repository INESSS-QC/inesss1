#' Connexion Teradata
#'
#' Connexion entre R et SQL Teradata.
#'
#' Voir \code{\link[odbc]{dbConnect}}.
#'
#' @param uid Identifiant.
#' @param pwd Mot de passe. Si `NULL`, le mot de passe est demandé lors de l'exécution.
#' @param dsn **D**ata **S**ource **N**ame. Par défaut `"PEI_PRD"`.
#' @param encoding `"latin1"` ou `"UTF-8"`. Encodage de la base de données. Par défaut `"latin1"`.
#'
#' @return Connexion Teradata, sinon `NULL`.
#' @importFrom odbc odbc dbConnect
#' @importFrom testthat capture_error
#' @importFrom askpass askpass
#' @export
#' @examples
#' \dontrun{
#' conn <- sql_connexion("abc007")
#' conn <- sql_connexion(uid = "abc007", pwd = "MonMotDePasse",
#'                       dsn = "PEI_PRD", encoding = "latin1")
#' }
sql_connexion <- function(uid, pwd = NULL, dsn = "PEI_PRD", encoding = "latin1") {

  if (is.null(pwd)) {  # demander le mot de passe s'il n'est pas inscrit
    pwd <- askpass("Mot de passe")
  }

  is_error <- capture_error({  # effectuer la connexion et conserver message erreur s'il y en a
    sql_conn <- dbConnect(odbc(), dsn, uid = uid, pwd = pwd, encoding = encoding)
  })

  if (is.null(is_error)) {  # pas d'erreur de connexion
    return(sql_conn)
  } else {
    return(NULL)
  }
}
