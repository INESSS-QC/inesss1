#' Utils
#'
#' Convertir une date Excel au format CHR en Date R. Soit une valeur 'AAAA-MM-JJ'
#' ou un nombre entier, mais au format CHR.
#'
#' @param x Vecteur contenant les dates Excel au format entier.
#' @keywords internal
#' @importFrom lubridate as_date
#' @importFrom stringr str_sub
#' @export
as_date_excel_chr <- function(x) {
  if (is.character(x)) {
    if (nchar(x) == 10 && str_sub(x, 5, 5) == "-" && str_sub(x, 8, 8) == "-") {
      return(as_date(x))
    } else {
      return(as_date(as.numeric(x)) - 25569L)
    }
  } else {
    stop("as_date_excel_chr(): x doit être au format character.")
  }
}


#' Utils
#'
#' Force les deux décimales, car `x` est un prix/cout.
#'
#' @param x Vecteur prix/cout
#' @keywords internal
#' @export
as_price <- function(x) {
  if (!is.numeric(x)) x <- as.numeric(x)
  return(round(x, 2))
}


#' Utils
#'
#' @param x Nombre de fois que l'on répète le retour de ligne.
#' @return '\\n'
#' @export
nl <- function(x = 1) {
  ### nl = New line = '\n'
  return(paste(rep("\n", x), collapse = ""))
}


#' Utils
#'
#' Supprime les NA du vecteur. Renvoie NULL si aucune valeur.
#'
#' @param x Vecteur
#'
#' @keywords internal
#' @export
rmNA <- function(x) {
  if (anyNA(x)) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NULL)
    }
  }
  return(x)
}
