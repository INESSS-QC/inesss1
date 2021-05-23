#' Utils
#'
#' Convertir une date Excel au format CHR en Date R. Doit être une valeur `AAAA-MM-JJ` ou un nombre entier, mais au format `CHR`.
#'
#' @param x Vecteur contenant les dates Excel au format entier.
#' @keywords internal
#' @encoding UTF-8
as_date_excel_chr <- function(x) {

  if (is.character(x)) {
    return(sapply(x, function(x) {
      if (nchar(x) == 10 && stringr::str_sub(x, 5, 5) == "-" && stringr::str_sub(x, 8, 8) == "-") {
        return(x)
      } else {
        return(lubridate::as_date(as.numeric(x)) - 25569L)
      }
    }))
  } else {
    stop("as_date_excel_chr(): x doit être au format character.")
  }

}
