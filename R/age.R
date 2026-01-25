#' Métrique
#'
#' Calcul de l'âge d'un individu à une date de référence.
#'
#' Il existe plusieurs manière de calculer l'âge dont diviser par 365 ou 365.25. Ces divisions ne gèrent pas les années bissextiles alors que l'utilisation de la fonction `age()` le fait.
#'
#' @param naissance Date de naissance de l'individu au format AAAA-MM-JJ.
#' @param reference Date de référence à laquelle on veut calculer l'âge de l'individu.
#'
#' @returns `vector`
#' @importFrom lubridate as.period interval is.Date year
#' @encoding UTF-8
#' @export
#'
#' @examples
#' library(inesss)
#'
#' # Vecteur
#' vec_age <- age(ech_population$NAISS, Sys.Date())
#'
#' # dplyr
#' library(dplyr)
#' df <- ech_population %>% mutate(AGE = age(NAISS, Sys.Date()))
#' \dontrun{
#' # data.table
#' library(data.table)
#' dt <- copy(ech_population)
#' dt[, AGE := age(NAISS, Sys.Date())]
#' }
age <- function(naissance, reference) {

  # Vérification des arguments
  age.verif_args(naissance, reference)

  # Vecteur Âge
  return(year(as.period(interval(naissance, reference))))

}


#' Verif Arguments
#'
#' @encoding UTF-8
#' @keywords internal
#' @export
age.verif_args <- function(naissance, reference) {

  check <- newArgCheck()
  if (!is.Date(naissance)) {
    addError("naissance n'est pas au format Date.", check)
  }
  if (!is.Date(reference)) {
    addError("reference n'est pas au format Date.", check)
  }
  finishArgCheck(check)

}
