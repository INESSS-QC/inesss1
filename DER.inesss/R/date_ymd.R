#' Astuce
#'
#' Retourne une date au format `AAAA-MM-JJ`. Utile dans des `for loop`, car `dd` peut prendre la valeur `'last'` (au lieu d'un nombre), donc pas besoin de savoir si le dernier jour du mois est le 28 ou le 29 en février, ou un 30 ou un 31 pour les autres mois.
#'
#' @param yyyy Nombre entier indiquant l'année.
#' @param mm Nombre entier compris entre 1 et 12, où 1 indique janvier et 12 décembre.
#' @param dd Nombre compris entre 1 et 31 selon les mois. Pour remplacer le dernier jour du mois (28, 29, 30, 31), il est possible d'inscrire `dd = 'last'`.
#'
#' @return lubridate::as_date
#' @encoding UTF-8
#' @export
#'
#' @examples
#' date_ymd(2020, 1, 15)
#' date_ymd(2020, 10, 31)
#' date_ymd(2020, 6, 'last')
#' for (yr in 1996:2004) {
#'   print(date_ymd(yyyy = yr, mm = 2, dd = 'last'))
#' }
date_ymd <- function(yyyy, mm, dd) {

  if (mm < 1 || mm > 12) {
    stop("mm est un nombre compris entre 1 (janvier) et 12 (décembre).")
  }

  if (dd != "last") {
    if (mm == 2) {
      if ((yyyy %% 4 == 0 && yyyy %% 100 != 0) || yyyy %% 400 == 0) {
        if (dd > 29) {
          stop(paste0("Le dernier jour de février en ",yyyy," est le 29 (",dd," > 29)."))
        } else if (dd > 28) {
          stop(paste0("Le dernier jour de février en ",yyyy," est le 28 (",dd," > 28)."))
        }
      }
    } else if (mm == 4 && dd > 30) {
      stop("Le dernier jour d'avril en ",yyyy," est le 30 (",dd," > 30).")
    } else if (mm == 6 && dd > 30) {
      stop("Le dernier jour de juin en ",yyyy," est le 30 (",dd," > 30).")
    } else if (mm == 9 && dd > 30) {
      stop("Le dernier jour de septembre en ",yyyy," est le 30 (",dd," > 30).")
    } else if (mm == 11 && dd > 30) {
      stop("Le dernier jour de novembre en ",yyyy," est le 30 (",dd," > 30).")
    } else if (dd > 31) {
      stop("dd ne peut avoir une valeur plus grande que 31.")
    }
  }

  if (dd == "last") {
    if (mm == 12) {
      date_value <- lubridate::as_date(paste(c(yyyy, 12, 31), collapse = "-"))
    } else {
      date_value <- lubridate::as_date(paste(c(yyyy, stringr::str_pad(mm + 1, 2, "left", "0"), "01"), collapse = "-")) - 1L
    }
  } else {
    date_value <- lubridate::as_date(paste(c(yyyy, stringr::str_pad(mm, 2, "left", "0"), dd), collapse = "-"))
  }

  return(date_value)

}
