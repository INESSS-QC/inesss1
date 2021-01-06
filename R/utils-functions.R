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
    return(sapply(x, function(x) {
      if (nchar(x) == 10 && str_sub(x, 5, 5) == "-" && str_sub(x, 8, 8) == "-") {
        return(x)
      } else {
        return(as_date(as.numeric(x)) - 25569L)
      }
    }))
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
#' Combine dans un même tableau le tableau des résultats, le tableau des arguments et un exemple de code SQL
#'
#' @param dt Tableau des résultats.
#' @param args_list `list` contenant les arguments.
#' @param query Requête SQL.
#'
#' @keywords internal
#' @import data.table
#' @importFrom stringr str_split
#' @export
create_dt_data_args_query <- function(dt, args_list, query) {
  ### Tableau des résultats, ajouter les arguments ainsi que la requête créée
  ### à partir des arguments.

  # Déterminer le nombre de lignes que l'onglet Excel doit avoir.
  query <- data.table(`Requête SQL` = str_split(query, "\n")[[1]]) # séparer la chaine de caractères en vecteur
  nb_row <- max(nrow(dt), sapply(args_list, length), nrow(query))  # nbre de lignes nécessaires

  # Ajouter des valeurs à tous les éléments qui ont une longueur < nb_row
  if (nrow(dt) < nb_row) {
    dt <- rbind(dt, data.table(DATE_DEBUT = rep(NA, nb_row - nrow(dt))), fill = TRUE)
  }
  for (i in 1:length(args_list)) {
    if (length(args_list[[i]]) < nb_row) {
      args_list[[i]] <- c(args_list[[i]], rep(NA, nb_row - length(args_list[[i]])))
    }
  }
  if (nrow(query) < nb_row) {
    query <- rbind(query, data.table(`Requête SQL` = rep(NA, nb_row - nrow(query))))
  }

  # Regrouper tous les éléments ensemble dans un même tableau
  tables_in_one <- cbind(
    dt,
    data.table(v_1 = rep(NA, nb_row),  # espaces pour séparer les éléments
               v_2 = rep(NA, nb_row), v_3 = rep(NA, nb_row)),
    as.data.table(args_list),
    data.table(v_4 = rep(NA, nb_row),
               v_5 = rep(NA, nb_row), v_6 = rep(NA, nb_row)),
    query
  )
  setnames(tables_in_one, paste0("v_",1:6), rep("", 6))  # supprimer le nom des colonnes pour espacement

  return(tables_in_one)
}


#' Utils
#'
#' Emplacement sur le disque dur où le script est sauvegardé.
#'
#' Si le script n'est pas sauvegardé, retourne `NULL`.
#'
#' @return CHR. Emplacement du dossier qui contient le script R.
#'
#' @keywords internal
#' @importFrom rstudioapi getActiveDocumentContext
#' @export
#' @examples
#' file_directory()
file_directory <- function(){
  dir <- dirname(getActiveDocumentContext()$path)
  if(dir == ""){
    return(NULL)
  } else {
    return(dir)
  }
}


#' Utils
#'
#' Transforme une `list` en `data.table` sans répéter les valeurs, ajoute des `NA`s.
#'
#' @param List Liste à convertir en data.table
#'
#' @keywords internal
#' @importFrom data.table as.data.table
#' @export
list_as_dt_no_recycled <- function(List) {

  nval_tot <- max(sapply(List, length))  # nombre de valeurs max

  # Ajouter des NA aux éléments qui ont moins de nval valeurs
  for (l in names(List)) {
    nval <- length(List[[l]])
    if (nval < nval_tot) {
      List[[l]] <- c(List[[l]], rep(NA, nval_tot - nval))
    }
  }

  return(as.data.table(List))

}


#' Utils
#'
#' @param x Nombre de fois que l'on répète le retour de ligne.
#' @return '\\n'
#' @keywords internal
#' @export
nl <- function(x = 1) {
  ### nl = New line = '\n'
  return(paste(rep("\n", x), collapse = ""))
}


#' Utils
#'
#' Indique le nom de la colonne indiquant le nom du type de Rx dans un data
#'
#' @param type_rx Type de code, "DENOM", "DIN", ...
#'
#' @keywords internal
#' @return `"DENOM"` : `"NOM_DENOM"`.\cr
#' `"DIN"` = `"NOM_MARQ_COMRC`.
#' @export
nom_type_rx <- function(type_rx) {
  ###

  if (type_rx == "DENOM") {
    return("NOM_DENOM")
  } else if (type_rx == "DIN") {
    return("NOM_MARQ_COMRC")
  } else {
    stop("nom_type_rx() valeur non permise.")
  }
}


#' Utils
#'
#' Remplace les `NA`s dans un tableau par `by`.
#'
#' @param dt Tableau contenant des `NA`s.
#' @param by Valeur de remplacement.
#'
#' @keywords internal
#' @export
replace_NA_in_dt <- function(dt, by) {
  dt <- as.data.table(dt)
  if (is.character(by)) {
    to_char <- TRUE
  }
  for (j in 1:ncol(dt)) {
    if (to_char && !is.character(dt[[j]])) {
      dt[[j]] <- as.character(dt[[j]])
    }
    set(dt, which(is.na(dt[[j]])), j, by)
  }
  return(dt)
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


#' Utils
#'
#' Combinaison de `sort()` et `unique()`.
#'
#' @param x Vecteur à trier et supprimer doublons.
#' @param decreasing Ordre décroissant = `TRUE`, sinon `FALSE`.
#' @param na.last Afficher les `NA` à la fin = `TRUE`, sinon `FALSE`. `NA` n'affiche pas les valeurs `NA`.
#'
#' @keywords internal
#' @export
sunique <- function(x, decreasing = FALSE, na.last = FALSE) {
  return(sort(unique(x), decreasing = decreasing, na.last = na.last))
}
