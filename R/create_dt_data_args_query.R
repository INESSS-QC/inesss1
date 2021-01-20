#' Utils
#'
#' Combine dans un même tableau un tableau des résultats, un tableau des arguments et un exemple de code SQL
#'
#' @param dt Tableau des résultats.
#' @param args_list `list` contenant les arguments.
#' @param query Requête SQL.
#'
#' @keywords internal
#' @encoding UTF-8
#' @import data.table
create_dt_data_args_query <- function(dt, args_list, query) {

  ### Déterminer le nombre de lignes que l'onglet Excel doit avoir.
  query <- data.table(`Requête SQL` = stringr::str_split(query, "\n")[[1]]) # séparer la chaine de caractères en vecteur
  nb_row <- max(nrow(dt), sapply(args_list, length), nrow(query))  # nbre de lignes nécessaires

  ### Ajouter des valeurs à tous les éléments qui ont une longueur < nb_row
  if (nrow(dt) < nb_row) {  # tableau des résultats : dt
    dt <- rbind(dt, data.table(DATE_DEBUT = rep(NA, nb_row - nrow(dt))), fill = TRUE)
  }
  for (i in 1:length(args_list)) {  # chaque élément de la liste des arguments
    if (length(args_list[[i]]) < nb_row) {
      args_list[[i]] <- c(args_list[[i]], rep(NA, nb_row - length(args_list[[i]])))
    }
  }
  if (nrow(query) < nb_row) {  # code SQL : query
    query <- rbind(query, data.table(`Requête SQL` = rep(NA, nb_row - nrow(query))))
  }

  ### Regrouper tous les éléments ensemble dans un même tableau
  tables_in_one <- cbind(
    dt,
    data.table(v_1 = rep(NA, nb_row),  # espaces pour séparer les éléments
               v_2 = rep(NA, nb_row), v_3 = rep(NA, nb_row)),
    as.data.table(args_list),
    data.table(v_4 = rep(NA, nb_row),
               v_5 = rep(NA, nb_row), v_6 = rep(NA, nb_row)),
    query
  )
  setnames(tables_in_one, paste0("v_",1:6), rep("", 6))  # supprimer le nom des colonnes servant d'espacement

  return(tables_in_one)

}
