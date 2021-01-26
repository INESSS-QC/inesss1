#' Tronquer un vecteur en plusieurs parties
#'
#' Divise le vecteur `x` en `n_chunks` parties ou le divise pour avoir au maximum `n_vals` valeurs dans chaque partie.
#'
#' Utiliser l'argument `n_chunks` ou `n_vals`, pas les deux.
#'
#' @param x Vecteur à tronquer en plusieurs parties.
#' @param n_chunks Diviser le vecteur en `n_chunks` parties.
#' @param n_vals Chaque partie aura au maximum `n_vals` valeurs.
#'
#' @return `list` ayant `n_chunks` éléments (ou `as.integer(length(x) / n_vals + 1L)`).
#' @export
#' @examples
#' chunk_vec(x = 1:10, n_chunks = 3)
#' chunk_vec(x = 1:10, n_vals = 3)
chunk_vec <- function(x, n_chunks = NULL, n_vals = NULL) {

  ### Utiliser un seul argument parmi n_chunks ou n_vals
  if ((is.null(n_chunks) && is.null(n_vals)) || (!is.null(n_chunks) && !is.null(n_vals))) {
    stop("chunk_vec(): Utiliser un argument parmi n_chunks et n_vals.")
  }

  ### Répartir les valeurs dans une liste de n_chunks partis.
  if (is.null(n_chunks)) {
    n_chunks <- as.integer(length(x) / n_vals + 1L)  # nombre de chunks à avoir
  }
  i <- parallel::splitIndices(length(x), n_chunks)  # indices de 1 à n dans chaque chunk
  x <- lapply(i, function(i) x[i])  # remplacer les indices par les valeurs de x

  return(x)

}
