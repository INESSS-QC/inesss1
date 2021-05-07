#' Convertion RLS
#'
#' Le projet de loi n\ifelse{html}{\out{<sup>o</sup>}}{\eqn{^{o}}}10 a modifié la plupart des codes RLS (voir la table `RLS_tab_convert`). Cette fonction permet de convertir les RLS d'une table si elle contient des codes qui existaient avant la loi 10.
#'
#' @param dt Table pouvant contenir des RLS à convertir.
#' @param rls_colname Nom de la colonne contenant les codes de RLS.
#'
#' @return `data.table`
#' @export
#'
#' @examples
#' dt = data.frame(id = 1:5,
#'                 age = c(45, 65, 78, 15, 35),
#'                 sexe = c("F", "M", "F", "F", "M"),
#'                 rls = c(1204, 215, 1611, 1503, 1610))
#' rls_colname = "rls"
#' dt_convert <- RLS_convert(dt, rls_colname = "rls")
RLS_convert <- function(dt, rls_colname) {

  ### Convertir en data.table
  if (!is.data.table(dt)) {
    setDT(dt)
  }

  ### Conserver ordre des colonnes
  ordercol <- names(dt)

  ### Convertir les anciens RLS vers les nouveaux RLS
  # Table de convertion
  rls_tab <- inesss::RLS_tab_convert
  setnames(rls_tab, names(rls_tab), c(rls_colname, "new_rls"))
  # Ajouter la colonne des nouveaux RLS
  dt <- rls_tab[dt, on = rls_colname]
  # Si NA -> pas de conversion; Sinon besoin de convertir
  dt[!is.na(new_rls), (rls_colname) := new_rls]
  dt[, new_rls := NULL]

  ### Ordre des colonnes
  setcolorder(dt, ordercol)

  return(dt)

}
