SQL_stat_gen1 <- function(
  conn = NULL, uid = NULL, pwd = NULL,
  debut, fin,
  type_Rx = "DENOM", codes,
  group_by = "Codes",
  code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
  code_list = NULL, code_list_filtre = "Inclusions",
  ...
) {

  ### Vérification des arguments
  dot_args <- list(...)
  if (!"verif" %in% names(dot_args)) {
    dot_args$verif <- TRUE  # vérification par défaut
  }
  if (dot_args$verif) {

  }

}

SQL_stat_gen1.verif_args <- function(conn, uid, pwd) {
  ### Vérification des arguments
  # Seulement les arguments conn, uid et pwd. Les autres sont vérifiés lors de
  # la fonction query_stat_gen1()

  check <- newArgCheck()

  # conn & uid & pwd
  if ((is.null(conn) && is.null(uid)) || (!is.null(conn) && !is.null(conn))) {
    addError("Utiliser la variable conn OU la combinaison uid et pwd.", check)
  }

  finishArgCheck(check)

}
