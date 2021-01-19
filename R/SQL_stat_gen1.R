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
  ### Seulement conn & uid - les autres sont vérifiés dans la fonction query_stat_gen1()
  dot_args <- list(...)
  if (!"verif" %in% names(dot_args)) {
    dot_args$verif <- TRUE  # vérification par défaut
  }
  if (dot_args$verif) {
    SQL_stat_gen1.verif_args(conn, uid, pwd)
  }

  ### Demander le mot de passe si pas inscrit
  if (is.null(conn) && is.null(pwd)) {
    pwd <- askpass::askpass("Quel est votre mot de passe")
  }

  ### Arranger les arguments
  # codes
  if (type_Rx == "DENOM") {
    # DENOM est une chaîne de caractères de longueur 5
    stringr::str_pad(codes, width = 5, side = "left", pad = "0")
  }
  # code_list
  if (!is.null(code_list)) {
    # code_list est une chaîne de caractères de longueur 2
  stringr::str_pad(code_list, width = 2, side = "left", pad = "0")
  }

  ### Effectuer la connexion si nécessaire
  if (is.null(conn)) {
    conn <- SQL_connexion(uid, pwd)
  }

  ### Effectuer la requête à partir des arguments

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
