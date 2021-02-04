#' Diagn Obstetric
#'
#' Extraction des événements obstétriques
#'
#' @inheritParams SQL_comorbidity_diagn
#'
#' @return
#' @export
SQL_obstetric <- function(
  conn, uid, pwd,
  cohort, debut, fin,
  CIM = c("CIM9", "CIM10"),
  dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
  verbose = TRUE
) {

  ### Arranger les arguments
  # Arguments possiblement manquants
  if (missing(conn)) {
    conn <- NULL
  }
  if (missing(uid)) {
    uid <- NULL
  }
  if (missing(pwd)) {
    pwd <- NULL
  }

  ### Connexion Teradata
  if (is.null(conn)) {  # doit se connecter avec uid+pwd
    if (is.null(pwd)) {  # demande le mot de passe s'il n'a pas été inscrit
      pwd <- askpass::askpass("Quel est votre mot de passe?")
    }
    conn <- SQL_connexion(uid, pwd)  # connexion à Teradata
  }

  ### Extraction des diagn
  if (is.null(conn)) {  # si encore NULL = Erreur
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {

    # CIM9 vs CIM10 -- Filtrer les types de codes si on veut seulement une version
    # de classification de code.
    diagn_codes <- inesss::Obstetrics_diagn_codes
    if (length(CIM) == 1) {
      for (i in names(diagn_codes)) {
        diagn_codes[[i]] <- diagn_codes[[i]][[CIM]]  # conserver seulement CIM9 ou CIM10
      }
    } else {
      for (i in names(diagn_codes)) {
        diagn_codes[[i]] <- unlist(diagn_codes[[i]], use.names = FALSE)  # grouper CIM9 et CIM10 en un seul vecteur
      }
    }

    # Extraction des diagnostiques
    DT <- vector("list", length(dt_source) * length(diagn_codes))  # contiendra toutes les requêtes
    i <- 1L
    for (sour in dt_source) {
      if (verbose) {
        cat(sour,"\n")
      }
      fct <- get(paste0("SQL_comorbidity_diagn.",sour))  # fonction d'extraction selon la source
      for (dia in names(diagn_codes)) {
        t1 <- Sys.time()
        DT[[i]] <- fct(  # tableau provenant de la requête
          conn = conn, ids = sunique(cohort), diagn = diagn_codes[[dia]],
          debut = debut, fin = fin,
          diag_desc = dia, sourc_desc = dt_desc[[sour]]
        )
        t2 <- Sys.time()
        i <- i + 1L
        # Afficher le temps d'exécution
        if (verbose) {
          cat(" - ",dia,
              " (",round(as.numeric(difftime(t2, t1)), 2),
              " ",attr(difftime(t2, t1), "units"), ")\n",
              sep = "")

        }
      }
    }
    DT <- data.table::rbindlist(DT)  # regrouper tous les tableaux en un seul

    if (nrow(DT)) {
      setkey(DT)  # tri
    }
    return(DT)

  }

}
