#' Comorbidity
#'
#' @import data.table
#' @export
SQL_comorbidity <- function(
  conn, uid, pwd,
  dt, ID, DATE_INDEX,
  method = c("Charlson", "Elixhauser"), CIM = c("CIM9", "CIM10"), scores = "CIM10",
  lookup = 2, n1 = 30, n2 = 730,
  dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
                "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MED-ECHO", V_SEJ_SERV_HOSP_CM = "MED-ECHO",
                 V_EPISO_SOIN_DURG_CM = "BDCU", I_SMOD_SERV_MD_CM = "SMOD"),
  confirm_sourc = list("MED-ECHO" = 1, "BDCU" = 2, "SMOD" = 2),
  obstetric_exclu = TRUE,
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

  if (is.null(conn)) {
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {

    ### Arranger dataset
    if (!is.data.table(dt)) {  # convertir data.table
      dt <- setDT(dt)
    }
    dt <- dt[, c(ID, DATE_INDEX), with = FALSE]  # sélection des colonnes
    setnames(dt, names(dt), c("ID", "DATE_INDEX"))  # renommer les colonnes
    if (!lubridate::is.Date(dt$DATE_INDEX)) {
      dt[, DATE_INDEX := lubridate::as_date(DATE_INDEX)]  # convertir au format date
    }

    ### Extraction des diagnostiques dans les années désirées
    DIAGN <- SQL_comorbidity_diagn(
      conn, uid = NULL, pwd = NULL,
      cohort = sunique(dt$ID),
      debut = min(dt$DATE_INDEX) - lubridate::years(lookup) - n1,
      fin = max(dt$DATE_INDEX),
      method = method, CIM = CIM,
      dt_source = dt_source, dt_desc = dt_desc,
      verbose = verbose
    )

    ### Filtrer dt pour en faire l'analyse
    # Supprimer les diagnostiques qui sont pas dans l'intervalle [DATE_INDEX - lookup - n1; DATE_INDEX]
    dt <- DIAGN[dt, on = .(ID)]  # ajouter les diagn aux dates index
    dt <- dt[!is.na(DATE_DX)]  # supprimer les ID qui n'ont pas de de cas
    setkey(dt, ID, DATE_DX, DATE_INDEX)
    dt <- dt[DATE_INDEX - lubridate::years(lookup) - n1 <= DATE_DX & DATE_DX <= DATE_INDEX]
    # Supprimer les dates < (DATE_INDEX - lookup) dont la source a une confirmation = 1
    sourc <- comorbidity.confirm_sourc_names(confirm_sourc, 1)
    if (length(sourc)) {
      idx <- intersect(
        dt[, .I[SOURCE %in% sourc]],
        dt[, .I[DATE_DX < DATE_INDEX - lubridate::years(lookup)]]
      )
      if (length(idx)) {
        dt <- dt[!idx]
      }
    }
    # Exclusion des cas gestationnelles


    ### Calcul des scores
    dt1 <- comorbidity(
      dt, "ID", "DIAGN", "DATE_DX", "SOURCE", n1, n2,
      method, scores, confirm_sourc
    )

  }

}

SQL_comorbidity.exclu_diab_gross <- function(conn, dt, ids, debut, fin, CIM, dt_source, verbose) {
  ### Supprimer les cas de diabètes de grosses. Un diagnostique de diabète sera
  ### supprimé s'il se trouve 120 jours avant le diagnostique et 180 jours après.

  dt_diab_hyp <- unique(dt[DIAGN %in% c("diab", "diabwc", "hyp"), .(ID, DATE_DX, DIAGN)])
  dt_gross <- SQL_obstetric(
    conn, uid = NULL, pwd = NULL,
    cohort = dt$ID,
    debut = min(dt_diab_hyp$DATE_DX - 180), fin = max(dt_diab_hyp$DATE_DX) + 120,
    CIM, dt_source, verbose
  )
  dt_gross <- unique(dt_gross[, .(ID, DATE_OBSTE = DATE_DX)])
  dt_diab_hyp <- dt_gross[dt_diab_hyp, on = .(ID), allow.cartesian = TRUE]
  dt_diab_hyp <- dt_diab_hyp[
    !is.na(DATE_OBSTE) &
      DATE_OBSTE + 180 >= DATE_DX & DATE_DX >= DATE_OBSTE - 120
  ]
  dt_diab_hyp <- unique(dt_diab_hyp[, .(ID, DATE_DX, DIAGN)])
  dt <- dt[!dt_diab_hyp, on = .(ID, DATE_DX, DIAGN)]

  return(dt)

}

dt <- readRDS("C:/Users/ms045/Desktop/Github/INESSS-QC_inesss1/data-fake/diagnostiques.rds")
setkey(dt, ID, DIAGN, DATE_DX)
dt <- dt[dt[, .I[1], .(ID, DATE_DX, DIAGN)]$V1]
dt_gross <- readRDS("C:/Users/ms045/Desktop/Github/INESSS-QC_inesss1/data-fake/diagnostiques_obstetric.rds")






