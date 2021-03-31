#' Extraction - Codes diagn comorbidity
#'
#' Extraction SQL des diagnostics pour l'étude de la comorbidité.
#'
#' \strong{\code{dt_source} :}
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6721&NomVue=V%5FDIAGN%5FSEJ%5FHOSP%5FCM+%28Diagnostic+s%E9jour+hospitalier%29}{`V_DIAGN_SEJ_HOSP_CM`} : Cette structure contient tous les diagnostics associés à un séjour hospitalier.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6724&NomVue=V%5FSEJ%5FSERV%5FHOSP%5FCM+%28S%E9jour+service+hospitalier%29}{`V_SEJ_SERV_HOSP_CM`} : Cette structure contient les séjours dans un service effectués par l'individu hospitalisé.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=6687&NomVue=V%5FEPISO%5FSOIN%5FDURG%5FCM+%28%C9pisodes+de+soins+en+D%E9partement+d%27urgence%29}{V_EPISO_SOIN_DURG_CM} : Cette structure contient les épisodes de soins des départements d'urgence de la province.
#' * \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1797&NomVue=I%5FSMOD%5FSERV%5FMD%5FCM}{I_SMOD_SERV_MD_CM} : Cette vue retourne différentes informations se rapportant aux Services rendus à l'acte par des médecins.
#'
#' \strong{`method` :} Voir les listes \link{Comorbidity_diagn_codes} pour connaître les codes de diagnostics extraits.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.
#' @param cohort Cohorte d'étude. Vecteur comprenant les numéros d'identification des individus à conserver.
#' @param debut Date de début de la période d'étude au format `AAAA-MM-JJ`.
#' @param fin Date de fin de la période d'étude au format `AAAA-MM-JJ`.
#' @param method Extraire les diagnostics associés aux méthodes de calcul *Charlson*, *Elixhauser* ou les deux.
#' @param CIM `'CIM9'`, `'CIM10'` ou les deux. Permet de filtrer les codes de diagnostics selon le numéro de révision de la *Classification statistique internationale des maladies et des problèmes de santé connexes* (CIM).
#' @param dt_source Vecteur comprenant la ou les bases de données où aller chercher l'information. Voir *Details*.
#' @param dt_desc `list` décrivant les bases de données demandées dans `dt_source` au format `list(BD = 'MaDescription')`. Voir *Details*.
#' @param exclu_diagn Vecteur contenant le nom du ou des diagnostics à exclure de l'analyse. Voir \code{names(inesss::Comorbidity_diagn_codes)} pour connaître les codes de diagnostics pouvant être exclus.
#' @param verbose `TRUE` ou `FALSE`. Affiche le temps qui a été nécessaire pour extraire les diagnostics d'une source (`dt_source`). Utile pour suivre le déroulement de l'extraction.
#'
#' @return `data.table` de 4 variables :
#' * **`ID`** : Numéro d'identification de l'usager.
#' * **`DATE_DX`** : Date de diagnostic.
#' * **`DIAGN`** : Code descriptif des diagnostics provenant de `diagn_codes`.
#' * **`SOURCE`** : Indique d'où provient l'information. Une valeur parmi `dt_source`.
#' @encoding UTF-8
#' @export
SQL_comorbidity_diagn <- function(
  conn, cohort, debut, fin,
  method = c('Charlson', 'Elixhauser'),
  CIM = c('CIM9', 'CIM10'),
  dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM',
                'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MED-ECHO', V_SEJ_SERV_HOSP_CM = 'MED-ECHO',
                 V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
  exclu_diagn = NULL, verbose = TRUE
) {

  ### Arranger les arguments
  # Arguments possiblement manquants
  if (missing(conn)) {
    conn <- NULL
  }

  ### Extraction des diagn
  if (!"info" %in% names(attributes(conn))) {  # si encore NULL = Erreur
    stop("Erreur de connexion. Vérifier l'identifiant (uid) et le mot de passe (pwd).")
  } else {

    # Charlson VS Elixhauser -- diagn_codes contient les codes de diagnostics
    # à extraire.
    if (length(method) == 2 && all(method %in% c("Charlson", "Elixhauser"))) {
      diagn_codes <- inesss::Comorbidity_diagn_codes
    } else if (length(method) == 1 && method == "Charlson") {
      diagn_codes <- inesss::Charlson_diagn_codes
    } else if (length(method) == 1 && method == "Elixhauser") {
      diagn_codes <- inesss::Elixhauser_diagn_codes
    } else {
      stop("SQL_diagn(): method ne contient pas une combinaison possible.")
    }
    # Exclusion des diagnostiques
    if (!is.null(exclu_diagn)) {
      diagn_codes <- diagn_codes[!names(diagn_codes) %in% exclu_diagn]
    }
    # CIM9 vs CIM10 -- Filtrer les types de codes si on veut seulement une version
    # de classification de code.
    if (length(CIM) == 1) {
      for (i in names(diagn_codes)) {
        diagn_codes[[i]] <- diagn_codes[[i]][[CIM]]  # conserver seulement CIM9 ou CIM10
      }
    } else {
      for (i in names(diagn_codes)) {
        diagn_codes[[i]] <- unlist(diagn_codes[[i]], use.names = FALSE)  # grouper CIM9 et CIM10 en un seul vecteur
      }
    }

    # Extraction des diagnostics
    DT <- vector("list", length(dt_source) * length(diagn_codes))  # contiendra toutes les requêtes
    i <- 1L
    for (sour in dt_source) {
      if (verbose) {
        cat(sour, "\n")
      }
      fct <- get(paste0("SQL_comorbidity_diagn.",sour))  # fonction d'extraction selon la source
      for (dia in names(diagn_codes)) {
        t1 <- Sys.time()
        DT[[i]] <- fct(  # tableau provenant de la requête
          conn = conn, ids = cohort, diagn = diagn_codes[[dia]],
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


#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.V_DIAGN_SEJ_HOSP_CM <- function(conn, ids, diagn, debut, fin, diag_desc, sourc_desc) {
  ### Requête SQL pour extraire les diagnostics de la vue V_DIAGN_SEJ_HOSP_CM
  ### @conn = Connexion teradata.
  ### @ids = Vecteur contenant les numéros des identifiants (cohorte).
  ### @diagn = Codes SQL regex à chercher dans la base de données.
  ### @debut,@fin = Date de début et de période où chercher les informations.

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = paste0(
        "select SHOP_NO_INDIV_BEN_BANLS as ID,\n",
        "       SHOP_DAT_DEPAR_SEJ_HOSP as DATE_DX\n",
        "from RES_SSS.V_DIAGN_SEJ_HOSP_CM\n",
        "where SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")\n",
        "    and SHOP_DAT_DEPAR_SEJ_HOSP between '",deb,"' and '",fi,"'\n",
        "    and SHOP_TYP_DIAGN_SEJ_HOSP in ('A', 'P', 'S');"
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.V_SEJ_SERV_HOSP_CM <- function(conn, ids, diagn, debut, fin, diag_desc, sourc_desc) {
  ### Requête SQL pour extraire les diagnostics de la vue V_SEJ_SERV_HOSP_CM
  ### @conn = Connexion teradata.
  ### @ids = Vecteur contenant les numéros des identifiants (cohorte).
  ### @diagn = Codes SQL regex à chercher dans la base de données.
  ### @debut,@fin = Date de début et de période où chercher les informations.

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = paste0(
        "select SHOP_NO_INDIV_BEN_BANLS as ID,\n",
        "       SHOP_DAT_DEPAR_SEJ_HOSP as DATE_DX\n",
        "from RES_SSS.V_SEJ_SERV_HOSP_CM\n",
        "where SHOP_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")\n",
        "    and SHOP_DAT_DEPAR_SEJ_HOSP between '",deb,"' and '",fi,"';"
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.I_SMOD_SERV_MD_CM <- function(conn, ids, diagn, debut, fin, diag_desc, sourc_desc) {
  ### Requête SQL pour extraire les diagnostics de la vue V_SEJ_SERV_HOSP_CM
  ### @conn = Connexion teradata.
  ### @ids = Vecteur contenant les numéros des identifiants (cohorte).
  ### @diagn = Codes SQL regex à chercher dans la base de données.
  ### @debut,@fin = Date de début et de période où chercher les informations.

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = paste0(
        "select SMOD_NO_INDIV_BEN_BANLS as ID,\n",
        "       SMOD_DAT_SERV as DATE_DX\n",
        "from PROD.I_SMOD_SERV_MD_CM\n",
        "where SMOD_COD_DIAGN_PRIMR like any (",qu(diagn),")\n",
        "    and SMOD_COD_STA_DECIS = 'PAY'\n",
        "    and SMOD_DAT_SERV between '",deb,"' and '",fi,"';"
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
#' @keywords internal
#' @import data.table
SQL_comorbidity_diagn.V_EPISO_SOIN_DURG_CM <- function(conn, ids, diagn, debut, fin, diag_desc, sourc_desc) {
  ### Requête SQL pour extraire les diagnostics de la vue V_SEJ_SERV_HOSP_CM
  ### @conn = Connexion teradata.
  ### @ids = Vecteur contenant les numéros des identifiants (cohorte).
  ### @diagn = Codes SQL regex à chercher dans la base de données.
  ### @debut,@fin = Date de début et de période où chercher les informations.

  yr_deb <- year(lubridate::as_date(debut))  # 1ere année à extraire
  yr_fin <- year(lubridate::as_date(fin))  # dernière année à extraire
  DT <- vector("list", yr_fin - yr_deb + 1L)  # contiendra toutes les années
  i <- 1L
  for (yr in yr_deb:yr_fin) {
    # Ajuster les dates
    if (yr == yr_deb) {
      deb <- debut
    } else {
      deb <- paste0(yr,"-01-01")
    }
    if (yr == yr_fin) {
      fi <- fin
    } else {
      fi <- paste0(yr,"-12-31")
    }
    # Extraction des diagn selon l'année
    DT[[i]] <- as.data.table(odbc::dbGetQuery(
      conn = conn, statement = paste0(
        "select SURG_NO_INDIV_BEN_BANLS as ID,\n",
        "       SURG_DH_DEPAR_USAG_DURG as DATE_DX\n",
        "from RES_SSS.V_EPISO_SOIN_DURG_CM\n",
        "where SURG_COD_DIAGN_MDCAL_CLINQ like any (",qu(diagn),")\n",
        "    and SURG_DH_DEPAR_USAG_DURG between To_Date('",deb,"') and To_Date('",fi,"');"
      )
    ))
    if (!is.null(ids)) {
      DT[[i]] <- DT[[i]][ID %in% ids]  # filtrer les IDs demandés
    }
    i <- i + 1L
  }
  DT <- rbindlist(DT)  # regrouper les tableaux

  ### Ajouter les descriptions et forcer les classes
  if (nrow(DT)) {
    DT[, `:=` (ID = as.integer(ID),
               DATE_DX = lubridate::as_date(DATE_DX),
               DIAGN = diag_desc,
               SOURCE = sourc_desc)]
  } else {
    DT <- NULL
  }

  return(DT)

}
