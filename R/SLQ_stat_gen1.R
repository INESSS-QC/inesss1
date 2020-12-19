#' Statistiques générales
#'
#' Tableau indiquant les statistiques générales d'un ou de plusieurs codes de médicaments selon certains critères.
#'
#' Utiliser soit le paramètres `conn` ou la combinaison `user` et `pwd`.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{sql_connexion}}.
#' @param user Nom de l'identifiant pour la connexion SQL Teradata.
#' @param pwd Mot de passe associé à l'identifiant. Si `NULL`, le programme demande le mot passe. Cela permet de ne pas afficher le mot de passe dans un script.
#' @param debut,fin Date(s) de début et de fin de la période d'étude au format `"AAAA-MM-JJ"`. `debut` et `fin` doivent contenir le même nombre de valeurs.
#' @param debut,fin Date de début et de fin de la période d'étude au format `"AAAA-MM_JJ"`.
#' @param type_Rx `"DENOM"` ou `"DIN"`. Indique le type de code analysé.
#' @param codes Vecteur comprenant le ou les codes d'analyse au format numérique, sans zéros.
#' @param groupby Regrouper les résultats par :
#' * **`"Période"`** : périodes d'étude.
#' @param code_serv Vecteur de type `character` comprenant le ou les codes de service à inclure ou exclure, sinon inscrire `NULL`.
#' @param code_serv_filtre `"Inclusion"` ou `"Exclusion"` des codes de service `code_serv`, sinon inscrire `NULL`.
#' @param code_list Vecteur de type `character` comprenant le ou les codes de catégories de liste de médicaments à inclure ou exclure, sinon inscrire `NULL`.
#' @param code_list_filtre `"Inclusion"` ou `"Exclusion"` des codes de catégories de liste de médicaments `code_list`, sinon inscrire `NULL`.
#'
#' @return `data.table`
#' @export
#' @import data.table
#' @importFrom lubridate as_date
#' @importFrom odbc dbGetQuery
#' @importFrom stringr str_remove_all
sql_stat_gen1 <- function(
  conn = NULL, user = NULL, pwd = NULL,
  debut, fin,
  type_Rx = "DENOM", codes, groupby = NULL,
  code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
  code_list = NULL, code_list_filtre = "Inclusion"
) {

# Fonctions ---------------------------------------------------------------

  verif_args <- function(conn, user, pwd, debut, fin, type_Rx, codes, groupby, code_serv, code_serv_filtre, code_list, code_list_filtre) {

    check <- newArgCheck()

    if (is.null(conn) && (is.null(user) || is.null(pwd))) {
      addError("Utiliser soit le paramètres conn, soit user et pwd.", check)
    }

    if (is.null(conn)) {

      if (!is.character(user)) {
        addError("user doit être une chaîne de caractères.", check)
      }
      if (length(user) != 1) {
        addError("user doit contenir une seule valeur.", check)
      }

      if (!is.null(pwd)) {
        if (!is.character(pwd)) {
          addError("pwd doit être une chaîne de caractères.", check)
        }
        if (length(pwd) != 1) {
          addError("pwd doit contenir une seule valeur.", check)
        }
      }

    }

    if (length(debut) != length(fin)) {
      addError(paste0(
        "Le nombre de valeurs de debut (",length(debut),")",
        "doit être le même que celui de fin (",length(fin),")."
      ), check)
    }
    if (anyNA(as_date(debut))) {
      addError("debut n'est pas une date au format 'AAAA-MM-JJ'.", check)
    }
    if (anyNA(as_date(fin))) {
      addError("fin m'est pas une date au format 'AAAA-MM-JJ'.", check)
    }

    if (length(type_Rx) != 1) {
      addError("type_Rx doit contenir une seule valeur.", check)
    }
    if (!all(type_Rx %in% c("DENOM", "DIN"))) {
      addError("type_Rx ne contient pas une valeur permise parmi {'DENOM', 'DIN'}.", check)
    }

    if (!is.null(groupby) && !all(groupby %in% c("Périodes"))) {
      addError("groupby contient au moins une valeur non permise : ", check)
    }

    if (!is.null(code_serv_filtre)) {
      if (length(code_serv_filtre) != 1) {
        addError("code_serv_filtre doit contenir une seule valeur.", check)
      }
      if (!all(code_serv_filtre %in% c("Exclusion", "Inclusion"))) {
        addError("code_serv_filtre contient au moins une valeur non permise qui n'est pas parmi {'Exclusion', 'Inclusion'}.", check)
      }
    }

    if (!is.null(code_list_filtre)) {
      if (length(code_list_filtre) != 1) {
        addError("code_list_filtre doit contenir une seule valeur.", check)
      }
      if (!all(code_serv_filtre %in% c("Exclusion", "Inclusion"))) {
        addError("code_list_filtre contient au moins une valeur non permise qui n'est pas parmi {'Exclusion', 'Inclusion'}.", check)
      }
    }

    finishArgCheck(check)


  }


# Code --------------------------------------------------------------------

  # Demander le mot de passe s'il n'a pas été inscrit.
  if (is.null(conn) && is.null(pwd)) {
    pwd <- askpass::askpass("Quel est votre mot de passe")
  }

  # Arranger les arguments
  codes <- sunique(codes)
  if (type_Rx == "DENOM") {  # les codes DENOM sont des CHR de longeur 5
    codes <- str_pad(codes, 5, "left", "0")
  }
  if (!is.null(groupby)) {
    groupby <- sunique(groupby)
  }
  if (!is.null(code_serv)) {
    code_serv <- sunique(code_serv)
  }
  if (!is.null(code_list)) {
    code_list <- str_pad(sunique(code_list), 2, "left", "0")
  }

  # Vérifier les arguments
  verif_args(conn, user, pwd, debut, fin, type_Rx, codes, groupby,
             code_serv, code_serv_filtre, code_list, code_list_filtre)

  # Effectuer la connexion si nécessaire
  if (is.null(conn)) {
    conn <- sql_connexion(user, pwd)
  }
  if (is.null(conn)) {
    stop("Erreur de connexion. Vérifier l'identifiant (user) et le mot de passe (pwd).")
  } else {

    # Préparation pour les requêtes
    DT <- data.table()  # stocker les résultats de chaque période
    nom_denom <- inesss::V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE  # data nom des denom
    nom_marq_comrc <- inesss::V_PRODU_MED.NOM_MARQ_COMRC[ # data nom marques commerciales pour DIN
      , .(DIN, NOM_MARQ_COMRC, DATE_DEBUT, DATE_FIN)
    ]

    # Effectuer une requête pour chaque période d'étude
    for (i in 1:length(debut)) {

      dt <- as.data.table(dbGetQuery(  # extraction SQL
        conn = conn,
        statement = stat_gen1_txt_query_1period(
          debut[i], fin[i], type_Rx, codes, groupby,
          code_serv, code_serv_filtre, code_list, code_list_filtre
        )
      ))

      if ("Périodes" %in% groupby) {
        query_codes_exist <- paste0(
          "select distinct(SMED_COD_DENOM_COMNE) as CODES\n",
          "from V_DEM_PAIMT_MED_CM\n",
          "where SMED_DAT_SERV between '",debut[i],"' and '",fin[i],"'\n",
          "  and SMED_COD_DENOM_COMNE in (",qu(codes),");"
        )
        dt[, (type_Rx) := paste(as.integer(dbGetQuery(conn, query_codes_exist)$CODES), collapse = "; ")]
      }

      DT <- rbind(DT, dt)

    }

    # Ajouter les noms des médicaments
    if (any(c("DENOM", "DIN") %in% names(dt))) {
      if (type_Rx == "DENOM") {
        DT <- nom_denom[DENOM %in% codes & DATE_DEBUT <= debut[i] & debut[i] <= DATE_FIN, .(DENOM, NOM_DENOM)][
          DT, on = .(DENOM)
        ]
      } else if (type_Rx == "DIN") {
        DT <- nom_marq_comrc[DIN %in% codes & DATE_DEBUT <= debut[i] & debut[i] <= DATE_FIN, .(DIN, NOM_MARQ_COMRC)][
          DT, on = .(DIN)
        ]
      }
      # Préciser l'ordre des colonnes et des obs
      colorder <- c(
        "DATE_DEBUT", "DATE_FIN",
        type_Rx, nom_type_rx(type_Rx),
        "MNT_MED", "MNT_SERV", "MNT_TOT",
        "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX"
      )
      orderv <- c("DATE_DEBUT", "DATE_FIN", type_Rx)  # ordre des données
    } else {
      colorder <- c(  # ordre des colonnes souhaitées si pas de codes
        "DATE_DEBUT", "DATE_FIN",
        "MNT_MED", "MNT_SERV", "MNT_TOT",
        "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX",
        type_Rx
      )
      orderv <- c("DATE_DEBUT", "DATE_FIN")  # ordre des données
    }

    # Ordre des colonnes et des données
    setcolorder(DT, colorder)
    setorderv(DT, orderv)

    # Codes au format integer
    if ("DENOM" %in% names(DT)) {
      DT[, DENOM := as.integer(DENOM)]
    }

    return(DT)

  }

}


#' Statistiques générales
#'
#' Code de la requête SQL - chaîne de caractères.
#'
#' @param debut,fin Date de début et de fin de la période d'étude au format `"AAAA-MM_JJ"`.
#' @param type_Rx `"DENOM"` ou `"DIN"`. Indique le type de code analysé.
#' @param codes Vecteur comprenant le ou les codes d'analyse.
#' @param groupby Regrouper les résultats par :
#' * **`"Période"`** : périodes d'étude.
#' @param code_serv Vecteur comprenant le ou les codes de service à inclure ou exclure, sinon inscrire `NULL`.
#' @param code_serv_filtre `"Inclusion"` ou `"Exclusion"` des codes de service `code_serv`, sinon inscrire `NULL`.
#' @param code_list Vecteur comprenant le ou les codes de catégories de liste de médicaments à inclure ou exclure, sinon inscrire `NULL`.
#' @param code_list_filtre `"Inclusion"` ou `"Exclusion"` des codes de catégories de liste de médicaments `code_list`, sinon inscrire `NULL`.
#'
#' @keywords internal
#' @importFrom stringr str_locate str_sub str_detect str_pad
#' @export
stat_gen1_txt_query_1period <- function(
  debut, fin,
  type_Rx, codes,
  groupby,
  code_serv, code_serv_filtre,
  code_list, code_list_filtre
) {

# Internal FCTS -----------------------------------------------------------

  select_type_rx <- function(groupby, type_Rx) {
    if (is.null(groupby)) {
      if (type_Rx == "DENOM") {
        return(paste0(indent(),"SMED_COD_DENOM_COMNE as DENOM,\n"))
      } else if (type_Rx == "DIN") {
        return(paste0(indent(),"SMED_COD_DIN as DIN,\n"))
      } else {
        stop("stat_gen1_txt_query_1period.select_CODES() valeur non permise.")
      }
    } else {
      return("")
    }
  }
  where_code_rx <- function(type_Rx, codes) {
    if (type_Rx == "DENOM") {
      return(paste0("and SMED_COD_DENOM_COMNE in (",qu(codes),")\n"))
    } else if (type_Rx == "DIN") {
      return(paste0("and SMED_COD_DIN in (",paste(codes, collapse = ", "),")\n"))
    } else {
      stop("stat_gen1_txt_query_1period.where_type_rx() type_Rx valeur non permise.")
    }
  }
  where_code_serv <- function(code_serv_filtre, code_serv) {
    if (is.null(code_serv)) {
      return("")
    } else if (code_serv_filtre == "Exclusion") {
      return(paste0(indent(),"and (SMED_COD_SERV_1 not in (",qu(code_serv),") or SMED_COD_SERV_1 is null)\n"))
    } else if (code_serv_filtre == "Inclusion") {
      return(paste0(indent(),"and SMED_COD_SERV_1 in (",qu(code_serv),")\n"))
    } else {
      stop("stat_gen1_txt_query_1period.where_code_serv() code_serv_filtre valeur non permise")
    }
  }
  where_code_list <- function(code_list_filtre, code_list) {
    if (is.null(code_list)) {
      return("")
    } else if (code_list_filtre == "Exclusion") {
      return(paste0(indent(),"and (SMED_COD_CATG_LISTE_MED not in (",qu(code_list),") or SMED_COD_CATG_LISTE_MED is null)\n"))
    } else if (code_list_filtre == "Inclusion") {
      return(paste0(indent(),"and SMED_COD_CATG_LISTE_MED in (",qu(code_list),")\n"))
    } else {
      stop("stat_gen1_txt_query_1period.where_code_list() code_list_filtre valeur non permise")
    }
  }
  group_by <- function(groupby, type_Rx) {
    if (is.null(groupby)) {
      txt_grpby <- "group by "
      if (type_Rx == "DENOM") {
        txt_grpby <- paste0(txt_grpby, "DENOM")
      } else if (type_Rx == "DIN") {
        txt_grpby <- paste0(txt_grpby, "DIN")
      }
    } else if ("Périodes" %in% groupby) {
      txt_grpby <- ""
    }
    return(txt_grpby)
  }

# Principal FCT -----------------------------------------------------------

  query <- paste0(
    "select ",qu(debut)," as DATE_DEBUT,\n",
    indent(),qu(fin)," as DATE_FIN,\n",
             select_type_rx(groupby, type_Rx),
    indent(),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",
    indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",
    indent(),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",
    indent(),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",
    indent(),"count(*) as NBRE_RX,\n",
    indent(),"sum(SMED_QTE_MED) as QTE_MED,\n",
    indent(),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",
    from_bd.vue("PROD","V_DEM_PAIMT_MED_CM"),"\n",
    "where SMED_DAT_SERV between ",qu(debut)," and ",qu(fin),"\n",
    indent(),where_code_rx(type_Rx, codes),
    where_code_serv(code_serv_filtre, code_serv),
    where_code_list(code_list_filtre, code_list),
    group_by(groupby, type_Rx),";"
  )

  if (str_detect(query, "\n;")) {  # supprimer retour de ligne (\n) si la commande est terminée (;)
    substr(query, nchar(query) - 1, nchar(query) - 1) <- " "  # supprime '\n' qui est compté comme 1 seul char
    # *** NOTE : stringr::str_sub() ne fonctionne pas, la fonction n'était pas disponible malgré le importFrom stringr str_sub.
    #            De plus, "" ne fonctionne pas dans certains cas -> insertion d'un espace
  }

  return(query)

}
