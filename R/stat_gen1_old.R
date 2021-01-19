#' Statistiques générales
#'
#' Tableau indiquant les statistiques générales d'un ou de plusieurs codes de médicaments selon certains critères.
#'
#' Utiliser soit le paramètres `conn` ou la combinaison `user` et `pwd`.\cr\cr
#' **debut, fin :** Doivent contenir le même nombre de valeurs.
#'
#' @param conn Variable contenant la connexion entre R et Teradata. Voir \code{\link{sql_connexion}}.
#' @param user Nom de l'identifiant pour la connexion SQL Teradata.
#' @param pwd Mot de passe associé à l'identifiant. Si `NULL`, le programme demande le mot passe. Cela permet de ne pas afficher le mot de passe dans un script.
#' @inheritParams stat_gen1_query
#'
#' @return `data.table`
#' @encoding UTF-8
#' @import data.table
#' @importFrom lubridate as_date
#' @importFrom odbc dbGetQuery
#' @importFrom stringr str_detect str_pad str_remove_all
#' @export
#' @examples
#' \dontrun{
#' library(inesss)
#' conn <- sql_connexion(uid = askpass::askpass("User"),
#'                       pwd = askpass::askpass("Mot de passe"))
#'
#' # DENOM
#' dt1 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),
#'   fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222)
#' )
#'
#' # DIN
#' dt2 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),
#'   fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DIN", codes = c(30848, 585092)
#' )
#'
#' # Résultats par Périodes
#' dt3 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),
#'   fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222),
#'   result_by = "Périodes"
#' )
#' # Résultats par Teneur et Format
#' dt4 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),
#'   fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222),
#'   result_by = c("Teneur", "Format")
#' )
#'
#' # Exclusion et Inclusion des codes de services et des codes de listes de médicaments
#' dt5 <- SQL_stat_gen1(
#'   conn = conn,
#'   debut = c("2017-01-01", "2018-01-01"),
#'   fin = c("2017-12-31", "2018-12-31"),
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222),
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = c("40", "41"), code_list_filtre = "Inclusion"
#' )
#' }
SQL_stat_gen1_old <- function(
  conn = NULL, user = NULL, pwd = NULL,
  debut, fin,
  type_Rx = "DENOM", codes, result_by = NULL,
  code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
  code_list = NULL, code_list_filtre = "Inclusion"
) {

  # Fonctions ---------------------------------------------------------------

  codes_in_period <- function(conn, dt, query, result_by) {
    ### Ajouter la colonne CODES_RX qui indique tous les codes analysés lorsque
    ### les résultats sont présentés par période, et peut-être autre.

    dt_codes <- as.data.table(dbGetQuery(conn, query))
    dt_by <- names(dt_codes)[!names(dt_codes) %in% "CODES"]
    if (length(dt_by)) {
      dt_codes <- dt_codes[, .(CODES_RX = paste(CODES, collapse = "; ")), keyby = dt_by]
      dt <- dt_codes[dt, on = dt_by]
    } else {
      dt[, CODES_RX := paste(sort(as.integer(dbGetQuery(conn, query)$CODES)), collapse = "; ")]
    }
    return(dt)

  }
  query_result_by_period <- function(result_by, type_Rx, codes, deb, fin) {
    ### Query SQL permettant de connaître les codes associés aux résultats

    if (length(result_by) == 1 && "Périodes" %in% result_by) {
      query <- paste0(
        "select distinct(",sql_select_type_rx_var(type_Rx),") as CODES\n",
        from_bd.vue("PROD", "V_DEM_PAIMT_MED_CM"),"\n",
        "where SMED_DAT_SERV between '",deb,"' and '",fin,"'\n",
        indent(),"and ",sql_select_type_rx_var(type_Rx)," in (",
            ifelse(type_Rx == "DENOM", qu(codes), paste(codes, collapse = ", ")),
            ");"
      )
    } else if (length(result_by) > 1 && "Périodes" %in% result_by) {
      query <- paste0("select distinct(",sql_select_type_rx_var(type_Rx),") as CODES,\n")
      if ("Teneur" %in% result_by) {
        query <- paste0(query,
                        indent("select"),"SMED_COD_TENR_MED as TENEUR,\n")
      }
      if ("Format" %in% result_by) {
        query <- paste0(query,
                        indent("select"),"SMED_COD_FORMA_ACQ_MED as FORMAT_ACQ,\n")
      }
      query <- paste0(
        query,
        from_bd.vue("PROD", "V_DEM_PAIMT_MED_CM"),"\n",
        "where ",sql_select_type_rx_var(type_Rx)," in (",ifelse(type_Rx == "DENOM", qu(codes), paste(codes, collapse = ", ")),")\n",
        indent(),"and SMED_DAT_SERV between '",deb,"' and '",fin,"'\n",
        "order by CODES;"
      )

      # Supprimer virgule si c'est le dernier select avant le from
      if (str_detect(query, ",\nfrom")) {
        stringr::str_sub(query, str_locate(query, ",\nfrom")[1,][[1]], str_locate(query, ",\nfrom")[1,][[1]]) <- ""
      }
    }
    return(query)
  }
  sql_select_type_rx_var <- function(type_Rx) {
    ### Déterminer le nom de la variable dans SQL selon le type de code Rx

    if (type_Rx == "DENOM") {
      return("SMED_COD_DENOM_COMNE")
    } else if (type_Rx == "DIN") {
      return("SMED_COD_DIN")
    } else {
      stop("SQL_stat_gen1.sql_select_type_rx_var() : type_Rx non permi.")
    }
  }
  verif_args <- function(conn, user, pwd, debut, fin, type_Rx, codes, result_by,
                         code_serv, code_serv_filtre, code_list, code_list_filtre) {

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

    if (!is.null(result_by) && !all(result_by %in% c("Périodes", "Teneur", "Format"))) {
      addError("result_by contient au moins une valeur non permise : ", check)
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
  if (!is.null(result_by)) {
    result_by <- sunique(result_by)
  }
  if (!is.null(code_serv)) {
    code_serv <- sunique(code_serv)
  }
  if (!is.null(code_list)) {
    code_list <- str_pad(sunique(code_list), 2, "left", "0")
  }

  # Vérifier les arguments
  verif_args(conn, user, pwd, debut, fin, type_Rx, codes, result_by,
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
    if (!"Périodes" %in% result_by) {
      if (type_Rx == "DENOM") {
        nom_denom <- inesss::V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE  # data nom des denom
        nom_denom <- nom_denom[DENOM %in% codes]
        # S'assurer que chaque médicament a un nom pour une période (exemple si commence en décembre, on change pour janvier)
        idx <- rmNA(nom_denom[, .I[.N == 1], .(DENOM)]$V1)
        if (length(idx)) {
          nom_denom[idx, `:=` (DATE_DEBUT = as_date(paste0(year(DATE_DEBUT),"-01-01")),
                               DATE_FIN = as_date(paste0(year(DATE_FIN),"-12-31")))]
        }
      } else if (type_Rx == "DIN") {
        nom_marq_comrc <- inesss::V_PRODU_MED.NOM_MARQ_COMRC[ # data nom marques commerciales pour DIN
          , .(DIN, NOM_MARQ_COMRC, DATE_DEBUT, DATE_FIN)
        ]
        nom_marq_comrc <- nom_marq_comrc[DIN %in% codes]
        idx <- rmNA(nom_marq_comrc[, .I[.N == 1], .(DIN)]$V1)
        if (length(idx)) {
          nom_marq_comrc[idx, `:=` (DATE_DEBUT = as_date(paste0(year(DATE_DEBUT),"-01-01")),
                                    DATE_FIN = as_date(paste0(year(DATE_FIN),"-12-31")))]
        }
      }
    }

    # Effectuer une requête pour chaque période d'étude
    for (i in 1:length(debut)) {

      dt <- as.data.table(dbGetQuery(  # extraction SQL
        conn = conn,
        statement = stat_gen1_query(
          debut[i], fin[i], type_Rx, codes, result_by,
          code_serv, code_serv_filtre, code_list, code_list_filtre
        )
      ))

      if ("Périodes" %in% result_by) {
        query <- query_result_by_period(result_by, type_Rx, codes, debut[i], fin[i])
        dt <- codes_in_period(conn, dt, query, result_by)
      }

      DT <- rbind(DT, dt)

    }

    # Ajouter les noms des médicaments
    if (any(c("DENOM", "DIN") %in% names(DT))) {
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
        {if ("Teneur" %in% result_by) "TENEUR" else NULL},
        {if ("Format" %in% result_by) "FORMAT_ACQ" else NULL},
        "MNT_MED", "MNT_SERV", "MNT_TOT",
        "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX"
      )
      orderv <- c(`1` = "DATE_DEBUT", `-1` = "DATE_FIN", `1` = type_Rx)
    } else {
      colorder <- c(  # ordre des colonnes souhaitées si pas de codes
        "DATE_DEBUT", "DATE_FIN",
        {if ("Teneur" %in% result_by) "TENEUR" else NULL},
        {if ("Format" %in% result_by) "FORMAT_ACQ" else NULL},
        "MNT_MED", "MNT_SERV", "MNT_TOT",
        "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX",
        "CODES_RX"
      )
      orderv <- c(`1` = "DATE_DEBUT", `-1` = "DATE_FIN")
    }
    if ("Teneur" %in% result_by) {
      orderv <- c(orderv, `1` = "TENEUR")
    }
    if ("Format" %in% result_by) {
      orderv <- c(orderv, `1` = "FORMAT_ACQ")
    }

    # Ordre des colonnes et des données
    setcolorder(DT, colorder)
    setorderv(DT, orderv, order = as.integer(names(orderv)))

    # Codes au format integer
    if ("DENOM" %in% names(DT)) {
      DT[, DENOM := as.integer(DENOM)]
    }
    if ("CODES_RX" %in% names(DT)) {
      setnames(DT, "CODES_RX", type_Rx)  # remplacer CODES_RX par le type de code
    }

    return(DT)

  }

}


#' Statistiques générales
#'
#' Générateur de requêtes SQL (texte) pour la méthode `stat_gen1`.
#'
#' **debut, fin :** Doivent contenir le même nombre de valeurs.
#'
#' @param debut Date de début de la période d'étude au format `"AAAA-MM-JJ"` (une seule valeur).
#' @param fin Date de fin de la période d'étude au format `"AAAA-MM-JJ"` (une seule valeur).
#' @param type_Rx `"DENOM"` ou `"DIN"`. Indique le type de code analysé.
#' @param codes Vecteur comprenant le ou les codes d'analyse au format numérique, sans zéros.
#' @param result_by Afficher les résultats par :
#' * **`"Période"`** : Résultats par période d'étude. Somme des montants de tous les `codes` analysés.
#' * **`"Teneur"`** : Résultats par teneur du médicament (`SMED_COD_TENR_MED`).
#' * **`"Format"`** : Résultats par format d'acquisition du médicament (`SMED_COD_FORMA_ACQ_MED`).
#' @param code_serv Vecteur de type `character` comprenant le ou les codes de service à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_serv_filtre `"Inclusion"` ou `"Exclusion"` des codes de service `code_serv`, sinon inscrire `NULL`.
#' @param code_list Vecteur de type `character` comprenant le ou les codes de catégories de liste de médicaments à exclure ou à inclure, sinon inscrire `NULL`.
#' @param code_list_filtre `"Inclusion"` ou `"Exclusion"` des codes de catégories de liste de médicaments `code_list`, sinon inscrire `NULL`.
#'
#' @encoding UTF-8
#' @importFrom stringr str_detect str_locate str_pad
#' @export
#' @examples
#' ## Avantage d'utiliser cat() si c'est pour afficher dans la console :
#' # Avec cat()
#' cat(stat_gen1_query(
#'   debut = "2020-01-01", fin = "2020-12-31",
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222), result_by = NULL,
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = NULL, code_list_filtre = "Inclusion"
#' ))
#' # Sans cat()
#' stat_gen1_query(
#'   debut = "2020-01-01", fin = "2020-12-31",
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222), result_by = NULL,
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = NULL, code_list_filtre = "Inclusion"
#' )
#'
#' ## Résultats par
#' # Périodes
#' cat(stat_gen1_query(
#'   debut = "2020-01-01", fin = "2020-12-31",
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222),
#'   result_by = "Périodes",
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = NULL, code_list_filtre = "Inclusion"
#' ))
#' # Teneur et Format
#' cat(stat_gen1_query(
#'   debut = "2020-01-01", fin = "2020-12-31",
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222),
#'   result_by = c("Teneur", "Format"),
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = NULL, code_list_filtre = "Inclusion"
#' ))
#'
#' ## Exclusion VS Inclusion des codes de service ou des codes de liste de médicaments
#' cat(stat_gen1_query(
#'   debut = "2020-01-01", fin = "2020-12-31",
#'   type_Rx = "DENOM", codes = c(47092, 47135, 48222),
#'   result_by = c("Teneur", "Format"),
#'   code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
#'   code_list = c("40", "41"), code_list_filtre = "Inclusion"
#' ))
stat_gen1_query <- function(
  debut, fin,
  type_Rx = "DENOM", codes,
  result_by = NULL,
  code_serv = c("1", "AD"), code_serv_filtre = "Exclusion",
  code_list = NULL, code_list_filtre = "Inclusion"
) {

  # Internal FCTS -----------------------------------------------------------

  select_format <- function(result_by) {
    if ("Format" %in% result_by) {
      return(paste0(indent("select"),"SMED_COD_FORMA_ACQ_MED as FORMAT_ACQ,\n"))
    } else {
      return("")
    }
  }
  select_periodes <- function(result_by, type_Rx) {
    if ("Périodes" %in% result_by) {
      return("")
    } else {
      if (type_Rx == "DENOM") {
        return(paste0(indent("select"),"SMED_COD_DENOM_COMNE as DENOM,\n"))
      } else if (type_Rx == "DIN") {
        return(paste0(indent("select"),"SMED_COD_DIN as DIN,\n"))
      } else {
        stop("stat_gen1_txt_query_1period.select_CODES() valeur non permise.")
      }
    }
  }
  select_teneur <- function(result_by) {
    if ("Teneur" %in% result_by) {
      return(paste0(indent("select"),"SMED_COD_TENR_MED as TENEUR,\n"))
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
  resultby <- function(result_by, type_Rx) {
    if ("Périodes" %in% result_by) {
      txt <- NULL
    } else {
      txt <- type_Rx
    }
    if ("Teneur" %in% result_by) {
      txt <- c(txt, "TENEUR")
    }
    if ("Format" %in% result_by) {
      txt <- c(txt, "FORMAT_ACQ")
    }

    if (is.null(txt)) {
      return("")
    } else {
      return(paste0("group by ",paste(txt, collapse = ", ")))
    }
  }

  # Principal FCT -----------------------------------------------------------

  if (type_Rx == "DENOM") {  # les codes DENOM sont des CHR de longeur 5
    codes <- str_pad(codes, 5, "left", "0")
  }
  if (!is.null(code_list)) {
    code_list <- str_pad(code_list, 2, "left", "0")
  }

  query <- paste0(
    "select ",qu(debut)," as DATE_DEBUT,\n",
    indent("select"),qu(fin)," as DATE_FIN,\n",
    select_periodes(result_by, type_Rx),
    select_teneur(result_by),
    select_format(result_by),
    indent("select"),"sum(SMED_MNT_AUTOR_MED) as MNT_MED,\n",
    indent("select"),"sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV,\n",
    indent("select"),"sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as MNT_TOT,\n",
    indent("select"),"count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,\n",
    indent("select"),"count(*) as NBRE_RX,\n",
    indent("select"),"sum(SMED_QTE_MED) as QTE_MED,\n",
    indent("select"),"sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX\n",
    from_bd.vue("PROD","V_DEM_PAIMT_MED_CM"),"\n",
    "where SMED_DAT_SERV between ",qu(debut)," and ",qu(fin),"\n",
    indent(),where_code_rx(type_Rx, codes),
    where_code_serv(code_serv_filtre, code_serv),
    where_code_list(code_list_filtre, code_list),
    resultby(result_by, type_Rx),";"
  )

  if (str_detect(query, "\n;")) {  # supprimer retour de ligne (\n) si la commande est terminée (;)
    stringr::str_sub(query, nchar(query) - 1, nchar(query) - 1) <- ""
  }

  return(query)

}
