#' Usage statistics
#'
#' Tableau de fréquence de différentes variables provenant de la vue *V_DEM_PAIMT_MED_CM*.
#'
#' **À Faire** AHFS : Attendre de voir comment on gère les classes AHFS. Selon les cas, à tester avec Teradata.
#'
#' @param user Nom d'usager SQL.
#' @param years Année(s) à analyser. Deux valeurs à inscrire, soit la date de début et la date de fin. Par exemple de 2015 à 2020, inscrire `years = c(2015, 2020)`. Si seulement une année à analyser, inscrire une seule valeur : `years = 2015`.
#' @param code_var Une des trois valeurs suivante : `"AHFS"`, `"DC"` ou `"DIN"`.
#' * `AHFS` : Code de classe AHFS.
#' * `DC` : Code dénomination commune.
#' * `DIN` : Code d'identification du médicament.
#' @param codes Codes à analyser.
#'
#' @author Guillaume Boucher
#' @import data.table
#' @importFrom askpass askpass
#' @export
SQL_usage_statistics1 <- function(user, years, code_var, codes,
                                  ...) {

  self <- SQL_usage_statistics1_fcts_int()

  # Creer variable si inexistantes
  if (!exists("sql_pwd"))
    sql_pwd <- NULL  # password pour connexion SQL
  if (!exists("execute_verifs"))
    execute_verifs <- TRUE  # verification des arguments utilisateur

  if (execute_verifs)
    self$verif_args(user, years, code_var, codes)  # faire verifications

  code_var <- sort(unique(toupper(code_var)))  # SAFE : code_var en majuscules
  if (is.null(sql_pwd)) {
    # Demander password si l'utilisateur ne l'a pas inscrit
    sql_pwd <- askpass("S'il vous plait entrez votre mot de passe:")
  }

  # Statistiques cree par code SQL
  dt <- as.data.table(self$sql_extract(user, sql_pwd, years, code_var, codes))

  # Fixer les couts a deux decimales
  dt[, `:=` (Cout = as_price(Cout),
             Honor = as_price(Honor),
             CoutTot = as_price(CoutTot))]

  return(dt)

}

#' FCTS internal
#'
#' @keywords internal
#' @importFrom odbc dbGetQuery dbConnect odbc
#' @importFrom stringr str_sub
#' @export
SQL_usage_statistics1_fcts_int <- function() {return(list(
  sql_ahfs_where_clause = function(code_var, codes) {
    ### Determiner si on inclut la sous-sous-classe ou pas.
    ### Le moyen de le déterminer est le nombre de caractere : 2 = classe, 6 = ssclasse

    if (code_var == "AHFS" && unique(nchar(codes)) == 6) {
      return(paste0(
        "  AND SMED_COD_SCLA_AHF IN (",sql_quote(sort(unique(str_sub(codes, 3, 4)))),")\n",
        "  AND SMED_COD_SSCLA_AHF IN (",sql_quote(sort(unique(str_sub(codes, 5, 6)))),")\n"
      ))
    } else {
      return("")
    }
  },
  sql_codes_where_clause = function(code_var, codes) {
    ### Choix entre les codes si DC ou DIN, sinon 2 premier caracteres si AHFS

    if (code_var %in% c("DC", "DIN")) {
      return(codes)
    } else {
      return(str_sub(codes, 1, 2))
    }
  },
  sql_extract = function(user, sql_pwd, years, code_var, codes) {
    ### Connexion R <-> SQL + exécution du code d'extraction

    return(dbGetQuery(
      conn = dbConnect(odbc(),
                       dsn = "PEI_PRD",  # dsn : data source name
                       uid = user, pwd = sql_pwd),
      statement = SQL_usage_statistics1_fcts_int()$sql_query_code(years, code_var, codes)
    ))
  },
  sql_max_year = function(years) {
    ### 31 decembre de l'annee la moins recente : min(years)

    return(sql_quote(paste0(max(years),"-12-31")))
  },
  sql_min_year = function(years) {
    ### 1er janvier de l'année la moins recente : min(years)

    return(sql_quote(paste0(min(years),"-01-01")))
  },
  sql_query_code = function(years, code_var, codes) {
    ### Code SQL pour l'extraction selon les arguments utilisateur

    self <- SQL_usage_statistics1_fcts_int()
    return(paste0(
      "SELECT\n",
      "  Extract (YEAR From SMED_DAT_SERV) AS Annee,\n",
      "  ",self$sql_select_var(code_var)," AS ",code_var,",\n",
      "  Count (DISTINCT SMED_NO_INDIV_BEN_BANLS) AS nbre_Id,\n",
      "  Count (*) AS nbre_Med,\n",
      "  Sum (SMED_MNT_AUTOR_MED) AS Cout,\n",
      "  Sum (SMED_MNT_AUTOR_FRAIS_SERV) AS Honor,\n",
      "  Sum (SMED_QTE_MED) AS QteMed,\n",
      "  Sum (SMED_NBR_JR_DUREE_TRAIT) AS DureeTrait,\n",
      "  Sum (SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) AS CoutTot\n",
      "FROM\n",
      "  Prod.V_DEM_PAIMT_MED_CM\n",
      "WHERE\n",
      "  SMED_DAT_SERV BETWEEN ",self$sql_min_year(years)," AND ",self$sql_max_year(years),"\n",
      "  AND ",self$sql_select_var(code_var)," IN (",sql_quote(self$sql_codes_where_clause(code_var, codes)),")\n",
         self$sql_ahfs_where_clause(code_var, codes),
      "  AND (SMED_COD_SERV_1 NOT = '1' OR SMED_COD_SERV_1 IS NULL)\n",
      "GROUP BY Annee, ",code_var,"\n",
      "ORDER BY Annee, ",code_var,";"
    ))
  },
  sql_select_var = function(code_var) {
    ### Selection de la bonne variable selon l'utilisateur
    if (code_var == "AHFS") {
      return("SMED_COD_CLA_AHF")
    } else if (code_var == "DC") {
      return("SMED_COD_DENOM_COMNE")
    } else if (code_var == "DIN") {
      return("SMED_COD_DIN")
    }
  },
  verif_args = function(user, years, code_var, codes) {
    ### Verification des arguments s'ils sont inscrit de la bonne maniere

    check <- newArgCheck()

    if (!is.character(user))
      addError("user doit être de type character.", check)

    if (!is.numeric(years))
      addError("years doit être de type numeric.", check)

    possible_var <- c("ahfs", "dc", "din")
    possible_var <- c(possible_var, toupper(possible_var))
    if (length(code_var) != 1 || !code_var %in% possible_var)
      addError("code_var doit etre une valeur parmi : {'AHFS', 'DC', 'DIN'}.")

    finishArgCheck(check)

  }
))}
