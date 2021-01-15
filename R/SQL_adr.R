#' SQL
#'
#' Vue : I_BENF_ADR_CM\cr
#' Différentes informations se rapportant aux adresses d'une personne assurée (bénéficiaire) à travers le temps.
#'
#' **Variables disponibles :**\cr
#' BENF_COD_TYP_ADR : CODE TYPE ADRESSE. Ce code indique à quelle fin sert l'adresse.\cr
#' BENF_DD_ADR_BEN : Date de début de la période d'application.\cr
#' BENF_DF_ADR_BEN : Date de fin de la période d'application.\cr
#' BENF_DM_ADR_BEN : DATE MISE À JOUR ADRESSE BÉNÉFICIAIRE. Date de mise à jour de l'occurrence.\cr
#' BENF_IND_ADR_HQ : INDICATEUR ADRESSE HORS-QUÉBEC. Ce champ indique si l'adresse inscrite est une adresse qui est hors du Québec (autre province ou hors du Canada).\cr
#' BENF_NO_INDIV_BEN_BANLS : Numéro identifiant de manière unique une personne physique qui peut être un bénéficiaire d'un service. Ce numéro est banalisé spécifiquement pour le Conseil du médicament.\cr
#' BENF_NO_INDIV_BEN_PORTR_BANLS : Numéro identifiant de manière unique une personne physique qui peut être un bénéficiaire et porteur de l'adresse d'un autre bénéficiaire. Ce numéro est banalisé spécifiquement pour le Conseil du médicament.\cr
#' LGEO_COD_RSS : Code de région socio-sanitaire.\cr
#' LGEO_COD_POSTL_PART : Code postal partiel, c'est-à-dire les 3 premiers caractères à partir de la gauche du code postal.\cr
#' LGEO_COD_TERRI_RLS : Code de territoire de RLS.\cr
#' LGEO_COD_TERRI_CLSC : Code de territoire de CLSC\cr
#' LGEO_COD_TERRI_RUIS : Code de territoire de RUIS\cr
#' LGEO_COD_LOCAL : CODE LOCALITE.\cr
#' LGEO_COD_MUNIC : Code de municipalité.\cr
#' LGEO_COD_MRC : CODE MUNICIPALITES REGIONALES DE COMTE.\cr
#' LGEO_COD_AIRE_DIFSN_STCAN : Code identifiant une aire de diffusion selon Statistique Canada.
#'
#' @param user Nom d'usager SQL.
#' @param t1,t2 Dates au format "AAAA-MM-JJ".\cr`t1=NULL` & `t2=NULL` : Toutes les observations.\cr`t1='AAAA-MM-JJ'` & `t2=NULL` : Observations où la date de début de la période d'application est plus petite ou égale à `t1` (`BENF_DD_ADR_BEN <= t1`).\cr`t1='AAAA-MM-JJ` & `t2='AAAA-MM-JJ` : Observations qui chevauchent l'intervalle `[t1, t2]` (`BENF_DD_ADR_BEN<=t2` et `BENF_DF_ADR_BEN>=t1`).
#' @param cols Vecteur des colonnes à extraire. `NULL` implique toutes les colonnes. Voir *Details*.
#' @param cols_name `list(nom_initial = nouveau_nom)`. Permet de modifier le nom des variables. `NULL` implique que l'on conserve les noms initiaux.
#' @param cols_type `list(nom_initial = type)`. Permet de modifier/forcer la classe des variables. `"int"=integer`, `"num"=numeric`, `"chr"=character`, `"date"=Date` et `"logical"=logical`. `NULL` implique aucune modification, conservation des classes initiales.
#' @param select_ids Vecteur contenant les bénéficiaires/usagers/individus à conserver.
#' @param save_file `"repertoire/nom_fichier"` permettant d'enregistrer le dataset sur le disque dur. Exemple : `"dir1/dir2/dir3/nom_du_fichier"`.
#' @param save_ext Extension du fichier à enregistrer. `"rds"`, `"csv"` ou `"txt"`.
#'
#' @import data.table
#' @encoding UTF-8
#' @keywords internal
#' @export
SQL_adr <- function(
  user,
  t1 = NULL, t2 = NULL,
  cols = c(
    "BENF_NO_INDIV_BEN_BANLS", "LGEO_COD_TERRI_CLSC", "LGEO_COD_TERRI_RLS",
    "LGEO_COD_RSS", "BENF_DD_ADR_BEN", "BENF_DF_ADR_BEN"
  ),
  cols_name = list(
    BENF_NO_INDIV_BEN_BANLS = "ID",
    LGEO_COD_TERRI_CLSC = "CLSC",
    LGEO_COD_TERRI_RLS = "RLS",
    LGEO_COD_RSS = "RSS",
    BENF_DD_ADR_BEN = "DateDebut",
    BENF_DF_ADR_BEN = "DateFin"
  ),
  cols_type = list(
    BENF_NO_INDIV_BEN_BANLS = "int",
    LGEO_COD_TERRI_CLSC = "int",
    LGEO_COD_TERRI_RLS = "int",
    LGEO_COD_RSS = "int",
    BENF_DD_ADR_BEN = "date",
    BENF_DF_ADR_BEN = "date"
  ),
  select_ids = NULL,
  save_file = NULL, save_ext = "rds",
  ...
) {

  self <- .SQL_adr_internal()
  self$verif_args(user, t1, t2, cols, cols_name, cols_type, select_ids, save_file, save_ext)
  DT <- self$sql_extract(user, t1, t2, cols, pwd)  # extraction SQL
  DT <- setDT(DT)  # convertir data.table

  # Filtrer les ID
  if (!is.null(select_ids)) {
    if (!is.numeric(select_ids))  # convertir numeric au besoin
      select_ids <- as.numeric(select_ids)
    DT <- DT[BENF_NO_INDIV_BEN_BANLS %in% select_ids]  # selection IDs
  }

  # Spécifier class des colonnes
  if (!is.null(cols_type)) {
    for (typ in names(cols_type)) {
      DT[, (typ) := self$possible_class[[cols_type[[typ]]]](get(typ))]
    }
  }

  # Renommer les colonnes
  if (!is.null(cols_name)) {
    for (name in names(cols_name)) {
      setnames(DT, name, cols_name[[name]])
    }
  }

  # Inscrire infos extraction en attribut
  attr(DT, "cols") <- cols
  attr(DT, "cols_name") <- cols_name
  attr(DT, "cols_type") <- cols_type
  attr(DT, "date_extract") <- Sys.time()

  # Save
  if (is.character(save_file)) {
    if (save_ext == "rds") {
      saveRDS(DT, paste0(save_file,".rds"))
    } else {
      fwrite(DT, paste(save_file,".",save_ext))
    }
  }
  return(DT)

}


#' FCTs internes
#'
#' Fonctions internes de \code{\link{SQL_adr}}.
#'
#' @keywords internal
#' @importFrom DBI dbConnect dbGetQuery
#' @importFrom odbc odbc
#' @importFrom askpass askpass
#' @importFrom lubridate as_date
#' @export
.SQL_adr_internal <- function() {
  return(list(

    # Args
    possible_cols = c(
      ### Nom des colonnes existantes de la vue I_BENF_ADR_CM
      "BENF_COD_TYP_ADR", "BENF_DD_ADR_BEN", "BENF_DF_ADR_BEN", "BENF_DM_ADR_BEN",
      "BENF_IND_ADR_HQ", "BENF_NO_INDIV_BEN_BANLS", "BENF_NO_INDIV_BEN_PORTR_BANLS",
      "LGEO_COD_RSS", "LGEO_COD_POSTL_PART", "LGEO_COD_TERRI_RLS", "LGEO_COD_TERRI_CLSC",
      "LGEO_COD_TERRI_RUIS", "LGEO_COD_LOCAL", "LGEO_COD_MUNIC",
      "LGEO_COD_MRC", "LGEO_COD_AIRE_DIFSN_STCAN"
    ),
    possible_class = list(
      ### class des variables qu'il est possible de convertir et la fonction a utiliser
      int = as.integer, num = as.numeric, chr = as.character, date = as_date,
      logical = as.logical
    ),
    # SQL
    sql_extract = function(user, t1, t2, cols, pwd) {
      ### Extraction SQL permettant de créer le dataset d'analyse
      ### @param t1,t2 : Date de début et de fin de l'étude
      ### @param cols : nom des colonnes à importer

      if (missing(pwd))
        pwd <- askpass()
      return(dbGetQuery(
        conn = dbConnect(  # Connexion de l'utilisateur
          odbc(),
          "PEI_PRD",  # data source name
          uid = user, pwd = pwd  # user name + password
        ),
        statement = .SQL_adr_internal()$sql_query_code(t1, t2, cols)
      ))
    },
    sql_query_code = function(t1, t2, cols) {
      ### Code SQL permettant d'exécuter l'extraction
      ### @param Voir sql_extract()
      return(paste0(
        "SELECT ", .SQL_adr_internal()$sql_select_cols(cols),"\n",
        "FROM PROD.I_BENF_ADR_CM",
        .SQL_adr_internal()$sql_where_deb_fin(t1, t2)
      ))
    },
    sql_select_cols = function(cols) {
      ### Convertir le vecteur cols en code SQL pour la sélection des colonnes
      if (is.null(cols)) {
        return("*")  # toutes les colonnes
      } else {
        return(paste(cols, collapse = ","))  # colonnes sélectionnées par l'utilisateur
      }
    },
    sql_where_deb_fin = function(t1, t2) {
      ### Création du code SQL si on doit filtrer pour un interval de temps
      if (is.null(t1) && is.null(t2)) {
        return(";")  # fin du query
      } else if (is.null(t2)) {
        return(paste0(
          "\n",
          "WHERE BENF_DD_ADR_BEN <= '",t1,"';"
        ))
      } else if (!is.null(t1) && !is.null(t2)) {
        return(paste0(
          "\n",
          "WHERE BENF_DD_ADR_BEN <= '",t2,"' AND BENF_DF_ADR_BEN >= '",t1,"';"
        ))
      }
    },
    # Verifications
    verif_args = function(user, t1, t2, cols, cols_name, cols_type, select_ids, save_file, save_ext) {
      ### Vérification de la valeur des arguments
      check <- newArgCheck()

      # user doit etre une chaine de caracteres
      if (!is.character(user))
        addError("user n'est pas de type character.", check)

      # verifier si les dates sont au format AAAA-MM-JJ
      if (!is.null(t1))
        if (is.na(as_date(t1)))
          addError("t1 n'est pas au format 'AAAA-MM-JJ'.", check)
      if (!is.null(t2))
        if (is.na(as_date(t2)))
          addError("t2 n'est pas au format 'AAAA-MM-JJ'.", check)
      if (!is.null(t1) && !is.null(t2) && t1 >= t2) {
        addError("t1 ne peut être plus grand ou égal à t2.", check)
      } else if (is.null(t1) && !is.null(t2)) {
        addError("Utiliser t1 au lieu de t2 : t1='AAAA-MM-JJ', t2=NULL.", check)
      }

      # nom des colonnes existe dans I_BENF_ADR_CM?
      if (!is.null(cols)) {  # nom des colonnes choisies existe?
        for (col in cols) {
          if (!col %in% .SQL_adr_internal()$possible_cols) {
            addError(paste0(col," (cols) n'est pas une colonne de la vue I_BENF_ADR_CM"), check)
          }
        }
      }

      # save_file & save_ext
      if (!is.null(save_file) && !is.character(save_file))
        addError("save_file doit être une chaîne de caractères ou NULL.", check)
      if (!is.null(save_ext) && !save_ext %in% c("rds", "csv", "txt"))
        addError("Les valeurs permises de save_ext sont ['rds', 'csv', 'txt'].", check)
      if (!is.null(save_file) && is.null(save_ext))
        addError("save_ext ne doit pas être NULL si save_file ne l'est pas.", check)
      finishArgCheck(check)

      # cols_name doit contenir le meme nombre de variable et les memes noms que cols
      if (is.null(cols)) {
        cols2 <- .SQL_adr_internal()$possible_cols
      } else {
        cols2 <- cols
      }
      if (!all(sort(cols2) == sort(names(cols_name)))) {
        addError("Certains éléments de cols_name n'existent pas dans cols ou dans I_BENF_ADR_CM.", check)
      }
      # cols_name ne peut contenir deux fois le meme nouveau nom
      if (length(cols_name) != length(unlist(cols_name, use.names = FALSE))) {
        addError("cols_name ne peut contenir deux fois le même nouveau nom de colonne.", check)
      }

      # cols_type doit contenir le meme nombre de variable et les memes noms que cols
      if (!all(sort(cols2) == sort(names(cols_type)))) {
        addError("Certains éléments de cols_type n'existent pas dans cols ou dans I_BENF_ADR_CM.", check)
      }
      # Verifier si les valeurs inscrites sont permises
      for (var in cols_type) {
        if (!var %in% names(.SQL_adr_internal()$possible_class)) {
          addError("Les valeurs permises de cols_type sont : {'int', 'num', 'chr', 'date', 'logical'}.", check)
        }
      }

      # select_ids : BENF_NO_INDIV_BEN_BANLS doit être dans cols pour filtrer
      if (!is.null(select_ids)) {
        if (!"BENF_NO_INDIV_BEN_BANLS" %in% cols)  # colonne des beneficiaires doit etre presente
          addError(
            "BENF_NO_INDIV_BEN_BANLS doit être présent dans cols si on utilise l'argument select_ids.",
            check
          )
        # Ne doit pas contenir de NA une fois convertie en numeric
        if (!is.numeric(select_ids))
          select_ids <- as.numeric(select_ids)
        if (anyNA(select_ids))
          addError("select_ids, au format numeric, ne doit pas contenir de NA.",
                   check)
      }

      finishArgCheck(check)

    }

  ))
}
