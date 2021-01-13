#' Formulaire Interactif
#'
#' Permet d'exécuter des requêtes Excel à partir d'un formulaire interactif. La documentation complète du formulaire, *AIDE_FORMULAIRE_DATE.pdf*, est disponible \href{https://github.com/INESSS-QC/inesss1/tree/master/Documentation}{ici}.
#'
#' **Requêtes via Excel :**\cr
#' Il est conseillé d'utiliser les gabarits pour éviter des erreurs de structures dans les tableaux d'arguments. Le fichier Excel *Gabarits-formulaire.xlsx* est disponible \href{https://github.com/INESSS-QC/inesss1/tree/master/Documentation}{ici}.
#'
#' @import data.table
#' @importFrom fs path_home
#' @importFrom odbc odbc dbConnect dbGetQuery
#' @importFrom readxl excel_sheets read_excel
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles shinyFilesButton shinyFileChoose shinyFileSave shinySaveButton parseFilePaths parseSavePath getVolumes
#' @importFrom stringr str_split str_remove_all str_sub
#' @importFrom writexl write_xlsx
#' @export
formulaire <- function() {

# Variables ---------------------------------------------------------------

  cols_Excel_file <- function() {
    ### Colonnes nécessaires pour chaque méthode

    return(list(
      sg1 = c("DATE_DEBUT", "DATE_FIN", "TYPE_RX", "CODE_RX", "RESULTATS_PAR",
              "CODE_SERV_FILTRE", "CODE_SERV", "CODE_LIST_FILTRE", "CODE_LIST")
    ))
  }
  values_Excel_file <- function() {
    ### Valeurs permises dans les colonnes de cols_Excel_file()

    return(list(
      sg1 = list(
        TYPE_RX = c("DENOM", "DIN"),
        RESULTATS_PAR = c("Périodes", "Teneur", "Format"),
        CODE_SERV_FILTRE = c("Exclusion", "Inclusion"),
        CODE_SERV = c("1", "AD", "L", "M", "M1", "M2", "M3"),
        CODE_LIST_FILTRE = c("Exclusion", "Inclusion"),
        CODE_LIST = c("03", "40", "41")
      )
    ))
  }
  methods_Excel_file <- function() {
    ### Liste des méthodes existantes

    return(c(
      "stat_gen1"
    ))
  }

# Fonctions ---------------------------------------------------------------

  adapt_code_serv <- function(code_serv) {
    ### Séparer des groupes de codes de service en valeurs uniques pour les
    ### requêtes SQL

    if ("L, M, M1 à M3" %in% code_serv) {  # valeurs inscrites dans les formulaires/gabarits
      code_serv <- code_serv[code_serv != "L, M, M1 à M3"]  # effacer cette valeur
      code_serv <- sort(c(code_serv, "L", "M", "M1", "M2", "M3"))  # ajouter chaque valeur individuellement
    }
    # Effectuer les mêmes étapes que précédemment, mais sans espace. Permet de
    # gérer les cas où on importe la colonne avec la fonction str_replace_all(x, " ", "")
    if ("L,M,M1àM3" %in% code_serv) {
      code_serv <- code_serv[code_serv != "L,M,M1àM3"]
      code_serv <- sort(c(code_serv, "L", "M", "M1", "M2", "M3"))
    }
    return(code_serv)
  }
  desadapt_code_serv <- function(code_serv) {
    ### Contraire à la fonction adapt_code_serv()

    if (all(c("L", "M", "M1", "M2", "M3") %in% code_serv)) {
      code_serv <- code_serv[!code_serv %in% c("L", "M", "M1", "M2", "M3")]
      code_serv <- sort(c(code_serv, "L, M, M1 à M3"))
    }
    return(code_serv)
  }
  cols_select_from_method <- function(dt, method) {
    ### Puisque l'onglet Excel peut contenir des colonnes facultatives pour le
    ### formulaire, on doit les supprimer après l'avoir importé.
    ### Possible de le faire rapidement grâce à cols_Excel_file()

    # Colonnes à sélectionner
    if (method == "stat_gen1") {  # méthode statistiques générales 1
      cols <- names(dt)[names(dt) %in% c("METHODE", cols_Excel_file()$sg1)]
    }

    dt <- dt[, ..cols]  # sélection des colonnes
    return(dt)
  }
  create_dt_code_list_med <- function() {
    ### Data contenant tous les codes de liste de médicament ainsi que leur
    ### description

    dt <- inesss::V_DES_COD[
      TYPE_CODE == "COD_CATG_LISTE_MED",  # sélection des code catégorie liste médicament
      .(code = CODE, desc = CODE_DESC)  # colonnes code + description
    ]
    return(dt)
  }
  create_dt_code_serv <- function() {
    ### Data contenant la liste des codes de services et leur description

    dt <- inesss::V_DEM_PAIMT_MED_CM.SMED_COD_SERV[
      , .(code = COD_SERV, desc = COD_SERV_DESC)  # colonnes code + description
    ]

    # Codes L à M3 sont tous inclus dans le même, donc modification du data
    dt <- dt[!code %in% c("L", "M", "M1", "M2", "M3")]
    dt <- rbind(dt, data.table(code = "L, M, M1 à M3", desc = "PREPARATION MAGISTRALE"))
    dt <- setkey(dt, code)
    return(dt)
  }
  format_xl_err_nl <- function(l = 60) {
    ### Répétition de '=' collé 'l' fois. Souvent utilisé pour une nouvelle
    ### section

    return(paste0("\n", paste(rep("=", l), collapse = ""), "\n\n"))
  }
  format_xl_err_sh <- function(name) {
    ### Indiquer le nom de l'onglet dans les messages d'erreur = 'sh : '

    return(paste0(name," :\n"))
  }
  format_price <- function(x) {
    ### Converti un nombre en chaîne de caractères avec deux décimales, même si
    ### c'est zéro (0)

    return(sapply(x, formatC, digits = 2, format = "f", big.mark = " ", decimal.mark = ","))
  }
  msg_error_from_xlfile <- function(file) {
    ### À partir d'un fichier Excel, vérifie chaque onglet et renvoie les
    ### messages d'erreur s'il y en a. Utilisé dans la section Requêtes via Excel

    sheets <- excel_sheets(file)  # nom des onglets du fichier importé
    msg_error <- ""  # contiendra les messages d'erreur
    at_least_1 <- FALSE  # doit y avoir au moins un onglet qui contient des arguments

    for (sh in sheets) {
      # Importation du data
      suppressMessages(suppressWarnings({  # supprime avertissements puisque les tableaux sont irréguliers
        dt <- as.data.table(read_excel(file, sheet = sh, col_types = "text"))
      }))

      if ("METHODE" %in% names(dt)) {
        method <- str_remove_all(rmNA(dt$METHODE), " ")  # méthode à utiliser
        # Gérer les erreurs selon le cas
        if (length(method) == 1) {
          if (method %in% methods_Excel_file()) {
            dt <- cols_select_from_method(dt, method)  # sélection des colonnes en lien avec la méthode
            if (is.null(rmNA(dt$DATE_DEBUT)) && is.null(rmNA(dt$DATE_FIN))) {
              next
            }
            msg_error <- verif_method()[[method]](dt, sh, msg_error)  # vérifications selon méthode
            if (!at_least_1) {
              at_least_1 <- TRUE
            }
          } else {
            msg_error <- paste0(  # erreur si la méthode est inconnue
              msg_error,
              sh, " :\n",
              " -  METHODE ne contient pas une valeur permise.\n",
              format_xl_err_nl()
            )
            if (!at_least_1) {
              at_least_1 <- TRUE
            }
          }
        } else {
          msg_error <- paste0(  # si la méthode contient plusieurs valeurs ou aucune
            msg_error,
            sh, " :\n",
            " -  METHODE doit contenir une valeur.\n",
            format_xl_err_nl()
          )
          if (!at_least_1) {
            at_least_1 <- TRUE
          }
        }
      } else {
        msg_error <- paste0(  # si la colonne METHODE
          msg_error,
          sh, " :\n",
          " -  METHODE est absente.\n",
          format_xl_err_nl()
        )
        if (!at_least_1) {
          at_least_1 <- TRUE
        }
      }
    }

    if (msg_error == "") {
      if (!at_least_1) {
        return("Aucun onglet ne semble contenir des arguments.")
      } else {
        return(NULL)
      }
    } else {
      return(msg_error)
    }
  }
  save_Excel <- function(dt, save_path) {
    ### Enregistrer au format Excel

    write_xlsx(dt, save_path$datapath)
  }
  save_xl_file_queries_method <- function(conn, filepath, savepath) {
    ### Effectuer la requête de chaque onglet du fichier Excel contenant les
    ### arguments de chacune d'elles.

    sheets <- excel_sheets(filepath)  # nom des onglets du fichier excel
    excel_requetes <- list()  # contiendra les tableaux résultats

    i <- 1L
    for (sh in 1:length(sheets)) {
      suppressMessages(suppressWarnings({  # supprimer messages d'avertissements
        dt <- read_excel(filepath, sheet = sheets[sh])  # importer les arguments
      }))
      method <- str_remove_all(rmNA(dt$METHODE), " ")  # détecter la méthode
      # Tableau des résultats selon la méthode à utiliser
      if (is.null(rmNA(dt$DATE_DEBUT)) && is.null(rmNA(dt$DATE_FIN))) {
        next
      } else {
        if (method == "stat_gen1") {
          excel_requetes[[i]] <- save_xl_file_queries_sg1(dt, conn)
        }
        names(excel_requetes)[i] <- sheets[sh]  # conserver le nom initial des onglets
        i <- i + 1L
      }

    }

    write_xlsx(excel_requetes, savepath)  # sauvegarder les tableaux en Excel sur le poste

  }
  save_xl_file_queries_sg1 <- function(dt, conn) {
    ### Référence à save_xl_file_queries_method() lorsque la méthode est "stat_gen1"

    # Arguments selon les valeurs du tableau dt
    dates_debut <- as_date_excel_chr(str_remove_all(rmNA(dt$DATE_DEBUT), " "))
    dates_fin <- as_date_excel_chr(str_remove_all(rmNA(dt$DATE_FIN), " "))
    type_rx <- str_remove_all(rmNA(dt$TYPE_RX), " ")
    code_rx <- str_remove_all(rmNA(dt$CODE_RX), " ")
    resultby <- str_remove_all(rmNA(dt$RESULTATS_PAR), " ")
    if (!length(resultby)) resultby <- NULL
    code_serv_filtre <- str_remove_all(rmNA(dt$CODE_SERV_FILTRE), " ")
    code_serv <- sort(adapt_code_serv(str_remove_all(rmNA(dt$CODE_SERV), " ")))
    if (!length(code_serv)) code_serv <- NULL
    code_list_filtre <- str_remove_all(rmNA(dt$CODE_LIST_FILTRE), " ")
    code_list <- sort(str_remove_all(rmNA(dt$CODE_LIST), " "))
    if (!length(code_list)) code_list <- NULL

    # Tableau des résultats
    DT <- sql_stat_gen1(
      conn = conn,
      debut = dates_debut, fin = dates_fin,
      type_Rx = type_rx, codes = code_rx, result_by = resultby,
      code_serv = code_serv, code_serv_filtre = code_serv_filtre,
      code_list = code_list, code_list_filtre = code_list_filtre
    )

    # Mettre sur une page
    #   - Tableau des résultats
    #   - Arguments
    #   - Exemple de code SQL de la première période d'étude
    return(create_dt_data_args_query(
      dt = DT,  # tableau des résultats
      args_list = list(  # liste des arguments
        METHODE = "stat_gen1", DATE_DEBUT = dates_debut, DATE_FIN = dates_fin,
        TYPE_RX = type_rx, CODE_RX = code_rx,
        RESULTATS_PAR = resultby,
        CODE_SERV_FILTRE = code_serv_filtre, CODE_SERV = desadapt_code_serv(code_serv),
        CODE_LIST_FILTRE = code_list_filtre, CODE_LIST = code_list
      ),
      query = stat_gen1_query(  # code SQL de la 1ere période d'étude
        debut = dates_debut[1], fin = dates_fin[1], type_Rx = type_rx, codes = code_rx,
        result_by = resultby,
        code_serv = code_serv, code_serv_filtre = code_serv_filtre,
        code_list = code_list, code_list_filtre = code_list_filtre
      )
    ))

  }
  sg1_find_date <- function(input, method = "deb") {
    ### Trouver toutes les valeurs des dates de début, method="deb", ou de fin
    ### (method = "fin") qui se retrouvent dans input.

    if (method == "deb") {
      type_date <- 1L
    } else if (method == "fin") {
      type_date <- 2L
    } else {
      stop("formulaire.sg1_find_date(): valeurs permises de method = {'deb', 'fin'}.")
    }

    if (is.null(input$sg1_date1)) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements. Pourrait être géré autrement avec
      # des ordres de priorité.
      return(as.character(Sys.Date()))
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input$sg1_nb_per) {
        vec <- c(vec, as.character(input[[paste0("sg1_date",i)]][type_date]))
      }
      return(vec)
    }
  }
  sg1_find_code <- function(input) {
    ### Trouver toutes les valeurs de codes d'analyse qui se retrouvent dans
    ### input. Si aucune valeur inscrite, retourne ""

    if (is.null(input$sg1_code1)) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements
      return("")
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input$sg1_nb_codes) {
        vec <- c(vec, as.character(input[[paste0("sg1_code",i)]]))
      }
      return(vec)
    }
  }
  sg1_code_list_choices <- function(dt_code_list) {
    ### Valeurs utilisées pour le choix des codes de liste de médicaments

    dt <- dt_code_list[code %in% c("03", "40", "41")]  # codes utilisés
    dt[, ch_name := paste0(code," : ",desc), .(code)]  # indiquer la description
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))
  }
  sg1_code_serv_choices <- function(dt_code_serv) {
    ### Valeurs utilisées pour le choix des codes de service

    dt <- dt_code_serv[code %in% c("1", "AD", "L, M, M1 à M3")]  # codes utilisés
    dt[, ch_name := paste0(code," : ",desc), .(code)]  # indiquer la description
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))
  }
  sg1_dbGetQuery <- function(input, conn) {
    ### Effectue la ou les requêtes de statistiques générales selon les arguments
    ### @param input : Équivalent à une liste. Les éléments doivent avoir les
    ###                mêmes noms que les input de l'onglet sg1 = Statistiques générales
    ### @param conn_values : Variable de connexion créé dans la section SERVER

    DT <- sql_stat_gen1(
      conn = conn,
      debut = sg1_find_date(input, "deb"), fin = sg1_find_date(input, "fin"),
      type_Rx = input$sg1_type_Rx, codes = sg1_find_code(input), result_by = input$sg1_group_by,
      code_serv = adapt_code_serv(input$sg1_code_serv), code_serv_filtre = input$sg1_code_serv_filter,
      code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter
    )
    return(DT)

  }
  sg1_table_format <- function(dt) {
    ### Format visuel des colonnes pour meilleure présentation du data créé par
    ### la requête

    if (is.null(dt)) {
      return(NULL)
    } else {
      dt <- copy(dt)
      dt[  # formatage des résultats pour présentation
        , `:=` (MNT_MED = paste(format_price(MNT_MED), "$"),
                MNT_SERV = paste(format_price(MNT_SERV), "$"),
                MNT_TOT = paste(format_price(MNT_TOT), "$"),
                COHORTE = formatC(COHORTE, big.mark = " "),
                NBRE_RX = formatC(NBRE_RX, big.mark = " "),
                QTE_MED = formatC(QTE_MED, format = "f", digits = 3,
                                  big.mark = " ", decimal.mark = ","),
                DUREE_TX = formatC(DUREE_TX, big.mark = " "))
      ]
      return(dt)
    }
  }
  shinyFiles_directories <- function(input_name, method) {
    ### Créer le répertoire à partir d'un shinyFileButton
    if (is.integer(input_name)) {
      return(NULL)
    } else if (method == "save") {
      return(parseSavePath(Volumes_path(), input_name))
    } else if (method == "file") {
      return(parseFilePaths(Volumes_path(), input_name))
    } else {
      stop("formulaire.shinyFiles_directories() method valeur non permise.")
    }
  }
  verif_sg1 <- function(dt, sh, msg_error) {
    ### Vérification de chaque colonne pour la méthode sg1/stat_gen1/Statistiques générales
    ### lorsque les arguments sont inscrit dans un fichier Excel

    cols <- cols_Excel_file()$sg1  # colonnes nécessaires
    vals <- values_Excel_file()$sg1  # valeurs possible des colonnes
    new_error <- TRUE  # nouvelle erreur -> indiquer nom onglet
    init_msg_error <- msg_error  # comparer a la fin pour déterminer s'il y a une erreur

    # Vérifier la présence de chaque colonne
    for (col in cols) {
      if (!col %in% names(dt)) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error," -  La colonne ",col," est absente.\n")
      }
    }

    # Vérifier la valeur dans chaque colonne (celles nécessaires)
    # DATE_DEBUT & DATE_FIN
    for (col in c("DATE_DEBUT", "DATE_FIN")) {
      if (col %in% names(dt)) {  # si la colonne existe - erreur gérée plus haut
        col_date <- str_remove_all(rmNA(dt[[col]]), " ")  # vecteur contenant les valeurs
        if (length(col_date)) {  # s'il y a des valeurs
          col_date <- as_date_excel_chr(col_date)  # convertir au format date
          if (anyNA(col_date)) {  # NA après conversion -> erreur
            if (new_error) {
              msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
              new_error <- FALSE
            }
            msg_error <- paste0(msg_error,
              " -  ",col," a au moins une valeur qui n'est pas au format 'AAAA-MM-JJ'.\n"
            )
          }
        } else {
          # La colonne ne contient pas de valeurs
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
            " -  ",col," ne contient pas de valeurs.\n"
          )
        }
      }
    }

    # DATE_DEBUT & DATE_FIN doivent avoir le même nombre de valeurs
    if ("DATE_DEBUT" %in% names(dt) && "DATE_FIN" %in% names(dt)) {
      lng_deb <- length(str_remove_all(rmNA(dt$DATE_DEBUT), " "))  # nombre de valeurs
      lng_fin <- length(str_remove_all(rmNA(dt$DATE_FIN), " "))
      if (lng_deb != lng_fin) {  # si le nbre de valeurs est différent
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
          " -  DATE_DEBUT et DATE_FIN n'ont pas le même nombre de valeurs.\n"
        )
      }
    }

    # TYPE_RX, CODE_SERV_FILTRE, CODE_LIST_FILTRE
    for (col in c("TYPE_RX", "CODE_SERV_FILTRE", "CODE_LIST_FILTRE")) {
      if (col %in% names(dt)) {
        vec <- str_remove_all(rmNA(dt[[col]]), " ")  # extraire la valeur du tableau
        if (length(vec) == 1) {
          if (!all(vec %in% vals[[col]])) {
            if (new_error) {
              msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
              new_error <- FALSE
            }
            msg_error <- paste0(msg_error,
              " -  ",col," ne contient pas une valeur permise.\n"
            )
          }
        } else {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
            " -  ",col," doit contenir une valeur.\n"
          )
        }
      }
    }

    # CODE_RX
    if ("CODE_RX" %in% names(dt)) {
      code_rx <- str_remove_all(rmNA(dt$CODE_RX), " ")
      if (!length(code_rx)) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
          " -  CODE_RX doit contenir au moins une valeur.\n"
        )
      }
    }

    # RESULTATS_PAR
    if ("RESULTATS_PAR" %in% names(dt)) {
      resultby <- str_remove_all(rmNA(dt$RESULTATS_PAR), " ")
      if (length(resultby)) {
        if (!all(resultby %in% vals$RESULTATS_PAR)) {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
            " -  RESULTATS_PAR ne contient pas une valeur permise.\n"
          )
        }
      }
    }

    # CODE_SERV, CODE_LIST
    for (col in c("CODE_SERV", "CODE_LIST")) {
      if (col %in% names(dt)) {
        vec <- str_remove_all(rmNA(dt[[col]]), " ")
        if (length(vec)) {
          if (col == "CODE_SERV") {
            vec <- adapt_code_serv(vec)
          }
          if (!all(vec %in% vals[[col]])) {
            if (new_error) {
              msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
              new_error <- FALSE
            }
            msg_error <- paste0(msg_error,
              " -  ",col," ne contient pas une valeur permise.\n"
            )
          }
        }
      }
    }

    # Vérifier s'il y a eu une erreur et ajouter une séparation au texte s'il y a lieu
    if (init_msg_error != msg_error) {
      msg_error <- paste0(msg_error, format_xl_err_nl())
    }

    return(msg_error)

  }
  verif_method <- function() {
    ### Détermine la fonction vérification à utiliser selon la méthode

    return(list(stat_gen1 = verif_sg1))
  }
  Volumes_path <- function() {
    ### Répertoires disponible sur l'ordinateur où l'on peut sélectionner ou
    ### enregistrer un fichier.

    return(c(
      `Bureau` = paste0("C:/Users/",Sys.info()[["login"]],"/Desktop"),
      `Par défaut` = path_home(),
      R = R.home(),
      getVolumes()()
    ))
  }

# Datas -------------------------------------------------------------------

  dt_code_list <- create_dt_code_list_med()
  dt_code_serv <- create_dt_code_serv()

# Interface Utilisateur - UI ----------------------------------------------

  ui <- dashboardPage(

    #### HEADER SECTION
    dashboardHeader(title = "Requêtes SQL"),

    #### SIDEBAR SECTION
    dashboardSidebar(
      sidebarMenu(
        ## Connexion SQL - tabConn
        ## Indique si la connexion SQL vers Teradata est faite
        menuItem("Connexion", tabName = "tabConn"),

        ## Requêtes Excel - tabExcel
        ## ffectuer une ou des requêtes à partir d'un fichier
        ## Excel où chaque onglet est un tableau résultat.
        menuItem("Requêtes via Excel", tabName = "tabExcel"),

        ## Statistiques Gérales - tabStatGen1
        ## Utiliser les arguments du formulaire shiny pour effectuer une requête
        menuItem("Statistiques générales", tabName = "tabStatGen1")
      )
    ),

    #### BODY SECTION
    dashboardBody(
      tabItems(  # Contenu de chaque section du sidebar
        ### Connexion SQL - tabConn
        ### L'utilisateur entre ses informations pour faire la connexion SQL au
        ### serveur Teradata.
        tabItem(
          tabName = "tabConn",
          # Informations nécessaires à la connexion
          textInput("sql_user", "Identifiant", value = ""),  # no identifiant
          passwordInput("sql_pwd", "Mot de passe", value = ""),  # mot de passe
          # Établir la connexion ou se déconnecter
          fluidRow(
            column(2, actionButton("sql_conn", "Connexion")),
            column(2, actionButton("sql_deconn", "Déconnexion"))
          ),
          # Indiquer l'état de la connexion
          h5(strong("État de la connexion :")),
          verbatimTextOutput("sql_is_conn", placeholder = TRUE)
        ),


        ### Requêtes via Excel - tabExcel
        ### Importer un fichier Excel contenant les arguments et exécuter les
        ### requêtes, soit chaque onglet
        tabItem(
          tabName = "tabExcel",
          shinyFilesButton(  # bouton pour sélectionner le fichier Excel
            "select_xl_file", "Sélectionner fichier Excel",
            "Sélectionner fichier Excel", multiple = FALSE,
            viewtype = "detail"
          ), p(),  # espace entre le bouton et ce qui suit
          # Indiquer les erreurs de chaque onglet s'il y a lieu
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          # Effectuer les extractions s'il n'y a pas d'erreur
          uiOutput("save_xl_file")

          # ---------------------------------------------------- -
          # --- À FAIRE ---
          # # Inscrire le ou les courriels à envoyer les résultats
          # # Peut-être remplacer les résultats par un message
          # h5("Envoyer résultats par courriel"),
          # textAreaInput("mails", "Courriels"),
          # textInput("mail_obj", "Object")
          # ---------------------------------------------------- -
        ),





        ### Statistiques générales - tabStatGen1
        ### Exécuter une requête simple à partir des arguments disponibles dans
        ### le formulaire.
        tabItem(
          tabName = "tabStatGen1",

          # ARGUMENTS SECTION
          fluidRow(
            h4(HTML("&nbsp;&nbsp;"), "Arguments"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px"),
          # Afficher sur la première ligne
          #   - Nombre de périodes d'analyse
          #   - Nombre de codes Rx à analyser
          #   - Les variables 'Grouper Par', 'Exclusion Code Services',
          #     'Selection Categorie Liste', ...
          fluidRow(
            column(  # Périodes d'analyse
              width = 3,
              # Nombre de périodes à afficher
              numericInput("sg1_nb_per", "Nombre de périodes", value = 1),
              uiOutput("sg1_nb_per")
            ),
            column(  # Codes d'analyse
              width = 3,
              # Nombre de codes à afficher pour l'analyse
              numericInput("sg1_nb_codes", "Nombre de Codes Rx", value = 1),
              # Sélection du type de code Rx
              selectInput("sg1_type_Rx", "Type de Code Rx",
                          choices = c("DENOM", "DIN"), selected = "DENOM"),
              # Text inputs où indiquer les codes d'analyse
              uiOutput("sg1_nb_codes")
            ),
            column(
              width = 2,
              # Grouper par
              checkboxGroupInput(
                "sg1_group_by", "Résultats par",
                choices = c("Périodes", "Teneur", "Format")
              )
            ),
            column(
              width = 4,
              # Codes de services
              selectInput("sg1_code_serv_filter", "Codes de Services",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Exclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "sg1_code_serv", "",
                choiceNames = sg1_code_serv_choices(dt_code_serv)$ch_name,
                choiceValues = sg1_code_serv_choices(dt_code_serv)$value,
                selected = c("1", "AD")
              ),
              # Codes liste médicaments
              selectInput("sg1_code_list_filter", "Codes Liste Médicament",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Inclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "sg1_code_list", "",
                choiceNames = sg1_code_list_choices(dt_code_list)$ch_name,
                choiceValues = sg1_code_list_choices(dt_code_list)$value
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              actionButton(  # Exécution de la requête SQL
                "sg1_go_extract", "Exécuter Requête",
                style = paste0("color: #ffffff;",
                               "background-color: #006600;",
                               "border-color: #000000;")
              )
            ),
            column(
              width = 3,
              actionButton(  # Réinitialiser les codes Rx
                "sg1_reset_args", "Réinitialiser Arguments",
                style = paste0("color: #ffffff;",
                               "background-color: #990000;",
                               "border-color: #000000;")
              )
            )
          ),


          # RESULTATS SECTION
          uiOutput("sg1_html_result_section"),  # En-tête
          fluidRow(
            p(),
            dataTableOutput("sg1_table_req"),  # tableau des résultats
            p()
          ),
          fluidRow(
            column(
              width = 3,
              uiOutput("sg1_save") # bouton sauvegarder les résultats de la requête
            )
          ),


          # CODE SQL SECTION
          fluidRow(
            uiOutput("sg1_html_SQL_section"),
            uiOutput("sg1_html_code_SQL")
          )
        )
      )
    )

  )


# Server ------------------------------------------------------------------

  server <- function(input, output, session) {

    #### Fermer l'application lorsque la fenêtre se ferme
    session$onSessionEnded(function() {stopApp()})





    #### CONNEXION SQL - tabConn
    # Valeurs nécessaires à la connexion de teradata
    conn_values <- reactiveValues(
      uid = NULL, pwd = NULL,  # no utilisateur & mot de passe
      msg = "",  # message d'erreur
      conn = FALSE  # paramètres de connexion. FALSE au lieu de NULL pour éviter des messages d'erreur lors du démarrage
    )

    # Vérifier si les informations entrées sont correctes.
    # Enregistrer les valeurs dans 'conn_values' si c'est le cas.
    observeEvent(input$sql_conn, {
      showNotification("Connexion en cours...", id = "sql_conn", type = "message", duration = NULL)
      if (input$sql_user == "" || input$sql_pwd == "") {
        # Indiquer d'inscrire une valeur dans toutes les cases
        conn_values$msg <- "**Inscrire le numéro d'identifiant ainsi que le mot de passe**"
        conn_values$conn <- NULL  # aucune connexion
      } else {
        conn_values$conn <- sql_connexion(input$sql_user, input$sql_pwd)  # effectuer une connexion
        if (is.null(conn_values$conn)) {
          # Message d'erreur si la connexion ne fonctionnait pas
          conn_values$msg <- "**Vérifier l'identifiant et le mot de passe**"
        } else {
          # Si pas d'erreur, on sauvegarde les paramètres de connexion
          conn_values$uid <- toupper(input$sql_user)
          conn_values$pwd <- input$sql_pwd
          conn_values$msg <- paste0("Connexion : ", Sys.time())  # msg indiquant l'heure de la dernière connexion/sauvegarde
        }
      }
      removeNotification("sql_conn")
    })

    # Déconnexion à Teradata
    observeEvent(input$sql_deconn, {
      conn_values$conn <- NULL  # aucune connexion
      conn_values$msg <- NULL
      conn_values$uid <- NULL
      conn_values$pwd <- NULL
      updateTextInput(session, "sql_user", value = "")
      updateTextInput(session, "sql_pwd", value = "")
    })

    # Afficher l'état de la connexion
    output$sql_is_conn <- renderText({ conn_values$msg })





    #### REQUÊTES VIA Excel - tabExcel
    # Sélection du fichier Excel
    shinyFileChoose(input, "select_xl_file", roots = Volumes_path())
    select_xl_file <- reactive({ shinyFiles_directories(input$select_xl_file, "file") })  # select_xl_file()datapath indique répertoire + nom du fichier à importer

    # Indiquer les messages d'erreurs une fois le fichier Excel importé
    xl_errors_msg <- eventReactive(select_xl_file(), {  # vérifier le contenu du fichier Excel une fois importé
      showNotification("Vérification en cours...", id = "xl_errors_msg", type = "message", duration = NULL)
      msg_error <- msg_error_from_xlfile(select_xl_file()$datapath)
      removeNotification("xl_errors_msg")
      if (is.null(msg_error)) {
        return("Aucune erreur, exécution possible.")
      } else {
        return(msg_error)
      }
    })
    output$xl_errors_msg <- renderText({ xl_errors_msg() })

    # Bouton pour enregistrer les requêtes via fichier Excel
    output$save_xl_file <- renderUI({
      if (xl_errors_msg() == "Aucune erreur, exécution possible.") {
        return(shinySaveButton(
          "save_xl_file", "Exécuter requêtes", "Enregistrer sous...",
          filetype = list(`Classeur Excel` = "xlsx"),
          viewtype = "list",
          style = paste0("color: #ffffff;",
                         "background-color: #006600;",
                         "border-color: #000000;")
        ))
      } else {
        return(NULL)
      }
    })
    shinyFileSave(input, "save_xl_file", roots = Volumes_path())  # bouton pour déterminer le répertoire
    save_xl_file <- reactive({ shinyFiles_directories(input$save_xl_file, "save")})
    # Enregistrer les requêtes dans un fichier Excel
    observeEvent(save_xl_file(), {
      if (xl_errors_msg() == "Aucune erreur, exécution possible." && !is.null(conn_values$conn)) {
        showNotification("Exécution en cours...", id = "save_xl_file", type = "message", duration = NULL)
        save_xl_file_queries_method(  # effectuer les requêtes pour chaque onglet du fichier
          conn = conn_values$conn,
          filepath = select_xl_file()$datapath,
          savepath = save_xl_file()$datapath
        )
        removeNotification("save_xl_file")
      } else {
        showNotification("Exécution impossible. Vérifier la connexion ou corriger les erreurs.")
      }
    })





    #### STATISTIQUE GENERALES - tabStatGen1
    # Variables réactives pour sg1
    sg1_val <- reactiveValues(
      show_query = FALSE,  # afficher la requête ou pas
      query = NULL,  # message à afficher. NULL = même pas une case où afficher du texte
      show_tab = FALSE  # contient le tableau de la requête/résultats
    )

    # Périodes d'analyse : afficher le bon nombre de dateRangeInput selon valeur
    # de input$sg1_nb_per
    output$sg1_nb_per <- renderUI({
      n <- input$sg1_nb_per  # nb périodes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "sg1_nb_per", value = 1)
      }
      isolate({  # voir commentaire 'output$sg1_nb_codes'
        dates_input <- vector("list", length = n)
        # Créer des dateRangeInput. Possible de conserver les valeurs précédentes
        # si input$sg1_nb_per diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("sg1_date",i)]])) {
            dates_input[[i]] <- dateRangeInput(
              inputId = paste0("sg1_date",i), label = paste("Période", i),
              separator = " au ",
              autoclose = FALSE  # permet d'écrire manuellement la date sans erreur
            )
          } else {
            dates_input[[i]] <- dateRangeInput(
              inputId = paste0("sg1_date",i), label = paste("Période", i),
              start = input[[paste0("sg1_date",i)]][1],
              end = input[[paste0("sg1_date",i)]][2],
              separator = " au ",
              autoclose = FALSE
            )
          }
        }
        return(tagList(dates_input))
      })
    })


    # Codes Rx d'analyse : afficher le bon nombre de textInput selon la valeur
    # de input$sg1_nb_codes
    sg1_nb_codes <- reactive({
      n <- input$sg1_nb_codes  # nb codes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "sg1_nb_codes", value = 1)
      }
      isolate({  # enlève la réactivité de chaque input créé, permet d'écrire
        # dans le textInput sans qu'il y ait de réactivité
        codes_input <- vector("list", length = n)
        # Créer des textInput. Possible de conserver les valeurs précédentes
        # si input$sg1_nb_codes diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("sg1_code",i)]])) {
            codes_input[[i]] <- textInput(inputId = paste0("sg1_code",i),
                                          label = paste("Code Rx", i),
                                          value = "")
          } else {
            codes_input[[i]] <- textInput(inputId = paste0("sg1_code",i),
                                          label = paste("Code Rx", i),
                                          value = input[[paste0("sg1_code",i)]])
          }
        }
        return(tagList(codes_input))
      })
    })
    output$sg1_nb_codes <- renderUI({ sg1_nb_codes() })


    # En-tête Résultats - Apparaît seulement s'il y a eu une requête
    output$sg1_html_result_section <- renderUI({
      if (sg1_val$show_tab) {
        return(tagList(
          div(style = "margin-top:15px"),
          fluidRow(
            h4(HTML("&nbsp;&nbsp;"), "Résultats"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px")
        ))
      } else {
        return(NULL)
      }
    })
    # Requête SQL
    sg1_requete_sql <- eventReactive(input$sg1_go_extract, {
      if (any("" %in% str_remove_all(sg1_find_code(input), " "))) {
        sg1_val$show_tab <- FALSE
        showNotification("Inscrire un Code Rx dans chaque zone prévu à cet effet.", type = "error")
        return(NULL)
      } else if (!is.null(conn_values$conn)) {
        if (is.logical(conn_values$conn) && conn_values$conn == FALSE) {
          sg1_val$show_tab <- FALSE
          showNotification("Exécution impossible. Connexion requise.", type = "error")
          return(NULL)
        }
        showNotification("Exécution en cours...", id = "sg1_go_extract", type = "message", duration = NULL)
        sg1_val$show_tab <- TRUE
        DT <- sg1_dbGetQuery(input, conn_values$conn)
        removeNotification("sg1_go_extract")
        return(DT)
      } else {
        sg1_val$show_tab <- FALSE
        showNotification("Exécution impossible. Connexion requise.", type = "error")
        return(NULL)
      }
    })
    # Afficher le tableau demandé
    output$sg1_table_req <- renderDataTable({
      DT <- sg1_table_format(sg1_requete_sql())
      if (sg1_val$show_tab) {
        return(DT)
      } else {
        return(NULL)
      }

      },
      options = list(scrollX = TRUE)  # scrolling si le tableau est plus large que la fenêtre
    )

    # Enregistrer le fichier au format Excel
    output$sg1_save <- renderUI({  # faire apparaître bouton de sauvegarde s'il y a eu une extraction
      if (sg1_val$show_tab) {
        return(shinySaveButton(
          "sg1_save", "Sauvegarder Résultats en Excel",
          "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
          filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
          viewtype = "list",
          style = paste0("color: #ffffff;",
                         "background-color: #006600;",
                         "border-color: #000000;")
        ))
      } else {
        return(NULL)
      }
    })
    shinyFileSave(input, "sg1_save", roots = Volumes_path())  # détermine les répertoires à afficher pour le bouton sg1_save
    sg1_file_save <- reactive({ shinyFiles_directories(input$sg1_save, "save") })
    observeEvent(sg1_file_save(), {  # sauvegarde de la requête en Excel
      if (nrow(sg1_file_save())) {  # car à la base table de 0 ligne 4 colonnes, 1 ligne si on sélectionne un répertoire
        showNotification("Sauvegarde en cours.", id = "sg1_file_save", type = "message", duration = NULL)
        save_Excel(
          dt = create_dt_data_args_query(
            dt = sg1_requete_sql(),
            args_list = list(
              METHODE = "stat_gen1",
              DATE_DEBUT = sg1_find_date(input, "deb"),
              DATE_FIN = sg1_find_date(input, "fin"),
              TYPE_RX = input$sg1_type_Rx, CODE_RX = sg1_find_code(input),
              RESULTATS_PAR = input$sg1_group_by,
              CODE_SERV_FILTRE = input$sg1_code_serv_filter,
              CODE_SERV = adapt_code_serv(input$sg1_code_serv),
              CODE_LIST_FILTRE = input$sg1_code_list_filter,
              CODE_LIST = input$sg1_code_list
            ),
            query = stat_gen1_query(
              debut = sg1_find_date(input, "deb")[1], fin = sg1_find_date(input, "fin")[1],
              type_Rx = input$sg1_type_Rx, codes = sg1_find_code(input),
              result_by = input$sg1_group_by,
              code_serv = input$sg1_code_serv, code_serv_filtre = input$sg1_code_serv_filter,
              code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter
            )
          ),
          save_path = sg1_file_save()
        )
        removeNotification("sg1_file_save")
      }
    })

    # En-tête SQL - Apparaît seulement s'il y a eu une requête
    output$sg1_html_SQL_section <- renderUI({
      if (sg1_val$show_tab) {
        return(tagList(
          div(style = "margin-top:15px"),
          fluidRow(
            h4(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Requête SQL"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px")
        ))
      } else {
        return(NULL)
      }
    })

    # Code SQL en lien avec les résultats
    output$sg1_code_SQL <- renderText({  # code sql de la requête selon les arguments
      stat_gen1_query(
        debut = sg1_find_date(input, "deb")[1], fin = sg1_find_date(input, "fin")[1],
        type_Rx = input$sg1_type_Rx, codes = sort(sg1_find_code(input)),
        result_by = input$sg1_group_by,
        code_serv = adapt_code_serv(input$sg1_code_serv), code_serv_filtre = input$sg1_code_serv_filter,
        code_list = sort(input$sg1_code_list), code_list_filtre = input$sg1_code_list_filter
      )
    })
    output$sg1_html_code_SQL <- renderUI({  # section affichant le code SQL de la requête
      if (sg1_val$show_tab) {
        verbatimTextOutput("sg1_code_SQL")
      } else {
        return(NULL)
      }
    })

    # Réinitialiser les arguments comme initialement
    observeEvent(input$sg1_reset_args, {
      # Effacer périodes sauf la 1ere
      updateNumericInput(session, "sg1_nb_per", value = 1)

      # Effacer code et remettre à 1
      n <- input$sg1_nb_codes
      for (i in 1:n) {  # Créer des textInput vide -> efface les valeurs précédentes
        updateTextInput(session, inputId = paste0("sg1_code",i),
                        label = paste("Code Rx", i), value = "")
      }
      updateNumericInput(session, "sg1_nb_codes", value = 1)

      # Mettre à jour les checkboxGroup
      updateCheckboxGroupInput(session, inputId = "sg1_group_by",
                               selected = character(0))
      updateSelectInput(session, inputId = "sg1_code_serv_filter",
                        selected = "Exclusion")
      updateCheckboxGroupInput(session, inputId = "sg1_code_serv",
                               selected = c("1", "AD"))
      updateSelectInput(session, inputId = "sg1_code_list_filter",
                        selected = "Inclusion")
      updateCheckboxGroupInput(session, inputId = "sg1_code_list",
                               selected = character(0))

      # Effacer section Résultats et Requête SQL
      sg1_val$show_tab <- FALSE  # variable qui détermine si on affiche les sections
    })

  }

# Application -------------------------------------------------------------

  shinyApp(ui, server)

}
