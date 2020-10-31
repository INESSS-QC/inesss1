#' Formulaire Shiny
#'
#' @import data.table
#' @importFrom fs path_home
#' @importFrom odbc odbc dbConnect dbGetQuery
#' @importFrom readxl excel_sheets read_excel
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles shinyFilesButton shinyFileChoose shinyFileSave shinySaveButton parseFilePaths parseSavePath
#' @importFrom stringr str_split str_remove_all
#' @importFrom writexl write_xlsx
#' @export
formulaire <- function() {

  library(data.table)
  library(fs)
  library(odbc)
  library(readxl)
  library(shiny)
  library(shinydashboard)
  library(shinyFiles)
  library(stringr)
  library(writexl)

# Variables ---------------------------------------------------------------

  cols_EXCEL_file <- function() {
    ### Colonnes nécessaires pour chaque méthode dans les
    return(list(
      sg1 = c("DATE_DEBUT", "DATE_FIN", "TYPE_RX", "CODE_RX",
              "CODE_SERV_FILTRE", "CODE_SERV", "CODE_LIST_FILTRE", "CODE_LIST")
    ))
  }
  values_EXCEL_file <- function() {
    ### Valeurs permises dans les colonnes de cols_EXCEL_file()
    return(list(
      sg1 = list(
        TYPE_RX = c("DENOM", "DIN"),
        CODE_SERV_FILTRE = c("Exclusion", "Sélection"),
        CODE_SERV = c("1", "A", "AD", "I", "L", "M", "M1", "M2", "M3", "Q", "RA"),
        CODE_LIST_FILTRE = c("Exclusion", "Sélection"),
        CODE_LIST = c("03", "40", "41")
      )
    ))
  }
  methods_EXCEL_file <- function() {
    return(c(
      "stat_gen1"
    ))
  }

# Fonctions ---------------------------------------------------------------

  adapt_code_serv <- function(code_serv) {
    ### Séparer des groupes de codes de service en valeurs uniques pour les
    ### requêtes SQL

    if ("L, M, M1 à M3" %in% code_serv) {
      code_serv <- code_serv[code_serv != "L, M, M1 à M3"]
      code_serv <- sort(c(code_serv, "L", "M", "M1", "M2", "M3"))
    }
    return(code_serv)
  }
  create_dt_code_list_med <- function() {
    ### Data conteant tous les codes de liste de médicament ainsi que leur
    ### description

    dt <- PROD.V_DES_COD[TYPE_CODE == "COD_CATG_LISTE_MED", .(code = CODE, desc = CODE_DESC)]
    return(dt)
  }
  create_dt_code_serv <- function() {
    ### Data contenant la liste des codes de services et leur description

    dt <- PROD.V_DEM_PAIMT_MED_CM.SMED_COD_SERV[, .(code = COD_SERV, desc = COD_SERV_DESC)]
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
  nom_type_rx <- function(type_rx) {
    ### Indique le nom de la colonne indiquant le nom du type de Rx dans un data

    if (type_rx == "DENOM") {
      return("NOM_DENOM")
    } else if (type_rx == "DIN") {
      return("NOM_MARQ_COMRC")
    } else {
      stop("formulaire.nom_type_rx() valeur non permise.")
    }
  }
  save_EXCEL_data_args_query <- function(dt, args_list, query, save_path) {
    ### Enregistrer au format EXCEL le tableau des résultats, ajouter les arguments
    ### du formulaire ainsi que la requête en lien avec ces arguments.

    # Déterminer le nombre de lignes que l'onglet Excel doit avoir.
    query <- data.table(`Requête SQL` = str_split(query, "\n")[[1]]) # séparer la chaine de caractères en vecteur
    nb_row <- max(nrow(dt), sapply(args_list, length), nrow(query))  # nbre de lignes nécessaires

    # Ajouter des valeurs à tous les éléments qui ont une longueur < nb_row
    if (nrow(dt) < nb_row) {
      dt <- rbind(dt, data.table(DATE_DEBUT = rep(NA, nb_row - nrow(dt))), fill = TRUE)
    }
    for (i in 1:length(args_list)) {
      if (length(args_list[[i]]) < nb_row) {
        args_list[[i]] <- c(args_list[[i]], rep(NA, nb_row - length(args_list[[i]])))
      }
    }
    if (nrow(query) < nb_row) {
      query <- rbind(query, data.table(`Requête SQL` = rep(NA, nb_row - nrow(query))))
    }

    # Regrouper tous les éléments ensemble dans un même tableau
    tab_to_export <- cbind(
      dt,
      data.table(v_1 = rep(NA, nb_row),  # espaces pour séparer les éléments
                 v_2 = rep(NA, nb_row), v_3 = rep(NA, nb_row)),
      as.data.table(args_list),
      data.table(v_4 = rep(NA, nb_row),
                 v_5 = rep(NA, nb_row), v_6 = rep(NA, nb_row)),
      query
    )
    setnames(tab_to_export, paste0("v_",1:6), rep("", 6))  # supprimer le nom des colonnes pour espacement

    write_xlsx(tab_to_export, save_path$datapath)

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
      # éviter des messages d'avertissements
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
    ### input.

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
    ### Tableau indiquant les choix et les valeurs des codes de liste
    dt <- dt_code_list[code %in% c("03", "40", "41")]
    dt[, ch_name := paste0(code," : ",desc), .(code)]
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))
  }
  sg1_code_serv_choices <- function(dt_code_serv) {
    ### Tableau indiquant les choix et les valeurs des codes de services
    dt <- dt_code_serv[code %in% c("1", "A", "AD", "I", "L, M, M1 à M3", "Q", "RA")]
    dt[, ch_name := paste0(code," : ",desc), .(code)]
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))
  }
  sg1_dbGetQuery <- function(input, conn) {
    ### Effectue la ou les requêtes de statistiques générales selon les arguments
    ### @param input : Équivalent à une liste. Les éléments doivent avoir les
    ###                mêmes noms que les input de l'onglet sg1 = Statistiques générales
    ### @param conn : Variable de connexion créé par sql_connecion

    DT <- data.table()  # tableau contenant la ou les requêtes
    nom_denom <- copy(PROD.V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE)  # data nom des denom
    nom_marq_comrc <- PROD.V_PRODU_MED.NOM_MARQ_COMRC[, .(DIN, NOM_MARQ_COMRC, DATE_DEBUT, DATE_FIN)]  # data nom marques commerciales

    # Extraire les arguments souvent utilisés des input
    dates_debut <- sg1_find_date(input, "deb")
    dates_fin <- sg1_find_date(input, "fin")
    codes_rx <- sort(sg1_find_code(input))
    codes_serv <- adapt_code_serv(input$sg1_code_serv)

    # Effectuer une requête par période d'étude et joindre les tableaux
    for (i in 1:length(sg1_find_date(input, "deb"))) {

      dt <- as.data.table(dbGetQuery(  # requête pour la ième période d'étude
        conn = conn,  # connexion faite à partir de l'onglet connexion
        statement = stat_gen1_txt_query_1period(
          debut = dates_debut[i], fin = dates_fin[i],
          type_Rx = input$sg1_type_Rx, codes = codes_rx,
          code_serv = codes_serv, code_serv_filtre = input$sg1_code_serv_filter,
          code_list = sort(input$sg1_code_list), code_list_filtre = input$sg1_code_list_filter
        )
      ))
      dt[, `:=` (MNT_MED = as_price(MNT_MED),  # s'assurer que les prix ont deux décimales
                 MNT_SERV = as_price(MNT_SERV),
                 MNT_TOT = as_price(MNT_TOT))]

      # Ajouter le nom du DENOM ou le nom de la marque commerciale (DIN)
      if (input$sg1_type_Rx == "DENOM") {
        dt <- nom_denom[  # ajouter le nom des DENOM à la date de départ de la période
          DATE_DEBUT <= dates_debut[i] & dates_debut[i] <= DATE_FIN,
          .(DENOM, NOM_DENOM)
        ][
          dt, on = .(DENOM)
        ]
      } else if (input$sg1_type_Rx == "DIN") {
        dt <- nom_marq_comrc[  # ajouter le nom des DIN à la date de départ de la période
          DATE_DEBUT <= dates_debut[i] & dates_debut[i] <= DATE_FIN,
          .(DIN, NOM_MARQ_COMRC)
        ][
          dt, on = .(DIN)
        ]
      }
      DT <- rbind(DT, dt)  # ajouter cette période aux précédentes

    }

    # Ordre des colonnes
    setcolorder(DT, c(
      "DATE_DEBUT", "DATE_FIN",
      input$sg1_type_Rx, nom_type_rx(input$sg1_type_Rx),
      "MNT_MED", "MNT_SERV", "MNT_TOT",
      "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX"
    ))
    # Ordre des données
    setorderv(DT, c("DATE_DEBUT", "DATE_FIN", input$sg1_type_Rx))

    return(DT)
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
    ### lorsque les arguments sont inscrit dans un fichier EXCEL

    cols <- cols_EXCEL_file()$sg1  # colonnes nécessaires
    vals <- values_EXCEL_file()$sg1  # valeurs possible des colonnes
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
    # CODE_SERV, CODE_LIST
    for (col in c("CODE_SERV", "CODE_LIST")) {
      if (col %in% names(dt)) {
        vec <- str_remove_all(rmNA(dt[[col]]), " ")
        if (length(vec)) {
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
  verif_method <- function(method) {
    ### Détermine la fonction vérification à utiliser selon la méthode

    return(list(stat_gen1 = verif_sg1))
  }
  Volumes_path <- function() {
    ### Répertoires disponible sur l'ordinateur où l'on peut sélectionner ou
    ### enregistrer un fichier.

    return(c(
      getVolumes()(),
      `Par défaut` = path_home(),
      R = R.home()
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

        ## Requêtes EXCEL - tabEXCEL
        ## ffectuer une ou des requêtes à partir d'un fichier
        ## Excel où chaque onglet est un tableau résultat.
        menuItem("Requêtes via EXCEL", tabName = "tabEXCEL"),

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
          # Établir la connexion
          actionButton("sql_conn", "Connexion"),
          # Indiquer l'état de la connexion
          h5(strong("État de la connexion :")),
          verbatimTextOutput("sql_is_conn", placeholder = TRUE)
        ),


        ### Requêtes via EXCEL - tabEXCEL
        ### Importer un fichier EXCEL contenant les arguments et exécuter les
        ### requêtes, soit chaque onglet
        tabItem(
          tabName = "tabEXCEL",
          shinyFilesButton(  # bouton pour sélectionner le fichier Excel
            "select_xl_file", "Sélectionner fichier EXCEL",
            "Sélectionner fichier EXCEL", multiple = FALSE,
            viewtype = "detail"
          ), p(),  # espace entre le bouton et ce qui suit
          # Indiquer les erreurs de chaque onglet s'il y a lieu
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          # Effectuer les extractions s'il n'y a pas d'erreur
          actionButton("go_xl_extract", "Exécuter requête(s)"),
          # ---------------------------------------------------- -
          # --- À FAIRE ---
          # # Inscrire le ou les courriels à envoyer les résultats
          # h5("Envoyer résultats par courriel"),
          # textAreaInput("mails", "Courriels"),
          # textInput("mail_obj", "Object")
          # ---------------------------------------------------- -
          verbatimTextOutput("test_tabEXCEL")
        ),


        ### Statistiques générales - tabStatGen1
        ### Exécuter une requête simple à partir des arguments disponibles dans
        ### le formulaire.
        tabItem(
          tabName = "tabStatGen1",

          # Afficher sur la première ligne
          #   - Nombre de périodes d'analyse
          #   - Nombre de codes Rx à analyser
          #   - Les variables 'Grouper Par', 'Exclusion Code Services',
          #     'Selection Categorie Liste', ...
          fluidRow(
            column(  # Périodes d'analyse
              width = 3,
              # Nombre de périodes à afficher
              numericInput("sg1_nb_per", "Nombre de périodes", value = 1,
                           min = 1, max = 99),
              uiOutput("sg1_nb_per")
            ),
            column(  # Codes d'analyse
              width = 3,
              # Nombre de codes à afficher pour l'analyse
              numericInput("sg1_nb_codes", "Nombre de codes Rx", value = 1,
                           min = 1, max = 99),
              # Sélection du type de code Rx
              selectInput("sg1_type_Rx", "Type de code Rx",
                          choices = c("DENOM", "DIN"), selected = "DENOM"),
              # Text inputs où indiquer les codes d'analyse
              uiOutput("sg1_nb_codes")
            ),
            column(
              width = 3,
              # Codes de service
              selectInput("sg1_code_serv_filter", "Codes de Service",
                          choices = c("Exclusion", "Sélection"),
                          selected = "Exclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              # Codes de service
              checkboxGroupInput(
                "sg1_code_serv", "",
                choiceNames = sg1_code_serv_choices(dt_code_serv)$ch_name,
                choiceValues = sg1_code_serv_choices(dt_code_serv)$value,
                selected = c("1", "AD")
              )
            ),
            column(
              width = 3,
              # Codes liste médicaments
              selectInput("sg1_code_list_filter", "Code Liste Médicament",
                          choices = c("Exclusion", "Sélection"),
                          selected = "Sélection", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "sg1_code_list", "",
                choiceNames = sg1_code_list_choices(dt_code_list)$ch_name,
                choiceValues = sg1_code_list_choices(dt_code_list)$value
              )
            )
          ),

          # Boutons d'exécutions :
          #   - Exécuter requête
          #   - Enregistrer requête
          #   - Visualiser/MaJ code requête
          fluidRow(
            column(  # Bouton exécution requête
              width = 3,
              actionButton("sg1_go_extract", "Exécuter requête")
            ),
            column(
              width = 3,
              shinySaveButton("sg1_save", "Enregistrer requête", "Enregistrer sous...",
                              filetype = list(`Classeur EXCEL` = "xlsx"),
                              viewtype = "list")
            ),
            column(
              width = 3,
              actionButton("sg1_maj_req", "Visualiser/MaJ code requête")
            )
          ),

          # Tableau & Affichage extraction SQL
          fluidRow(
            dataTableOutput("sg1_table_req"),
            verbatimTextOutput("sg1_code_req")
          )
        )
      )
    )

  )


# Server ------------------------------------------------------------------

  server <- function(input, output, session) {

    #### CONNEXION SQL - tabConn
    # Valeurs nécessaires à la connexion de teradata
    conn_values <- reactiveValues(
      uid = NULL, pwd = NULL,  # no utilisateur & mot de passe
      msg = "",  # message d'erreur
      conn = NULL  # contient les paramètres de connexion à utiliser pour une requête
    )

    # Vérifier si les informations entrées sont correctes.
    # Enregistrer les valeurs dans 'conn_values' si c'est le cas.
    observeEvent(input$sql_conn, {
      if (input$sql_user == "" || input$sql_pwd == "") {
        # Indiquer d'inscrire une valeur dans toutes les cases
        conn_values$msg <- "**Inscrire le numéro d'identifiant ainsi que le mot de passe**"
      } else {
        conn_values$conn <- sql_connexion("PEI_PRD", input$sql_user, input$sql_pwd)  # effectuer une connexion
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
    })

    # Afficher l'état de la connexion
    output$sql_is_conn <- renderText({ conn_values$msg })



    #### REQUÊTES VIA EXCEL - tabEXCEL
    # Sélection du fichier EXCEL
    shinyFileChoose(input, "select_xl_file", roots = Volumes_path())
    select_xl_file <- reactive({  # select_xl_file()datapath indique répertoire + nom du fichier à importer
      shinyFiles_directories(input$select_xl_file, "file")
    })

    # Indiquer les messages d'erreurs une fois le fichier EXCEL importé
    xl_errors_msg <- eventReactive(select_xl_file(), {  # vérifier le contenu du fichier EXCEL une fois importé

      file <- select_xl_file()$datapath
      # file = "Rx_stat_gen1 - Copie.xlsx"
      sheets <- excel_sheets(file)
      msg_error <- ""
      # sh = sheets[3]

      # Importer chaque feuille et inscrire les messages d'erreur
      for (sh in sheets) {
        dt <- read_excel(file, sheet = sh, col_types = "text")
        if ("METHOD" %in% names(dt)) {
          method <- str_remove_all(rmNA(dt$METHOD), " ")
          if (length(method) == 1) {
            if (method %in% methods_EXCEL_file()) {
              msg_error <- verif_method()[[method]](dt, sh, msg_error)
            } else {
              msg_error <- paste0(msg_error,
                sh, " :\n",
                " -  METHOD ne contient pas une valeur permise.\n",
                format_xl_err_nl()
              )
            }
          } else {
            msg_error <- paste0(msg_error,
              sh, " :\n",
              " -  METHOD doit contenir une valeur.\n",
              format_xl_err_nl()
            )
          }
        } else {
          msg_error <- paste0(msg_error,
            sh, " :\n",
            " -  METHOD est absente.\n",
            format_xl_err_nl()
          )
        }
      }

      if (msg_error == "") {
        return("Aucune error, exécution des requêtes possible.")
      } else {
        return(msg_error)
      }
    })
    output$xl_errors_msg <- renderText({
      if (is.null(select_xl_file())) {
        return("**Importer un fichier EXCEL**")
      } else {
        return(xl_errors_msg())
      }
    })

    # TEST - VOIR VALEURS
    output$test_tabEXCEL <- renderPrint({
      list(
        go_button = input$go_xl_extract,
        file_selected = select_xl_file()
      )
    })



    #### STATISTIQUE GENERALES - tabStatGen1
    # Périodes d'analyse : afficher le bon nombre de dateRangeInput selon valeur
    # de input$sg1_nb_per
    output$sg1_nb_per <- renderUI({
      n <- input$sg1_nb_per  # nb périodes & déclenche réactivité
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
    output$sg1_nb_codes <- renderUI({
      n <- input$sg1_nb_codes  # nb codes & déclenche réactivité
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

    # Requête SQL
    sg1_requete_sql <- eventReactive(input$sg1_go_extract, { sg1_dbGetQuery(input, conn_values$conn) })
    # Afficher le tableau demandé
    output$sg1_table_req <- renderDataTable({
      DT <- copy(sg1_requete_sql())
      DT[  # formatage des résultats pour présentation
        , `:=` (MNT_MED = paste(format_price(MNT_MED), "$"),
                MNT_SERV = paste(format_price(MNT_SERV), "$"),
                MNT_TOT = paste(format_price(MNT_TOT), "$"),
                COHORTE = formatC(COHORTE, big.mark = " "),
                NBRE_RX = formatC(NBRE_RX, big.mark = " "),
                QTE_MED = formatC(QTE_MED, format = "f", digits = 3,
                                  big.mark = " ", decimal.mark = ","),
                DUREE_TX = formatC(DUREE_TX, big.mark = " "))
      ]
      return(DT)
    })

    # Enregistrer le fichier au format EXCEL, doit avoir 1) tableau des résultats,
    # 2) les arguments et 3) la requête SQL.
    shinyFileSave(input, "sg1_save", roots = Volumes_path())  # bouton pour déterminer le répertoire
    sg1_file_save <- reactive({  # répertoire de sauvegarde à partir de input$sg1_save
      shinyFiles_directories(input$sg1_save, "save")
    })
    observeEvent(sg1_file_save(), {
      save_EXCEL_data_args_query(
        dt = sg1_requete_sql(),
        args_list = list(
          METHOD = "stat_gen1",
          DATE_DEBUT = sg1_find_date(input, "deb"),
          DATE_FIN = sg1_find_date(input, "fin"),
          TYPE_RX = input$sg1_type_Rx, CODE_RX = sg1_find_code(input),
          CODE_RX = sg1_find_code(input),
          CODE_SERV_FILTRE = input$sg1_code_serv_filter,
          CODE_SERV = adapt_code_serv(input$sg1_code_serv),
          CODE_LIST_FILTRE = input$sg1_code_list_filter,
          CODE_LIST = input$sg1_code_list
        ),
        query = stat_gen1_txt_query_1period(
          debut = sg1_find_date(input, "deb"), fin = sg1_find_date(input, "fin"),
          type_Rx = input$sg1_type_Rx, codes = sg1_find_code(input),
          code_serv = input$sg1_code_serv, code_serv_filtre = input$sg1_code_serv_filter,
          code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter
        ),
        save_pat = sg1_file_save()
      )
    })

    # Afficher code de la requête SQL généré par les arguments du formulaire
    output$sg1_code_req <- eventReactive(input$sg1_maj_req, {
      stat_gen1_txt_query_1period(
        debut = sg1_find_date(input, "deb")[1], fin = sg1_find_date(input, "fin")[1],
        type_Rx = input$sg1_type_Rx, codes = sort(sg1_find_code(input)),
        code_serv = adapt_code_serv(input$sg1_code_serv), code_serv_filtre = input$sg1_code_serv_filter,
        code_list = sort(input$sg1_code_list), code_list_filtre = input$sg1_code_list_filter
      )
    })

  }

# Application -------------------------------------------------------------

  shinyApp(ui, server)

}





























