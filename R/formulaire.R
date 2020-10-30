#' Formulaire Shiny
#'
#' @import data.table
#' @importFrom fs path_home
#' @importFrom odbc dbConnect odbc
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles shinyFilesButton shinyFileChoose shinyFileSave shinySaveButton parseFilePaths parseSavePath
#' @importFrom stringr str_split
#' @export
formulaire <- function() {

  library(data.table)
  library(fs)
  library(odbc)
  library(shiny)
  library(shinydashboard)
  library(shinyFiles)
  library(stringr)
  library(writexl)

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
          h5("État de la connexion :"),
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
          actionButton("go_xl_extract", "Exécuter requête(s)")
          # ---------------------------------------------------- -
          # --- À FAIRE ---
          # # Inscrire le ou les courriels à envoyer les résultats
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
    sg1_requete_sql <- eventReactive(input$sg1_go_extract, {
      data.table(DATE_DEBUT = "2018-01-01", DATE_FIN = "2018-12-31",
                 DENOM = c(123, 789), MNT_COUT = c(45612.56, 956412.45),
                 MNT_SERV = c(88512.56, 984512.12), MNT_TOT = c(45678.12, 451236.12))
    })
    # Afficher le tableau demandé
    output$sg1_table_req <- renderDataTable({ sg1_requete_sql() })

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
        debut = sg1_find_date(input, "deb"), fin = sg1_find_date(input, "fin"),
        type_Rx = input$sg1_type_Rx, codes = sg1_find_code(input),
        code_serv = input$sg1_code_serv, code_serv_filtre = input$sg1_code_serv_filter,
        code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter
      )
    })

  }

# Application -------------------------------------------------------------

  shinyApp(ui, server)

}





























