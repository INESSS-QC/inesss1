#' Requêtes SQL
#'
#' Formulaire Shiny permettant d'exécuter une ou plusieurs requêtes.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles getVolumes shinyFileSave shinySaveButton
#' @importFrom fs path_home
#' @importFrom testthat capture_error
#' @importFrom stringr str_replace_all str_remove_all str_split
#' @export
formulaire <- function() {


# Table des matières - conception -----------------------------------------

  # UI
  #   Header
  #   Sidebar
  #     Connexion - conn
  #     Requête simple - simple_req
  #     Requete via EXCEL
  #   Body
  #     Connexion - conn
  #     Requête simple - simple_req
  #

# FCTS internal -----------------------------------------------------------


# Interface Utilisateur ---------------------------------------------------

  library(fs)
  library(shiny)
  library(shinydashboard)
  library(shinyFiles)
  library(testthat)

  ui <- dashboardPage(

    ### Header
    dashboardHeader(title = "Requêtes SQL"),

    ### Sidebar
    # Contient 3 onglets:
    #   - Connexion
    #   - Requete Simple
    #   - Requete via EXCEL
    dashboardSidebar(
      sidebarMenu(
        # Connexion SQL:
        # indique si la connexion est faite ou pas
        menuItem("Connexion", tabName = "conn"),
        # Requête simple:
        # utiliser les arguments du Shiny pour effectuer une extraction
        menuItem("Requête simple", tabName = "simple_req"),
        # Requête via EXCEL:
        # utiliser un fichier EXCEL pour effectuer une requete ou chaque onglet
        # du fichier correspond à un tableau, une extraction, une requete
        menuItem("Requêtes via EXCEL", tabName = "excel_req")
      )
    ),

    ### Body
    # Contient 3 onglets, les même que dans Sidebar
    dashboardBody(
      tabItems(
        # Connexion:
        # l'utilisateur entre ses informations (identifiant + mot de passe) et le
        # programme indique si la connexion est etablie
        tabItem(
          tabName = "conn",
          # Informations nécessaires à la connexion
          textInput("teradata_user", "Identifiant", value = ""),  # no identifiant
          passwordInput("teradata_pwd", "Mot de passe", value = ""),  # mot de passe
          # Établir la connexion
          actionButton("check_conn", "Connexion"),
          # Indiquer l'état de la connexion
          h5("État de la connexion :"),
          verbatimTextOutput("is_conn", placeholder = TRUE)
        ),

        # Requête simple
        # Utiliser les arguments shiny pour effectuer une requête
        tabItem(
          tabName = "simple_req",
          # 1ere ligne: Arguments de la requête
          fluidRow(
            # Afficher les arguments sur trois (3) colonnes -> 12/3 = 4 = width
            column(
              width = 4,
              # Indiquer le nom de période d'étude à analyser au total
              numericInput("nbr_dates", "Nombre de périodes d'étude",
                           value = 1, min = 1, max = 99),
              # Affiche le nombre de périodes d'étude indiquer dans input$nbr_dates
              uiOutput("input_mult_dates")
            ),

            column(
              width = 4,
              # Type de code à analyser - sélection de la variable d'analyse
              selectInput("typevar", "Type variable",
                          choices = c("DENOM", "DIN"),
                          selected = "DENOM"),
              # Codes d'analyse
              textAreaInput("codes", "Codes"),
              # Statistiques
              checkboxGroupInput(
                "stats", "Statistiques",
                choices = c("Coût" = "COUT", "Honoraire" = "HONORAIRE",
                            "Coût total" = "COUT_TOT",
                            "ID uniques" = "ID_UNIQUE", "Durée traitements" = "DUREE_TX",
                            "Rx uniques" = "RX_UNIQUE", "Rx nombre" = "RX_NBRE",
                            "Rx quantité" = "RX_QTE"),
                selected = c("ID_UNIQUE", "RX_NBRE",
                             "COUT", "HONORAIRE", "COUT_TOT",
                             "RX_QTE", "DUREE_TX")
              ),
              # Ajout Information
              checkboxGroupInput(
                "addinfo", "Ajout Information",
                choices = c("Nom", "DENOM", "Nom Service")
              )
            ),
            column(
              width = 4,
              # Grouper par
              checkboxGroupInput(
                "groupby", "Grouper par",
                choices = c("Année" = "ANNEE", "Trimestre",
                            "Codes" = "CODES", "Format",
                            "CodeService",
                            "TypeService", "Age", "CodeSelection", "ServiceInclu",
                            "Teneur"),
                selected = c("ANNEE", "CODES")
              ),
              # Exclusion Code Service
              textInput("excl_code_serv", "Exclusion Code Service",
                        value = "1"),
              # Filtre Categorie Liste
              textInput("filter_cat_list", "Filtre Catégorie Liste",
                        value = "")
            )
          ),
          # Bouton pour exécuter la requête SQL
          fluidRow(
            column(
              width = 4,
              actionButton("go_extract", "Exécuter Requête"), p()
            ),
          ),
          fluidRow(p()), # insérer un espace entre le bouton et le tableau qui s'affiche après
          # Afficher tout ce qui n'est pas un argument de requête:
          #   - Tableau des résultats
          #   - code SQL exécuté ou à exécuter pour la requête
          fluidRow(
            column(
              width = 12, align = "left",
              dataTableOutput("tab_simple_req"),
              shinySaveButton("save_simple", "Enregistrer requête", "Enregistrer sous...",
                              filetype = list(`Classeur Excel` = "xlsx"),
                              viewtype = "list"), p(),

              verbatimTextOutput("query_simple_req"),
              verbatimTextOutput("test_variables")  # afficher variables voir si code bon pour extraire les arguments
            )
          )
        ),
        # Requêtes via EXCEL
        tabItem(
          tabName = "excel_req",
          shinyFilesButton("select_xl_file", "Sélectionner fichier Excel",
                           "Sélectionner fichier Excel", multiple = FALSE,
                           viewtype = "detail"), p(),
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          actionButton("go_xl_extract", "Exécuter Requête(s)")
        )

      )

    )

  )


# Serveur -----------------------------------------------------------------


  server <- function(input, output, session) {

    ### Valeurs à initialiser
    Vals <- reactiveValues(
      conn = FALSE  # etat de la connexion SQL
    )
    # Répertoire possible pour les filesButton
    volumes <- c(`Par défaut` = path_home(),
                 R = R.home(),
                 getVolumes()())  # répertoires principaux (C:, E:, ...)

    ### Connexion SQL
    # Messages indiquant si la connexion est établie ou pas
    observeEvent(input$check_conn, {
      if (input$teradata_user == "" && input$teradata_pwd == "") {
        # Cas où aucune information n'est inscrite
        Vals$is_conn <- "***Veuillez inscire numéro d'identifiant et mot de passe"
        Vals$conn <- FALSE
      } else if (is.null(capture_error(sql_conn("PEI_PRD", input$teradata_user, input$teradata_pwd)))) {
        # La connexion est établie -> aucune erreur
        Vals$is_conn <- "CONNEXION ÉTABLIE"
        Vals$conn <- TRUE
      } else {
        # Malgré les infos inscrites, la connexion ne s'est pas effectuée
        Vals$is_conn <- "***Vérifier identifiant et mot de passe."
        Vals$conn <- FALSE
      }
    })
    output$is_conn <- renderText(Vals$is_conn)  # afficher message de la connexion

    ### Requête simple
    # Périodes d'analyse à afficher selon choix utilisateur
    output$input_mult_dates <- renderUI({
      dates_inputs <- vector("list", length = input$nbr_dates)  # nombre d'élément à créer
      for (i in 1:input$nbr_dates) {
        # Conserver les valeurs précédemment inscrites en vérifiant si
        # input$datesX existe ou pas
        if (is.null(input[[paste0("dates",i)]])) {
          dates_inputs[[i]] <- dateRangeInput(  # cas où n'existe pas
            inputId = paste0("dates",i), label = paste0("Période ",i),
            separator = " au ")
        } else {
          dates_inputs[[i]] <- dateRangeInput(  # cas où existait: conserver valeur inscrites
            inputId = paste0("dates",i), label = paste0("Période ",i),
            start = input[[paste0("dates",i)]][1],
            end = input[[paste0("dates",i)]][2],
            separator = " au ")
        }
      }
      return(tagList(dates_inputs))
    })
    # Répertoire de sauvegarde
    shinyFileSave(input, "save_simple", roots = volumes, session = session)
    simple_dir_save <- reactive({
      if (is.integer(input$save_simple)) {
        return(NULL)
      } else {
        return(parseSavePath(volumes, input$save_simple))
      }
    })
    # Tableau à afficher
    output$tab_simple_req <- renderDataTable({
      data.frame(
        DateDebut = c("2019-01-01", "2020-01-01"),
        DateFin = c("2019-12-31", "2020-12-31"),
        An = c(2019, 2020),
        TypeVar = "DIN",
        Code = 123,
        Cout = c(456.12, 789.12),
        Honoraire = c(753.78, 951.23),
        CoutTotal = c(1209.90, 1740.35)
      )
    })
    # SQL query à afficher
    output$query_simple_req <- renderText({
      txt <- stat_gen1_txt_query_1period(DatesDebut()[1], DatesFin()[1],
                                         TypeVar(), Codes(),
                                         Statistiques(), GroupBy(),
                                         ExcluCodeServ(), CategorieListe())
      return(txt)
    })

    # Extraire les valeurs des arguments
    DatesDebut <- reactive({
      vec <- c()
      for (i in 1:input$nbr_dates) {
        vec <- c(vec, as.character(input[[paste0("dates",i)]][1]))
      }
      return(vec)
    })
    DatesFin <- reactive({
      vec <- c()
      for (i in 1:input$nbr_dates) {
        vec <- c(vec, as.character(input[[paste0("dates",i)]][2]))
      }
      return(vec)
    })
    TypeVar <- reactive({ input$typevar })
    Codes <- reactive({
      vec <- as.character(input$codes)
      if (vec != "") {
        # Séparer tous les éléments par un ";"
        vec <- str_replace_all(vec, ",", ";")  # convertir ',' en ';'
        vec <- str_replace_all(vec, "\n", ";")  # convertir '\n' en ';'
        vec <- str_remove_all(vec, " ")  # supprimer espaces
        vec <- str_split(vec, ";")[[1]]  # séparer les codes et garder vecteur
        vec <- vec[vec != ""]  # supprimer les éléments vides
        return(vec)
      } else {
        return(NULL)
      }
    })
    Statistiques <- reactive({ input$stats })
    GroupBy <- reactive({ input$groupby })
    ExcluCodeServ <- reactive({
      vec <- as.character(input$excl_code_serv)
      if (vec != "") {
        # Séparer tous les éléments par un ";"
        vec <- str_replace_all(vec, ",", ";")  # convertir ',' en ';'
        vec <- str_replace_all(vec, "\n", ";")  # convertir '\n' en ';'
        vec <- str_remove_all(vec, " ")  # supprimer espaces
        vec <- str_split(vec, ";")[[1]]  # séparer les codes et garder vecteur
        vec <- vec[vec != ""]  # supprimer les éléments vides
        return(unique(vec))
      } else {
        return(NULL)
      }
    })
    CategorieListe <- reactive({
      vec <- as.character(input$filter_cat_list)
      if (vec != "") {
        # Séparer tous les éléments par un ";"
        vec <- str_replace_all(vec, ",", ";")  # convertir ',' en ';'
        vec <- str_replace_all(vec, "\n", ";")  # convertir '\n' en ';'
        vec <- str_remove_all(vec, " ")  # supprimer espaces
        vec <- str_split(vec, ";")[[1]]  # séparer les codes et garder vecteur
        vec <- vec[vec != ""]  # supprimer les éléments vides
        return(unique(vec))
      } else {
        return(NULL)
      }
    })


    ### À SUPPRIMER - VÉRIFIER LA VALEUR DES COMMANDES
    output$test_variables <- renderPrint({
      list(DatesDebut = DatesDebut(),
           DatesFin = DatesFin(),
           TypeVar = TypeVar(),
           Codes = Codes(),
           Statistiques = Statistiques(),
           GroupBy = GroupBy(),
           ExcluCodeServ = ExcluCodeServ(),
           CategorieListe = CategorieListe())
    })


    ### Requête via EXCEL
    # Sélection du fichier Excel
    shinyFileChoose(input, "select_xl_file", roots = volumes, session = session)
    select_xl_file <- reactive({
      if (is.integer(input$select_xl_file)) {
        return(NULL)
      } else {
        parseFilePaths(volumes, input$select_xl_file)
      }
    })
    # Indiquer message d'erreur
    output$xl_errors_msg <- renderText({paste0(
      "Nom Onglet 1:", nl(),
      " - DateDebut n'est pas au format AAAA-MM-JJ.", nl(),
      " - DateFin n'est pas au format AAAA-MM-JJ.", nl(),
      "---------------------------------------------", nl(),
      "Nom Onglet 2:", nl(),
      " - Erreur 1", nl()
    )})

  }


# Exécuter Application ----------------------------------------------------

  shinyApp(ui, server)

}
