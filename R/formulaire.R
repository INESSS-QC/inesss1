#' Requêtes SQL
#'
#' Formulaire Shiny permettant d'exécuter une ou plusieurs requêtes.
#'
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles shinyDirButton
#' @importFrom testthat capture_error
#' @export
formulaire <- function() {

# Interface Utilisateur ---------------------------------------------------

  library(shiny)
  library(shinydashboard)
  library(shinyFiles)
  library(testthat)

  UI <- dashboardPage(

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
                           value = 1, min = 1),
              # Affiche le nombre de périodes d'étude indiquer dans input$nbr_dates
              uiOutput("input_mult_dates")
            ),

            column(
              width = 4,
              # Type de code à analyser - sélection de la variable d'analyse
              selectInput("typevar", "Type variable",
                          choices = c("DC", "DIN", "AHFS"),
                          selected = "DC"),
              # Codes d'analyse
              textAreaInput("codes", "Codes"),
              # Statistiques
              checkboxGroupInput(
                "stats", "Statistiques",
                choices = c("Cout", "CoutTotal", "DureeTx", "Honoraire",
                            "nRx", "QteRx", "uniqID", "uniqRx")
              ),
              # Ajout Information
              checkboxGroupInput(
                "addinfo", "Ajout Information",
                choices = c("Nom", "Type Variable", "Nom Service")
              )
            ),
            column(
              width = 4,
              # Grouper par
              checkboxGroupInput(
                "groupby", "Grouper par",
                choices = c("An", "Trimestre", "Code", "Format", "CodeService",
                            "TypeService", "Age", "CodeSelection", "ServiceInclu",
                            "Teneur"),
                selected = c("An", "Code")
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
              actionButton("go_extract", "Exécuter Requête")
            )
          ),
          fluidRow(p()), # insérer un espace entre le bouton et le tableau qui s'affiche après
          # Afficher tout ce qui n'est pas un argument de requête:
          #   - Tableau des résultats
          #   - code SQL exécuté ou à exécuter pour la requête
          fluidRow(
            column(
              width = 12, align = "left",
              dataTableOutput("tab_simple_req"),
              verbatimTextOutput("sql_query"),
              verbatimTextOutput("query_simple_req")
            )
          )
        ),
        # Requêtes via EXCEL
        tabItem(
          tabName = "excel_req",
          fileInput("xl_file", "Sélectionner fichier EXCEL",
                    buttonLabel = "Sélectionner",
                    placeholder = "Aucun fichier sélectionné"),
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          actionButton("go_xl_extract", "Exécuter extraction")
        )

      )

    )

  )


# Serveur -----------------------------------------------------------------


  SERVER <- function(input, output, session) {

    ### Valeurs à initialiser
    Vals <- reactiveValues(
      conn = FALSE  # etat de la connexion SQL
    )

    ### Connexion SQL
    # Messages indiquant si la connexion est établie ou pas
    observeEvent(input$check_conn, {
      if (input$teradata_user == "" && input$teradata_pwd == "") {
        # Cas où aucune information n'est inscrite
        Vals$is_conn <- "***Veuillez inscire votre identifiant et votre mot de passe"
        Vals$conn <- FALSE
      } else if (is.null(capture_error(sql_conn("PEI_PRD", input$teradata_user, input$teradata_pwd)))) {
        # La connexion est établie -> aucune erreur
        Vals$is_conn <- "Connexion établie"
        Vals$conn <- TRUE
      } else {
        # Malgré les infos inscrites, la connexion ne s'est pas effectuée
        Vals$is_conn <- "***Vérifier numéro d'identifiant et mot de passe."
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
    output$query_simple_req <- renderText({paste0(
      "SELECT", nl(),
      indent(), "Extract (YEAR From SMED_DAT_SERV) AS Annee,", nl(),
      indent(), "SMED_COD_DENOM_COMNE AS DC,", nl(),
      indent(), "Sum (SMED_MNT_AUTOR_MED) AS Cout,", nl(),
      indent(), "Sum (SMED_MNT_AUTOR_FRAIS_SERV) AS Honor,", nl(),
      indent(), "Sum (SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) AS CoutTot", nl(),
      "FROM Prod.V_DEM_PAIMT_MED_CM", nl(),
      "WHERE", nl(),
      indent(), "SMED_DAT_SERV BETWEEN '2019-01-01' AND '2019-12-31'", nl(),
      indent(), "AND SMED_COD_DENOM_COMNE IN ('47092','47135')", nl(),
      indent(), "AND (SMED_COD_SERV_1 NOT = '1' OR SMED_COD_SERV_1 IS NULL)", nl(),
      "GROUP BY Annee, DC", nl(),
      "ORDER BY Annee, DC;"
    )})


    ### Requête via EXCEL
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

  shinyApp(UI, SERVER)

}
