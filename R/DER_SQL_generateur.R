#' SQL générateur
#'
#' Générateur de requêtes SQL pour la DER.
#'
#' L'utilisateur sélectionne une méthode et la requête SQL est générée. Il peut ensuite modifier la requête directement ou simplement inscrire les valeurs des variables dans l'espace dédié aux arguments.
#'
#' @return `shiny`
#' @encoding UTF-8
#' @export
DER_SQL_generateur <- function() {

  library(shiny)
  library(shinyAce)

  UI <- pageWithSidebar(
    headerPanel("DEV - SQL générateur"),
    sidebarPanel(
      width = 4,
      fluidRow(
        column(
          width = 6,
          actionButton("ajouterMethod", "Ajouter")
        ),
        column(
          width = 6,
          actionButton("reset", "Réinitialiser")
        )
      ),
      selectInput("method", "Méthode", choices = "Statistique Générale"),
      selectInput("sousMethod", "Sous-Méthode", choices = c("DENOM", "DIN")),
      uiOutput("methodVars")
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Requêtes SQL",
          aceEditor(
            outputId = "sqlEditor",
            value = "",
            mode = "sql",
            theme = "sqlserver",
            height = "600px",
            showPrintMargin = FALSE
          )
        ),
        tabPanel(
          title = "Documentation",
          p("En développement")
        )
      )
    )
  )

  SERVER <- function(input, output, session) {

    observeEvent(input$ajouterMethod, {
      sql_code <- input$sqlEditor
      add_code <- query.statistiques_generales.method(
        debut = input$debut,
        fin = input$fin,
        type_Rx = input$sousMethod,
        codes = { if (input$codes == "") NULL else input$codes },
        code_serv = { if (input$codeServ == "") NULL else input$codeServ },
        code_serv_filtre = input$codeServFiltre,
        group_by = { if (input$groupby == "") NULL else input$groupby},
        cohort = input$cohort
      )
      updateAceEditor(session, "sqlEditor", value = paste0(sql_code, "\n", add_code))
    })

    observeEvent(input$reset, {
      updateAceEditor(session, "sqlEditor", value = "")
    })

    output$methodVars <- renderUI({
      # Statistiques générales ####
      if (input$method == "Statistique Générale") {
        ## DENOM ####
        if (input$sousMethod == "DENOM") {
          return(tagList(
            fluidRow(
              column(
                width = 6,
                textInput("debut", "Date Début", value = "2022-01-01")
              ),
              column(
                width = 6,
                textInput("fin", "Date Fin", value = "2022-12-31")
              )
            ),
            fluidRow(
              column(
                width = 12,
                textInput("codes", paste0("Codes ", input$sousMethod), value = "48135")
              )
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  "codeServ", "Codes de service",
                  choices = c("1", "AD"), selected = "1",
                  multiple = TRUE
                )
              ),
              column(
                width = 6,
                selectInput(
                  "codeServFiltre", "Filtre Codes Service",
                  choices = c("Exclusion", "Inclusion"), selected = "Exclusion"
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                selectInput("groupby", "Grouper Par",
                            choices = c("DENOM", "AGE"),
                            selected = "DENOM",
                            multiple = TRUE)
              )
            ),
            fluidRow(
              column(
                width = 12,
                checkboxInput("cohort", "Cohorte")
              )
            )
          ))
        }
      }
    })

  }

  shinyApp(UI, SERVER)

}
