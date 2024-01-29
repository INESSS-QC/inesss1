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
      actionButton("ajouterMethod", "Ajouter"),
      selectInput("method", "Méthode", choices = "Statistique Générale"),
      selectInput("sousMethod", "Sous-Méthode", choices = c("DENOM", "DIN")),
      uiOutput("methodVars")
    ),
    mainPanel(
      width = 8,
      aceEditor(
        outputId = "sqlEditor",
        value = "",
        mode = "sql",
        theme = "sqlserver",
        height = "600px",
        showPrintMargin = FALSE
      )
    )
  )

  SERVER <- function(input, output, session) {

    observeEvent(input$ajouterMethod, {

      sql_code <- input$sqlEditor
      add_code <- query.statistiques_generales.method(
        debut, fin,
        type_Rx, codes = NULL,
        code_serv = c('1', 'AD'), code_serv_filtre = 'Exclusion',
        group_by = NULL,
        cohort = FALSE
      )
      updateAceEditor(session, "sqlEditor", value = paste0(sql_code, "\n", add_code))
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
                textInput("debut", "Date Début")
              ),
              column(
                width = 6,
                textInput("fin", "Date Fin")
              )
            )
          ))
        }
      }
    })

  }

  shinyApp(UI, SERVER)

}
