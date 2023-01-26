#' Générateur de code
#'
#' @encoding UTF-8
#' @import shiny
#' @import shinydashboard
#' @export
SQL_code_generateur <- function() {

  prismCodeBlock <- function(code) {
    tagList(
      HTML(code),
      tags$script("Prism.highlightAll()")
    )
  }
  prismDependencies <- tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
  )
  prismSqlDependency <- tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-sql.min.js")
  )

  UI <- fluidPage(
    prismDependencies,
    prismSqlDependency,

    tabsetPanel(
      type = "tabs",
      tabPanel(
        title = "Générateur de code",
        fluidRow(
          column(
            width = 5,
            selectInput(
              "methodInsert", label = "Méthode à insérer",
              choices = list(
                "Statistiques générales (1)" = "stat_gen1",
                "Naïfs / Switch" = "naifs_switch1"
              )
            ),
            actionButton("insertTextMethod", label = "Insérer méthode")
          )
        ),
        fluidRow(
          textAreaInput(
            "codeGenerator", label = "Code SQL",
            value = "",
            width = "100%", rows = 80
          )
        )
      ),
      tabPanel(
        title = "Code Visualisateur",
        htmlOutput("sqlCode")
      )
    )
  )

  SERVER <- function(input, output, session) {

    observeEvent(input$insertTextMethod, {
      if (input$codeGenerator == "") {
        text_init <- ""
      } else {
        text_init <- paste0(input$codeGenerator, rep("\n", 4))
      }
      if (!is.null(input$methodInsert)) {
        if (input$methodInsert == "stat_gen1") {
          updateTextAreaInput(
            session, "codeGenerator",
            value = paste0(
              text_init,
              query_stat_gen1(
                debut = "2020-01-01", fin = "2020-12-31",
                type_Rx = "DENOM", codes = c(39, 48135),
                group_by = "DENOM"
              )
            )
          )
        }
      }
    }, ignoreInit = TRUE)

    output$sqlCode <- renderUI({
      prismCodeBlock(paste0(
        "<pre><code class='language-sql'>",
        input$codeGenerator,
        "</code></pre>"
      ))
    })

  }

  shinyApp(UI, SERVER)

}
SQL_code_generateur()
