#' Formulaire
#'
#' @import data.table
#' @import shiny
#' @importFrom stringr str_split str_remove_all
#' @import plotly
#' @import ggplot2
#' @import rmarkdown
#' @import shinyFiles
#' @export
formulaire_2_delete <- function() {

  library(inesss)
  library(data.table)
  library(shiny)
  library(stringr)
  library(plotly)
  library(ggplot2)
  library(rmarkdown)
  library(shinyFiles)

# UI ----------------------------------------------------------------------

ui <- fluidPage(

  sidebarLayout(

    sidebarPanel(

      # Titre
      titlePanel("Formulaire"),

      # User + Password
      textInput("sql_user", "Utilisateur : ", value = "ms..."),
      passwordInput("sql_pwd", "Mot de passe :"),

      # Selection de la direction
      selectInput("select_direction", "Direction :", choices = names(list_fcts)),

      # Selection de la fonction selon les directions
      conditionalPanel(
        condition = "input.select_direction == 'Direction1'",
        selectInput("select_fct", "Fonctions :",
                    choices = list_fcts[["Direction1"]])
      ),
      conditionalPanel(
        condition = "input.select_direction == 'Direction2'",
        selectInput("select_fct", "Fonctions :",
                    choices = list_fcts[["Direction2"]])
      ),
      conditionalPanel(
        condition = "input.select_direction == 'Direction3'",
        selectInput("select_fct", "Fonctions :",
                    choices = list_fcts[["Direction3"]])
      )

    ),

    mainPanel(

      tabsetPanel(

        # TESTS
        tabPanel(
          title = "TESTS", h1(" "),
          verbatimTextOutput("dir_gabarit"),
          verbatimTextOutput("dir")
        ),

        # Page des arguments
        tabPanel(
          title = "Arguments", h1(" "),
          fileInput("arg_file", "Fichier EXCEL :"),
          selectInput(
            "select_an", "Années :",
            choices = sort(unique(data_ex1$an)),
            multiple = TRUE
          ),
          selectInput(  # sélectionner type de variable
            "select_var", "Variable :",
            choices = c("DIN", "DC", "AHFS")
          ),
          textAreaInput(  # inscrire codes d'analyse
            "select_codes", "Codes :", rows = 7
          ),
          actionButton("go", "Exécuter filtre")  # effectuer filtre du data
        ),

        # Page du data filtre
        tabPanel(
          title = "Data", h1(" "),
          dataTableOutput("table")
        ),

        # Evolution des couts
        tabPanel(
          title = "Coûts", h1(" "),
          selectInput(
            "plot_codes", "Code(s) à afficher",
            choices = "", multiple = TRUE
          ),
          plotlyOutput("plot"),
          textOutput("dir", inline = TRUE),
          textInput("save_name", "Nom de sauvegarde", paste0("Test-Exemple_",Sys.Date(),".pdf")),
          actionButton("save_plot", "Créer PDF")
        )

      )

    )

  )

)

server <- function(input, output, session) {

  ########################################################################### #
  ### TESTS ###

  ########################################################################### #

  # Data filtre selon arguments
  table <- eventReactive(input$go, {
    dt <- data_ex1[  # Filtrer data selon inputs
      type_var == input$select_var &
        an %in% input$select_an &
        code %in% str_split(str_remove_all(input$select_codes, " "), ",")[[1]]
    ]
    dt <- dt[  # statistiques
      , .(IDunique = uniqueN(id),
          cout = sum(cout),
          honor = sum(honor),
          tot = sum(tot)),
      keyby = .(an, type_var, code)
    ]
    return(dt)
  })
  output$table <-   # afficher le data filtre
    renderDataTable({ table() })


  # Evolution des coûts
  observe({
    codes <- 1:100
    x <- input$select_codes
    if (is.null(x))
      x <- character(0)
    updateSelectInput(
      session, "plot_codes",
      choices = codes[codes %in% unique(table()$code)],
      selected = codes[codes %in% unique(table()$code)[1]]
    )
  })

  output$plot <- renderPlotly({  # creation graphique des couts
    validate(
      need(is.vector(input$plot_codes), "Sélectionner au moins un code à afficher.")
    )
    ggplotly({
      dt <- table()
      dt <- dt[code %in% input$plot_codes]
      ggplot(dt, aes(an, cout, colour = factor(code))) +
        geom_point() +
        geom_line()
    })
  })
  plot_pdf <- reactive({
    dt <- table()
    dt <- dt[code %in% input$plot_codes]

    ggplot(dt, aes(an, cout, colour = factor(code))) +
      geom_point() +
      geom_line()
  })

}


shinyApp(ui, server)


}

