#' formulaire
#'
#' @import data.table
#' @import shiny
#' @import stringr
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#' @import rmarkdown
#' @import shinyFiles
#' @export
formulaire <- function() {

  ########################################################################### #
  # library(inesss)
  # library(data.table)
  # library(shiny)
  # library(stringr)
  # library(plotly)
  # library(ggplot2)
  # library(rmarkdown)
  # library(shinyFiles)
  ########################################################################### #

  ui <- fluidPage(

    sidebarLayout(

      sidebarPanel(

        # Titre
        titlePanel("Formulaire"),

        # User + password
        textInput("sql_user", "Utilisateur :", value = "ms..."),

        # Direction (selection)
        selectInput("select_direction", "Direction :", choices = names(list_fcts),
                    selected = names(list_fcts)[1]),

        # Fonctions selon directions (selection)
        selectInput("select_fct", "Fonctions :", choices = list_fcts[["Direction1"]],
                    selected = list_fcts[["Direction1"]][1])

      ),

      mainPanel(

        tabsetPanel(

          tabPanel(
            title = "Arguments", h1(" "),
            fileInput("arg_xlsx", "Fichier EXCEL :"),
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
            textInput("save_name", "Nom de sauvegarde", paste0("Test-Exemple_",Sys.Date(),".pdf")),
            shinyFilesButton("gabarit", "Gabarit :", "Choisir un gabarit", FALSE),
            textOutput("gabarit"),
            shinyDirButton("dir", "Répertoire sauvegarde", "Choisir répertoire"),
            textOutput("dir"),
            actionButton("save_plot", "Créer PDF"),
          )

        )

      )

    )

  )

  server <- function(input, output, session) {

    # Updates selectInputs
    observe({
      codes <- sort(unique(unlist(list_fcts)))
      x <- input$select_direction
      if (is.null(x))
        x <- character(0)
      updateSelectInput(
        session, "select_fct",
        choices = codes[codes %in% list_fcts[[x]]],
        selected = codes[codes %in% list_fcts[[x]]][1]
      )
    })

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
    pdf_table <- reactive({
      dt <- table()
      dt <- dt[code %in% input$plot_codes]
      ggplot(dt, aes(an, cout, colour = factor(code))) +
        geom_point() +
        geom_line()
    })


    # ShinyFiles
    shinyFileChoose(
      input, "gabarit",
      roots = c(`E:/Documents/GitHub/INESSS-QC_inesss` = "E:/Documents/GitHub/INESSS-QC_inesss")
    )
    gabarit <- reactive({
      if (is.null(names(input$gabarit))) {
        return(NULL)
      } else {
        file_d <- input$gabarit$files[[1]][-1]
        txt <- paste0(input$gabarit$root, "/")
        if (length(file_d)) {
          for (i in 1:length(file_d)) {
            txt <- paste0(txt, file_d[[i]])
            if (i != length(file_d))
              txt <- paste0(txt, "/")
          }
        }
        return(txt)
      }
    })
    output$gabarit <- renderText( gabarit() )

    shinyDirChoose(
      input, 'dir',
      roots = c(`E:/Documents/GitHub/INESSS-QC_inesss` = "E:/Documents/GitHub/INESSS-QC_inesss")
    )
    dir <- reactive({
      if (is.null(names(input$dir))) {
        return(NULL)
      } else {
        dir_d <- input$dir$path[-1]
        txt <- paste0(input$dir$root, "/")
        if (length(dir_d)) {
          for (i in 1:length(dir_d)) {
            txt <- paste0(txt, dir_d[[i]])
            if (i != length(dir_d))
              txt < -paste0(txt, "/")
          }
        }
        return(txt)
      }
    })
    output$dir <- renderText( dir() )

    observeEvent(input$save_plot, {
      render(
        input = gabarit(),
        output_file = input$save_name,
        output_dir = dir(),
        params = list(shiny_plot = pdf_table())
      )
    })

  }

  shinyApp(ui, server)

}
