#' Domaine de valeurs
#'
#' Domaine de valeurs de différentes bases de données disponibles dans le domaine de la santé, principalement à la RAMQ.
#'
#' @encoding UTF-8
#' @import data.table
#' @import shiny
#' @import shinydashboard
domaine_valeurs <- function() {


# USER INTERFACE ----------------------------------------------------------

  ui <- dashboardPage(

    # * Header section ####
    dashboardHeader(
      title = paste0("version ", as.character(packageVersion("inesss")))
    ),

    # * Sidebar section ####
    dashboardSidebar(
      width = 260,  # ajuster l'espace nécessaire selon le nom de la base de données

      sidebarMenu(

        # div(style = "margin-top:10px"),  # baisser le premier élément
        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM")

      )

    ),

    # * Body section ####
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM",
          fluidRow(
            h4(  # Dernière mise à jour de la table
              HTML("&nbsp;&nbsp;&nbsp;"),
              "Mise à jour de la table : ",
              attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Base de données",
              fluidRow(
                column(
                  width = 4,
                  div(style = "margin-top:10px"),
                  selectInput(
                    inputId = "I_APME_DEM_AUTOR_CRITR_ETEN_CM__data",
                    label = "Élément",
                    choices = names(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)
                  )
                )
              )
            ),
            tabPanel(
              title = "Fiche technique",
              h3("I_APME_DEM_AUTOR_CRITR_ETEN_CM"),
              h4("Descriptif"),
              p("Description de la table blah blah blah")
            )
          )
        )
      )
    )
  )


# SERVER ------------------------------------------------------------------

  server <- function(input, output, session) {


    # * General -----------------------------------------------------------------

    ### Fermer l'application lorsque la fenêtre se ferme
    session$onSessionEnded(function() {stopApp()})


    # * I_APME_DEM_AUTOR_CRITR_ETEN_CM ------------------------------------------

    ### Paramètres à afficher


  }


# APPLICATION -------------------------------------------------------------

  shinyApp(ui, server)

}
