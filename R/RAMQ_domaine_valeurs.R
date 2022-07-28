#' Domaine de valeur RAMQ
#'
#' Domaine de valeurs pour différentes table de la RAMQ
#'
#' @encoding UTF-8
#' @keywords internal
#' @import data.table
#' @import shiny
#' @import shinydashboard
#' @return Application shiny
RAMQ_domaine_valeurs <- function() {


# UI ------------------------------------------------------------------------------------------

  ui <- dashboardPage(

    #* Header Section ####
    dashboardHeader(
      title = paste0("version ", as.character(packageVersion("inesss")))
    ),

    #* Sidebar Section ####
    dashboardSidebar(
      width = 260,
      sidebarMenu(
        div(style = "margin-top:10px"),

        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "tab_I_APME_DEM_AUTOR_CRITR_ETEN_CM")
      )
    ),

    #* Body Section ####
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "tab_I_APME_DEM_AUTOR_CRITR_ETEN_CM",
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeurs",
              textInput("id1", "Test1")
            ),
            tabPanel(
              title = "Documentation",
              textInput("id2", "Test2")
            )
          )
        )
      )
    )

  )


# SERVER --------------------------------------------------------------------------------------

  server <- function(input, output, session) {

  }


# APP -----------------------------------------------------------------------------------------

  shinyApp(ui, server)

}
