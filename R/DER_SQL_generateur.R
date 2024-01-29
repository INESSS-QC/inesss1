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
      actionButton("ajouterMethod", "Ajouter"),
      selectInput("method", "Méthode", choices = "Statistique Générale"),
      selectInput("sousMethod", "Sous-Méthode", choices = c("Denom", "Din"))
    ),
    mainPanel(
      aceEditor(
        outputId = "sqlEditor",
        value = "select ValeurInitiale from dual;",
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

      if (input$sousMethod == "Denom") {
        add_code <- paste0(
          "select *\n",
          "from v_dem_paimt_med_cm\n",
          "where smed_dat_serv between '2020-01-01' and '2020-12-31'\n",
          "    and smed_cod_denom_comne = 123456\n",
          "order by smed_cod_denom_comne, smed_dat_serv;"
        )
      } else if (input$sousMethod == "Din") {
        add_code <- paste0(
          "select *\n",
          "from v_dem_paimt_med_cm\n",
          "where smed_dat_serv between '2020-01-01' and '2020-12-31'\n",
          "    and smed_cod_din = 123456\n",
          "order by smed_cod_din, smed_dat_serv;"
        )
      }
      updateAceEditor(session, "sqlEditor", value = paste0(sql_code, "\n", add_code))
    })

  }

  shinyApp(UI, SERVER)

}
