#' Démarrer DER SQL Générateur
#' @encoding UTF-8
#' @keywords internal
DER_SQL_generateur.addins <- function() {
  inesss::DER_SQL_generateur()
}

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
      add_code <- stat_generales_1(
        fin = { if (input$fin == "") "<Insérer Valeur>" else input$fin },
        typeRx = input$sousMethod,
        codesRx = { if (input$codes == "") "'<Insérer Valeur(s)>'" else input$codes },
        catg_liste_med = { if (length(input$catg_liste_med)) input$catg_liste_med else rep("'<Insérer Valeur>'", 2) },
        code_serv = { if (length(input$codeServ)) input$codeServ else "'<Insérer Valeur(s)'" },
        code_serv_filtre = input$codeServFiltre,
        grp_age = { if (input$grp_age == "NULL") NULL else input$grp_age }
      )
      if (input$sqlEditor == "") {
        updateAceEditor(session, "sqlEditor", value = add_code)
      } else {
        updateAceEditor(session, "sqlEditor", value = paste0(sql_code, "\n", add_code))
      }
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
                textInput("fin", "Date Fin", value = "2022-12-31")
              )
            ),
            fluidRow(
              column(
                width = 12,
                textInput("codes", paste0("Codes ", input$sousMethod))
              )
            ),
            fluidRow(
              column(
                width = 12,
                selectInput(
                  "catg_liste_med", "Catégorie de liste de médicament",
                  choices = inesss::V_DES_COD[TYPE_CODE == "COD_CATG_LISTE_MED"]$CODE,
                  selected = c("03", "40", "41"), multiple = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  "codeServ", "Codes de service",
                  choices = unique(inesss::V_PARAM_SERV_MED$COD_SERV),
                  selected = "1", multiple = TRUE
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
                selectInput(
                  "grp_age", "Groupe Âge",
                  choices = list(`Mineur-Majeur` = "Mineur-Majeur", `10ans` = 10, `5ans` = 5, Aucun = "NULL"),
                  selected = "Mineur-Majeur"
                )
              )
            )
          ))
        }
      }
    })

  }

  shinyApp(UI, SERVER)

}
