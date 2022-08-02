#' Domaine de valeurs
#'
#' Domaine de valeurs de différentes bases de données disponibles dans le domaine de la santé, principalement à la RAMQ.
#'
#' @encoding UTF-8
#' @import data.table
#' @import shiny
#' @import shinydashboard
domaine_valeurs <- function() {


# FONCTIONS INTERNES ------------------------------------------------------

  button_go_style <- function() {
    ### Couleur et format du bouton qui fait apparaître les tableaux

    return(paste0(
      "color: #ffffff;",
      "background-color: #006600;",
      "border-color: #000000;"
    ))
  }
  button_reset_style <- function() {
    ### Couleur et format du bouton qui réinitialise les arguments

    return(paste0(
      "color: #ffffff;",
      "background-color: #990000;",
      "border-color: #000000;"
    ))
  }
  download_data <- function(input, datasave, data, subdata = NULL) {
    ### Télécharge sur l'ordinateur le data affiché à l'écran
    ### @param datasave Tableau à enregistrer.
    ### @param data Nom du dataset, par exemple I_APME_DEM_AUTOR_CRITR_ETEN_CM
    ### @param subdata Nom du sous data qui est inclut, selon le cas, sous le data. Par exemple DES_COURT_INDCN_RECNU.

    if (is.null(subdata)) {

    } else {
      return(downloadHandler(
        filename = function() {
          paste0(
            input[[paste0(data,"__",subdata,"__savename")]],
            ".",
            input[[paste0(data,"__",subdata,"__saveext")]]
          )
        },
        content = function(file) {
          if (input[[paste0(data,"__",subdata,"__saveext")]] == "xlsx") {
            writexl::write_xlsx(datasave, file)
          } else if (input[[paste0(data,"__",subdata,"__saveext")]] == "csv") {
            write.csv2(datasave, file)
          }
        }
      ))
    }
  }
  header_MaJ_datas <- function(date_MaJ) {
    ### Indique la date à laquelle la table a été mise à jour : "Actualisé le JJ MM YYYY"

    # Mois en caractères
    if (lubridate::month(date_MaJ) == 1) {
      mois <- "janvier"
    } else if (lubridate::month(date_MaJ) == 2) {
      mois <- "février"
    } else if (lubridate::month(date_MaJ) == 3) {
      mois <- "mars"
    } else if (lubridate::month(date_MaJ) == 4) {
      mois <- "avril"
    } else if (lubridate::month(date_MaJ) == 5) {
      mois <- "mai"
    } else if (lubridate::month(date_MaJ) == 6) {
      mois <- "juin"
    } else if (lubridate::month(date_MaJ) == 7) {
      mois <- "juillet"
    } else if (lubridate::month(date_MaJ) == 8) {
      mois <- "août"
    } else if (lubridate::month(date_MaJ) == 9) {
      mois <- "septembre"
    } else if (lubridate::month(date_MaJ) == 10) {
      mois <- "octobre"
    } else if (lubridate::month(date_MaJ) == 11) {
      mois <- "novembre"
    } else if (lubridate::month(date_MaJ) == 12) {
      mois <- "décembre"
    } else {
      stop("Le mois de 'date_MaJ' est une valeur non permise.")
    }
    txt_date <- paste(lubridate::day(date_MaJ), mois, lubridate::year(date_MaJ))  # JJ MM AAAA

    return(tagList(
      h4(  # header size 4
        HTML("&nbsp;&nbsp;&nbsp;"),  # espaces
        "Actualisé le ",txt_date  # Actualisé le JJ MM AAAA
      )
    ))
  }
  mois_debut_fct <- function() {
    ### Calcul le mois de début pour les valeurs initiales des tables lors de l'ouverture

    # I_APME_DEM_AUTOR_CRITR_ETEN_CM
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU <- unique(
      inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU[
        ANNEE == min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE),
        .(ANNEE, MOIS)
      ]
    )
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU <- unique(
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU[
        MOIS == min(I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU$MOIS),
        .(MOIS)
      ]
    )

    return(list(
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU = I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU$MOIS
    ))
  }
  search_value_chr <- function(dt, col, values) {
    ### Filtre les valeurs CHR dans la table dt
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.

    values <- unlist(stringr::str_split(values, "\\+"))  # séparer les valeurs dans un vecteur
    dt <- dt[get(col) %in% values]  # conserver les valeurs voulues
    return(dt)
  }


# DATAS -------------------------------------------------------------------

  mois_debut <- mois_debut_fct()  # valeur initiales pour le début


# USER INTERFACE ----------------------------------------------------------

  ui <- dashboardPage(


    # * Header section ----------------------------------------------------------------------------
    dashboardHeader(title = "Domaine de valeurs"),


    # * Sidebar section ---------------------------------------------------------------------------
    dashboardSidebar(
      width = 266,  # ajuster l'espace nécessaire selon le nom de la base de données
      sidebarMenu(
        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM")
      )
    ),


    # * Body section ------------------------------------------------------------------------------
    dashboardBody(
      tabItems(
        # * * I_APME_DEM_AUTOR_CRITR_ETEN_CM --------------------------------------------------------------
        tabItem(
          tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM",
          fluidRow(
            header_MaJ_datas(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ),
            column(
              width = 4,
              selectInput(  # sélection de la base de données
                inputId = "I_APME_DEM_AUTOR_CRITR_ETEN_CM__data",
                label = "Élément",
                choices = names(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)
              )
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Base de données",
              div(style = "margin-top:10px"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__params"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt")
            ),
            tabPanel(
              title = "Fiche technique",
              h3("I_APME_DEM_AUTOR_CRITR_ETEN_CM"),
              h4("Descriptif"),
              p("En développement")
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
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__val <- reactiveValues(
      show_tab = FALSE  # afficher la table ou pas
    )

    # * * UI ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__params <- renderUI({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
          return(tagList(
            fluidRow(
              column(
                width = 4,
                textInput(  # Code de DENOM
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom",
                  "DENOM_DEM"
                )
              ),
              column(
                width = 4,
                textInput(  # Code de DIN
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__din",
                  "DIN_DEM"
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                selectInput(  # Année début
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnDebut",
                  "Début période - Année",
                  choices = c(max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE):
                                min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)),
                  selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)
                )
              ),
              column(
                width = 4,
                selectInput(  # Mois début
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisDebut",
                  "Début période - Mois",
                  choices = 1:12, selected = mois_debut$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                selectInput(  # Année fin
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnFin",
                  "Fin période - Année",
                  choices = c(max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE):
                                min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)),
                  selected = lubridate::year(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ)
                )
              ),
              column(  # Mois fin
                width = 4,
                selectInput(
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisFin",
                  "Fin période - Mois",
                  choices = 1:12,
                  selected = lubridate::month(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ)
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                textInput(  # Recherche mot-clé
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__search",
                  "DES_COURT_INDCN_RECNU"
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                actionButton(  # Faire apparaître table selon critère
                  "I_APME_DEM_AUTOR_CRITR_ETEN__DES_COURT_INDCN_RECNU__go",
                  "Exécuter",
                  style = button_go_style()
                )
              ),
              column(
                width = 4,
                actionButton(  # Remettre les arguments comme au départ
                  "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__reset",
                  "Réinitialiser",
                  style = button_reset_style()
                )
              )
            )
          ))
      } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
        return(tagList(

        ))
      }
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__save_button <- renderUI({
      if (!is.null(I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt()) && nrow(I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt())) {
        return(tagList(
          div(style = "margin-top:10px"),
          fluidRow(
            column(
              width = 4,
              textInput(
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__savename",
                "Nom du fichier à sauvegarder"
              )
            ),
            column(
              width = 4,
              selectInput(  # déterminer l'extension du fichier
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__saveext",
                "Extension du fichier",
                choices = c("xlsx", "csv"),
                selected = "xlsx"
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              downloadButton(  # Sauvegarder la table en Excel
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__save",
                "Sauvegarder"
              ),
            ),
          )
        ))
      } else {
        return(NULL)
      }
    })

    # * * Datatable ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN__DES_COURT_INDCN_RECNU__go, {
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- TRUE
    })
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt <- eventReactive(
      c(input$I_APME_DEM_AUTOR_CRITR_ETEN__DES_COURT_INDCN_RECNU__go, I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab),
      {
        if (I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab) {
          dt <- copy(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM[[input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data]])
          if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
            if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom != "") {
              dt <- search_value_chr(
                dt, col = "DENOM_DEM",
                values = input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom
              )
            }
          }
          return(dt)
        } else {
          return(NULL)
        }
      },
      ignoreInit = TRUE
    )
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt <- renderDataTable(I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt())

    # * * Export ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__save <- download_data(
      input,
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt(),  # data à enregistrer
      data = "I_APME_DEM_AUTOR_CRITR_ETEN_CM",
      subdata = "DES_COURT_INDCN_RECNU"
    )

    # * * Update buttons ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__reset, {
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- FALSE  # faire disparaître la table
      # Remettre les valeurs initiales
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__din", value = "")
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnDebut",
                        selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE))
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisDebut",
                        selected = mois_debut$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU)
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnFin",
                        selected = lubridate::year(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ))
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisFin",
                        selected = lubridate::month(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ))
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__search", value = "")
    })

  }


# APPLICATION -------------------------------------------------------------

  shinyApp(ui, server)

}
