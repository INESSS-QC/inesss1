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

  button_go_reset <- function(dataname, goname = "Exécuter", resetname = "Réinitialiser") {
    return(tagList(
      fluidRow(
        column(
          width = 4,
          actionButton(  # Faire apparaître table selon critère
            paste0(dataname, "__go"),
            goname,
            style = button_go_style()
          )
        ),
        column(
          width = 4,
          actionButton(  # Remettre les arguments comme au départ
            paste0(dataname, "__reset"),
            resetname,
            style = button_reset_style()
          )
        )
      )
    ))
  }
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
  button_save <- function(dataname, dataset) {
    if (!is.null(dataset) && nrow(dataset)) {
      return(tagList(
        div(style = "margin-top:10px"),
        fluidRow(
          column(
            width = 4,
            textInput(
              paste0(dataname, "__savename"),
              "Nom du fichier à sauvegarder"
            )
          ),
          column(
            width = 4,
            selectInput(  # déterminer l'extension du fichier
              paste0(dataname, "__saveext"),
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
              paste0(dataname, "__save"),
              "Sauvegarder"
            ),
          ),
        )
      ))
    } else {
      return(NULL)
    }
  }
  download_data <- function(input, datasave, dataname) {
    ### Télécharge sur l'ordinateur le data affiché à l'écran
    ### @param datasave Tableau à enregistrer.
    ### @param dataname Nom du dataset, par exemple I_APME_DEM_AUTOR_CRITR_ETEN_CM

    return(downloadHandler(
      filename = function() {
        paste0(
          input[[paste0(dataname, "__savename")]],
          ".",
          input[[paste0(dataname, "__saveext")]]
        )
      },
      content = function(file) {
        if (input[[paste0(dataname, "__saveext")]] == "xlsx") {
          writexl::write_xlsx(datasave, file)
        } else if (input[[paste0(dataname, "__saveext")]] == "csv") {
          write.csv(datasave, file)
        }
      }
    ))
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
  renderDataTable_options <- function() {
    return(list(
      lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "Tout")),
      pageLength = 50,
      scrollX = TRUE,
      searching = FALSE
    ))
  }
  search_keyword <- function(dt, col, values, lower = TRUE) {
    ### Rechercher un ou des mots clés dans une colonne
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.

    values <- unlist(stringr::str_split(values, "\\+"))  # séparer les valeurs dans un vecteur
    values <- paste(values, collapse = "|")  # rechercher tous les mots dans une même chaîne de caractères
    if (lower) {
      dt <- dt[stringr::str_detect(tolower(get(col)), tolower(values))]
    } else {
      dt <- dt[stringr::str_detect(get(col), values)]
    }
    return(dt)
  }
  search_value_chr <- function(dt, col, values, lower = TRUE, pad = NULL) {
    ### Filtre les valeurs CHR dans la table dt
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.
    ### @param lower Si on doit convertir en minuscule ou chercher la valeur exacte.

    values <- unlist(stringr::str_split(values, "\\+"))  # séparer les valeurs dans un vecteur
    if (!is.null(pad)) {
      values <- stringr::str_pad(values, width = pad, pad = "0")
    }
    # Conserver les valeurs voulues
    if (lower) {
      dt <- dt[tolower(get(col)) %in% tolower(values)]  # convertir en minuscule au besoin
    } else {
      dt <- dt[get(col) %in% values]
    }
    return(dt)
  }
  search_value_num <- function(dt, col, values, type = "int") {
    ### Filtre les valeurs NUM dans la table dt
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.
    ### @param type "int" pour integer ou "num" pour "numeric"

    values <- unlist(stringr::str_split(values, "\\+"))  # séparer les valeurs dans un vecteur
    if (type == "int") {
      values <- as.integer(values)
    } else if (type == "num") {
      values <- as.numeric(values)
    } else {
      stop("search_value_num(): type a une valeur non permise.")
    }
    dt <- dt[get(col) %in% values]  # conserver les valeurs voulues
    return(dt)
  }
  search_value_XXXX_YYYY <- function(dt, col, values) {
    ### Permet de rechercher une année qui est inscrite au format XXXX-YYYY = Début-Fin
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.

    values <- unlist(stringr::str_split(values, "\\+"))
    for (val in values) {
      dt <- dt[
        stringr::str_sub(get(col), 1, 4) <= val &
          val <= stringr::str_sub(get(col), 6, 9)
      ]
    }
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
        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        menuItem("V_CLA_AHF", tabName = "tabV_CLA_AHF")
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
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__go_reset_button"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt")
            ),
            tabPanel(
              title = "Fiche technique"
            )
          )
        ),

        # * * V_CLA_AHF ---------------------------------------------------------------
        tabItem(
          tabName = "tabV_CLA_AHF",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_CLA_AHF)$MaJ)
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Base de données",
              div(style = "margin-top:10px"),
              uiOutput("V_CLA_AHF__params"),
              uiOutput("V_CLA_AHF__go_reset_button"),
              uiOutput("V_CLA_AHF__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_CLA_AHF__dt")
            ),
            tabPanel(
              title = "Fiche technique"
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
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__denom",
                "DENOM_DEM"
              ),
              selectInput(  # Année début
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnDebut",
                "Début période - Année",
                choices = c(max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE):
                              min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)),
                selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)
              ),
              selectInput(  # Année fin
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnFin",
                "Fin période - Année",
                choices = c(max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE):
                              min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)),
                selected = lubridate::year(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ)
              ),
              textInput(  # Recherche mot-clé
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__search",
                "DES_COURT_INDCN_RECNU"
              )
            ),
            column(
              width = 4,
              textInput(  # Code de DIN
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__din",
                "DIN_DEM"
              ),
              selectInput(  # Mois début
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisDebut",
                "Début période - Mois",
                choices = 1:12, selected = mois_debut$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU
              ),
              selectInput(
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisFin",
                "Fin période - Mois",
                choices = 1:12,
                selected = lubridate::month(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ)
              ),
              selectInput(
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__typeRecherche",
                "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        ))
      } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__noSeqIndcnRecnu",
                        "NO_SEQ_INDCN_RECNU"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__debTraitDem",
                        "DD_TRAIT_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__debAutor",
                        "DD_AUTOR"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__debAplicAutor",
                        "DD_APLIC_AUTOR")
            ),
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__datStaDem",
                        "DAT_STA_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__finTraitDem",
                        "DF_TRAIT_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__finAutor",
                        "DF_AUTOR"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__finAplicAutor",
                        "DF_APLIC_AUTOR")
            )
          )
        ))
      }
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go_reset_button <- renderUI({
      button_go_reset("I_APME_DEM_AUTOR_CRITR_ETEN_CM")
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__save_button <- renderUI({
      button_save("I_APME_DEM_AUTOR_CRITR_ETEN_CM", I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt())
    })

    # * * Datatable ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go, {
      # Afficher la table si on clique sur Exécuter
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- TRUE
    }, ignoreInit = TRUE)
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data, {
      # Faire disparaitre la table si on change le data
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- FALSE
    }, ignoreInit = TRUE)
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt <- eventReactive(
      c(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go, I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab),
      {
        if (I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab) {
          dt <- inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM[[input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data]]
          if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
            if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__denom != "") {  # rechercher les DENOM
              dt <- search_value_chr(
                dt, col = "DENOM_DEM",
                values = input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__denom, pad = 5
              )
            }
            if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__din != "") {  # rechercher les DIN
              dt <- search_value_chr(
                dt, col = "DIN_DEM",
                values = input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__din
              )
            }
            if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__search != "") {
              if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__typeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "DES_COURT_INDCN_RECNU",
                  values = input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__search
                )
              } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__typeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "DES_COURT_INDCN_RECNU",
                  values = input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__search
                )
              }

            }
            # Filtrer selon les années demandées
            debut <- as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnDebut) * 100 + as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisDebut)
            fin <- as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnFin) * 100 + as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisFin)
            dt[, filtre_an := ANNEE * 100 + MOIS]
            dt <- dt[filtre_an <= fin & filtre_an >= debut]
            dt[, filtre_an := NULL]
          } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
            if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__noSeqIndcnRecnu != "") {
              dt <- search_value_num(
                dt, col = "NO_SEQ_INDCN_RECNU",
                values = input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__noSeqIndcnRecnu
              )
            }
            # Filtrer les années pour toutes les autre colonnes
            cols <- c(  # noms input = nom colonne
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__datStaDem = "DAT_STA_DEM",
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__debTraitDem = "DD_TRAIT_DEM",
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__finTraitDem = "DF_TRAIT_DEM",
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__debAutor = "DD_AUTOR",
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__finAutor = "DF_AUTOR",
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__debAplicAutor = "DD_APLIC_AUTOR",
              I_APME_DEM_AUTOR_CRITR_ETEN_CM__finAplicAutor = "DF_APLIC_AUTOR"
            )
            for (c in 1:length(cols)) {  # boucle pour filtrer toutes les colonnes
              if (input[[names(cols)[c]]] != "") {
                dt <- search_value_XXXX_YYYY(
                  dt, col = cols[c],
                  values = input[[names(cols)[c]]]
                )
              }
            }
          }
          return(dt)
        } else {
          return(NULL)
        }
      }, ignoreInit = TRUE
    )
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt <- renderDataTable({
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt()
    }, options = renderDataTable_options())

    # * * Export ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__save <- download_data(
      input,
      datasave = I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt(),
      dataname = "I_APME_DEM_AUTOR_CRITR_ETEN_CM"
    )

    # * * Update buttons ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__reset, {
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- FALSE  # faire disparaître la table
      # Remettre les valeurs initiales
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__denom", value = "")
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__din", value = "")
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnDebut",
                          selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE))
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisDebut",
                          selected = mois_debut$I_APME_DEM_AUTOR_CRITR_ETEN_CM)
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnFin",
                          selected = lubridate::year(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ))
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisFin",
                          selected = lubridate::month(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ))
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__search", value = "")
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__typeRecherche",
                          selected = "keyword")
      } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
        input_name <- paste0(
          "I_APME_DEM_AUTOR_CRITR_ETEN_CM__",
          c(
            "noSeqIndcnRecnu", "datStaDem",
            "debTraitDem", "finTraitDem",
            "debAutor", "finAutor",
            "debAplicAutor", "finAplicAutor"
          )
        )
        for (inp in input_name) {
          updateTextInput(session, inp, value = "")
        }
      }
    }, ignoreInit = TRUE)

    # * * Erreurs possibles ####
    ### DES_COURT_INDCN_RECNU
    observeEvent(
      eventExpr = {
        # modifier un des éléments déclenche la vérification
        c(
          # DES_COURT_INDCN_RECNU
          input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnDebut,
          input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnFin,
          input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisDebut,
          input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisFin
        )
      },
      handlerExpr = {
        if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
          annee_deb <- as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnDebut)
          annee_fin <- as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnFin)
          mois_deb <- as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisDebut)
          mois_fin <- as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisFin)
          # La combinaison année+mois du début doit être <= à la fin
          if (annee_deb >= annee_fin && mois_deb > mois_fin) {
            updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__MoisDebut",
                              selected = mois_fin)
          }
          if (annee_deb > annee_fin) {
            updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__AnDebut",
                              selected = annee_fin)
          }
        }
      },
      ignoreInit = TRUE
    )


    # * V_CLA_AHF ------------------------------------------------------------
    V_CLA_AHF__val <- reactiveValues(
      show_tab = FALSE  # afficher la table ou pas
    )

    # * * UI ####
    output$V_CLA_AHF__params <- renderUI({
      return(tagList(
        fluidRow(
          column(
            width = 4,
            textInput("V_CLA_AHF__ahfsCla", "AHFS_CLA"),
            textInput("V_CLA_AHF__nomAhfs", "NOM_AHFS"),
            textInput("V_CLA_AHF__nomAnglaisAhfs", "NOM_ANGLAIS_AHFS")
          ),
          column(
            width = 4,
            textInput("V_CLA_AHF__ahfsScla", "AHFS_SCLA"),
            selectInput(
              "V_CLA_AHF__nomAhfs__typeRecherche",
              "Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            ),
            selectInput(
              "V_CLA_AHF__nomAnglaisAhfs__typeRecherche",
              "Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          ),
          column(
            width = 4,
            textInput("V_CLA_AHF__ahfsSscla", "AHFS_SSCLA")
          )
        )
      ))
    })
    output$V_CLA_AHF__go_reset_button <- renderUI({
      button_go_reset("V_CLA_AHF")
    })
    output$V_CLA_AHF__save_button <- renderUI({
      button_save("V_CLA_AHF", V_CLA_AHF__dt())
    })

    # * * Datatable ####
    observeEvent(input$V_CLA_AHF__go, {
      # Afficher la table si on clique sur Exécuter
      V_CLA_AHF__val$show_tab <- TRUE
    }, ignoreInit = TRUE)
    V_CLA_AHF__dt <- eventReactive(
      c(input$V_CLA_AHF__go, V_CLA_AHF__val$show_tab),
      {
        if (V_CLA_AHF__val$show_tab) {
          dt <- inesss::V_CLA_AHF
          # Classe AHFS
          if (input$V_CLA_AHF__ahfsCla != "") {
            dt <- search_value_chr(
              dt, col = "AHFS_CLA",
              values = input$V_CLA_AHF__ahfsCla, pad = 2
            )
          }
          # Sous-Classe AHFS
          if (input$V_CLA_AHF__ahfsScla != "") {
            dt <- search_value_chr(
              dt, col = "AHFS_SCLA",
              values = input$V_CLA_AHF__ahfsScla, pad = 2
            )
          }
          # Sous-Sous-Classe AHFS
          if (input$V_CLA_AHF__ahfsSscla != "") {
            dt <- search_value_chr(
              dt, col = "AHFS_SSCLA",
              values = input$V_CLA_AHF__ahfsSscla, pad = 2
            )
          }
          # Nom Classe AHFS
          if (input$V_CLA_AHF__nomAhfs != "") {
            if (input$V_CLA_AHF__nomAhfs__typeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_AHFS",
                values = input$V_CLA_AHF__nomAhfs
              )
            } else if (input$V_CLA_AHF__nomAhfs__typeRecherche == "exactWord") {
              dt <- search_value_chr(
                dt, col = "NOM_AHFS",
                values = input$V_CLA_AHF__nomAhfs
              )
            }
          }
          # Nom Anglais Classe AHFS
          if (input$V_CLA_AHF__nomAnglaisAhfs != "") {
            if (input$V_CLA_AHF__nomAnglaisAhfs__typeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_ANGLAIS_AHFS",
                values = input$V_CLA_AHF__nomAnglaisAhfs
              )
            } else if (input$V_CLA_AHF__nomAnglaisAhfs__typeRecherche == "exactWord") {
              dt <- search_value_chr(
                dt, col = "NOM_ANGLAIS_AHFS",
                values = input$V_CLA_AHF__nomAnglaisAhfs
              )
            }
          }
          return(dt)
        } else {
          return(NULL)
        }
      }, ignoreInit = TRUE
    )
    output$V_CLA_AHF__dt <- renderDataTable({
      V_CLA_AHF__dt()
    }, options = renderDataTable_options())

    # * * Export ####
    output$V_CLA_AHF__save <- download_data(
      input,
      datasave = V_CLA_AHF__dt(),
      dataname = "V_CLA_AHF"
    )

    # * * Update Buttons ####
    observeEvent(input$V_CLA_AHF__reset, {
      updateTextInput(session, "V_CLA_AHF__ahfsCla", value = "")
      updateTextInput(session, "V_CLA_AHF__ahfsScla", value = "")
      updateTextInput(session, "V_CLA_AHF__ahfsSscla", value = "")
      updateTextInput(session, "V_CLA_AHF__nomAhfs", value = "")
      updateTextInput(session, "V_CLA_AHF__nomAnglaisAhfs", value = "")
    })

  }


# APPLICATION -------------------------------------------------------------

  shinyApp(ui, server)

}
