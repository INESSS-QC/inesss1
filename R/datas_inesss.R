#' Datasets - Visualisation
#'
#' Exploration interactive des datasets du package `inesss`.
#'
#' @import data.table
#' @import shiny
#' @import shinydashboard
#' @export
datas_inesss <- function() {

# UI ----------------------------------------------------------------------

  ui <- dashboardPage(

    dashboardHeader(),

    dashboardSidebar(
      sidebarMenu(
        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "I_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        # menuItem("Pop_QC", tabName = "Pop_QC"),
        # menuItem("RLS_convert", tabName = "RLS_convert"),
        menuItem("V_DEM_PAIMT_MED_CM", tabName = "V_DEM_PAIMT_MED_CM"),
        menuItem("V_DENOM_COMNE_MED", tabName = "V_DENOM_COMNE_MED"),
        menuItem("V_DES_COD", tabName = "V_DES_COD"),
        menuItem("V_PRODU_MED", tabName = "V_PRODU_MED")
      ),
      width = 260
    ),

    dashboardBody(
      tabItems(
        # I_APME_DEM_AUTOR_CRITR_ETEN_CM
        tabItem(
          tabName = "I_APME_DEM_AUTOR_CRITR_ETEN_CM",
          selectInput(inputId = "I_APME_DEM_AUTOR_CRITR_ETEN_CM__data",
                      label = "Élément",
                      choices = names(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)),
          uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__params"),
          dataTableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt")
        ),
        # V_DEM_PAIMT_MED_CM
        tabItem(
          tabName = "V_DEM_PAIMT_MED_CM",
          selectInput(inputId = "V_DEM_PAIMT_MED_CM__data",
                      label = "Élément",
                      choices = names(inesss::V_DEM_PAIMT_MED_CM)),
          uiOutput("V_DEM_PAIMT_MED_CM__params"),
          dataTableOutput("V_DEM_PAIMT_MED_CM__dt")
        ),
        # V_DENOM_COMNE_MED
        tabItem(
          tabName = "V_DENOM_COMNE_MED",
          uiOutput("V_DENOM_COMNE_MED__params"),
          dataTableOutput("V_DENOM_COMNE_MED__dt")
        ),
        # V_DES_COD
        tabItem(
          tabName = "V_DES_COD",
          uiOutput("V_DES_COD__params"),
          dataTableOutput("V_DES_COD__dt")
        ),
        # V_PRODU_MED
        tabItem(
          tabName = "V_PRODU_MED",
          selectInput(inputId = "V_PRODU_MED__data",
                      label = "Élément",
                      choices = names(inesss::V_PRODU_MED)),
          uiOutput("V_PRODU_MED__params"),
          dataTableOutput("V_PRODU_MED__dt")
        )
      )
    )

  )



# SERVER ------------------------------------------------------------------

  server <- function(input, output, session) {

    ### Fermer l'application lorsque la fenêtre se ferme
    session$onSessionEnded(function() {stopApp()})


# I_APME_DEM_AUTOR_CRITR_ETEN_CM ------------------------------------------

    # Paramètres à afficher
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__params <- reactive({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__search", "Chaîne de caractères"),
              actionButton("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__reset", "Réinitialiser")
            )
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__NO_SEQ_INDCN_RECNU", "NO_SEQ_INDCN_RECNU"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_TRAIT_DEM", "DD_TRAIT_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_AUTOR", "DD_AUTOR"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_APLIC_AUTOR", "DD_APLIC_AUTOR"),
              actionButton("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__reset", "Réinitialiser")
            ),
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DAT_STA_DEM", "DAT_STA_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_TRAIT_DEM", "DF_TRAIT_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_AUTOR", "DF_AUTOR"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_APLIC_AUTOR", "DF_APLIC_AUTOR")
            )
          ),
          div(style = "margin-top:20px")
        ))
      } else {
        return(NULL)
      }
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__params <- renderUI({ I_APME_DEM_AUTOR_CRITR_ETEN_CM__params() })
    # Tableau
    I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt <- reactive({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        DT <- copy(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU)
        search_words <- unlist(stringr::str_split(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__search,
                                                  "\\+"))
        if (length(search_words) == 1 && search_words == "") {
          # rien pour le moment...
        } else {
          for (i in 1:length(search_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(DES_COURT_INDCN_RECNU), tolower(search_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
        return(DT)
      } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
        DT <- copy(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$NO_SEQ_INDCN_RECNU_PME)
        for (col in names(DT)) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__",col)]], "\\+"
          ))
          if (length(search_words) == 1 && search_words == "") {
            next
          } else {
            for (i in 1:length(search_words)) {
              DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
              DT <- DT[get(paste(i)) == TRUE]
              DT[, paste(i) := NULL]
            }
          }
        }
        return(DT)
      } else {
        return(NULL)
      }
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt <- renderDataTable({
      I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt()
    }, options = list(lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE, searching = FALSE)
    )
    # Réinitialisation
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__reset, {
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__search", value = "")
    })
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__reset, {
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__NO_SEQ_INDCN_RECNU", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_TRAIT_DEM", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_AUTOR", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_APLIC_AUTOR", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DAT_STA_DEM", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_TRAIT_DEM", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_AUTOR", value = "")
      updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_APLIC_AUTOR", value = "")
    })


# V_DEM_PAIMT_MED_CM ------------------------------------------------------

    # Paramètres à afficher
    V_DEM_PAIMT_MED_CM__params <- reactive({
      if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("V_DEM_PAIMT_MED_CM__COD_DIN__DIN", "DIN"),
              sliderInput("V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN", "DEBUT - FIN",
                          min = min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DEBUT),
                          max = max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$FIN),
                          value = c(min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DEBUT),
                                    max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$FIN)),
                          sep = "", step = 1),
              actionButton("V_DEM_PAIMT_MED_CM__COD_DIN__reset", "Réinitialiser")
            )
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_SERV__COD_SERV", "COD_SERV")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_SERV__COD_SERV_DESC", "COD_SERV_DESC")),
            column(4, checkboxGroupInput("V_DEM_PAIMT_MED_CM__COD_SERV__SERV_FILTER", "Filtrer",
                                         choices = c("SERV_1", "SERV_2", "SERV_3")))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_SERV__SERV_1", "SERV_1",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1)),
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_SERV__SERV_2", "SERV_2",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1)),
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_SERV__SERV_3", "SERV_3",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_SERV__reset", "Réinitialiser"))
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_STA_DECIS__COD_STA_DECIS", "COD_STA_DECIS"))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_STA_DECIS__DEBUT_FIN", "DEBUT - FIN",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_STA_DECIS__reset", "Réinitialiser"))
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DENOM_COMNE") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__DENOM", "DENOM")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__NOM_DENOM", "NOM_DENOM"))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__DEBUT_FIN", "DEBUT - FIN",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__reset", "Réinitialiser"))
          ),
          div(style = "margin-top:20px")
        ))
      } else {
        return(NULL)
      }
    })
    output$V_DEM_PAIMT_MED_CM__params <- renderUI({ V_DEM_PAIMT_MED_CM__params() })
    # Tableau
    V_DEM_PAIMT_MED_CM__dt <- reactive({
      if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        DT <- copy(inesss::V_DEM_PAIMT_MED_CM$COD_DIN)
        if (length(input$V_DEM_PAIMT_MED_CM__COD_DIN__DIN) && input$V_DEM_PAIMT_MED_CM__COD_DIN__DIN != "") {
          search_words <- unlist(stringr::str_split(input$"V_DEM_PAIMT_MED_CM__COD_DIN__DIN", "\\+"))
          for (i in 1:length(search_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(DIN), tolower(search_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
        DT <- DT[
          input$V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN[[2]]
        ]
        return(DT)
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {
        DT <- copy(inesss::V_DEM_PAIMT_MED_CM$COD_SERV)
        for (col in c("COD_SERV", "COD_SERV_DESC")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__COD_SERV__",col)]], "\\+"
          ))
          if (length(search_words) == 1 && search_words == "") {
            next
          } else {
            for (i in 1:length(search_words)) {
              DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
              DT <- DT[get(paste(i)) == TRUE]
              DT[, paste(i) := NULL]
            }
          }
        }
        for (filt in c("SERV_1", "SERV_2", "SERV_3")) {
          if (filt %in% input$V_DEM_PAIMT_MED_CM__COD_SERV__SERV_FILTER) {
            DT[, min_serv := stringr::str_sub(get(filt), 1, 4)]
            DT[, max_serv := stringr::str_sub(get(filt), 6, 9)]
            DT <- DT[
              input[[paste0("V_DEM_PAIMT_MED_CM__COD_SERV__", filt)]][[1]] <= min_serv &
                max_serv <= input[[paste0("V_DEM_PAIMT_MED_CM__COD_SERV__", filt)]][[2]]
            ]
            DT[, `:=` (min_serv = NULL, max_serv = NULL)]
          }
        }
        return(DT)
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
        DT <- copy(inesss::V_DEM_PAIMT_MED_CM$COD_STA_DECIS)
        for (col in c("COD_STA_DECIS")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__COD_STA_DECIS__",col)]], "\\+"
          ))
          if (length(search_words) == 1 && search_words == "") {
            next
          } else {
            for (i in 1:length(search_words)) {
              DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
              DT <- DT[get(paste(i)) == TRUE]
              DT[, paste(i) := NULL]
            }
          }
        }
        DT <- DT[
          input$V_DEM_PAIMT_MED_CM__COD_STA_DECIS__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__COD_STA_DECIS__DEBUT_FIN[[2]]
        ]
        return(DT)
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DENOM_COMNE") {
        DT <- copy(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE)
        for (col in c("DENOM", "NOM_DENOM")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__",col)]], "\\+"
          ))
          if (length(search_words) == 1 && search_words == "") {
            next
          } else {
            for (i in 1:length(search_words)) {
              DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
              DT <- DT[get(paste(i)) == TRUE]
              DT[, paste(i) := NULL]
            }
          }
        }
        DT <- DT[
          input$V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__DEBUT_FIN[[2]]
        ]
        return(DT)
      } else {
        return(NULL)
      }
    })
    output$V_DEM_PAIMT_MED_CM__dt <- renderDataTable({
      V_DEM_PAIMT_MED_CM__dt()
    }, options = list(lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE, searching = FALSE)
    )
    # Réinitialisation
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_DIN__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__DIN", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN",
                        value = c(min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DEBUT),
                                  max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$FIN)))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_SERV__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__COD_SERV", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__COD_SERV_DESC", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_1", value = c(1996, year(Sys.Date())))
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_2", value = c(1996, year(Sys.Date())))
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_3", value = c(1996, year(Sys.Date())))
      updateCheckboxGroupInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_FILTER", selected = character(0))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_STA_DECIS__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_STA_DECIS__COD_STA_DECIS", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_STA_DECIS__DEBUT_FIN", value = c(1996, year(Sys.Date())))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__DENOM", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__NOM_DENOM", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__DEBUT_FIN", value = c(1996, year(Sys.Date())))
    })


# V_DENOM_COMNE_MED -------------------------------------------------------

    # Paramètre à afficher
    V_DENOM_COMNE_MED__params <- reactive({
      return(tagList(
        fluidRow(
          column(4, textInput("V_DENOM_COMNE_MED__DENOM", "DENOM")),
          column(4, textInput("V_DENOM_COMNE_MED__NOM_DENOM", "NOM_DENOM/SYNON (Fr/En)"))
        ),
        fluidRow(
          column(4, actionButton("V_DENOM_COMNE_MED__reset", "Réinitialiser"))
        ),
        div(style = "margin-top:20px")
      ))
    })
    output$V_DENOM_COMNE_MED__params <- renderUI({ V_DENOM_COMNE_MED__params() })
    # Tableau
    V_DENOM_COMNE_MED__dt <- reactive({
      DT <- copy(inesss::V_DENOM_COMNE_MED)
      # Recherche DENOM
      search_words <- unlist(stringr::str_split(
        input$V_DENOM_COMNE_MED__DENOM, "\\+"
      ))
      if (length(search_words) >= 1 && search_words[1] != "") {
        for (i in 1:length(search_words)) {
          DT[, paste(i) := stringr::str_detect(tolower(DENOM), tolower(search_words[i]))]
          DT <- DT[get(paste(i)) == TRUE]
          DT[, paste(i) := NULL]
        }
      }
      # Recherche mots-clés
      search_words <- unlist(stringr::str_split(
        input$V_DENOM_COMNE_MED__NOM_DENOM, "\\+"
      ))
      if (length(search_words) >= 1 && search_words[1] != "") {
        for (i in 1:length(search_words)) {
          DT[, paste(i) := FALSE]
          for (col in c("NOM_DENOM", "NOM_DENOM_SYNON", "NOM_DENOM_ANGLAIS", "NOM_DENOM_SYNON_ANGLAIS")) {
            idx <- DT[, .I[stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]]
            if (length(idx)) {
              DT[idx, paste(i) := TRUE]
            }
          }
          DT <- DT[get(paste(i)) == TRUE]
          DT[, paste(i) := NULL]
        }
      }
      return(DT)
    })
    output$V_DENOM_COMNE_MED__dt <- renderDataTable({
      V_DENOM_COMNE_MED__dt()
    }, options = list(lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE, searching = FALSE)
    )
    # Réinitialisation
    observeEvent(input$V_DENOM_COMNE_MED__reset, {
      updateTextInput(session, "V_DENOM_COMNE_MED__DENOM", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__NOM_DENOM", value = "")
    })


# V_DES_COD ---------------------------------------------------------------

    # Paramètres à afficher
    V_DES_COD__params <- reactive({
      return(tagList(
        fluidRow(
          column(4, textInput("V_DES_COD__CODE", "CODE")),
          column(4, textInput("V_DES_COD__TYPE_CODE", "TYPE_CODE")),
          column(4, textInput("V_DES_COD__CODE_DESC", "CODE_DESC"))
        ),
        fluidRow(
          column(4, actionButton("V_DES_COD__reset", "Réinitialiser"))
        ),
        div(style = "margin-top:20px")
      ))
    })
    output$V_DES_COD__params <- renderUI({ V_DES_COD__params() })
    # Tableau
    V_DES_COD__dt <- reactive({
      DT <- inesss::V_DES_COD
      for (col in c("CODE", "TYPE_CODE", "CODE_DESC")) {
        search_words <- unlist(stringr::str_split(
          input[[paste0("V_DES_COD__",col)]], " \\+"
        ))
        if (length(search_words) == 1 && search_words == "") {
          next
        } else {
          for (i in 1:length(search_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
      }
      return(DT)
    })
    output$V_DES_COD__dt <- renderDataTable({
      V_DES_COD__dt()
    }, options = list(lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE, searching = FALSE)
    )
    # Réinitialisation
    observeEvent(input$V_DES_COD__reset, {
      updateTextInput(session, "V_DES_COD__CODE", value = "")
      updateTextInput(session, "V_DES_COD__TYPE_CODE", value = "")
      updateTextInput(session, "V_DES_COD__CODE_DESC", value = "")
    })


# V_PRODU_MED -------------------------------------------------------------

    # Paramètres à afficher
    V_PRODU_MED__params <- reactive({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_PRODU_MED__NOM_MARQ_COMRC__DENOM", "DENOM")),
            column(4, textInput("V_PRODU_MED__NOM_MARQ_COMRC__DIN", "DIN")),
            column(4, textInput("V_PRODU_MED__NOM_MARQ_COMRC__NOM_MARQ_COMRC", "NOM_MARQ_COMRC"))
          ),
          fluidRow(
            column(4, actionButton("V_PRODU_MED__NOM_MARQ_COMRC__reset", "Réinitialiser"))
          ),
          div(style = "margin-top:20px")
        ))
      }
    })
    output$V_PRODU_MED__params <- renderUI({ V_PRODU_MED__params() })
    # Tableau
    V_PRODU_MED__dt <- reactive({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        DT <- inesss::V_PRODU_MED$NOM_MARQ_COMRC
        for (col in c("DENOM", "DIN", "NOM_MARQ_COMRC")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_PRODU_MED__NOM_MARQ_COMRC__",col)]], "\\+"
          ))
          if (length(search_words) == 1 && search_words == "") {
            next
          } else {
            for (i in 1:length(search_words)) {
              DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
              DT <- DT[get(paste(i)) == TRUE]
              DT[, paste(i) := NULL]
            }
          }
        }
      }
      return(DT)
    })
    output$V_PRODU_MED__dt <- renderDataTable({
      V_PRODU_MED__dt()
    }, options = list(lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE, searching = FALSE)
    )
    # Réinitialisation
    observeEvent(input$V_PRODU_MED__NOM_MARQ_COMRC__reset, {
      updateTextInput(session, "V_PRODU_MED__NOM_MARQ_COMRC__DENOM", value = "")
      updateTextInput(session, "V_PRODU_MED__NOM_MARQ_COMRC__DIN", value = "")
      updateTextInput(session, "V_PRODU_MED__NOM_MARQ_COMRC__NOM_MARQ_COMRC", value = "")
    })

  }



# RUN APP -----------------------------------------------------------------

  shinyApp(ui, server)

}






















