#' Datasets - Visualisation
#'
#' Exploration interactive des datasets du package `inesss`.
#'
#' @import data.table
#' @import shiny
#' @import shinydashboard
#' @export
data_explore <- function() {

  ui <- dashboardPage(

    dashboardHeader(),

    dashboardSidebar(
      sidebarMenu(
        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "I_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        # menuItem("Pop_QC", tabName = "Pop_QC"),
        # menuItem("RLS_convert", tabName = "RLS_convert"),
        menuItem("V_DEM_PAIMT_MED_CM", tabName = "V_DEM_PAIMT_MED_CM")
        # menuItem("V_DENOM_COMNE_MED", tabName = "V_DENOM_COMNE_MED"),
        # menuItem("V_DES_COD", tabName = "V_DES_COD"),
        # menuItem("V_PRODU_MED", tabName = "V_PRODU_MED")
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
                      choices = names(I_APME_DEM_AUTOR_CRITR_ETEN_CM)),
          uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__params"),
          dataTableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt")
        ),
        # V_DEM_PAIMT_MED_CM
        tabItem(
          tabName = "V_DEM_PAIMT_MED_CM",
          selectInput(inputId = "V_DEM_PAIMT_MED_CM__data",
                      label = "Élément",
                      choices = names(V_DEM_PAIMT_MED_CM)),
          uiOutput("V_DEM_PAIMT_MED_CM__params"),
          dataTableOutput("V_DEM_PAIMT_MED_CM__dt")
        )
      )
    )

  )


  server <- function(input, output, session) {

    ### Fermer l'application lorsque la fenêtre se ferme
    session$onSessionEnded(function() {stopApp()})

    ### I_APME_DEM_AUTOR_CRITR_ETEN_CM
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
        DT <- copy(I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU)
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
        DT <- copy(I_APME_DEM_AUTOR_CRITR_ETEN_CM$NO_SEQ_INDCN_RECNU_PME)
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
    }, options = list(searching = FALSE,
                      lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE)
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


    ### V_DEM_PAIMT_MED_CM
    # Paramètres à afficher
    V_DEM_PAIMT_MED_CM__params <- reactive({
      if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("V_DEM_PAIMT_MED_CM__COD_DIN__DIN", "DIN"),
              sliderInput("V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN", "DEBUT - FIN",
                          min = min(V_DEM_PAIMT_MED_CM$COD_DIN$DEBUT),
                          max = max(V_DEM_PAIMT_MED_CM$COD_DIN$FIN),
                          value = c(min(V_DEM_PAIMT_MED_CM$COD_DIN$DEBUT), max(V_DEM_PAIMT_MED_CM$COD_DIN$FIN)),
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
      } else {
        return(NULL)
      }
    })
    output$V_DEM_PAIMT_MED_CM__params <- renderUI({ V_DEM_PAIMT_MED_CM__params() })
    # Tableau
    V_DEM_PAIMT_MED_CM__dt <- reactive({
      if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        DT <- copy(V_DEM_PAIMT_MED_CM$COD_DIN)
        if (length(input$V_DEM_PAIMT_MED_CM__COD_DIN__DIN) && input$V_DEM_PAIMT_MED_CM__COD_DIN__DIN != "") {
          search_words <- unlist(stringr::str_split(input$"V_DEM_PAIMT_MED_CM__COD_DIN__DIN", "\\+"))
          for (i in 1:length(search_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(DIN), tolower(search_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
        DT <- DT[input$V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN[[1]] <= DEBUT & FIN <= input$V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN[[2]]]
        return(DT)
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {
        DT <- copy(V_DEM_PAIMT_MED_CM$COD_SERV)
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
      } else {
        return(NULL)
      }
    })
    output$V_DEM_PAIMT_MED_CM__dt <- renderDataTable({
      V_DEM_PAIMT_MED_CM__dt()
    }, options = list(searching = FALSE,
                      lengthMenu = list(c(25, 100, -1), c("25", "100", "All")), pageLength = 100,
                      scrollX = TRUE)
    )
    # Réinitialisation
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_DIN__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__DIN", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN",
                        value = c(min(V_DEM_PAIMT_MED_CM$COD_DIN$DEBUT), max(V_DEM_PAIMT_MED_CM$COD_DIN$FIN)))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_SERV__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__COD_SERV", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__COD_SERV_DESC", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_1", value = c(1996, year(Sys.Date())))
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_2", value = c(1996, year(Sys.Date())))
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_3", value = c(1996, year(Sys.Date())))
      updateCheckboxGroupInput(session, "V_DEM_PAIMT_MED_CM__COD_SERV__SERV_FILTER", selected = character(0))
    })

  }

  shinyApp(ui, server)

}
