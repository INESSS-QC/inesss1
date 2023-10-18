#' Démarrer domaine de valeurs
#' @keywords internal
#' @encoding UTF-8
domaine_valeurs.addins <- function() {
  inesss::domaine_valeurs()
}


#' Domaine de valeurs
#'
#' Domaine de valeurs de différentes bases de données disponibles dans le domaine de la santé, principalement à la RAMQ.
#'
#' @import data.table
#' @import shiny
#' @import shinydashboard
#' @importFrom stringr str_sub str_pad
#'
#' @encoding UTF-8
#' @keywords internal
#' @export
domaine_valeurs <- function() {


  # FONCTIONS INTERNES ------------------------------------------------------

  button_go_reset <- function(dataname, goname = "Exécuter", resetname = "Réinitialiser", colwidth = NULL) {
    return(tagList(
      fluidRow(
        column(
          width = ui_col_width(colwidth),
          actionButton(  # Faire apparaître table selon critère
            paste0(dataname, "__go"),
            goname,
            style = button_go_style()
          )
        ),
        column(
          width = ui_col_width(colwidth),
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
  button_save <- function(dataname, dataset, colwidth = NULL) {
    if (!is.null(dataset) && nrow(dataset)) {
      return(tagList(
        div(style = "margin-top:10px"),
        fluidRow(
          column(
            width = ui_col_width(colwidth),
            textInput(
              paste0(dataname, "__savename"),
              "Nom du fichier à sauvegarder"
            )
          ),
          column(
            width = ui_col_width(colwidth),
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
            width = ui_col_width(colwidth),
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
  renderDataTable_options <- function() {
    return(list(
      lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "Tout")),
      pageLength = 50,
      scrollX = TRUE,
      searching = FALSE
    ))
  }
  search_keyword <- function(dt, col, values, lower = TRUE, no_accent = TRUE) {
    ### Rechercher un ou des mots clés dans une colonne
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.

    if (no_accent) {
      values <- unaccent(unlist(stringr::str_split(values, "\\+")))  # séparer les valeurs dans un vecteur
    } else {
      values <- unlist(stringr::str_split(values, "\\+"))  # séparer les valeurs dans un vecteur
    }
    values <- unaccent(unlist(stringr::str_split(values, "\\+")))  # séparer les valeurs dans un vecteur
    values <- paste(values, collapse = "|")  # rechercher tous les mots dans une même chaîne de caractères
    if (lower) {
      if (no_accent) {
        dt <- dt[stringr::str_detect(unaccent(tolower(get(col))), tolower(values))]
      } else {
        dt <- dt[stringr::str_detect(tolower(get(col))), tolower(values)]
      }
    } else {
      if (no_accent) {
        dt <- dt[stringr::str_detect(unaccent(get(col)), values)]
      } else {
        dt <- dt[stringr::str_detect(get(col), values)]
      }
    }
    return(dt)
  }
  search_value_chr <- function(dt, col, values, lower = TRUE, pad = NULL, no_accent = TRUE) {
    ### Filtre les valeurs CHR dans la table dt
    ### @param dt Data à modifier
    ### @param col Colonne à filtrer
    ### @param values La ou les valeurs à conserver. Chaîne de caractères, un "+"  indique qu'il y
    ###               aura plusieurs codes.
    ### @param lower Si on doit convertir en minuscule ou chercher la valeur exacte.

    if (no_accent) {
      values <- unaccent(unlist(stringr::str_split(values, "\\+")))  # séparer les valeurs dans un vecteur
    } else {
      values <- unlist(stringr::str_split(values, "\\+"))  # séparer les valeurs dans un vecteur
    }
    if (!is.null(pad)) {
      values <- stringr::str_pad(values, width = pad, pad = "0")
    }
    # Conserver les valeurs voulues
    if (lower) {
      if (no_accent) {
        dt <- dt[unaccent(tolower(get(col))) %in% tolower(values)]  # convertir en minuscule au besoin
      } else {
        dt <- dt[tolower(get(col)) %in% tolower(values)]  # convertir en minuscule au besoin
      }
    } else {
      if (no_accent) {
        dt <- dt[unaccent(get(col)) %in% values]
      } else {
        dt <- dt[get(col) %in% values]
      }
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
  ui_col_width <- function(width = NULL) {
    ### Largeur par défaut des input. Permet de changer d'idée rapidement et ne pas
    ### devoir le changer partout dans le programme
    ### Par défaut = 3 (modifier cette valeur au besoin)
    ### Ne pas oublier que la somme de tous les input doit être plus petite ou égal à 12

    default <- 3

    if (is.null(width)) {
      return(default)
    } else {
      return(width)
    }
  }
  V_DENOM_COMNE_MED_arrange <- function() {
    dt <- inesss::V_DENOM_COMNE_MED
    dt[DATE_FIN > Sys.Date(), DATE_FIN := Sys.Date()]

    dt[, `:=` (DATE_DEBUT = NULL, DATE_FIN = NULL)]
    return(dt)
  }
  V_FORM_MED_cod_typ_forme <- function() {
    ### Valeurs uniques des codes et des descriptions de type de forme
    ### @return Vecteur
    cod_desc <- unique(inesss::V_FORME_MED[, .(COD_TYP_FORME, NOM_TYPE_FORME)])
    setkey(cod_desc)
    cod_desc[, MENU_TYP_FORME := paste0(COD_TYP_FORME, " - ", NOM_TYPE_FORME)]
    return(cod_desc$MENU_TYP_FORME)
  }


  # DATAS -------------------------------------------------------------------
  V_FORM_MED__menuCodTypForme <- V_FORM_MED_cod_typ_forme()


  # USER INTERFACE ----------------------------------------------------------

  ui <- dashboardPage(


    ## Header section ----------------------------------------------------------------------------
    dashboardHeader(title = "Domaine de valeur"),


    ## Sidebar section ---------------------------------------------------------------------------
    dashboardSidebar(
      width = 266,  # ajuster l'espace nécessaire selon le nom de la base de données
      sidebarMenu(

        div(style = "margin-top:10px"),

        p(HTML("&nbsp;&nbsp;"), tags$u("Combinaisons uniques")),
        menuItem("Médicaments d'exception", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        menuItem("Demandes de paiement de médicaments", tabName = "tabV_DEM_PAIMT_MED_CM"),

        div(style = "margin-top:30px"),

        p(HTML("&nbsp;&nbsp;"), tags$u("Dictionnaires")),
        menuItem("CIM-9 et CIM-10", tabName = "tabCIM"),
        menuItem("Classes AHFS", tabName = "tabV_CLA_AHF"),
        menuItem("Dénomination commune", tabName = "tabV_DENOM_COMNE_MED"),
        menuItem("Forme", tabName = "tabV_FORM_MED"),
        menuItem("Produit médicament", tabName = "tabV_PRODU_MED")

      )
    ),


    ## Body section ------------------------------------------------------------------------------
    dashboardBody(
      tabItems(

        ### I_APME_DEM_AUTOR_CRITR_ETEN_CM --------------------------------------------------------------
        tabItem(
          tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM",
          fluidRow(
            header_MaJ_datas(attributes(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)$MaJ),
            column(
              width = 12,
              strong("Vue : I_APME_DEM_AUTOR_CRITR_ETEN_CM"),
              p("Demandes d'autorisation de Patient-Médicament d'exceptions.")
            ),
            column(
              width = ui_col_width(),
              selectInput(  # sélection du domaine de valeur
                inputId = "I_APME_DEM_AUTOR_CRITR_ETEN_CM__data",
                label = "Élément",
                choices = names(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(style = "margin-top:-10px"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__dataDesc"),
              div(style = "margin-top:20px")
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__params"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__go_reset_button"),
              uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt")
            ),
            tabPanel(
              title = "Fiche technique",
              div(style = "margin-top:20px"),
              column(
                width = 12,
                em(inesss::domaine_valeurs_fiche_technique$I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$MaJ)
              ),
              tableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__varDesc")
            )
          )
        ),

        ### V_DEM_PAIMT_MED_CM ------------------------------------------------------
        tabItem(
          tabName = "tabV_DEM_PAIMT_MED_CM",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_DEM_PAIMT_MED_CM)$MaJ),
            column(
              width = 12,
              strong("Vue : V_DEM_PAIMT_MED_CM"),
              p("Demandes de paiement de médicaments."),
              p("Combinaisons uniques des codes de médicaments ou autres variables utiles.")
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              fluidRow(
                h4(HTML("&nbsp;&nbsp;"), "Variables à afficher"),
                style = "color: #ffffff; background-color: #0086b3;"
              ),
              fluidRow(
                column(
                  width = 2,
                  checkboxGroupInput("V_DEM_PAIMT_MED_CM__varSelect1", "", choices = c("DENOM", "DIN"))
                ),
                column(
                  width = 2,
                  checkboxGroupInput("V_DEM_PAIMT_MED_CM__varSelect2", "", choices = c("AHFS", "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA"))
                ),
                column(
                  width = 2,
                  checkboxGroupInput("V_DEM_PAIMT_MED_CM__varSelect3", "", choices = c("FORME", "TENEUR", "COD_STA_DECIS"))
                ),
                column(
                  width = 2,
                  checkboxGroupInput("V_DEM_PAIMT_MED_CM__varSelect4", "", choices = c("COD_SERV_1", "COD_SERV_2", "COD_SERV_3"))
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  uiOutput("V_DEM_PAIMT_MED_CM__descChoices")
                )
              ),
              fluidRow(
                h4(HTML("&nbsp;&nbsp;"), "Paramètres"),
                style = "color: #ffffff; background-color: #0086b3;"
              ),
              fluidRow(
                column(
                  width = 2,
                  checkboxInput("V_DEM_PAIMT_MED_CM__periodDetail", "Périodes Détaillées")
                )
              ),
              fluidRow(
                h4(HTML("&nbsp;&nbsp;"), "Paramètres de recherche"),
                style = "color: #ffffff; background-color: #0086b3;"
              ),
              div(style = "margin-top:10px"),
              uiOutput("V_DEM_PAIMT_MED_CM__params"),
              fluidRow(
                h4(HTML("&nbsp;&nbsp;"), "Résultats"),
                style = "color: #ffffff; background-color: #0086b3;"
              ),
              div(style = "margin-top:10px"),
              uiOutput("V_DEM_PAIMT_MED_CM__go_reset_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_DEM_PAIMT_MED_CM__dt")
            ),

            tabPanel(
              title = "Fiche technique",
              div(style = "margin-top:20px"),
              column(
                width = 12,
                em(inesss::domaine_valeurs_fiche_technique$V_DEM_PAIMT_MED_CM$MaJ)
              )
            )
          )
        ),

        ### CIM ---------------------------------------------------------------------

        tabItem(
          tabName = "tabCIM",
          fluidRow(
            header_MaJ_datas(attributes(inesss::CIM)$MaJ),
            column(
              width = 12,
              strong("Répertoires des diagnostics"),
              p("Les codes de diagnostic proviennent de la Classification statistique internationale des maladies et des problèmes de santé connexes (CIM) de l’Organisation mondiale de la santé. La RAMQ accepte les codes de diagnostic de la 9e et de la 10e révision de la CIM (CIM-9 et CIM-10).")
            ),
            column(
              width = ui_col_width(),
              selectInput(
                inputId = "CIM__data",
                label = "Élément",
                choices = names(inesss::CIM)
              )
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("CIM__params"),
              uiOutput("CIM__go_reset_button"),
              uiOutput("CIM__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("CIM__dt")
            )
          )
        ),

        ### V_CLA_AHF ---------------------------------------------------------------
        tabItem(
          tabName = "tabV_CLA_AHF",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_CLA_AHF)$MaJ),
            column(
              width = 12,
              strong("Vue : V_CLA_AHF"),
              p("Système de classification américain élaboré par l'American Hospital Formulary Service (AHFS).")
            ),
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("V_CLA_AHF__params"),
              uiOutput("V_CLA_AHF__go_reset_button"),
              uiOutput("V_CLA_AHF__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_CLA_AHF__dt")
            ),
            tabPanel(
              title = "Fiche technique",
              div(style = "margin-top:20px"),
              column(
                width = 12,
                em(inesss::domaine_valeurs_fiche_technique$V_CLA_AHF$MaJ)
              ),
              tableOutput("V_CLA_AHF__varDesc")
            )
          )
        ),

        ### V_DENOM_COMNE_MED --------------------------------------------------
        tabItem(
          tabName = "tabV_DENOM_COMNE_MED",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_DEM_PAIMT_MED_CM)$MaJ),
            column(
              width = 12,
              strong("Vue : V_DENOM_COMNE_MED"),
              p("Cette structure contient l'information qui décrit un code de dénomination commune de médicament.")
            ),
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("V_DENOM_COMNE_MED__params"),
              uiOutput("V_DENOM_COMNE_MED__go_reset_button"),
              uiOutput("V_DENOM_COMNE_MED__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_DENOM_COMNE_MED__dt")
            ),
            tabPanel(
              title = "Fiche technique",
              div(style = "margin-top:20px"),
              column(
                width = 12,
                em(inesss::domaine_valeurs_fiche_technique$V_DENOM_COMNE_MED$MaJ)
              ),
              tableOutput("V_DENOM_COMNE_MED__varDesc")
            )
          )
        ),


        ### V_FORM_MED ----------------------------------------------------------
        tabItem(
          tabName = "tabV_FORM_MED",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_PRODU_MED)$MaJ),
            column(
              width = 12,
              strong("Vue : V_FORM_MED"),
              p("Forme pharmaceutique que peut prendre un médicament. Cette structure contient l'information qui décrit un code de forme de médicament. Une forme pharmaceutique équivaut à une apparence, à un aspect visible par lequel est dispensé un médicament.")
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("V_FORM_MED__params"),
              uiOutput("V_FORM_MED__go_reset_button"),
              uiOutput("V_FORM_MED__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_FORM_MED__dt")
            )
          )
        ),


        ### V_PRODU_MED ----------------------------------------------------------
        tabItem(
          tabName = "tabV_PRODU_MED",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_PRODU_MED)$MaJ),
            column(
              width = 12,
              strong("Vue : V_PRODU_MED"),
              p("Cet élément représente le nom sous lequel est commercialisé un produit pharmaceutique. Il sert à désigner, plus précisément, un code DIN.")
            ),
            column(
              width = ui_col_width(),
              selectInput(  # sélection du domaine de valeur
                inputId = "V_PRODU_MED__data",
                label = "Élément",
                choices = names(inesss::V_PRODU_MED)
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(style = "margin-top:-10px"),
              uiOutput("V_PRODU_MED__dataDesc"),
              div(style = "margin-top:20px")
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("V_PRODU_MED__params"),
              uiOutput("V_PRODU_MED__go_reset_button"),
              uiOutput("V_PRODU_MED__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_PRODU_MED__dt")
            ),
            tabPanel(
              title = "Fiche technique",
              div(style = "margin-top:20px"),
              column(
                width = 12,
                em(inesss::domaine_valeurs_fiche_technique$V_PRODU_MED$NOM_MARQ_COMRC$MaJ)
              ),
              tableOutput("V_PRODU_MED__varDesc"),
              column(
                width = 12,
                uiOutput("V_PRODU_MED__footnote")
              )
            )
          )
        )

      )
    )
  )


  # SERVER ------------------------------------------------------------------

  server <- function(input, output, session) {


    # General -----------------------------------------------------------------

    ### Fermer l'application lorsque la fenêtre se ferme
    session$onSessionEnded(function() {stopApp()})


    # I_APME_DEM_AUTOR_CRITR_ETEN_CM ------------------------------------------
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__val <- reactiveValues(
      show_tab = FALSE  # afficher la table ou pas
    )

    ## Fiche Technique ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__varDesc <- renderTable({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        return(inesss::domaine_valeurs_fiche_technique$I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$tab_desc)
      } else {
        return(NULL)
      }
    })

    ## Descriptif Data ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__dataDesc <- renderUI({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons des descriptions courtes complètes des indications reconnues de Patient-Médicament d'exceptions.")
            )
          )
        ))
      }
    })

    ## UI ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__params <- renderUI({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput(  # Code de DENOM
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__denom",
                "DENOM_DEM"
              ),
              textInput(  # Recherche mot-clé
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__search",
                "DES_COURT_INDCN_RECNU"
              )
            ),
            column(
              width = ui_col_width(),
              textInput(  # Code de DIN
                "I_APME_DEM_AUTOR_CRITR_ETEN_CM__din",
                "DIN_DEM"
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
      }
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go_reset_button <- renderUI({
      button_go_reset("I_APME_DEM_AUTOR_CRITR_ETEN_CM")
    })
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__save_button <- renderUI({
      button_save("I_APME_DEM_AUTOR_CRITR_ETEN_CM", I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt())
    })

    ## Datatable ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go, {
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- TRUE  # Afficher la table si on clique sur Exécuter
    }, ignoreInit = TRUE)
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data, {
      # Faire disparaitre la table si on change le data
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- FALSE
    }, ignoreInit = TRUE)
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt <- eventReactive(
      c(
        input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go,
        I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab
      ),
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
              dt <- search_value_num(
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

    ## Export ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__save <- downloadHandler(
      filename = function() {
        paste0(
          input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__savename, ".",
          input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__saveext
        )
      },
      content = function(file) {
        if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__saveext == "xlsx") {
          writexl::write_xlsx(I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt(), file)
        } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__saveext == "csv") {
          write.csv2(I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt(), file, row.names = FALSE,
                     fileEncoding = "latin1")
        }
      }
    )

    ## Update buttons ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__reset, {
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- FALSE  # faire disparaître la table
      # Remettre les valeurs initiales
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__denom", value = "")
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__din", value = "")
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__search", value = "")
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__typeRecherche",
                          selected = "keyword")
      }
    }, ignoreInit = TRUE)




    # V_DEM_PAIMT_MED_CM ----------------------------------------------------
    V_DEM_PAIMT_MED_CM__val <- reactiveValues(
      show_tab = FALSE
    )

    ## Fiche Technique ####
    output$V_DEM_PAIMT_MED_CM__varDesc <- renderTable({
      return(inesss::domaine_valeurs_fiche_technique$V_DEM_PAIMT_MED_CM$tab_desc)
    }, sanitize.text.function = identity)

    ## UI ####
    output$V_DEM_PAIMT_MED_CM__descChoices <- renderUI({
      varSelect <- c(
        input$V_DEM_PAIMT_MED_CM__varSelect1,
        input$V_DEM_PAIMT_MED_CM__varSelect2,
        input$V_DEM_PAIMT_MED_CM__varSelect3,
        input$V_DEM_PAIMT_MED_CM__varSelect4
      )
      choices_vec <- vector("character", 7)
      if (any(c("AHFS", "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA") %in% varSelect)) {
        choices_vec[1] <- "AHFS"
      }
      if (any(paste0("COD_SERV_", 1:3) %in% varSelect)) {
        choices_vec[2] <- "COD_SERV"
      }
      if (any("COD_STA_DECIS" == varSelect)) {
        choices_vec[3] <- "COD_STA_DECIS"
      }
      if (any("DENOM" == varSelect)) {
        choices_vec[4] <- "DENOM"
      }
      if (any("FORME" == varSelect)) {
        choices_vec[5] <- "FORME"
      }
      if (any("DIN" == varSelect)) {
        choices_vec[6] <- "MARQ_COMRC"
      }
      if (any("TENEUR" %in% varSelect)) {
        choices_vec[7] <- "TENEUR"
      }
      return(tagList(
        selectInput(
          "V_DEM_PAIMT_MED_CM__varSelectDesc", "Variables Descriptives",
          choices = choices_vec[choices_vec != ""],
          multiple = TRUE
        )
      ))
    })
    output$V_DEM_PAIMT_MED_CM__params <- renderUI({
      varSelect <- c(
        input$V_DEM_PAIMT_MED_CM__varSelect1,
        input$V_DEM_PAIMT_MED_CM__varSelect2,
        input$V_DEM_PAIMT_MED_CM__varSelect3,
        input$V_DEM_PAIMT_MED_CM__varSelect4
      )
      varSelectDesc <- c(
        input$V_DEM_PAIMT_MED_CM__varSelectDesc
      )

      # Périodes - Début & Fin
      minDebut <- min(inesss::V_DEM_PAIMT_MED_CM$DATE_DEBUT)
      maxDebut <- max(inesss::V_DEM_PAIMT_MED_CM$DATE_DEBUT)
      minFin <- min(inesss::V_DEM_PAIMT_MED_CM$DATE_FIN)
      maxFin <- max(inesss::V_DEM_PAIMT_MED_CM$DATE_FIN)
      periods_debut_fin <- fluidRow(
        column(
          width = 3,
          numericInput(
            "V_DEM_PAIMT_MED_CM__debutAnnee", "Début Période - Année",
            value = as.integer(str_sub(minDebut, 1, 4)),
            min = as.integer(str_sub(minDebut, 1, 4)),
            max = as.integer(str_sub(maxDebut, 1, 4))
          )
        ),
        column(
          width = 3,
          numericInput(
            "V_DEM_PAIMT_MED_CM__debutMois", "Début Période - Mois",
            value = as.integer(str_sub(minDebut, 6, 7)),
            min = as.integer(str_sub(minDebut, 6, 7)),
            max = as.integer(str_sub(maxDebut, 6, 7))
          )
        ),
        column(
          width = 3,
          numericInput(
            "V_DEM_PAIMT_MED_CM__finAnnee", "Fin Période - Année",
            value = as.integer(str_sub(maxFin, 1, 4)),
            min = as.integer(str_sub(maxFin, 1, 4)),
            max = as.integer(str_sub(maxFin, 1, 4))
          )
        ),
        column(
          width = 3,
          numericInput(
            "V_DEM_PAIMT_MED_CM__finMois", "Fin Période - Mois",
            value = as.integer(str_sub(maxFin, 6, 7)),
            min = as.integer(str_sub(maxFin, 6, 7)),
            max = as.integer(str_sub(maxFin, 6, 7))
          )
        )
      )

      # DENOM
      if ("DENOM" %in% varSelect && "DENOM" %in% varSelectDesc) {
        denom <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__denom", "DENOM")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__denomDesc", "NOM_DENOM")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__denomTypeRecherche",
              "NOM_DENOM Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if ("DENOM" %in% varSelect) {
        denom <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__denom", "DENOM")
          )
        )
      } else {
        denom <- NULL
      }

      # DIN
      if (any("DIN" == varSelect) && any("MARQ_COMRC" == varSelectDesc)) {
        din <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__din", "DIN")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__MarqComrc", "NOM_MARQ_COMRC")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__marqComrcTypeRecherche",
              "NOM_MARQ_COMRC Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if (any("DIN" == varSelect)) {
        din <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__din", "DIN")
          )
        )
      } else {
        din <- NULL
      }

      # AHFS
      if (any("AHFS" == varSelect) && any("AHFS" == varSelectDesc)) {
        ahfs <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfs", "AHFS")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsDesc", "NOM_AHFS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__nomAhfsTypeRecherche",
              "NOM_AHFS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if (any("AHFS" == varSelect)) {
        ahfs <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfs", "AHFS")
          )
        )
      } else {
        ahfs <- NULL
      }

      # AHFS CLA
      if (any("AHFS_CLA" == varSelect) && !any("AHFS" == varSelect) && any("AHFS" == varSelectDesc)) {
        ahfs_cla <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsCla", "AHFS_CLA")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsDesc", "NOM_AHFS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__nomAhfsTypeRecherche",
              "NOM_AHFS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if (any("AHFS_CLA" == varSelect)) {
        ahfs_cla <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsCla", "AHFS_CLA")
          )
        )
      } else {
        ahfs_cla <- NULL
      }

      # AHFS SCLA
      if (any("AHFS_SCLA" == varSelect) && !any(c("AHFS", "AHFS_CLA") %in% varSelect) && any("AHFS" == varSelectDesc)) {
        ahfs_scla <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsScla", "AHFS_SCLA")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsDesc", "NOM_AHFS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__nomAhfsTypeRecherche",
              "NOM_AHFS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if (any("AHFS_SCLA" == varSelect)) {
        ahfs_scla <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsScla", "AHFS_SCLA")
          )
        )
      } else {
        ahfs_scla <- NULL
      }

      # AHFS SSCLA
      if (any("AHFS_SSCLA" == varSelect) && !any(c("AHFS", "AHFS_CLA", "AHFS_SCLA") %in% varSelect) && any("AHFS" == varSelectDesc)) {
        ahfs_sscla <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsSscla", "AHFS_SSCLA")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsDesc", "NOM_AHFS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__nomAhfsTypeRecherche",
              "NOM_AHFS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if (any("AHFS_SSCLA" == varSelect)) {
        ahfs_sscla <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__ahfsSscla", "AHFS_SSCLA")
          )
        )
      } else {
        ahfs_sscla <- NULL
      }

      # FORME
      if ("FORME" %in% varSelect && "FORME" %in% varSelectDesc) {
        forme <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__forme", "FORME")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__formeDesc", "NOM_FORME")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__nomFormeTypeRecherche",
              "NOM_FORME Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if ("FORME" %in% varSelect) {
        forme <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__forme", "FORME")
          )
        )
      } else {
        forme <- NULL
      }

      # TENEUR
      if ("TENEUR" %in% varSelect && "TENEUR" %in% varSelectDesc) {
        teneur <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__teneur", "TENEUR")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__teneurDesc", "NOM_TENEUR")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__nomTeneurTypeRecherche",
              "NOM_TENEUR Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if ("TENEUR" %in% varSelect) {
        teneur <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__teneur", "TENEUR")
          )
        )
      } else {
        teneur <- NULL
      }

      # COD_SERV_[1:3]
      for (i in 1:3) {
        if (paste0("COD_SERV_", i) %in% varSelect && "COD_SERV" %in% varSelectDesc) {
          cs <- fluidRow(
            column(
              width = 3,
              textInput(paste0("V_DEM_PAIMT_MED_CM__codServ", i), paste0("COD_SERV_", i))
            ),
            column(
              width = 3,
              textInput(paste0("V_DEM_PAIMT_MED_CM__codServDesc", i), paste0("NOM_COD_SERV_", i))
            ),
            column(
              width = 3,
              selectInput(
                paste0("V_DEM_PAIMT_MED_CM__codServDesc", i),
                paste0("NOM_COD_SERV_", i, " Type Recherche"),
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        } else if (paste0("COD_SERV_", i) %in% varSelect) {
          cs <- fluidRow(
            column(
              width = 3,
              textInput(paste0("V_DEM_PAIMT_MED_CM__codServ", i), paste0("COD_SERV_", i))
            )
          )
        } else {
          cs <- NULL
        }
        assign(paste0("cod_serv_", i), cs)
      }

      # COD_STA_DECIS
      if ("COD_STA_DECIS" %in% varSelect && "COD_STA_DECIS" %in% varSelectDesc) {
        cod_sta_decis <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__codStaDecis", "COD_STA_DECIS")
          ),
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__codStaDecisDesc", "NOM_COD_STA_DECIS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DEM_PAIMT_MED_CM__codStaDecisTypeRecherche",
              "NOM_COD_STA_DECIS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      } else if ("COD_STA_DECIS" %in% varSelect) {
        cod_sta_decis <- fluidRow(
          column(
            width = 3,
            textInput("V_DEM_PAIMT_MED_CM__codStaDecis", "COD_STA_DECIS")
          )
        )
      } else {
        cod_sta_decis <- NULL
      }

      return(tagList(
        periods_debut_fin,
        denom, din,
        ahfs, ahfs_cla, ahfs_scla, ahfs_sscla,
        forme, teneur,
        cod_serv_1, cod_serv_2, cod_serv_3,
        cod_sta_decis
      ))
    })
    output$V_DEM_PAIMT_MED_CM__go_reset_button <- renderUI({
      varSelect <- c(
        input$V_DEM_PAIMT_MED_CM__varSelect1,
        input$V_DEM_PAIMT_MED_CM__varSelect2,
        input$V_DEM_PAIMT_MED_CM__varSelect3,
        input$V_DEM_PAIMT_MED_CM__varSelect4
      )
      if (is.null(varSelect)) {
        return(NULL)
      } else {
        return(button_go_reset("V_DEM_PAIMT_MED_CM"))
      }
    })

    ## Datatable ####
    observeEvent(input$V_DEM_PAIMT_MED_CM__go, { V_DEM_PAIMT_MED_CM__val$show_tab <- TRUE })
    V_DEM_PAIMT_MED_CM__dt <- eventReactive(
      input$V_DEM_PAIMT_MED_CM__go,
      {
        if (V_DEM_PAIMT_MED_CM__val$show_tab) {
          varSelect <- c(
            input$V_DEM_PAIMT_MED_CM__varSelect1,
            input$V_DEM_PAIMT_MED_CM__varSelect2,
            input$V_DEM_PAIMT_MED_CM__varSelect3,
            input$V_DEM_PAIMT_MED_CM__varSelect4,
            "DATE_DEBUT", "DATE_FIN"
          )
          dt <- unique(inesss::V_DEM_PAIMT_MED_CM[, ..varSelect])
          by_vars <- varSelect[!varSelect %in% c("DATE_DEBUT", "DATE_FIN")]
          # Périodes détaillées
          if (!input$V_DEM_PAIMT_MED_CM__periodDetail) {
            dt <- dt[
              , .(PremierePrescription = min(DATE_DEBUT),
                  DernierePrescription = max(DATE_FIN)),
              keyby = by_vars
            ]
          }
          setkey(dt)

          # DENOM
          if (any("DENOM" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__denom != "") {
              dt <- search_value_chr(
                dt, col = "DENOM",
                values = input$V_DEM_PAIMT_MED_CM__denom, pad = 5
              )
            }
          }
          # DIN
          if (any("DIN" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__din != "") {
              dt <- search_value_chr(
                dt, col = "DIN",
                values = input$V_DEM_PAIMT_MED_CM__din
              )
            }
          }
          # AHFS
          if (any("AHFS" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__ahfs != "") {
              dt <- search_value_chr(
                dt, col = "AHFS",
                values = input$V_DEM_PAIMT_MED_CM__ahfs, pad = 6
              )
            }
          }
          # AHFS_CLA
          if (any("AHFS_CLA" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__ahfsCla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_CLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsCla, pad = 2
              )
            }
          }
          # AHFS_SCLA
          if (any("AHFS_SCLA" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__ahfsScla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_SCLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsScla, pad = 2
              )
            }
          }
          # AHFS_SSCLA
          if (any("AHFS_SSCLA" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__ahfsSscla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_SSCLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsSscla, pad = 2
              )
            }
          }
          # FORME
          if (any("FORME" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__forme != "") {
              dt <- search_value_chr(
                dt, col = "FORME",
                values = input$V_DEM_PAIMT_MED_CM__forme, pad = 5
              )
            }
          }
          # TENEUR
          if (any("TENEUR" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__teneur != "") {
              dt <- search_value_chr(
                dt, col = "TENEUR",
                values = input$V_DEM_PAIMT_MED_CM__teneur, pad = 5
              )
            }
          }
          # COD_SERV_[1:3]
          for (i in 1:3) {
            if (any(paste0("COD_SERV_", i) == names(dt))) {
              if (input[[paste0("V_DEM_PAIMT_MED_CM__codServ", i)]] != "") {
                dt <- search_value_chr(
                  dt, col = paste0("COD_SERV_", i),
                  values = input[[paste0("V_DEM_PAIMT_MED_CM__codServ", i)]]
                )
              }
            }
          }
          # COD_STA_DECIS
          if (any("COD_STA_DECIS" == names(dt))) {
            if (input$V_DEM_PAIMT_MED_CM__codStaDecis != "") {
              dt <- search_value_chr(
                dt, col = "COD_STA_DECIS",
                values = input$V_DEM_PAIMT_MED_CM__codStaDecis
              )
            }
          }

          # Regrouper les périodes qui se chevauchent dans le temps
          if (input$V_DEM_PAIMT_MED_CM__periodDetail) {
            dt <- fusion_periodes(dt, "DATE_DEBUT", "DATE_FIN", by_vars, 1L)
          }

          return(dt)
        } else {
          return(NULL)
        }
      }
    )
    output$V_DEM_PAIMT_MED_CM__dt <- renderDataTable({
      V_DEM_PAIMT_MED_CM__dt()
    }, options = renderDataTable_options())




    # CIM ------------------------------------------------------------------
    CIM__val <- reactiveValues(
      show_tab = FALSE
    )

    ## UI ####
    output$CIM__params <- renderUI({
      if (input$CIM__data %in% c("CIM9", "CIM10")) {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("CIM__code", input$CIM__data),
            ),
            column(
              width = ui_col_width(),
              textInput("CIM__diagn", "DIAGNOSTIC")
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "CIM__diagnTypeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        ))
      } else if (input$CIM__data == "Correspondance") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("CIM__code9", "CIM9"),
              textInput("CIM__code10", "CIM10")
            ),
            column(
              width = ui_col_width(),
              textInput("CIM__diagn9", "DIAGNOSTIC_CIM9"),
              textInput("CIM__diagn10", "DIAGNOSTIC_CIM10")
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "CIM__diagn9TypeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              ),
              selectInput(
                "CIM__diagn10TypeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        ))
      }
    })
    output$CIM__go_reset_button <- renderUI({ button_go_reset("CIM") })
    output$CIM__save_button <- renderUI({ button_save("CIM", CIM__dt()) })

    ## Datatable ####
    observeEvent(input$CIM__go, { CIM__val$show_tab <- TRUE })
    observeEvent(input$CIM__data, { CIM__val$show_tab <- FALSE })
    CIM__dt <- eventReactive(
      c(input$CIM__go, CIM__val$show_tab),
      {
        if (CIM__val$show_tab) {
          dt <- inesss::CIM[[input$CIM__data]]
          if (input$CIM__data %in% c("CIM9", "CIM10")) {
            # Code
            if (input$CIM__code != "") {
              dt <- search_value_chr(
                dt, col = input$CIM__data,
                values = input$CIM__code
              )
            }
            # Diagnostic
            if (input$CIM__diagn != "") {
              if (input$CIM__diagnTypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "DIAGNOSTIC",
                  values = input$CIM__diagn
                )
              } else if (input$CIM__diagnTypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "DIAGNOSTIC",
                  values = input$CIM_diagn
                )
              }
            }
          } else if (input$CIM__data == "Correspondance") {
            # CIM9
            if (input$CIM__code9 != "") {
              dt <- search_value_chr(
                dt, col = "CIM9",
                values = input$CIM__code9
              )
            }
            # CIM10
            if (input$CIM__code10 != "") {
              dt <- search_value_chr(
                dt, col = "CIM10",
                values = input$CIM__code10
              )
            }
            # DIAGNOSTIC_CIM9
            if (input$CIM__diagn9 != "") {
              if (input$CIM__diagn9TypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "DIAGNOSTIC_CIM9",
                  values = input$CIM__diagn9
                )
              } else if (input$CIM__diagn9TypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "DIAGNOSTIC_CIM9",
                  values = input$CIM__diagn9
                )
              }
            }
            # DIAGNOSTIC_CIM10
            if (input$CIM__diagn10 != "") {
              if (input$CIM__diagn10TypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "DIAGNOSTIC_CIM10",
                  values = input$CIM__diagn10
                )
              } else if (input$CIM__diagn10TypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "DIAGNOSTIC_CIM10",
                  values = input$CIM__diagn10
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
    output$CIM__dt <- renderDataTable(CIM__dt(), options = renderDataTable_options())

    ## Export ####
    output$CIM__save <- downloadHandler(
      filename = function() {
        paste0(input$CIM__savename, ".", input$CIM__saveext)
      },
      content = function(file) {
        if (input$CIM__saveext == "xlsx") {
          writexl::write_xlsx(CIM__dt(), file)
        } else if (input$CIM__saveext == "csv") {
          write.csv2(CIM__dt(), file, row.names = FALSE,
                     fileEncoding = "latin1")
        }
      }
    )

    ## Update Buttons ####
    observeEvent(input$CIM__reset, {
      CIM__val$show_tab <- FALSE
      if (input$CIM__data %in% c("CIM9", "CIM10")) {
        updateTextInput(session, "CIM__code", value = "")
        updateTextInput(session, "CIM__diagn", value = "")
      } else if (input$CIM__data == "Correspondance") {
        updateTextInput(session, "CIM__code9", value = "")
        updateTextInput(session, "CIM__code10", value = "")
        updateTextInput(session, "CIM__diagn9", value = "")
        updateTextInput(session, "CIM__diagn10", value = "")
      }
    })





    # V_CLA_AHF ------------------------------------------------------------
    V_CLA_AHF__val <- reactiveValues(
      show_tab = FALSE  # afficher la table ou pas
    )

    ## Fiche Technique ####
    output$V_CLA_AHF__varDesc <- renderTable({
      return(inesss::domaine_valeurs_fiche_technique$V_CLA_AHF$tab_desc)
    })

    ## UI ####
    output$V_CLA_AHF__params <- renderUI({
      return(tagList(
        fluidRow(
          column(
            width = ui_col_width(),
            textInput("V_CLA_AHF__ahfsCla", "AHFS_CLA"),
            textInput("V_CLA_AHF__nomAhfs", "NOM_AHFS"),
            textInput("V_CLA_AHF__nomAnglaisAhfs", "NOM_ANGLAIS_AHFS")
          ),
          column(
            width = ui_col_width(),
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
            width = ui_col_width(),
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

    ## Datatable ####
    observeEvent(input$V_CLA_AHF__go, {
      V_CLA_AHF__val$show_tab <- TRUE  # Afficher la table si on clique sur Exécuter
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

    ## Export ####
    output$V_CLA_AHF__save <- downloadHandler(
      filename = function() {
        paste0(
          input$V_CLA_AHF__savename, ".",
          input$V_CLA_AHF__saveext
        )
      },
      content = function(file) {
        if (input$V_CLA_AHF__saveext == "xlsx") {
          writexl::write_xlsx(V_CLA_AHF__dt(), file)
        } else if (input$V_CLA_AHF__saveext == "csv") {
          write.csv2(V_CLA_AHF__dt(), file, row.names = FALSE,
                     fileEncoding = "latin1")
        }
      }
    )

    ## Update Buttons ####
    observeEvent(input$V_CLA_AHF__reset, {
      V_CLA_AHF__val$show_tab <- FALSE
      updateTextInput(session, "V_CLA_AHF__ahfsCla", value = "")
      updateTextInput(session, "V_CLA_AHF__ahfsScla", value = "")
      updateTextInput(session, "V_CLA_AHF__ahfsSscla", value = "")
      updateTextInput(session, "V_CLA_AHF__nomAhfs", value = "")
      updateTextInput(session, "V_CLA_AHF__nomAnglaisAhfs", value = "")
    })


    # V_DENOM_COMNE_MED ---------------------------------------------------------------------------
    V_DENOM_COMNE_MED__val <- reactiveValues(
      show_tab = FALSE
    )

    ## Fiche Technique ####
    output$V_DENOM_COMNE_MED__varDesc <- renderTable({
      return(inesss::domaine_valeurs_fiche_technique$V_DENOM_COMNE_MED$tab_desc)
    })

    ## UI ####
    output$V_DENOM_COMNE_MED__params <- renderUI({
      return(tagList(
        fluidRow(
          column(
            width = 3,
            textInput("V_DENOM_COMNE_MED__denom", "DENOM")
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput("V_DENOM_COMNE_MED__nomDenom", "NOM_DENOM"),
            textInput("V_DENOM_COMNE_MED__nomAnglais", "NOM_DENOM_ANGLAIS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DENOM_COMNE_MED__denomTypeRecherche",
              "NOM_DENOM Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            ),
            selectInput(
              "V_DENOM_COMNE_MED__anglaisTypeRecherche",
              "NOM_DENOM_ANGLAIS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          ),
          column(
            width = 3,
            textInput("V_DENOM_COMNE_MED__synDenom", "NOM_DENOM_SYNON"),
            textInput("V_DENOM_COMNE_MED__synAnglais", "NOM_DENOM_SYNON_ANGLAIS")
          ),
          column(
            width = 3,
            selectInput(
              "V_DENOM_COMNE_MED__synTypeRecherche",
              "NOM_DENOM_SYNON Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            ),
            selectInput(
              "V_DENOM_COMNE_MED__synAnglaisTypeRecherche",
              "NOM_DENOM_SYNON_ANGLAIS Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput("V_DENOM_COMNE_MED__debut", "Début période - Année")
          ),
          column(
            width = 3,
            textInput("V_DENOM_COMNE_MED__fin", "Fin période - Année")
          )
        )
      ))
    })
    output$V_DENOM_COMNE_MED__go_reset_button <- renderUI({
      button_go_reset("V_DENOM_COMNE_MED")
    })
    output$V_DENOM_COMNE_MED__save_button <- renderUI({
      button_save("V_DENOM_COMNE_MED", V_DENOM_COMNE_MED__dt())
    })

    ## Datatable ####
    observeEvent(input$V_DENOM_COMNE_MED__go, {
      V_DENOM_COMNE_MED__val$show_tab <- TRUE  # Afficher la table si on clique sur Exécuter
    }, ignoreInit = TRUE)
    V_DENOM_COMNE_MED__dt <- eventReactive(
      c(input$V_DENOM_COMNE_MED__go, V_DENOM_COMNE_MED__val$show_tab),
      {
        if (V_DENOM_COMNE_MED__val$show_tab) {
          dt <- inesss::V_DENOM_COMNE_MED
          # DENOM
          if (input$V_DENOM_COMNE_MED__denom != "") {
            dt <- search_value_chr(
              dt, col = "DENOM",
              values = input$V_DENOM_COMNE_MED__denom, pad = 5
            )
          }
          # NOM_DENOM
          if (input$V_DENOM_COMNE_MED__nomDenom != "") {
            if (input$V_DENOM_COMNE_MED__denomTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_DENOM",
                values = input$V_DENOM_COMNE_MED__nomDenom
              )
            } else if (input$V_DENOM_COMNE_MED__denomTypeRecherche == "keyword") {
              dt <- search_value_chr(
                dt, col = "NOM_DENOM",
                values = input$V_DENOM_COMNE_MED__nomDenom
              )
            }
          }
          # NOM_DENOM_SYNON
          if (input$V_DENOM_COMNE_MED__synDenom != "") {
            if (input$V_DENOM_COMNE_MED__synTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_DENOM_SYNON",
                values = input$V_DENOM_COMNE_MED__synDenom
              )
            } else if (input$V_DENOM_COMNE_MED__synTypeRecherche == "keyword") {
              dt <- search_value_chr(
                dt, col = "NOM_DENOM_SYNON",
                values = input$V_DENOM_COMNE_MED__synDenom
              )
            }
          }
          # NOM_DENOM_ANGLAIS
          if (input$V_DENOM_COMNE_MED__nomAnglais != "") {
            if (input$V_DENOM_COMNE_MED__anglaisTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_DENOM_ANGLAIS",
                values = input$V_DENOM_COMNE_MED__nomAnglais
              )
            } else if (input$V_DENOM_COMNE_MED__anglaisTypeRecherche == "keyword") {
              dt <- search_value_chr(
                dt, col = "NOM_DENOM_ANGLAIS",
                values = input$V_DENOM_COMNE_MED__nomAnglais
              )
            }
          }
          # NOM_DENOM_SYNON_ANGLAIS
          if (input$V_DENOM_COMNE_MED__synAnglais != "") {
            if (input$V_DENOM_COMNE_MED__synAnglaisTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_DENOM_SYNON_ANGLAIS",
                values = input$V_DENOM_COMNE_MED__synAnglais
              )
            } else if (input$V_DENOM_COMNE_MED__synAnglaisTypeRecherche == "keyword") {
              dt <- search_value_chr(
                dt, col = "NOM_DENOM_SYNON_ANGLAIS",
                values = input$V_DENOM_COMNE_MED__synAnglais
              )
            }
          }
          # Période d'étude
          if (input$V_DENOM_COMNE_MED__debut != "" && input$V_DENOM_COMNE_MED__fin != "") {
            deb <- as.integer(input$V_DENOM_COMNE_MED__debut)
            fin <- as.integer(input$V_DENOM_COMNE_MED__fin)
            dt <- dt[deb <= year(DATE_FIN) & fin >= year(DATE_DEBUT)]
          } else if (input$V_DENOM_COMNE_MED__debut != "" || input$V_DENOM_COMNE_MED__fin != "") {
            deb <- as.integer(input$V_DENOM_COMNE_MED__debut)
            dt <- dt[year(DATE_DEBUT) <= deb & deb <= year(DATE_FIN)]
          } else if (input$V_DENOM_COMNE_MED__fin != "") {
            fin <- as.integer(input$V_DENOM_COMNE_MED__fin)
            dt <- dt[year(DATE_DEBUT) <= fin & fin <= year(DATE_FIN)]
          }
          return(dt)
        } else {
          return(NULL)
        }
      }
    )
    output$V_DENOM_COMNE_MED__dt <- renderDataTable({
      V_DENOM_COMNE_MED__dt()
    }, options = renderDataTable_options())

    ## Export ####
    output$V_DENOM_COMNE_MED__save <- downloadHandler(
      filename = function() {
        paste0(
          input$V_DENOM_COMNE_MED__savename, ".",
          input$V_DENOM_COMNE_MED__saveext
        )
      },
      content = function(file) {
        if (input$V_DENOM_COMNE_MED__saveext == "xlsx") {
          writexl::write_xlsx(V_DENOM_COMNE_MED__dt(), file)
        } else if (input$V_DENOM_COMNE_MED__saveext == "csv") {
          write.csv2(V_DENOM_COMNE_MED__dt(), file, row.names = FALSE,
                     fileEncoding = "latin1")
        }
      }
    )

    ## Update Buttons ####
    observeEvent(input$V_DENOM_COMNE_MED__reset, {
      V_DENOM_COMNE_MED__val$show_tab <- FALSE
      updateTextInput(session, "V_DENOM_COMNE_MED__denom", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__nomDenom", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__nomAnglais", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__synDenom", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__synAnglais", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__debut", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__fin", value = "")
    })

    ## Erreurs possibles ####
    observeEvent(
      eventExpr = c(input$V_DENOM_COMNE_MED__debut, input$V_DENOM_COMNE_MED__fin),
      handlerExpr = {
        if (input$V_DENOM_COMNE_MED__debut != "" && input$V_DENOM_COMNE_MED__fin != "") {
          debut <- as.integer(input$V_DENOM_COMNE_MED__debut)
          fin <- as.integer(input$V_DENOM_COMNE_MED__fin)
          if (debut > fin) {
            updateTextInput(session, "V_DENOM_COMNE_MED__debut", value = input$V_DENOM_COMNE_MED__fin)
          }
        }
      }
    )





    # V_FORM_MED ####
    V_FORM_MED__val <- reactiveValues(
      show_tab = FALSE
    )

    ## Fiche Technique ####

    ## UI ####
    output$V_FORM_MED__params <- renderUI({
      return(tagList(
        fluidRow(
          column(
            width = ui_col_width(),
            textInput("V_FORM_MED__code", "COD_FORME")
          ),
          column(
            width = ui_col_width(),
            selectInput(
              "V_FORM_MED__codTypForme", "COD_TYP_FORME",
              choices = V_FORM_MED__menuCodTypForme,
              selected = NULL, multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = ui_col_width(),
            textInput("V_FORM_MED__nom", "NOM_FORME")
          ),
          column(
            width = ui_col_width(),
            selectInput(
              "V_FORM_MED__nomTypeRecherche", "Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        ),
        fluidRow(
          column(
            width = ui_col_width(),
            textInput("V_FORM_MED__nomAbr", "NOM_FORME_ABR")
          ),
          column(
            width = ui_col_width(),
            selectInput(
              "V_FORM_MED__nomAbrTypeRecherche", "Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        ),
        fluidRow(
          column(
            width = ui_col_width(),
            textInput("V_FORM_MED__nomAngl", "NOM_ANGL_FORME")
          ),
          column(
            width = ui_col_width(),
            selectInput(
              "V_FORM_MED__nomAnglTypeRecherche", "Type Recherche",
              choices = c("Mot-clé" = "keyword",
                          "Valeur exacte" = "exactWord"),
              selected = "Mot-clé"
            )
          )
        )
      ))
    })
    output$V_FORM_MED__go_reset_button <- renderUI({
      button_go_reset("V_FORM_MED")
    })
    output$V_FORM_MED__save_button <- renderUI({
      button_save("V_FORM_MED", V_FORM_MED__dt())
    })

    ## Datatable ####
    observeEvent(input$V_FORM_MED__go, {
      V_FORM_MED__val$show_tab <- TRUE
    }, ignoreInit = TRUE)
    V_FORM_MED__dt <- eventReactive(
      c(input$V_FORM_MED__go, V_FORM_MED__val$show_tab),
      {
        if (V_FORM_MED__val$show_tab) {
          dt <- inesss::V_FORME_MED
          # Code Forme
          if (input$V_FORM_MED__code != "") {
            dt <- search_value_chr(
              dt, col = "COD_FORME",
              values = input$V_FORM_MED__code, pad = 5
            )
          }
          # Code Type Forme
          if (!is.null(input$V_FORM_MED__codTypForme)) {
            dt <- dt[paste0(COD_TYP_FORME, " - ", NOM_TYPE_FORME) %in% input$V_FORM_MED__codTypForme]
          }
          # Nom Forme
          if (input$V_FORM_MED__nom != "") {
            if (input$V_FORM_MED__nomTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_FORME",
                values = input$V_FORM_MED__nom
              )
            } else if (input$V_FORM_MED__nomTypeRecherche == "exactWord") {
              dt <- search_value_chr(
                dt, col = "NOM_FORME",
                values = input$V_FORM_MED__nom
              )
            }
          }
          # Nom Forme Abbrégé
          if (input$V_FORM_MED__nomAbr != "") {
            if (input$V_FORM_MED__nomAbrTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_FORME_ABR",
                values = input$V_FORM_MED__nomAbr
              )
            } else if (input$V_FORM_MED__nomAbrTypeRecherche == "exactWord") {
              dt <- search_value_chr(
                dt, col = "NOM_FORME_ABR",
                values = input$V_FORM_MED__nomAbr
              )
            }
          }
          # Nom Forme Anglais
          if (input$V_FORM_MED__nomAngl != "") {
            if (input$V_FORM_MED__nomAnglTypeRecherche == "keyword") {
              dt <- search_keyword(
                dt, col = "NOM_ANGL_FORME",
                values = input$V_FORM_MED__nomAngl
              )
            } else if (input$V_FORM_MED__nomAnglTypeRecherche == "exactWord") {
              dt <- search_value_chr(
                dt, col = "NOM_ANGL_FORME",
                values = input$V_FORM_MED__nomAngl
              )
            }
          }
          return(dt)
        } else {
          return(NULL)
        }
      }
    )
    output$V_FORM_MED__dt <- renderDataTable({
      V_FORM_MED__dt()
    }, options = renderDataTable_options())

    ## Export ####
    output$V_FORM_MED__save <- downloadHandler(
      filename = function() {
        paste0(
          input$V_FORM_MED__savename, ".",
          input$V_FORM_MED__saveext
        )
      },
      content = function(file) {
        if (input$V_FORM_MED__saveext == "xlsx") {
          writexl::write_xlsx(V_FORM_MED__dt(), file)
        } else if (input$V_FORM_MED__saveext == "csv") {
          write.csv2(V_FORM_MED__dt(), file, row.names = FALSE,
                     fileEncoding = "latin1")
        }
      }
    )

    ## Update Buttons ####
    observeEvent(input$V_FORM_MED__reset, {
      V_FORM_MED__val$show_tab <- FALSE
      updateTextInput(session, "V_FORM_MED__code", value = "")
      updateSelectInput(session, "V_FORM_MED__codTypForme",
                        choices = V_FORM_MED__menuCodTypForme,
                        selected = NULL)
      updateTextInput(session, "V_FORM_MED__nom", value = "")
      updateTextInput(session, "V_FORM_MED__nomAngl", value = "")
      updateTextInput(session, "V_FORM_MED__nomAbr", value = "")
    })





    # V_PRODU_MED ####
    V_PRODU_MED__val <- reactiveValues(
      show_tab = FALSE
    )

    ## Fiche Technique ####
    output$V_PRODU_MED__varDesc <- renderTable({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        return(inesss::domaine_valeurs_fiche_technique$V_PRODU_MED$NOM_MARQ_COMRC$tab_desc)
      } else {
        return(NULL)
      }
    })
    output$V_PRODU_MED__footnote <- renderUI({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        return(em("Les combinaisons DENOM-DIN où la période DEBUT-FIN se juxtaposent sont regroupées en une période continue."))
      } else {
        return(NULL)
      }
    })

    ## Descriptif Data ####
    output$V_PRODU_MED__dataDesc <- renderUI({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Description courte complète de l'indication reconnue de Patient-Médicament d'exceptions.")
            )
          )
        ))
      }
    })

    ## UI ####
    output$V_PRODU_MED__params <- renderUI({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_PRODU_MED__denom", "DENOM")
            ),
            column(
              width = ui_col_width(),
              textInput("V_PRODU_MED__din", "DIN")
            )
          ),
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_PRODU_MED__nomMarqComrc", "NOM_MARC_COMRC")
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "V_PRODU_MED__nomMarqComrcTypeRecherche",
                "NOM_MARQ_COMRC Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-Clé"
              )
            )
          )
        ))
      }
    })
    output$V_PRODU_MED__go_reset_button <- renderUI({
      button_go_reset("V_PRODU_MED")
    })
    output$V_PRODU_MED__save_button <- renderUI({
      button_save("V_PRODU_MED", V_PRODU_MED__dt())
    })

    ## Datatable ####
    observeEvent(input$V_PRODU_MED__go, {
      V_PRODU_MED__val$show_tab <- TRUE  # Afficher la table si on clique sur Exécuter
    }, ignoreInit = TRUE)
    observeEvent(input$V_PRODU_MED__data, {
      V_PRODU_MED__val$show_tab <- FALSE  # Faire disparaitre la table si on change le data
    }, ignoreInit = TRUE)
    V_PRODU_MED__dt <- eventReactive(
      c(input$V_PRODU_MED__go, V_PRODU_MED__val$show_tab),
      {
        if (V_PRODU_MED__val$show_tab) {
          dt <- inesss::V_PRODU_MED[[input$V_PRODU_MED__data]]
          if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
            # DENOM
            if (input$V_PRODU_MED__denom != "") {
              dt <- search_value_chr(
                dt, col = "DENOM",
                values = input$V_PRODU_MED__denom, pad = 5
              )
            }
            # DIN
            if (input$V_PRODU_MED__din != "") {
              dt <- search_value_chr(
                dt, col = "DIN",
                values = input$V_PRODU_MED__din
              )
            }
            # NOM_MARQ_COMRC
            if (input$V_PRODU_MED__nomMarqComrc != "") {
              if (input$V_PRODU_MED__nomMarqComrcTypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "NOM_MARQ_COMRC",
                  values = input$V_PRODU_MED__nomMarqComrc
                )
              } else if (input$V_PRODU_MED__nomMarqComrcTypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "NOM_MARQ_COMRC",
                  values = input$V_PRODU_MED__nomMarqComrc
                )
              }
            }
          }
          return(dt)
        }
      }
    )
    output$V_PRODU_MED__dt <- renderDataTable({
      V_PRODU_MED__dt()
    }, options = renderDataTable_options())

    ## Export ####
    output$V_PRODU_MED__save <- downloadHandler(
      filename = function() {
        paste0(
          input$V_PRODU_MED__savename, ".",
          input$V_PRODU_MED__saveext
        )
      },
      content = function(file) {
        if (input$V_PRODU_MED__saveext == "xlsx") {
          writexl::write_xlsx(V_PRODU_MED__dt(), file)
        } else if (input$V_PRODU_MED__saveext == "csv") {
          write.csv2(V_PRODU_MED__dt(), file, row.names = FALSE,
                     fileEncoding = "latin1")
        }
      }
    )

    ## Update buttons ####
    observeEvent(input$V_PRODU_MED__reset, {
      V_PRODU_MED__val$show_tab <- FALSE
      updateTextInput(session, "V_PRODU_MED__denom", value = "")
      updateTextInput(session, "V_PRODU_MED__din", value = "")
      updateTextInput(session, "V_PRODU_MED__nomMarqComrc", value = "")
    })
  }


  # APPLICATION -------------------------------------------------------------

  shinyApp(ui, server)

}
