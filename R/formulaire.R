#' Requêtes SQL
#'
#' Formulaire Shiny permettant d'exécuter une ou plusieurs requêtes.
#'
#' @import data.table
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles getVolumes shinyFileSave shinySaveButton
#' @importFrom fs path_home
#' @importFrom testthat capture_error
#' @importFrom stringr str_replace_all str_remove_all str_split str_sub
#' @importFrom writexl write_xlsx
#' @importFrom readxl read_excel excel_sheets
#' @export
formulaire <- function() {


# Table des matières - conception -----------------------------------------

  # UI
  #   Header
  #   Sidebar
  #     Connexion - conn
  #     Requête simple - simple_req
  #     Requete via EXCEL
  #   Body
  #     Connexion - conn
  #     Requête simple - simple_req
  #

# FCTS internal -----------------------------------------------------------

  creer_input_dates <- function(input) {
    ### Créer les dateRangeInput selon le nombre de périodes à afficher

    dates_inputs <- vector("list", length = input$nbr_dates)  # nombre d'éléments à créer
    for (i in 1:input$nbr_dates) {
      # Conserver les valeurs précédemment inscrites en vérifiant si
      # input$datesX existe ou pas
      if (is.null(input[[paste0("dates",i)]])) {
        dates_inputs[[i]] <- dateRangeInput(  # cas où n'existe pas
          inputId = paste0("dates",i), label = paste0("Période ",i),
          separator = " au ")
      } else {
        dates_inputs[[i]] <- dateRangeInput(  # cas où existait: conserver valeur inscrites
          inputId = paste0("dates",i), label = paste0("Période ",i),
          start = input[[paste0("dates",i)]][1],
          end = input[[paste0("dates",i)]][2],
          separator = " au ")
      }
    }
    return(tagList(dates_inputs))
  }
  error_next_section <- function() {
    return(paste(rep("-", 50), collapse = ""))
  }
  file_directory <- function(input_name, method) {
    ### Créer le répertoire à partir d'un shinyFileButton

    if (is.integer(input_name)) {
      return(NULL)
    } else if (method == "save") {
      return(parseSavePath(repertoire_volumes(), input_name))
    } else if (method == "file") {
      return(parseFilePaths(repertoire_volumes(), input_name))
    } else {
      stop("file_directory(): method != {'save', 'file'}.")
    }
  }
  find_datesX <- function(input, type = "debut") {
    ### Extraire les valeurs des dates widgets (debut ou fin)

    # Debut ou Fin
    if (type == "debut") {
      typ_dat <- 1
    } else if (type == "fin") {
      typ_dat <- 2
    } else {
      stop("find_datesX(), valeurs permises de type: {'debut', 'fin'}.")
    }
    if (is.null(input$dates1)) {
      # Cette valeur ne sera jamais affiché, mais nécessaire pour éviter un
      # message d'avertissement. input$datesX sont nécessaires, mais n'est pas
      # créé dès le départ, il est créé après avoir analysé input$nbr_dates.
      # Selon le message d'erreur, j'en ai désuis que output$query_simple_req
      # utilisait les valeurs DatesDebut() et DatesFin() avant que input$datesX
      # ne soit créé. Vérifier si input$dates1 existe est un moyen d'éviter ce
      # message. Prendre note qu'il n'y avait aucun bug.
      return(as.character(Sys.Date()))
    } else {
      vec <- c()
      for (i in 1:input$nbr_dates) {
        vec <- c(vec, as.character(input[[paste0("dates",i)]][typ_dat]))
      }
      return(vec)
    }
  }
  ordre_colonnes <- function(x, type) {
    ### Vecteur des colonnes existantes dans l'ordre désiré

    if (type == "result") {
      # colonnes qui pourraient exister
      cols <- c("DEBUT", "FIN", "ANNEE",
                "DENOM", "DIN",  # seulement 1 parmi ceux-ci
                "MNT_MED", "MNT_SERV", "MNT_TOT",
                "COHORTE", "NBRE_RX", "DUREE_TX", "QTE_MED")
      # Conserver seulement celles présentes, mais dans l'ordre souhaité
      return(cols[cols %in% x])
    } else if (type == "args") {
      return(c(
        "DateDebut", "DateFin", "Variable", "Codes", "Statistiques",
        "GrouperPar", "AjoutInfo", "ExcluCodeServ", "CategorieListe"
      ))
    } else {
      stop("ordre_colonnes(): type != {'result', 'args'}.")
    }
  }
  repertoire_volumes <- function() {
    ### Détection des répertoires de l'ordinateur (disques durs)

    return(c(
      `Par défaut` = path_home(),
      R = R.home(),
      getVolumes()()  # répertoires principaux (C:, E:, ...))
    ))
  }
  save_as_excel <- function(dt, args, save_path, col_sep = 3) {
    ### Enregistre au format Excel le tableau des résultats (requête) à gauche
    ### et les arguments à droite

    if (!is.null(dt) && !is.null(save_path)) {
      rows_to_have <- max(nrow(dt), sapply(args, length))
      if (nrow(dt) < rows_to_have) {
        dt <- rbind(
          dt,
          data.table(DEBUT = as.character(rep(NA, rows_to_have - nrow(dt)))),
          fill = TRUE
        )
      }
      for (i in 1:length(args)) {
        if (length(args[[i]]) < rows_to_have) {
          args[[i]] <- c(args[[i]], rep(NA, rows_to_have - length(args[[i]])))
        }
      }
      args <- as.data.table(args)
      if (col_sep >= 1) {
        tab_sep <- data.table(v1 = rep(NA, rows_to_have))
        if (col_sep > 1) {
          for (i in 2:col_sep) {
            tab_sep[, paste0("v",i) := rep(NA, rows_to_have)]
          }
        }
        tab_result <- cbind(dt, tab_sep, args)
        setnames(tab_result, paste0("v",1:3), rep("", 3))
      } else {
        tab_result <- cbind(dt, args)
      }
      write_xlsx(tab_result, save_path$datapath)
    }
  }
  text_as_vector <- function(input_name) {
    ### Convertir le input text en vecteur

    vec <- as.character(input_name)
    if (vec != "") {
      # Séparer tous les éléments par un ";"
      vec <- str_replace_all(vec, ",", ";")  # convertir ',' en ';'
      vec <- str_replace_all(vec, "\n", ";")  # convertir '\n' en ';'
      vec <- str_remove_all(vec, " ")  # supprimer espaces
      vec <- str_split(vec, ";")[[1]]  # séparer les codes et garder vecteur
      vec <- vec[vec != ""]  # supprimer les éléments vides
      return(vec)
    } else {
      return(NULL)
    }

  }

  # Import EXCEL
  import_excel <- function(filepath, sheets_name) {
    ### Importation du fichier Excel contenant les arguments de la requêtes

    sh_names <- excel_sheets(filepath)  # nom des onglets
    list_dt <- vector("list", length(sh_names))
    for (i in 1:length(sh_names)) {  # importer les tableaux
      list_dt[[i]] <- as.data.table(read_excel(filepath, sh_names[i]))
    }
    names(list_dt) <- sh_names
    return(list_dt)
  }

  # Verifications
  verif_stat_gen1 <- function(list_dt) {
    ### Messages d'erreurs à afficher dans l'onglet Requête via EXCEL

    txt_error <- ""
    for (i in 1:length(list_dt)) {
      dt <- list_dt[[i]]
      if (!"Methode" %in% names(dt)) {
        txt_error <- paste0(txt_error,
          sheet," :\n",
          " - La colonne Methode est absente.\n\n",
          error_next_section(),"\n\n"
        )
      } else {

      }
    }
  }

# Interface Utilisateur ---------------------------------------------------

  library(fs)
  library(shiny)
  library(shinydashboard)
  library(shinyFiles)
  library(testthat)
  library(stringr)
  library(data.table)
  library(writexl)
  library(readxl)

  ui <- dashboardPage(

    ### Header
    dashboardHeader(title = "Requêtes SQL"),

    ### Sidebar
    # Contient 3 onglets:
    #   - Connexion
    #   - Requete Simple
    #   - Requete via EXCEL
    dashboardSidebar(
      sidebarMenu(
        # Connexion SQL:
        # indique si la connexion est faite ou pas
        menuItem("Connexion", tabName = "conn"),
        # Requête simple:
        # utiliser les arguments du Shiny pour effectuer une extraction
        menuItem("Requête simple", tabName = "simple_req"),
        # Requête via EXCEL:
        # utiliser un fichier EXCEL pour effectuer une requete ou chaque onglet
        # du fichier correspond à un tableau, une extraction, une requete
        menuItem("Requêtes via EXCEL", tabName = "excel_req")
      )
    ),

    ### Body
    # Contient 3 onglets, les même que dans Sidebar
    dashboardBody(
      tabItems(
        ## Connexion:
        # l'utilisateur entre ses informations (identifiant + mot de passe) et le
        # programme indique si la connexion est etablie
        tabItem(
          tabName = "conn",
          # Informations nécessaires à la connexion
          textInput("teradata_user", "Identifiant", value = ""),  # no identifiant
          passwordInput("teradata_pwd", "Mot de passe", value = ""),  # mot de passe
          # Établir la connexion
          actionButton("check_conn", "Connexion"),
          # Indiquer l'état de la connexion
          h5("État de la connexion :"),
          verbatimTextOutput("is_conn", placeholder = TRUE)
        ),

        ## Requête simple
        # Utiliser les arguments shiny pour effectuer une requête
        tabItem(
          tabName = "simple_req",
          # 1ere ligne: Arguments de la requête
          fluidRow(
            # Afficher les arguments sur trois (3) colonnes -> 12/3 = 4 = width
            column(
              width = 4,
              # Indiquer le nom de période d'étude à analyser au total
              numericInput("nbr_dates", "Nombre de périodes",
                           value = 1, min = 1, max = 99),
              # Affiche le nombre de périodes d'étude indiquer dans input$nbr_dates
              uiOutput("input_mult_dates")
            ),

            column(
              width = 4,
              # Type de code à analyser - sélection de la variable d'analyse
                selectInput("variable", "Variable",
                          choices = c("DENOM", "DIN"),
                          selected = "DENOM"),
              # Codes d'analyse
              textAreaInput("codes", "Codes"),
              # Statistiques
              checkboxGroupInput(
                "stats", "Statistiques",
                choices = c("Montant médicament" = "MNT_MED",
                            "Montant service" = "MNT_SERV",
                            "Montant total" = "MNT_TOT",
                            "Cohorte" = "COHORTE",
                            "Nombre Rx" = "NBRE_RX",
                            "Durées traitements" = "DUREE_TX",
                            "Quantité médicament" = "QTE_MED"),
                selected = c("MNT_MED", "MNT_SERV", "MNT_TOT",
                             "COHORTE", "NBRE_RX", "DUREE_TX", "QTE_MED")
              ),
              # Ajout Information
              checkboxGroupInput(
                "addinfo", "Ajout Information",
                choices = c("Nom marque commerce" = "MARQ_COMRC")
              )
            ),
            column(
              width = 4,
              # Grouper par
              checkboxGroupInput(
                "groupby", "Grouper par",
                choices = c("Année" = "ANNEE", "Codes" = "CODES"),
                selected = c("ANNEE", "CODES")
              ),
              # Exclusion Code Service
              textInput("excl_code_serv", "Exclusion Code Service",
                        value = "1"),
              # Filtre Categorie Liste
              textInput("filter_cat_list", "Filtre Catégorie Liste",
                        value = "")
            )
          ),
          # Bouton pour exécuter la requête SQL
          fluidRow(
            column(
              width = 4,
              actionButton("go_extract", "Exécuter Requête"), p()
            ),
          ),
          fluidRow(p()), # insérer un espace entre le bouton et le tableau qui s'affiche après
          # Afficher tout ce qui n'est pas un argument de requête:
          #   - Tableau des résultats
          #   - code SQL exécuté ou à exécuter pour la requête
          fluidRow(
            column(
              width = 12, align = "left",
              dataTableOutput("requeteSQL"),
              shinySaveButton("save_simple", "Enregistrer requête", "Enregistrer sous...",
                              filetype = list(`Classeur Excel` = "xlsx"),
                              viewtype = "list"), p(),

              verbatimTextOutput("query_simple_req"),
              verbatimTextOutput("test_variables")  # afficher variables voir si code bon pour extraire les arguments
            )
          )
        ),

        ## Requêtes via EXCEL
        tabItem(
          tabName = "excel_req",
          shinyFilesButton("select_xl_file", "Sélectionner fichier Excel",
                           "Sélectionner fichier Excel", multiple = FALSE,
                           viewtype = "detail"), p(),
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          actionButton("go_xl_extract", "Exécuter Requête(s)"),
          verbatimTextOutput("test_via_excel")
        )

      )

    )

  )


# Serveur -----------------------------------------------------------------

  server <- function(input, output, session) {

    ### Valeurs à initialiser
    Vals <- reactiveValues(
      conn = FALSE  # etat de la connexion SQL,
    )

    ### Connexion SQL
    # Messages indiquant si la connexion est établie ou pas
    observeEvent(input$check_conn, {
      if (input$teradata_user == "" && input$teradata_pwd == "") {
        # Cas où aucune information n'est inscrite
        Vals$is_conn <- "***Veuillez inscire numéro d'identifiant et mot de passe"
        Vals$conn <- FALSE
      } else if (is.null(capture_error(sql_conn("PEI_PRD", input$teradata_user, input$teradata_pwd)))) {
        # La connexion est établie -> aucune erreur
        Vals$is_conn <- "CONNEXION ÉTABLIE"
        Vals$conn <- TRUE
      } else {
        # Malgré les infos inscrites, la connexion ne s'est pas effectuée
        Vals$is_conn <- "***Vérifier identifiant et mot de passe."
        Vals$conn <- FALSE
      }
    })
    output$is_conn <- renderText( Vals$is_conn )  # afficher message de la connexion


    ### Requête simple
    # Extraire les valeurs des arguments
    DatesDebut <- reactive({ find_datesX(input, "debut") })
    DatesFin <- reactive({ find_datesX(input, "fin") })
    Variable <- reactive({ input$variable })
    Codes <- reactive({ text_as_vector(input$codes) })
    Statistiques <- reactive({ input$stats })
    GroupBy <- reactive({ input$groupby })
    ExcluCodeServ <- reactive({ text_as_vector(input$excl_code_serv) })
    CategorieListe <- reactive({ text_as_vector(input$filter_cat_list) })
    # Périodes d'analyse à afficher selon choix utilisateur
    output$input_mult_dates <- renderUI({ creer_input_dates(input) })
    # Répertoire de sauvegarde
    shinyFileSave(input, "save_simple", roots = repertoire_volumes(), session = session)
    simple_dir_save <- reactive({ file_directory(input$save_simple, "save") })
    # Tableau à afficher
    requeteSQL <- eventReactive(input$go_extract, {
      if (is.null(Codes())) {
        return(NULL)
      } else {
        n <- length(DatesDebut())
        dt <- data.table(
          DEBUT = rep(DatesDebut(), each = length(Codes())),
          FIN = rep(DatesFin(), each = length(Codes())),
          ANNEE = rep(lubridate::year(as.Date(DatesDebut())), each = length(Codes())),
          Code = rep(Codes(), n),
          MNT_MED = sample(seq(100, 999, 0.01), n * length(Codes()), T),
          MNT_SERV = sample(seq(100, 999, 0.01), n * length(Codes()), T)
        )
        dt[, MNT_TOT := MNT_MED + MNT_SERV]
        setnames(dt, "Code", Variable())
        return(dt)
      }
    })
    output$requeteSQL <- renderDataTable({ requeteSQL() })
    # Enregistrement de la requête en EXCEL
    observeEvent(simple_dir_save(), {
      save_as_excel(dt = requeteSQL(),
                    args = list(DateDebut = DatesDebut(), DateFin = DatesFin(),
                                Variable = Variable(), Codes = Codes(),
                                Statistiques = Statistiques(), GrouperPar = GroupBy(),
                                AjoutInfo = NULL,
                                ExcluCodeServ = ExcluCodeServ(), CategorieListe = CategorieListe()),
                    save_path = simple_dir_save())
    })
    # SQL query à afficher
    output$query_simple_req <- renderText({
      stat_gen1_txt_query_1period(DateDebut = DatesDebut()[1],
                                  DateFin = DatesFin()[1],
                                  Variable = Variable(), Codes = Codes(),
                                  Stats = Statistiques(),
                                  GroupBy = GroupBy(),
                                  ExcluCodeServ = ExcluCodeServ())
    })

    ### À SUPPRIMER - VÉRIFIER LA VALEUR DES COMMANDES
    output$test_variables <- renderPrint({
      list(DatesDebut = DatesDebut(),
           DatesFin = DatesFin(),
           Variable = Variable(),
           Codes = Codes(),
           Statistiques = Statistiques(),
           GroupBy = GroupBy(),
           ExcluCodeServ = ExcluCodeServ(),
           CategorieListe = CategorieListe(),
           EnregistrerSQL = simple_dir_save())
    })


    ### Requête via EXCEL
    # Sélection du fichier Excel
    shinyFileChoose(input, "select_xl_file", roots = repertoire_volumes(), session = session)
    select_xl_file <- reactive({ file_directory(input$select_xl_file, "file") })
    # Indiquer message d'erreur
    output$xl_errors_msg <- renderText({paste0(
      "Nom Onglet 1:", nl(),
      " - DateDebut n'est pas au format AAAA-MM-JJ.", nl(),
      " - DateFin n'est pas au format AAAA-MM-JJ.", nl(),
      "---------------------------------------------", nl(),
      "Nom Onglet 2:", nl(),
      " - Erreur 1", nl()
    )})

    ### À SUPPRIMER - VÉRIFIER LA VALEUR DES COMMANDES
    output$test_via_excel <- renderPrint({
      list(Excel_directory = select_xl_file())
    })

  }


# Exécuter Application ----------------------------------------------------

  shinyApp(ui, server)

}
