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
#' @importFrom lubridate as_date
#' @export
formulaire_old <- function() {
  library(fs)
  library(shiny)
  library(shinydashboard)
  library(shinyFiles)
  library(testthat)
  library(stringr)
  library(data.table)
  library(writexl)
  library(readxl)
  library(lubridate)

# FCTS internal -----------------------------------------------------------

  possible_methods <- c("stat_gen1")

  creer_input_codes <- function(input) {
    ### Créer les textInput pour chaque code

    codes_input <- vector("list", length = input$nbr_codes)  # nombre d'éléments à créer
    for (i in 1:input$nbr_codes) {
      if (is.null(input[[paste0("codes",i)]])) {
        codes_input[[i]] <- textInput(inputId = paste0("codes",i),
                                      label = paste0("Code Rx ", i),
                                      value = "")

      } else {
        codes_input[[i]] <- textInput(inputId = paste0("codes", i),
                                      label = paste0("Code Rx ", i),
                                      value = input[[paste0("codes",i)]])
      }
    }
    return(tagList(codes_input))
  }
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
  find_codes <- function(input) {
    ### Extraire tous les codes d'analyse dans un vecteur

    if (is.null(input$codes1) || input$codes1 == "") {
      return("**INSCRIRE AU MOINS UN CODE D'ANALYSE**")
    } else {
      vec <- c()
      for (i in 1:input$nbr_codes) {
        vec <- c(vec, as.numeric(input[[paste0("codes",i)]]))
      }
      vec <- vec[!is.na(vec)]
      return(unique(vec))
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

      query_to_excel <- stat_gen1_txt_query_1period(
        DateDebut = args$DateDebut[1],
        DateFin = args$DateFin[1],
        Variable = args$Variable,
        Codes = args$Codes,
        Stats = args$Satistiques,
        GroupBy = args$GrouperPar,
        ExcluCodeServ = args$ExcluCodeServ
      )
      query_to_excel <- str_split(query_to_excel, "\n")[[1]]

      rows_to_have <- max(nrow(dt), sapply(args, length), length(query_to_excel))
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
      dt_query <- data.table(REQUETE = query_to_excel)
      if (nrow(dt_query) < rows_to_have) {
        dt_query <- rbind(
          dt_query,
          data.table(REQUETE = as.character(rep(NA, rows_to_have - nrow(dt_query)))),
          fill = TRUE
        )
      }

      if (col_sep >= 1) {
        tab_sep <- data.table(v1 = rep(NA, rows_to_have))
        if (col_sep > 1) {
          for (i in 2:col_sep) {
            tab_sep[, paste0("v",i) := rep(NA, rows_to_have)]
          }
        }
        tab_result <- cbind(dt, tab_sep, args)
        setnames(tab_result, paste0("v",1:col_sep), paste0("abc",1:col_sep))
      } else {
        tab_result <- cbind(dt, args)
      }
      if (col_sep >= 1) {
        tab_sep <- data.table(v1 = rep(NA, rows_to_have))
        if (col_sep > 1) {
          for (i in 2:col_sep) {
            tab_sep[, paste0("v",i) := rep(NA, rows_to_have)]
          }
        }
        tab_result <- cbind(tab_result, tab_sep, dt_query)
        setnames(tab_result, paste0("v",1:col_sep), paste0("xyz", 1:col_sep))
      } else {
        tab_result <- cbind(tab_result, dt_query)
      }
      setnames(tab_result, c(paste0("abc",1:col_sep), paste0("xyz",1:col_sep)),
               rep(paste(rep(" ", 3), collapse = ""), col_sep * 2))
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
  import_excel <- function(filepath) {
    ### Importation du fichier Excel contenant les arguments de la requêtes

    sh_names <- excel_sheets(filepath)  # nom des onglets
    list_dt <- vector("list", length(sh_names))
    for (i in 1:length(sh_names)) {  # importer les tableaux
      list_dt[[i]] <- as.data.table(read_excel(filepath, sh_names[i], col_types = "text"))
    }
    names(list_dt) <- sh_names
    return(list_dt)
  }

  # Verifications
  verif_excel_file_fcts <- function(method, dt, sheet) {
    if (method == "stat_gen1") {
      return(verif_stat_gen1(dt, sheet))
    } else {
      return(sheet, " : ", nl(),
             " - ",method," n'est pas une valeur permise dans la colonne Methode.",nl(2),
             error_next_section(),
             nl(2))
    }
  }
  verif_stat_gen1 <- function(dt, sheet) {
    ### Messages d'erreurs à afficher dans l'onglet Requête via EXCEL

    possible_ajoutinfo <- c("MARQ_COMRC")
    possible_categliste <- c("40", "41")
    possible_exclucodeserv <- c("1", "G", "P")
    possible_grp_par <- c("ANNEE")
    possible_stats <- c("MNT_MED", "MNT_SERV", "MNT_TOT",
                        "COHORTE", "NBRE_RX", "DUREE_TX", "QTE_MED")


    txt <- ""
    # Vérifier la présence des colonnes obligatoires
    cols <- c("DateDebut", "DateFin", "Variable", "Codes", "Statistiques")
    for (col in cols) {
      if (!col %in% names(dt)) {
        txt <- paste0(txt, " - La colonne ",col," n'est pas présente.\n")
      }
    }

    # Vérifier la structure de DateDebut et DateFin
    if ("DateDebut" %in% names(dt)) {
      datedebut <- rmNA(dt$DateDebut)
      if (anyNA(as_date_excel_chr(datedebut))) {
        txt <- paste0(txt, " - DateDebut doit contenir des dates au format 'AAAA-MM-JJ'.\n")
      }
    }
    if ("DateFin" %in% names(dt)) {
      datefin <- rmNA(dt$DateFin)
      if (anyNA(as_date_excel_chr(datefin))) {
        txt <- paste0(txt, " - DateFin doit contenir des dates au format 'AAAA-MM-JJ'.\n")
      }
    }
    if ("DateDebut" %in% names(dt) && "DateFin" %in% names(dt)) {
      if (length(datedebut) != length(datefin)) {
        txt <- paste0(txt, " - DateDebut et DateFin doivent contenir le même nombre de valeurs.\n")
      }
    }

    # Vérifier la valeur de Variable
    if ("Variable" %in% names(dt)) {
      variable <- rmNA(toupper(dt$Variable))
      if (length(variable) != 1 | !variable %in% c("DENOM", "DIN")) {
        txt <- paste0(txt, " - Variable doit contenir une seule valeur parmi DENOM ou DIN.\n")
      }
    }

    # Vérifier s'il y a au moins une valeur de Codes
    if ("Codes" %in% names(dt)) {
      if (!length(dt$Codes)) {
        txt <- paste0(txt, " - Codes doit contenir au moins une valeur.\n")
      }
    }

    # Vérifier les valeurs de statistiques
    if ("Statistiques" %in% names(dt)) {
      statistiques <- rmNA(toupper(dt$Statistiques))
      if (length(statistiques)) {
        for (st in statistiques) {
          if (!st %in% possible_stats) {
            txt <- paste0(txt, " - ",st," n'est pas une statistique permise.\n")
          }
        }
      } else {
        txt <- paste0(txt, " - Statistiques doit contenir au moins une valeur.\n")
      }
    }

    # Vérifier GrouperPar
    if ("GrouperPar" %in% names(dt)) {
      grouperpar <- rmNA(toupper(dt$GrouperPar))
      if (length(grouperpar)) {
        for (grp_var in grouperpar) {
          if (!grp_var %in% possible_grp_par) {
            txt <- paste0(txt, " - ",grp_var," n'est pas une valeur permise dans la colonne GrouperPar.\n")
          }
        }
      }
    }

    # Vérifier AjoutInfo
    if ("AjoutInfo" %in% names(dt)) {
      ajoutinfo <- rmNA(toupper(dt$AjoutInfo))
      if (length(ajoutinfo)) {
        for (info in ajoutinfo) {
          if (!info %in% possible_ajoutinfo) {
            txt <- paste0(txt, " - ",info," n'est pas une valeur permise dans la colonne AjoutInfo.\n")
          }
        }
      }
    }

    # Vérifier ExcluCodeServ
    if ("ExcluCodeServ" %in% names(dt)) {
      exclucodeserv <- rmNA(toupper(as.character(dt$ExcluCodeServ)))
      if (length(exclucodeserv)) {
        for (code_serv in exclucodeserv) {
          if (!code_serv %in% possible_exclucodeserv) {
            txt <- paste0(txt, " - ",code_serv," n'est pas une valeur permise dans la colonne ExcluCodeServ.\n")
          }
        }
      }
    }

    # Vérifier CategorieListe
    if ("CategorieListe" %in% names(dt)) {
      categliste <- rmNA(toupper(as.character(dt$CategorieListe)))
      if (length(categliste)) {
        for (catlist in categliste) {
          if (!catlist %in% possible_categliste) {
            txt <- paste0(txt, " - ",catlist," n'est pas une valeur permise dans la colonne CategorieListe.\n")
          }
        }
      }
    }

    if (txt != "") {
      txt <- paste0(
        sheet," : ",nl(),
        txt, nl(1),
        error_next_section(), nl(2)
      )
    }
    return(txt)

  }

# Interface Utilisateur ---------------------------------------------------

  ui <- dashboardPage(

    dashboardHeader(title = "Requêtes SQL"),
    dashboardSidebar(
      sidebarMenu(
        # Connexion SQL:
        # indique si la connexion est faite ou pas
        menuItem("Connexion", tabName = "conn"),
        # Requête via EXCEL:
        # utiliser un fichier EXCEL pour effectuer une requete ou chaque onglet
        # du fichier correspond à un tableau, une extraction, une requete
        menuItem("Requêtes via EXCEL", tabName = "excel_req"),
        # Requête simple:
        # utiliser les arguments du Shiny pour effectuer une extraction
        menuItem("Statistiques générales", tabName = "stat_gen1")
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

        ## Requêtes via EXCEL
        tabItem(
          tabName = "excel_req",
          shinyFilesButton("select_xl_file", "Sélectionner fichier Excel",
                           "Sélectionner fichier Excel", multiple = FALSE,
                           viewtype = "detail"), p(),
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          actionButton("go_xl_extract", "Exécuter Requête(s)")
        ),

        ## Statistiques générales
        # Utiliser les arguments shiny pour effectuer une requête
        tabItem(
          tabName = "stat_gen1",
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
              # Nombre de code à analyser
              numericInput("nbr_codes", "Nombre de codes Rx", value = 1,
                           min = 1, max = 99),
              # Type de code à analyser - sélection de la variable d'analyse
              selectInput("variable", "Type de code Rx",
                          choices = c("DENOM", "DIN"),
                          selected = "DENOM"),
              # Codes d'analyse
              uiOutput("input_mult_codes")
            ),
            column(
              width = 4,
              # Grouper par
              checkboxGroupInput(
                "groupby", "Grouper par",
                choices = c("Année" = "ANNEE"),
                selected = c("ANNEE")
              ),
              # Ajout Information
              checkboxGroupInput(
                "addinfo", "Ajout Information",
                choices = c("Nom marque commerce" = "MARQ_COMRC")
              ),
              # Exclusion Code Service
              checkboxGroupInput("excl_code_serv", "Exclusion Code Service",
                                 choices = c("1 : Descriptif1", "G : Descriptif2", "P : Descriptif3"),
                                 selected = c("1 : Descriptif1")),
              # Filtre Categorie Liste
              checkboxGroupInput("filter_cat_list", "Filtre Catégorie Liste",
                                 choices = c("41 : Description", "42 : Description"))
            )
          ),
          # Bouton pour exécuter la requête SQL
          fluidRow(
            column(
              width = 4,
              actionButton("go_extract", "Exécuter Requête")
            ),
            column(
              width = 4,
              shinySaveButton("save_simple", "Enregistrer requête", "Enregistrer sous...",
                              filetype = list(`Classeur Excel` = "xlsx"),
                              viewtype = "list")
            ),
            column(
              width = 4,
              actionButton("maj_requete", "Visualiser/MaJ requête")
            )
          ),
          fluidRow(p()), # insérer un espace entre les boutons et le tableau qui s'affiche après
          # Afficher tout ce qui n'est pas un argument de requête:
          #   - Tableau des résultats
          #   - code SQL exécuté ou à exécuter pour la requête
          fluidRow(
            column(
              width = 12, align = "left",
              dataTableOutput("requeteSQL"),
              verbatimTextOutput("query_simple_req")
            )
          )
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
    # Périodes d'analyse à afficher selon choix utilisateur
    output$input_mult_dates <- renderUI({ creer_input_dates(input) })
    output$input_mult_codes <- renderUI({ creer_input_codes(input) })
    # Répertoire de sauvegarde
    shinyFileSave(input, "save_simple", roots = repertoire_volumes(), session = session)
    simple_dir_save <- reactive({ file_directory(input$save_simple, "save") })
    # Tableau à afficher
    requeteSQL <- eventReactive(input$go_extract, {
      Codes <- function() return(find_codes(input))
      DatesDebut <- function() return(find_datesX(input, type = "debut"))
      DatesFin <- function() return(find_datesX(input, type = "fin"))
      Variable <- function() return(input$variable)
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
                    args = list(DateDebut = find_datesX(input, "debut"),
                                DateFin = find_datesX(input, "fin"),
                                Variable = input$variable, Codes = find_codes(input),
                                Statistiques = c("MNT_MED", "MNT_SERV", "MNT_TOT", "COHORTE",
                                                 "NBRE_RX", "DUREE_TX", "QTE_MED"),
                                GrouperPar = input$groupby,
                                AjoutInfo = NULL,
                                ExcluCodeServ = input$excl_code_serv,
                                CategorieListe = NULL),
                    save_path = simple_dir_save())
    })
    # SQL query à afficher
    maj_requete <- eventReactive(input$maj_requete, {
      ""
    })
    output$query_simple_req <- renderText({ maj_requete() })

    ### Requête via EXCEL
    # Sélection du fichier Excel
    shinyFileChoose(input, "select_xl_file", roots = repertoire_volumes(), session = session)
    select_xl_file <- reactive({ file_directory(input$select_xl_file, "file") })
    # Indiquer message d'erreur

    xl_errors_msg <- eventReactive(select_xl_file(), {
      if (is.null(select_xl_file())) {
        return("")
      } else {
        list_dt <- import_excel(select_xl_file()$datapath)
        txt_error <- ""
        for (sh in names(list_dt)) {
          dt <- list_dt[[sh]]
          if ("Methode" %in% names(dt)) {
            method <- rmNA(tolower(dt$Methode))
            if (length(method) == 1) {
              txt_error <- paste0(txt_error, verif_excel_file_fcts(method, dt, sh))
            } else {
              txt_error <- paste0(txt_error, sh, " : ",nl(), " - ")
              if (length(method) > 1) {
                txt_error <- paste0(txt_error, "Methode doit contenir une seule valeur.")
              } else {
                txt_error <- paste0(txt_error, "Methode doit contenir une valeur.")
              }
              txt_error <- paste0(txt_error, nl(2),
                                  error_next_section(), nl(2))
            }
          } else {
            txt_error <- paste0(
              txt_error, sh, " : ", nl(),
              " - La colonne Methode n'existe pas.",nl(2),
              error_next_section(),
              nl(2)
            )
          }
        }
        return(txt_error)
      }
    })
    output$xl_errors_msg <- renderText({ xl_errors_msg() })

  }


# Exécuter Application ----------------------------------------------------

  shinyApp(ui, server)

}
