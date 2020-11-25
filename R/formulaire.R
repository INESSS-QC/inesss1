#' Formulaire Shiny
#'
#' @keywords internal
#'
#' @import data.table
#' @importFrom fs path_home
#' @importFrom odbc odbc dbConnect dbGetQuery
#' @importFrom readxl excel_sheets read_excel
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles shinyFilesButton shinyFileChoose shinyFileSave shinySaveButton parseFilePaths parseSavePath getVolumes
#' @importFrom stringr str_split str_remove_all
#' @importFrom writexl write_xlsx
#' @export
formulaire <- function() {

# Variables ---------------------------------------------------------------

  cols_EXCEL_file <- function() {
    ### Colonnes nécessaires pour chaque méthode

    return(list(
      sg1 = c("DATE_DEBUT", "DATE_FIN", "TYPE_RX", "CODE_RX",
              "CODE_SERV_FILTRE", "CODE_SERV", "CODE_LIST_FILTRE", "CODE_LIST")
    ))
  }
  values_EXCEL_file <- function() {
    ### Valeurs permises dans les colonnes de cols_EXCEL_file()

    return(list(
      sg1 = list(
        TYPE_RX = c("DENOM", "DIN"),
        CODE_SERV_FILTRE = c("Exclusion", "Inclusion"),
        CODE_SERV = c("1", "AD", "L", "M", "M1", "M2", "M3"),
        CODE_LIST_FILTRE = c("Exclusion", "Inclusion"),
        CODE_LIST = c("03", "40", "41")
      )
    ))
  }
  methods_EXCEL_file <- function() {
    ### Liste des méthodes existantes

    return(c(
      "stat_gen1"
    ))
  }

# Fonctions ---------------------------------------------------------------

  adapt_code_serv <- function(code_serv) {
    ### Séparer des groupes de codes de service en valeurs uniques pour les
    ### requêtes SQL

    if ("L, M, M1 à M3" %in% code_serv) {
      code_serv <- code_serv[code_serv != "L, M, M1 à M3"]
      code_serv <- sort(c(code_serv, "L", "M", "M1", "M2", "M3"))
    }
    return(code_serv)
  }
  cols_select_from_method <- function(dt, method) {
    ### Puisque l'onglet Excel peut contenir des colonnes facultatives pour le
    ### formulaire, on doit les supprimer après l'avoir importé.
    ### Possible de le faire rapidement grâce à cols_EXCEL_file()

    if (method == "stat_gen1") {
      cols <- names(dt)[names(dt) %in% c("METHODE", cols_EXCEL_file()$sg1)]
    }
    dt <- dt[, ..cols]
    return(dt)
  }
  create_dt_code_list_med <- function() {
    ### Data conteant tous les codes de liste de médicament ainsi que leur
    ### description

    dt <- inesss::PROD.V_DES_COD[TYPE_CODE == "COD_CATG_LISTE_MED", .(code = CODE, desc = CODE_DESC)]
    return(dt)
  }
  create_dt_code_serv <- function() {
    ### Data contenant la liste des codes de services et leur description

    dt <- inesss::PROD.V_DEM_PAIMT_MED_CM.SMED_COD_SERV[, .(code = COD_SERV, desc = COD_SERV_DESC)]
    # Codes L à M3 sont tous inclus dans le même, donc modification du data
    dt <- dt[!code %in% c("L", "M", "M1", "M2", "M3")]
    dt <- rbind(dt, data.table(code = "L, M, M1 à M3", desc = "PREPARATION MAGISTRALE"))
    dt <- setkey(dt, code)
    return(dt)
  }
  create_dt_data_args_query <- function(dt, args_list, query) {
    ### Tableau des résultats, ajouter les arguments ainsi que la requête créée
    ### à partir des arguments.

    # Déterminer le nombre de lignes que l'onglet Excel doit avoir.
    query <- data.table(`Requête SQL` = str_split(query, "\n")[[1]]) # séparer la chaine de caractères en vecteur
    nb_row <- max(nrow(dt), sapply(args_list, length), nrow(query))  # nbre de lignes nécessaires

    # Ajouter des valeurs à tous les éléments qui ont une longueur < nb_row
    if (nrow(dt) < nb_row) {
      dt <- rbind(dt, data.table(DATE_DEBUT = rep(NA, nb_row - nrow(dt))), fill = TRUE)
    }
    for (i in 1:length(args_list)) {
      if (length(args_list[[i]]) < nb_row) {
        args_list[[i]] <- c(args_list[[i]], rep(NA, nb_row - length(args_list[[i]])))
      }
    }
    if (nrow(query) < nb_row) {
      query <- rbind(query, data.table(`Requête SQL` = rep(NA, nb_row - nrow(query))))
    }

    # Regrouper tous les éléments ensemble dans un même tableau
    tables_in_one <- cbind(
      dt,
      data.table(v_1 = rep(NA, nb_row),  # espaces pour séparer les éléments
                 v_2 = rep(NA, nb_row), v_3 = rep(NA, nb_row)),
      as.data.table(args_list),
      data.table(v_4 = rep(NA, nb_row),
                 v_5 = rep(NA, nb_row), v_6 = rep(NA, nb_row)),
      query
    )
    setnames(tables_in_one, paste0("v_",1:6), rep("", 6))  # supprimer le nom des colonnes pour espacement

    return(tables_in_one)
  }
  format_xl_err_nl <- function(l = 60) {
    ### Répétition de '=' collé 'l' fois. Souvent utilisé pour une nouvelle
    ### section

    return(paste0("\n", paste(rep("=", l), collapse = ""), "\n\n"))
  }
  format_xl_err_sh <- function(name) {
    ### Indiquer le nom de l'onglet dans les messages d'erreur = 'sh : '

    return(paste0(name," :\n"))
  }
  format_price <- function(x) {
    ### Converti un nombre en chaîne de caractères avec deux décimales, même si
    ### c'est zéro (0)

    return(sapply(x, formatC, digits = 2, format = "f", big.mark = " ", decimal.mark = ","))
  }
  msg_error_from_xlfile <- function(file) {
    ### À partir d'un fichier EXCEL, vérifie chaque onglet et renvoie les
    ### messages d'erreur s'il y en a. Utilisé dans la section Requêtes via EXCEL

    sheets <- excel_sheets(file)  # nom des onglets du fichier importé
    msg_error <- ""  # contiendra les messages d'erreur

    for (sh in sheets) {
      # Importation du data
      suppressMessages(suppressWarnings({  # supprime avertissements puisque les tableaux sont irréguliers
        dt <- as.data.table(read_excel(file, sheet = sh, col_types = "text"))
      }))

      if ("METHODE" %in% names(dt)) {
        method <- str_remove_all(rmNA(dt$METHODE), " ")  # méthode à utiliser
        # Gérer les erreurs selon le cas
        if (length(method) == 1) {
          if (method %in% methods_EXCEL_file()) {
            dt <- cols_select_from_method(dt, method)  # sélection des colonnes en lien avec la méthode
            msg_error <- verif_method()[[method]](dt, sh, msg_error)  # vérifications selon méthode
          } else {
            msg_error <- paste0(  # erreur si la méthode est inconnue
              msg_error,
              sh, " :\n",
              " -  METHODE ne contient pas une valeur permise.\n",
              format_xl_err_nl()
            )
          }
        } else {
          msg_error <- paste0(  # si la méthode contient plusieurs valeurs ou aucune
            msg_error,
            sh, " :\n",
            " -  METHODE doit contenir une valeur.\n",
            format_xl_err_nl()
          )
        }
      } else {
        msg_error <- paste0(  # si la colonne METHODE
          msg_error,
          sh, " :\n",
          " -  METHODE est absente.\n",
          format_xl_err_nl()
        )
      }
    }

    if (msg_error == "") {
      return(NULL)
    } else {
      return(msg_error)
    }
  }
  nom_type_rx <- function(type_rx) {
    ### Indique le nom de la colonne indiquant le nom du type de Rx dans un data

    if (type_rx == "DENOM") {
      return("NOM_DENOM")
    } else if (type_rx == "DIN") {
      return("NOM_MARQ_COMRC")
    } else {
      stop("formulaire.nom_type_rx() valeur non permise.")
    }
  }
  save_EXCEL <- function(dt, save_path) {
    ### Enregistrer au format EXCEL

    write_xlsx(dt, save_path$datapath)
  }
  sg1_find_date <- function(input, method = "deb") {
    ### Trouver toutes les valeurs des dates de début, method="deb", ou de fin
    ### (method = "fin") qui se retrouvent dans input.

    if (method == "deb") {
      type_date <- 1L
    } else if (method == "fin") {
      type_date <- 2L
    } else {
      stop("formulaire.sg1_find_date(): valeurs permises de method = {'deb', 'fin'}.")
    }

    if (is.null(input$sg1_date1)) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements. Pourrait être géré autrement avec
      # des ordres de priorité.
      return(as.character(Sys.Date()))
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input$sg1_nb_per) {
        vec <- c(vec, as.character(input[[paste0("sg1_date",i)]][type_date]))
      }
      return(vec)
    }
  }
  sg1_find_code <- function(input) {
    ### Trouver toutes les valeurs de codes d'analyse qui se retrouvent dans
    ### input.

    if (is.null(input$sg1_code1)) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements
      return("")
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input$sg1_nb_codes) {
        vec <- c(vec, as.character(input[[paste0("sg1_code",i)]]))
      }
      return(vec)
    }
  }
  sg1_code_list_choices <- function(dt_code_list) {
    ### Tableau indiquant les choix et les valeurs des codes de liste
    dt <- dt_code_list[code %in% c("03", "40", "41")]
    dt[, ch_name := paste0(code," : ",desc), .(code)]
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))
  }
  sg1_code_serv_choices <- function(dt_code_serv) {
    ### Tableau indiquant les choix et les valeurs des codes de services
    dt <- dt_code_serv[code %in% c("1", "AD", "L, M, M1 à M3")]
    # dt <- dt_code_serv[code %in% c("1", "A", "AD", "I", "L, M, M1 à M3", "Q", "RA")]  # anciennes valeurs proposées par Michèle Paré
    dt[, ch_name := paste0(code," : ",desc), .(code)]
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))
  }
  sg1_dbGetQuery <- function(input, conn) {
    ### Effectue la ou les requêtes de statistiques générales selon les arguments
    ### @param input : Équivalent à une liste. Les éléments doivent avoir les
    ###                mêmes noms que les input de l'onglet sg1 = Statistiques générales
    ### @param conn : Variable de connexion créé par sql_connecion

    DT <- data.table()  # tableau contenant la ou les requêtes
    nom_denom <- copy(inesss::PROD.V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE)  # data nom des denom
    nom_marq_comrc <- inesss::PROD.V_PRODU_MED.NOM_MARQ_COMRC[, .(DIN, NOM_MARQ_COMRC, DATE_DEBUT, DATE_FIN)]  # data nom marques commerciales

    # Extraire les arguments souvent utilisés des input
    dates_debut <- sg1_find_date(input, "deb")
    dates_fin <- sg1_find_date(input, "fin")
    codes_rx <- sort(sg1_find_code(input))
    codes_serv <- adapt_code_serv(input$sg1_code_serv)

    # Effectuer une requête par période d'étude et joindre les tableaux
    for (i in 1:length(dates_debut)) {

      dt <- as.data.table(dbGetQuery(  # requête pour la ième période d'étude
        conn = conn,  # connexion faite à partir de l'onglet connexion
        statement = stat_gen1_txt_query_1period(
          debut = dates_debut[i], fin = dates_fin[i],
          type_Rx = input$sg1_type_Rx, codes = codes_rx,
          groupby = input$sg1_group_by,
          code_serv = codes_serv, code_serv_filtre = input$sg1_code_serv_filter,
          code_list = sort(input$sg1_code_list), code_list_filtre = input$sg1_code_list_filter
        )
      ))
      dt[, `:=` (MNT_MED = as_price(MNT_MED),  # s'assurer que les prix ont deux décimales
                 MNT_SERV = as_price(MNT_SERV),
                 MNT_TOT = as_price(MNT_TOT))]

      # Ajouter le nom du DENOM ou le nom de la marque commerciale (DIN)
      if (input$sg1_type_Rx == "DENOM") {
        dt <- nom_denom[  # ajouter le nom des DENOM à la date de départ de la période
          DATE_DEBUT <= dates_debut[i] & dates_debut[i] <= DATE_FIN,
          .(DENOM, NOM_DENOM)
        ][
          dt, on = .(DENOM)
        ]
      } else if (input$sg1_type_Rx == "DIN") {
        dt <- nom_marq_comrc[  # ajouter le nom des DIN à la date de départ de la période
          DATE_DEBUT <= dates_debut[i] & dates_debut[i] <= DATE_FIN,
          .(DIN, NOM_MARQ_COMRC)
        ][
          dt, on = .(DIN)
        ]
      }
      DT <- rbind(DT, dt)  # ajouter cette période aux précédentes

    }

    # Ordre des colonnes
    setcolorder(DT, c(
      "DATE_DEBUT", "DATE_FIN",
      input$sg1_type_Rx, nom_type_rx(input$sg1_type_Rx),
      "MNT_MED", "MNT_SERV", "MNT_TOT",
      "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX"
    ))
    # Ordre des données
    setorderv(DT, c("DATE_DEBUT", "DATE_FIN", input$sg1_type_Rx))

    return(DT)
  }
  sg1_table_format <- function(dt) {
    ### Format visuel des colonnes pour meilleure présentation du data créé par
    ### la requête

    dt <- copy(dt)
    dt[  # formatage des résultats pour présentation
      , `:=` (MNT_MED = paste(format_price(MNT_MED), "$"),
              MNT_SERV = paste(format_price(MNT_SERV), "$"),
              MNT_TOT = paste(format_price(MNT_TOT), "$"),
              COHORTE = formatC(COHORTE, big.mark = " "),
              NBRE_RX = formatC(NBRE_RX, big.mark = " "),
              QTE_MED = formatC(QTE_MED, format = "f", digits = 3,
                                big.mark = " ", decimal.mark = ","),
              DUREE_TX = formatC(DUREE_TX, big.mark = " "))
    ]
    return(dt)
  }
  shinyFiles_directories <- function(input_name, method) {
    ### Créer le répertoire à partir d'un shinyFileButton

    if (is.integer(input_name)) {
      return(NULL)
    } else if (method == "save") {
      return(parseSavePath(Volumes_path(), input_name))
    } else if (method == "file") {
      return(parseFilePaths(Volumes_path(), input_name))
    } else {
      stop("formulaire.shinyFiles_directories() method valeur non permise.")
    }
  }
  sql_xl_file_queries_method <- function(method) {

  }
  verif_sg1 <- function(dt, sh, msg_error) {
    ### Vérification de chaque colonne pour la méthode sg1/stat_gen1/Statistiques générales
    ### lorsque les arguments sont inscrit dans un fichier EXCEL

    cols <- cols_EXCEL_file()$sg1  # colonnes nécessaires
    vals <- values_EXCEL_file()$sg1  # valeurs possible des colonnes
    new_error <- TRUE  # nouvelle erreur -> indiquer nom onglet
    init_msg_error <- msg_error  # comparer a la fin pour déterminer s'il y a une erreur

    # Vérifier la présence de chaque colonne
    for (col in cols) {
      if (!col %in% names(dt)) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error," -  La colonne ",col," est absente.\n")
      }
    }

    # Vérifier la valeur dans chaque colonne (celles nécessaires)
    # DATE_DEBUT & DATE_FIN
    for (col in c("DATE_DEBUT", "DATE_FIN")) {
      if (col %in% names(dt)) {  # si la colonne existe - erreur gérée plus haut
        col_date <- str_remove_all(rmNA(dt[[col]]), " ")  # vecteur contenant les valeurs
        if (length(col_date)) {  # s'il y a des valeurs
          col_date <- as_date_excel_chr(col_date)  # convertir au format date
          if (anyNA(col_date)) {  # NA après conversion -> erreur
            if (new_error) {
              msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
              new_error <- FALSE
            }
            msg_error <- paste0(msg_error,
              " -  ",col," a au moins une valeur qui n'est pas au format 'AAAA-MM-JJ'.\n"
            )
          }
        } else {
          # La colonne ne contient pas de valeurs
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
            " -  ",col," ne contient pas de valeurs.\n"
          )
        }
      }
    }
    # TYPE_RX, CODE_SERV_FILTRE, CODE_LIST_FILTRE
    for (col in c("TYPE_RX", "CODE_SERV_FILTRE", "CODE_LIST_FILTRE")) {
      if (col %in% names(dt)) {
        vec <- str_remove_all(rmNA(dt[[col]]), " ")  # extraire la valeur du tableau
        if (length(vec) == 1) {
          if (!all(vec %in% vals[[col]])) {
            if (new_error) {
              msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
              new_error <- FALSE
            }
            msg_error <- paste0(msg_error,
              " -  ",col," ne contient pas une valeur permise.\n"
            )
          }
        } else {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
            " -  ",col," doit contenir une valeur.\n"
          )
        }
      }
    }
    # CODE_RX
    if ("CODE_RX" %in% names(dt)) {
      code_rx <- str_remove_all(rmNA(dt$CODE_RX), " ")
      if (!length(code_rx)) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
          " -  CODE_RX doit contenir au moins une valeur.\n"
        )
      }
    }
    # CODE_SERV, CODE_LIST
    for (col in c("CODE_SERV", "CODE_LIST")) {
      if (col %in% names(dt)) {
        vec <- str_remove_all(rmNA(dt[[col]]), " ")
        if (length(vec)) {
          if (col == "CODE_SERV") {
            vec <- adapt_code_serv(vec)
          }
          if (!all(vec %in% vals[[col]])) {
            if (new_error) {
              msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
              new_error <- FALSE
            }
            msg_error <- paste0(msg_error,
              " -  ",col," ne contient pas une valeur permise.\n"
            )
          }
        }
      }
    }

    # Vérifier s'il y a eu une erreur et ajouter une séparation au texte s'il y a lieu
    if (init_msg_error != msg_error) {
      msg_error <- paste0(msg_error, format_xl_err_nl())
    }

    return(msg_error)

  }
  verif_method <- function() {
    ### Détermine la fonction vérification à utiliser selon la méthode

    return(list(stat_gen1 = verif_sg1))
  }
  Volumes_path <- function() {
    ### Répertoires disponible sur l'ordinateur où l'on peut sélectionner ou
    ### enregistrer un fichier.

    return(c(
      getVolumes()(),
      `Par défaut` = path_home(),
      R = R.home()
    ))
  }

# Datas -------------------------------------------------------------------

  dt_code_list <- create_dt_code_list_med()
  dt_code_serv <- create_dt_code_serv()

# Interface Utilisateur - UI ----------------------------------------------

  ui <- dashboardPage(

    #### HEADER SECTION
    dashboardHeader(title = "Requêtes SQL"),

    #### SIDEBAR SECTION
    dashboardSidebar(
      sidebarMenu(
        ## Connexion SQL - tabConn
        ## Indique si la connexion SQL vers Teradata est faite
        menuItem("Connexion", tabName = "tabConn"),

        ## Requêtes EXCEL - tabEXCEL
        ## ffectuer une ou des requêtes à partir d'un fichier
        ## Excel où chaque onglet est un tableau résultat.
        menuItem("Requêtes via EXCEL", tabName = "tabEXCEL"),

        ## Statistiques Gérales - tabStatGen1
        ## Utiliser les arguments du formulaire shiny pour effectuer une requête
        menuItem("Statistiques générales", tabName = "tabStatGen1")
      )
    ),

    #### BODY SECTION
    dashboardBody(
      tabItems(  # Contenu de chaque section du sidebar
        ### Connexion SQL - tabConn
        ### L'utilisateur entre ses informations pour faire la connexion SQL au
        ### serveur Teradata.
        tabItem(
          tabName = "tabConn",
          # Informations nécessaires à la connexion
          textInput("sql_user", "Identifiant", value = ""),  # no identifiant
          passwordInput("sql_pwd", "Mot de passe", value = ""),  # mot de passe
          # Établir la connexion
          actionButton("sql_conn", "Connexion"),
          # Indiquer l'état de la connexion
          h5(strong("État de la connexion :")),
          verbatimTextOutput("sql_is_conn", placeholder = TRUE)
        ),


        ### Requêtes via EXCEL - tabEXCEL
        ### Importer un fichier EXCEL contenant les arguments et exécuter les
        ### requêtes, soit chaque onglet
        tabItem(
          tabName = "tabEXCEL",
          shinyFilesButton(  # bouton pour sélectionner le fichier Excel
            "select_xl_file", "Sélectionner fichier EXCEL",
            "Sélectionner fichier EXCEL", multiple = FALSE,
            viewtype = "detail"
          ), p(),  # espace entre le bouton et ce qui suit
          # Indiquer les erreurs de chaque onglet s'il y a lieu
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          # Effectuer les extractions s'il n'y a pas d'erreur
          shinySaveButton("save_xl_file", "Exécuter requêtes", "Enregistrer sous...",
                          filetype = list(`Classeur EXCEL` = "xlsx"),
                          viewtype = "list")
          # ---------------------------------------------------- -
          # --- À FAIRE ---
          # # Inscrire le ou les courriels à envoyer les résultats
          # # Peut-être remplacer les résultats par un message
          # h5("Envoyer résultats par courriel"),
          # textAreaInput("mails", "Courriels"),
          # textInput("mail_obj", "Object")
          # ---------------------------------------------------- -
        ),


        ### Statistiques générales - tabStatGen1
        ### Exécuter une requête simple à partir des arguments disponibles dans
        ### le formulaire.
        tabItem(
          tabName = "tabStatGen1",

          # Afficher sur la première ligne
          #   - Nombre de périodes d'analyse
          #   - Nombre de codes Rx à analyser
          #   - Les variables 'Grouper Par', 'Exclusion Code Services',
          #     'Selection Categorie Liste', ...
          fluidRow(
            column(  # Périodes d'analyse
              width = 3,
              # Nombre de périodes à afficher
              numericInput("sg1_nb_per", "Nombre de périodes", value = 1, min = 1, max = 99),
              uiOutput("sg1_nb_per")
            ),
            column(  # Codes d'analyse
              width = 3,
              # Nombre de codes à afficher pour l'analyse
              numericInput("sg1_nb_codes", "Nombre de codes Rx", value = 1,
                           min = 1, max = 99),
              # # Grouper par période d'analyse - regroupe tous les codes ensemble pour les résultats
              # div(style = "margin-top:-20px"),
              # checkboxGroupInput("sg1_group_by", "",
              #                    choiceNames = c("Grouper par période"),
              #                    choiceValues = c("period")),
              # div(style = "margin-top:-5px"),

              # Sélection du type de code Rx
              selectInput("sg1_type_Rx", "Type de code Rx",
                          choices = c("DENOM", "DIN"), selected = "DENOM"),
              # Text inputs où indiquer les codes d'analyse
              uiOutput("sg1_nb_codes")
            ),
            column(
              width = 3,
              checkboxGroupInput("sg1_group_by", "Grouper par",
                                 choiceNames = c("Périodes"),
                                 choiceValues = c("period")),
              # Codes de service
              selectInput("sg1_code_serv_filter", "Codes de Service",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Exclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              # Codes de service
              checkboxGroupInput(
                "sg1_code_serv", "",
                choiceNames = sg1_code_serv_choices(dt_code_serv)$ch_name,
                choiceValues = sg1_code_serv_choices(dt_code_serv)$value,
                selected = c("1", "AD")
              )
            ),
            column(
              width = 3,
              # Codes liste médicaments
              selectInput("sg1_code_list_filter", "Code Liste Médicament",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Inclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "sg1_code_list", "",
                choiceNames = sg1_code_list_choices(dt_code_list)$ch_name,
                choiceValues = sg1_code_list_choices(dt_code_list)$value
              )
            )
          ),


          fluidRow(
            column(
              width = 3,
              # Exécution de la requête SQL
              actionButton("sg1_go_extract", "Exécuter Requête",
                           style = "background-color: #b3d9ff")  # couleur du bouton
            ),
            column(
              width = 3,
              # Sauvegarder les résultats de la requête
              shinySaveButton("sg1_save", "Sauvegarder Résultats en EXCEL",
                              "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                              filetype = list(`Classeur EXCEL` = "xlsx"),  # type de fichier permis
                              viewtype = "list",
                              style = "background-color: #b3d9ff")  # couleur du bouton
            )
          ),

          # Tableau & Affichage extraction SQL
          fluidRow(
            dataTableOutput("sg1_table_req"),
            p(),  # espacement avec la suite
          ),
          fluidRow(
            column(
              width = 3,
              actionButton("sg1_maj_req", "Afficher Code Requête",
                           style = "background-color: #c6ecc6")
            ),
            column(
              width = 3,
              actionButton("sg1_erase_req", "Effacer Code Requête",
                           style = "background-color: #ffc2b3")
            )
          ),
          fluidRow(
            p(),
            verbatimTextOutput("sg1_code_req")
          )
        )
      )
    )

  )


# Server ------------------------------------------------------------------

  server <- function(input, output, session) {

    #### CONNEXION SQL - tabConn
    # Valeurs nécessaires à la connexion de teradata
    conn_values <- reactiveValues(
      uid = NULL, pwd = NULL,  # no utilisateur & mot de passe
      msg = "",  # message d'erreur
      conn = NULL  # contient les paramètres de connexion à utiliser pour une requête
    )

    # Vérifier si les informations entrées sont correctes.
    # Enregistrer les valeurs dans 'conn_values' si c'est le cas.
    observeEvent(input$sql_conn, {
      showNotification("Connexion en cours...", id = "sql_conn", type = "message", duration = NULL)
      if (input$sql_user == "" || input$sql_pwd == "") {
        # Indiquer d'inscrire une valeur dans toutes les cases
        conn_values$msg <- "**Inscrire le numéro d'identifiant ainsi que le mot de passe**"
      } else {
        conn_values$conn <- sql_connexion(input$sql_user, input$sql_pwd)  # effectuer une connexion
        if (is.null(conn_values$conn)) {
          # Message d'erreur si la connexion ne fonctionnait pas
          conn_values$msg <- "**Vérifier l'identifiant et le mot de passe**"
        } else {
          # Si pas d'erreur, on sauvegarde les paramètres de connexion
          conn_values$uid <- toupper(input$sql_user)
          conn_values$pwd <- input$sql_pwd
          conn_values$msg <- paste0("Connexion : ", Sys.time())  # msg indiquant l'heure de la dernière connexion/sauvegarde
        }
      }
      removeNotification("sql_conn")
    })

    # Afficher l'état de la connexion
    output$sql_is_conn <- renderText({ conn_values$msg })



    #### REQUÊTES VIA EXCEL - tabEXCEL
    # Sélection du fichier EXCEL
    shinyFileChoose(input, "select_xl_file", roots = Volumes_path())
    select_xl_file <- reactive({ shinyFiles_directories(input$select_xl_file, "file") })  # select_xl_file()datapath indique répertoire + nom du fichier à importer

    # Indiquer les messages d'erreurs une fois le fichier EXCEL importé
    xl_errors_msg <- eventReactive(select_xl_file(), {  # vérifier le contenu du fichier EXCEL une fois importé
      showNotification("Vérification en cours...", id = "xl_errors_msg", type = "message", duration = NULL)
      msg_error <- msg_error_from_xlfile(select_xl_file()$datapath)
      removeNotification("xl_errors_msg")
      if (is.null(msg_error)) {
        return("Aucune error, exécution possible.")
      } else {
        return(msg_error)
      }
    })
    output$xl_errors_msg <- renderText({ xl_errors_msg() })

    # Bouton pour enregistrer les requêtes via fichier EXCEL
    shinyFileSave(input, "save_xl_file", roots = Volumes_path())  # bouton pour déterminer le répertoire
    save_xl_file <- reactive({ shinyFiles_directories(input$save_xl_file, "save")})
    # Enregistrer les requêtes dans un fichier EXCEL
    observeEvent(save_xl_file(), {
      if (is.null(conn_values$conn)) {
        showNotification("Exécution impossible. Connexion requise.")
      } else if (xl_errors_msg() == "Aucune error, exécution possible.") {
        showNotification("Exécution en cours...", id = "save_xl_file", type = "message", duration = NULL)
        file <- select_xl_file()$datapath
        sheets <- excel_sheets(file)
        excel_requetes <- vector("list", length(sheets))  # contiendra les tableaux à insérer dans un EXCEL
        conn <- conn_values$conn

        # Effectuer la requête de chaque onglet
        for (sh in 1:length(sheets)) {
          suppressMessages(suppressWarnings({
            dt <- read_excel(file, sheet = sheets[sh])
          }))
          method <- str_remove_all(rmNA(dt$METHODE), " ")

          if (method == "stat_gen1") {
            nom_denom <- copy(PROD.V_DENOM_COMNE_MED.NMED_COD_DENOM_COMNE)  # data nom des denom
            nom_marq_comrc <- PROD.V_PRODU_MED.NOM_MARQ_COMRC[, .(DIN, NOM_MARQ_COMRC, DATE_DEBUT, DATE_FIN)]  # data nom marques
            dates_debut <- as_date_excel_chr(str_remove_all(rmNA(dt$DATE_DEBUT), " "))
            dates_fin <- as_date_excel_chr(str_remove_all(rmNA(dt$DATE_FIN), " "))
            type_rx <- str_remove_all(rmNA(dt$TYPE_RX), " ")
            code_rx <- str_remove_all(rmNA(dt$CODE_RX), " ")
            grpby <- str_remove_all(rmNA(dt$GROUPER_PAR), " ")
            code_serv_filtre <- str_remove_all(rmNA(dt$CODE_SERV_FILTRE), " ")
            code_serv <- sort(adapt_code_serv(str_remove_all(rmNA(dt$CODE_SERV), " ")))
            code_list_filtre <- str_remove_all(rmNA(dt$CODE_LIST_FILTRE), " ")
            code_list <- sort(str_remove_all(rmNA(dt$CODE_LIST), " "))
            if (!length(code_serv)) code_serv <- NULL
            if (!length(code_list)) code_list <- NULL
            DT <- data.table()
            for (i in 1:length(dates_debut)) {
              dt <- as.data.table(dbGetQuery(  # requête pour la ième période d'étude
                conn = conn,  # connexion faite à partir de l'onglet connexion
                statement = stat_gen1_txt_query_1period(
                  debut = dates_debut[i], fin = dates_fin[i],
                  type_Rx = type_rx, codes = code_rx,
                  groupby = grpby,
                  code_serv = code_serv, code_serv_filtre = code_serv_filtre,
                  code_list = code_list, code_list_filtre = code_list_filtre
                )
              ))
              dt[, `:=` (MNT_MED = as_price(MNT_MED),  # s'assurer que les prix ont deux décimales
                         MNT_SERV = as_price(MNT_SERV),
                         MNT_TOT = as_price(MNT_TOT))]

              # Ajouter le nom du DENOM ou le nom de la marque commerciale (DIN)
              if (type_rx == "DENOM") {
                dt <- nom_denom[  # ajouter le nom des DENOM à la date de départ de la période
                  DATE_DEBUT <= dates_debut[i] & dates_debut[i] <= DATE_FIN,
                  .(DENOM, NOM_DENOM)
                ][
                  dt, on = .(DENOM)
                ]
              } else if (type_rx == "DIN") {
                dt <- nom_marq_comrc[  # ajouter le nom des DIN à la date de départ de la période
                  DATE_DEBUT <= dates_debut[i] & dates_debut[i] <= DATE_FIN,
                  .(DIN, NOM_MARQ_COMRC)
                ][
                  dt, on = .(DIN)
                ]
              }
              DT <- rbind(DT, dt)  # ajouter cette période aux précédentes

            }
            # Ordre des colonnes
            setcolorder(DT, c(
              "DATE_DEBUT", "DATE_FIN",
              type_rx, nom_type_rx(type_rx),
              "MNT_MED", "MNT_SERV", "MNT_TOT",
              "COHORTE", "NBRE_RX", "QTE_MED", "DUREE_TX"
            ))
            # Ordre des données
            setorderv(DT, c("DATE_DEBUT", "DATE_FIN", type_rx))
          }

          excel_requetes[[sh]] <- create_dt_data_args_query(
            dt = copy(DT),
            args_list = list(METHODE = "stat_gen1", DATE_DEBUT = dates_debut, DATE_FIN = dates_fin,
                             TYPE_RX = type_rx, CODE_RX = code_rx,
                             GROUPER_PAR = grpby,
                             CODE_SERV_FILTRE = code_serv_filtre, CODE_SERV = code_serv,
                             CODE_LIST_FILTRE = code_list_filtre, CODE_LIST = code_list
            ),
            query = stat_gen1_txt_query_1period(
              debut = dates_debut[1], fin = dates_fin[1], type_Rx = type_rx, codes = code_rx,
              groupby = grpby,
              code_serv = code_serv, code_serv_filtre = code_serv_filtre,
              code_list = code_list, code_list_filtre = code_list_filtre
            )
          )
        }
        names(excel_requetes) <- sheets
        write_xlsx(excel_requetes, save_xl_file()$datapath)
        removeNotification("save_xl_file")
      } else {
        showNotification("Exécution impossible.")
      }
    })



    #### STATISTIQUE GENERALES - tabStatGen1
    # Périodes d'analyse : afficher le bon nombre de dateRangeInput selon valeur
    # de input$sg1_nb_per
    output$sg1_nb_per <- renderUI({
      n <- input$sg1_nb_per  # nb périodes & déclenche réactivité
      isolate({  # voir commentaire 'output$sg1_nb_codes'
        dates_input <- vector("list", length = n)
        # Créer des dateRangeInput. Possible de conserver les valeurs précédentes
        # si input$sg1_nb_per diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("sg1_date",i)]])) {
            dates_input[[i]] <- dateRangeInput(
              inputId = paste0("sg1_date",i), label = paste("Période", i),
              separator = " au ",
              autoclose = FALSE  # permet d'écrire manuellement la date sans erreur
            )
          } else {
            dates_input[[i]] <- dateRangeInput(
              inputId = paste0("sg1_date",i), label = paste("Période", i),
              start = input[[paste0("sg1_date",i)]][1],
              end = input[[paste0("sg1_date",i)]][2],
              separator = " au ",
              autoclose = FALSE
            )
          }
        }
        return(tagList(dates_input))
      })
    })

    # Codes Rx d'analyse : afficher le bon nombre de textInput selon la valeur
    # de input$sg1_nb_codes
    output$sg1_nb_codes <- renderUI({
      n <- input$sg1_nb_codes  # nb codes & déclenche réactivité
      isolate({  # enlève la réactivité de chaque input créé, permet d'écrire
                 # dans le textInput sans qu'il y ait de réactivité
        codes_input <- vector("list", length = n)
        # Créer des textInput. Possible de conserver les valeurs précédentes
        # si input$sg1_nb_codes diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("sg1_code",i)]])) {
            codes_input[[i]] <- textInput(inputId = paste0("sg1_code",i),
                                          label = paste("Code Rx", i),
                                          value = "")
          } else {
            codes_input[[i]] <- textInput(inputId = paste0("sg1_code",i),
                                          label = paste("Code Rx", i),
                                          value = input[[paste0("sg1_code",i)]])
          }
        }
        return(tagList(codes_input))
      })
    })

    # Requête SQL
    sg1_requete_sql <- eventReactive(input$sg1_go_extract, {
      if (is.null(conn_values$conn)) {
        showNotification("Exécution impossible. Connexion requise.")
      } else {
        showNotification("Exécution en cours...", id = "sg1_go_extract", type = "message", duration = NULL)
        DT <- sg1_dbGetQuery(input, conn_values$conn)
        removeNotification("sg1_go_extract")
        return(DT)
      }
    })
    # Afficher le tableau demandé
    output$sg1_table_req <- renderDataTable({ sg1_table_format(sg1_requete_sql()) })

    # Enregistrer le fichier au format EXCEL, doit avoir 1) tableau des résultats,
    # 2) les arguments et 3) la requête SQL.
    shinyFileSave(input, "sg1_save", roots = Volumes_path())  # bouton pour déterminer le répertoire
    sg1_file_save <- reactive({ shinyFiles_directories(input$sg1_save, "save") })
    observeEvent(sg1_file_save(), {
      save_EXCEL(
        dt = create_dt_data_args_query(
          dt = sg1_requete_sql(),
          args_list = list(
            METHODE = "stat_gen1",
            DATE_DEBUT = sg1_find_date(input, "deb"),
            DATE_FIN = sg1_find_date(input, "fin"),
            TYPE_RX = input$sg1_type_Rx, CODE_RX = sg1_find_code(input),
            CODE_SERV_FILTRE = input$sg1_code_serv_filter,
            CODE_SERV = adapt_code_serv(input$sg1_code_serv),
            CODE_LIST_FILTRE = input$sg1_code_list_filter,
            CODE_LIST = input$sg1_code_list
          ),
          query = stat_gen1_txt_query_1period(
            debut = sg1_find_date(input, "deb")[1], fin = sg1_find_date(input, "fin")[1],
            type_Rx = input$sg1_type_Rx, codes = sg1_find_code(input),
            groupby = input$sg1_group_by,
            code_serv = input$sg1_code_serv, code_serv_filtre = input$sg1_code_serv_filter,
            code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter
          )
        ),
        save_path = sg1_file_save()
      )
    })

    # Afficher code de la requête SQL généré par les arguments du formulaire
    sg1_show_query <- reactiveValues(
      show = FALSE,  # afficher la requête ou pas
      query = NULL  # message à afficher. NULL = même pas une case où afficher du texte.
    )
    observeEvent(input$sg1_maj_req, {  # si on veut afficher/mettre à jour le code de la requête
      sg1_show_query$show <- TRUE
      sg1_show_query$query <- stat_gen1_txt_query_1period(
        debut = sg1_find_date(input, "deb")[1], fin = sg1_find_date(input, "fin")[1],
        type_Rx = input$sg1_type_Rx, codes = sort(sg1_find_code(input)),
        groupby = input$sg1_group_by,
        code_serv = adapt_code_serv(input$sg1_code_serv), code_serv_filtre = input$sg1_code_serv_filter,
        code_list = sort(input$sg1_code_list), code_list_filtre = input$sg1_code_list_filter
      )
    })
    observeEvent(input$sg1_erase_req, {  # si on veut effacer la requête
      sg1_show_query$show <- FALSE
      sg1_show_query$query <- NULL
    })
    output$sg1_code_req <- reactive({
      if (sg1_show_query$show) {
        return(sg1_show_query$query)
      } else {
        return(NULL)
      }
    })

  }

# Application -------------------------------------------------------------

  shinyApp(ui, server)

}
