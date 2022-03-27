#' Formulaire Interactif
#'
#' Permets d'exécuter des requêtes Excel à partir d'un formulaire interactif. La documentation complète du formulaire, *AIDE_FORMULAIRE_DATE.pdf*, est disponible \href{https://github.com/INESSS-QC/inesss1/tree/master/Documentation}{ici}.
#'
#' **Requêtes via Excel :**\cr
#' Il est conseillé d'utiliser les gabarits Excel pour éviter des erreurs de structures dans les tableaux d'arguments. Les fichiers Excel sont disponibles \href{https://github.com/INESSS-QC/inesss1/tree/master/Documentation/Gabarits}{ici}.
#'
#' @encoding UTF-8
#' @keywords internal
#' @import data.table
#' @importFrom fs path_home
#' @importFrom odbc odbc dbConnect dbGetQuery
#' @importFrom readxl excel_sheets read_excel
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyFiles shinyFilesButton shinyFileChoose shinyFileSave shinySaveButton parseFilePaths parseSavePath getVolumes
#' @importFrom stringr str_split str_remove_all
#' @importFrom writexl write_xlsx
formulaire <- function() {

  # Variables ---------------------------------------------------------------

  cols_Excel_file <- function() {
    ### Colonnes nécessaires pour chaque méthode

    return(list(
      # Naifs/Switch
      ns1 = c("DATE_DEBUT", "DATE_FIN", "TYPE_RX", "CODE_RX", "GROUPER_PAR",
              "TYPE_RX_RETRO", "RX_RETROSPECT_A_EXCLURE", "NJOURS_SANS_CONSO",
              "CODE_SERV_FILTRE", "CODE_SERV",
              "CODE_LIST_FILTRE", "CODE_LIST",
              "AGE_DATE"),
      # Statistiques générales
      sg1 = c("DATE_DEBUT", "DATE_FIN", "TYPE_RX", "CODE_RX", "GROUPER_PAR",
              "CODE_SERV_FILTRE", "CODE_SERV",
              "CODE_LIST_FILTRE", "CODE_LIST",
              "AGE_DATE")
    ))
  }
  values_Excel_file <- function() {
    ### Valeurs permises dans les colonnes de cols_Excel_file()

    return(list(
      # Naifs/Switch
      ns1 = list(
        TYPE_RX = c("DENOM", "DIN"),
        GROUPER_PAR = c("AHFS", "DENOM", "DIN", "CodeList", "CodeServ", "Teneur", "Format", "Age"),
        TYPE_RX_RETRO = c("AHFS", "DENOM", "DIN"),
        CODE_SERV_FILTRE = c("Exclusion", "Inclusion"),
        CODE_SERV = c("1", "AD", "L", "M", "M1", "M2", "M3"),
        CODE_LIST_FILTRE = c("Exclusion", "Inclusion"),
        CODE_LIST = c("3", "03", "40", "41")
      ),
      # Statistiques générales
      sg1 = list(
        TYPE_RX = c("AHFS", "DENOM", "DIN"),
        GROUPER_PAR = c("AHFS", "DENOM", "DIN", "CodeList", "CodeServ", "Teneur", "Format", "Age"),
        CODE_SERV_FILTRE = c("Exclusion", "Inclusion"),
        CODE_SERV = c("1", "AD", "L", "M", "M1", "M2", "M3"),
        CODE_LIST_FILTRE = c("Exclusion", "Inclusion"),
        CODE_LIST = c("3", "03", "40", "41")
      )
    ))
  }
  methods_Excel_file <- function() {
    ### Liste des méthodes existantes

    return(c(
      "naif_switch1",
      "stat_gen1"
    ))
  }
  renderDataTable_options <- function() {
    return(list(
      lengthMenu = list(c(25, 100, -1), c("25", "100", "Tout")),
      pageLength = 100,
      scrollX = TRUE,
      searching = FALSE
    ))
  }
  saveExcel_button_style <- function() {
    ### Couleur et format du bouton qui sauvegarde les tableaux

    return(paste0(
      "color: #ffffff;",
      "background-color: #006600;",
      "border-color: #000000;"
    ))
  }
  reset_button_style <- function() {
    ### Couleur et format du bouton qui réinitialise les arguments

    return(paste0(
      "color: #ffffff;",
      "background-color: #990000;",
      "border-color: #000000;"
    ))
  }

  # Fonctions ---------------------------------------------------------------

  adapt_code_serv <- function(code_serv) {
    ### Séparer des groupes de codes de service en valeurs uniques pour les
    ### requêtes SQL

    if ("L, M, M1 à M3" %in% code_serv) {  # valeurs inscrites dans les formulaires/gabarits
      code_serv <- code_serv[code_serv != "L, M, M1 à M3"]  # effacer cette valeur
      code_serv <- sort(c(code_serv, "L", "M", "M1", "M2", "M3"))  # ajouter chaque valeur individuellement
    }
    # Effectuer les mêmes étapes que précédemment, mais sans espace. Permet de
    # gérer les cas où on importe la colonne avec la fonction str_replace_all(x, " ", "")
    if ("L,M,M1àM3" %in% code_serv) {
      code_serv <- code_serv[code_serv != "L,M,M1àM3"]
      code_serv <- sort(c(code_serv, "L", "M", "M1", "M2", "M3"))
    }
    return(code_serv)
  }
  desadapt_code_serv <- function(code_serv) {
    ### Contraire à la fonction adapt_code_serv()

    if (all(c("L", "M", "M1", "M2", "M3") %in% code_serv)) {
      code_serv <- code_serv[!code_serv %in% c("L", "M", "M1", "M2", "M3")]
      code_serv <- sort(c(code_serv, "L, M, M1 à M3"))
    }
    return(code_serv)
  }
  code_list_choices <- function(dt_code_list) {
    ### Valeurs utilisées pour le choix des codes de liste de médicaments

    dt <- dt_code_list[code %in% c("03", "40", "41")]  # codes utilisés
    dt[, ch_name := paste0(code," : ",desc), .(code)]  # indiquer la description
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))

  }
  code_serv_choices <- function(dt_code_serv) {
    ### Valeurs utilisées pour le choix des codes de service

    dt <- dt_code_serv[code %in% c("1", "AD", "L, M, M1 à M3")]  # codes utilisés
    dt[, ch_name := paste0(code," : ",desc), .(code)]  # indiquer la description
    return(list(ch_name = as.list(dt$ch_name), value = as.list(dt$code)))

  }
  cols_select_from_method <- function(dt, method) {
    ### Puisque l'onglet Excel peut contenir des colonnes facultatives pour le
    ### formulaire, on doit les supprimer après l'avoir importé.
    ### Possible de le faire rapidement grâce à cols_Excel_file()

    # Colonnes à sélectionner
    if (method == "naif_switch1") {
      cols <- names(dt)[names(dt) %in% c("METHODE", cols_Excel_file()$ns1)]
    } else if (method == "stat_gen1") {  # méthode statistiques générales 1
      cols <- names(dt)[names(dt) %in% c("METHODE", cols_Excel_file()$sg1)]
    }

    dt <- dt[, ..cols]  # sélection des colonnes
    return(dt)
  }
  create_dt_code_list_med <- function() {
    ### Data contenant tous les codes de liste de médicament ainsi que leur
    ### description

    dt <- inesss::V_DES_COD[
      TYPE_CODE == "COD_CATG_LISTE_MED",  # sélection des code catégorie liste médicament
      .(code = CODE, desc = CODE_DESC)  # colonnes code + description
    ]
    return(dt)

  }
  create_dt_code_serv <- function() {
    ### Data contenant la liste des codes de services et leur description

    dt <- inesss::V_DEM_PAIMT_MED_CM$COD_SERV[
      , .(code = COD_SERV, desc = COD_SERV_DESC)  # colonnes code + description
    ]

    # Codes L à M3 sont tous inclus dans le même, donc modification du data
    dt <- dt[!code %in% c("L", "M", "M1", "M2", "M3")]
    dt <- rbind(dt, data.table(code = "L, M, M1 à M3", desc = "PREPARATION MAGISTRALE"))
    dt <- setkey(dt, code)
    dt <- dt[dt[, .I[.N], .(code)]$V1]  # description la plus récente des codes de service
    return(dt)

  }
  find_date <- function(input, prefix, method = "deb") {
    ### Trouver toutes les valeurs des dates de début, method="deb", ou de fin
    ### (method = "fin") qui se retrouvent dans input.

    if (method == "deb") {
      type_date <- 1L
    } else if (method == "fin") {
      type_date <- 2L
    } else {
      stop("formulaire.find_date(): valeurs permises de method = {'deb', 'fin'}.")
    }

    if (is.null(input[[paste0(prefix,"_date1")]])) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements. Pourrait être géré autrement avec
      # des ordres de priorité.
      return(as.character(Sys.Date()))
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input[[paste0(prefix,"_nb_per")]]) {
        vec <- c(vec, as.character(input[[paste0(prefix,"_date",i)]][type_date]))
      }
      return(vec)
    }
  }
  find_code <- function(input, prefix) {
    ### Trouver toutes les valeurs de codes d'analyse qui se retrouvent dans
    ### input. Si aucune valeur inscrite, retourne ""

    if (is.null(input[[paste0(prefix, "_code1")]])) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements
      return("")
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input[[paste0(prefix,"_nb_codes")]]) {
        vec <- c(vec, as.character(input[[paste0(prefix,"_code",i)]]))
      }
      return(vec)
    }

  }
  find_code_retro <- function(input, prefix) {
    ### Trouver toutes les valeurs de codes d'analyse qui se retrouvent dans
    ### input. Si aucune valeur inscrite, retourne ""

    if (is.null(input[[paste0(prefix, "_code_retro1")]])) {
      # Cette condition est utilisée seulement au lancement du shiny, c'est pour
      # éviter des messages d'avertissements
      return("")
    } else if (input[[paste0(prefix,"_nb_codes_retro")]] == 1 && input[[paste0(prefix,"_code_retro1")]] == "") {
      return(NULL)
    } else {
      vec <- c()  # pas possible d'utiliser vector(mode, length), à voir...
      for (i in 1:input[[paste0(prefix,"_nb_codes_retro")]]) {
        vec <- c(vec, as.character(input[[paste0(prefix,"_code_retro",i)]]))
      }
      return(vec)
    }

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
    ### À partir d'un fichier Excel, vérifie chaque onglet et renvoie les
    ### messages d'erreur s'il y en a. Utilisé dans la section Requêtes via Excel

    sheets <- excel_sheets(file)  # nom des onglets du fichier importé
    msg_error <- ""  # contiendra les messages d'erreur
    at_least_1 <- FALSE  # doit y avoir au moins un onglet qui contient des arguments

    for (sh in sheets) {
      # Importation du data
      suppressMessages(suppressWarnings({  # supprime avertissements puisque les tableaux sont irréguliers
        dt <- as.data.table(read_excel(file, sheet = sh, col_types = "text"))
      }))

      if ("METHODE" %in% names(dt)) {
        method <- stringr::str_remove_all(rmNA(dt$METHODE), " ")  # méthode à utiliser
        # Gérer les erreurs selon le cas
        if (length(method) == 1) {
          if (any(methods_Excel_file() == method)) {
            dt <- cols_select_from_method(dt, method)  # sélection des colonnes en lien avec la méthode
            if (is.null(rmNA(dt$DATE_DEBUT)) && is.null(rmNA(dt$DATE_FIN))) {
              next  # si pas de valeurs, implique que l'onglet ne doit pas être considéré, donc on passe au prochain
            }
            msg_error <- verif_method()[[method]](dt, sh, msg_error)  # vérifications selon méthode
            if (!at_least_1) {
              at_least_1 <- TRUE
            }
          } else {
            msg_error <- paste0(  # erreur si la méthode est inconnue
              msg_error,
              sh, " :\n",
              " -  METHODE ne contient pas une valeur permise.\n",
              format_xl_err_nl()
            )
            if (!at_least_1) {
              at_least_1 <- TRUE
            }
          }
        } else {
          msg_error <- paste0(  # si la méthode contient plusieurs valeurs ou aucune
            msg_error,
            sh, " :\n",
            " -  METHODE doit contenir une valeur.\n",
            format_xl_err_nl()
          )
          if (!at_least_1) {
            at_least_1 <- TRUE
          }
        }
      } else {
        msg_error <- paste0(  # si la colonne METHODE
          msg_error,
          sh, " :\n",
          " -  METHODE est absent.\n",
          format_xl_err_nl()
        )
        if (!at_least_1) {
          at_least_1 <- TRUE
        }
      }
    }

    if (msg_error == "") {
      if (!at_least_1) {
        return("Aucun onglet ne semble contenir d'arguments.")
      } else {
        return(NULL)
      }
    } else {
      return(msg_error)
    }
  }
  ns1_dbGetQuery <- function(input, conn) {
    ### Effectue la ou les requêtes de naifs switch selon les arguments
    ### @param input : Équivalent à une liste. Les éléments doivent avoir les
    ###                mêmes noms que les input de l'onglet sg1 = Statistiques générales
    ### @param conn_values : Variable de connexion créé dans la section SERVER

    DT <- SQL_naif_switch1(
      conn = conn,
      debut = find_date(input, "ns1", "deb"),
      fin = find_date(input, "sg1", "fin"),
      type_Rx = input$ns1_type_Rx,
      codes = find_code(input, "ns1"),
      group_by = input$ns1_group_by,
      type_Rx_retro = input$ns1_type_Rx_retro,
      rx_retrospect_a_exclure = find_code_retro(input, "ns1"),
      njours_sans_conso = input$ns1_njours_sans_conso,
      code_serv = adapt_code_serv(input$ns1_code_serv),
      code_serv_filtre = input$ns1_code_serv_filter,
      code_list = input$ns1_code_list,
      code_list_filtre = input$ns1_code_list_filter,
      age_date = input$ns1_age_date
    )
    return(DT)

  }
  save_Excel <- function(dt, save_path) {
    ### Enregistrer au format Excel

    write_xlsx(dt, save_path$datapath)

  }
  save_xl_file_queries_method <- function(conn, filepath, savepath) {
    ### Effectuer la requête de chaque onglet du fichier Excel contenant les
    ### arguments de chacune d'elles.

    sheets <- excel_sheets(filepath)  # nom des onglets du fichier excel
    excel_requetes <- list()  # contiendra les tableaux résultats

    i <- 1L
    for (sh in 1:length(sheets)) {
      suppressMessages(suppressWarnings({  # supprimer messages d'avertissements
        dt <- read_excel(filepath, sheet = sheets[sh])  # importer les arguments
      }))
      method <- str_remove_all(rmNA(dt$METHODE), " ")  # détecter la méthode
      # Tableau des résultats selon la méthode à utiliser
      if (is.null(rmNA(dt$DATE_DEBUT)) && is.null(rmNA(dt$DATE_FIN))) {
        next
      } else {
        if (method == "naif_switch1") {
          excel_requetes[[i]] <- save_xl_file_queries_ns1(dt, conn)
        } else if (method == "stat_gen1") {
          excel_requetes[[i]] <- save_xl_file_queries_sg1(dt, conn)
        }
        names(excel_requetes)[i] <- sheets[sh]  # conserver le nom initial des onglets
        i <- i + 1L
      }

    }

    write_xlsx(excel_requetes, savepath)  # sauvegarder les tableaux en Excel sur le poste

  }
  save_xl_file_queries_ns1 <- function(dt, conn) {
    ### Référence à save_xl_file_queries_method() lorsque la méthode est "naif_switch1"

    ### Arguments selon les valeurs du tableau dt
    dates_debut <- as_date_excel_chr(stringr::str_remove_all(rmNA(dt$DATE_DEBUT), " "))
    dates_fin <- as_date_excel_chr(stringr::str_remove_all(rmNA(dt$DATE_FIN), " "))
    type_rx <- stringr::str_remove_all(rmNA(dt$TYPE_RX), " ")
    code_rx <- stringr::str_remove_all(rmNA(dt$CODE_RX), " ")
    grpby <- stringr::str_remove_all(rmNA(dt$GROUPER_PAR), " ")
    if (!length(grpby)) {
      grpby <- NULL
    }
    type_rx_retro <- stringr::str_remove_all(rmNA(dt$TYPE_RX_RETRO), " ")
    if (!length(type_rx_retro)) {
      type_rx_retro <- type_rx
    }
    rx_retro <- stringr::str_remove_all(rmNA(dt$RX_RETROSPECT_A_EXCLURE), " ")
    if (!length(rx_retro)) {
      rx_retro <- code_rx
    }
    njours_sans_conso <- as.numeric(stringr::str_remove_all(rmNA(dt$NJOURS_SANS_CONSO), " "))
    code_serv_filtre <- stringr::str_remove_all(rmNA(dt$CODE_SERV_FILTRE), " ")
    if (!length(code_serv_filtre)) {
      code_serv_filtre <- NULL
    }
    code_serv <- sort(adapt_code_serv(stringr::str_remove_all(rmNA(dt$CODE_SERV), " ")))
    if (!length(code_serv)) {
      code_serv <- NULL
    }
    code_list_filtre <- stringr::str_remove_all(rmNA(dt$CODE_LIST_FILTRE), " ")
    if (!length(code_list_filtre)) {
      code_list_filtre <- NULL
    }
    code_list <- sort(stringr::str_remove_all(rmNA(dt$CODE_LIST), " "))
    if (!length(code_list)) {
      code_list <- NULL
    }
    age_date <- stringr::str_remove_all(rmNA(dt$AGE_DATE), " ")
    if (!length(age_date)) {
      age_date <- dates_debut[1]
    }

    ### Tableau des résultats
    DT <- SQL_naif_switch1(
      conn,
      dates_debut, dates_fin,
      type_rx, code_rx, grpby,
      type_rx_retro, rx_retro, njours_sans_conso,
      code_serv, code_serv_filtre,
      code_list, code_list_filtre,
      age_date
    )

    return(create_dt_data_args_query(
      dt = DT,
      args_list = list(
        METHODE = "naif_switch1",
        DATE_DEBUT = dates_debut, DATE_FIN = dates_fin,
        TYPE_RX = type_rx, CODE_RX = code_rx, GROUPER_PAR = grpby,
        TYPE_RX_RETRO = type_rx_retro, RX_RETROSPECT_A_EXCLURE = rx_retro,
        NJOURS_SANS_CONSO = njours_sans_conso,
        CODE_SERV_FILTRE = code_serv_filtre, CODE_SERV = code_serv,
        CODE_LIST_FILTRE = code_list_filtre, CODE_LIST = code_list,
        AGE_DATE = age_date
      ),
      query = query_naif_switch1(
        debut = dates_debut[1], fin = dates_fin[1],
        type_Rx = type_rx, codes = code_rx, group_by = grpby,
        type_Rx_retro = type_rx_retro, rx_retrospect_a_exclure = rx_retro,
        njours_sans_conso = njours_sans_conso,
        code_serv = code_serv, code_serv_filtre = code_serv_filtre,
        code_list = code_list, code_list_filtre = code_list_filtre,
        age_date = age_date
      )
    ))

  }
  save_xl_file_queries_sg1 <- function(dt, conn) {
    ### Référence à save_xl_file_queries_method() lorsque la méthode est "stat_gen1"

    # Arguments selon les valeurs du tableau dt
    dates_debut <- as_date_excel_chr(str_remove_all(rmNA(dt$DATE_DEBUT), " "))
    dates_fin <- as_date_excel_chr(str_remove_all(rmNA(dt$DATE_FIN), " "))
    type_rx <- str_remove_all(rmNA(dt$TYPE_RX), " ")
    code_rx <- str_remove_all(rmNA(dt$CODE_RX), " ")
    grpby <- str_remove_all(rmNA(dt$GROUPER_PAR), " ")
    if (!length(grpby)) {
      grpby <- NULL
    }
    code_serv_filtre <- str_remove_all(rmNA(dt$CODE_SERV_FILTRE), " ")
    code_serv <- sort(adapt_code_serv(str_remove_all(rmNA(dt$CODE_SERV), " ")))
    if (!length(code_serv)) {
      code_serv <- NULL
    }
    code_list_filtre <- str_remove_all(rmNA(dt$CODE_LIST_FILTRE), " ")
    code_list <- sort(str_remove_all(rmNA(dt$CODE_LIST), " "))
    if (!length(code_list)) {
      code_list <- NULL
    }
    age_date <- stringr::str_remove_all(rmNA(dt$AGE_DATE), " ")
    if (!length(age_date)) {
      age_date <- NULL
    }

    # Tableau des résultats
    DT <- SQL_stat_gen1(
      conn = conn, debut = dates_debut, fin = dates_fin,
      type_Rx = type_rx, codes = code_rx, group_by = grpby,
      code_serv = code_serv, code_serv_filtre = code_serv_filtre,
      code_list = code_list, code_list_filtre = code_list_filtre,
      age_date = age_date
    )

    # Mettre sur une page
    #   - Tableau des résultats
    #   - Arguments
    #   - Exemple de code SQL de la première période d'étude
    return(create_dt_data_args_query(
      dt = DT,  # tableau des résultats
      args_list = list(  # liste des arguments
        METHODE = "stat_gen1", DATE_DEBUT = dates_debut, DATE_FIN = dates_fin,
        TYPE_RX = type_rx, CODE_RX = code_rx, GROUPER_PAR = grpby,
        CODE_SERV_FILTRE = code_serv_filtre, CODE_SERV = desadapt_code_serv(code_serv),
        CODE_LIST_FILTRE = code_list_filtre, CODE_LIST = code_list,
        AGE_DATE = age_date
      ),
      query = query_stat_gen1(  # code SQL de la 1ere période d'étude
        debut = dates_debut[1], fin = dates_fin[1],
        type_Rx = type_rx, codes = code_rx, group_by = grpby,
        code_serv = code_serv, code_serv_filtre = code_serv_filtre,
        code_list = code_list, code_list_filtre = code_list_filtre,
        age_date = age_date
      )
    ))

  }
  sg1_dbGetQuery <- function(input, conn) {
    ### Effectue la ou les requêtes de statistiques générales selon les arguments
    ### @param input : Équivalent à une liste. Les éléments doivent avoir les
    ###                mêmes noms que les input de l'onglet sg1 = Statistiques générales
    ### @param conn_values : Variable de connexion créé dans la section SERVER

    DT <- SQL_stat_gen1(
      conn = conn,
      debut = find_date(input, "sg1", "deb"), fin = find_date(input, "sg1", "fin"),
      type_Rx = input$sg1_type_Rx, codes = find_code(input, "sg1"), group_by = input$sg1_group_by,
      code_serv = adapt_code_serv(input$sg1_code_serv), code_serv_filtre = input$sg1_code_serv_filter,
      code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter,
      age_date = input$sg1_age_date
    )
    return(DT)

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
  table_format <- function(dt) {
    ### Format visuel des colonnes pour meilleure présentation du data créé par
    ### la requête

    if (is.null(dt)) {
      return(NULL)
    } else {
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

  }
  verif_ns1 <- function(dt, sh, msg_error) {
    ### Vérification de chaque colonne pour la méthode ns1/naif_switch1
    ### lorsque les arguments sont inscrit dans un fichier Excel

    cols <- cols_Excel_file()$ns1  # colonnes nécessaires
    vals <- values_Excel_file()$ns1  # valeurs possible des colonnes
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

    # DATE_DEBUT & DATE_FIN doivent avoir le même nombre de valeurs & debut <= fin
    if ("DATE_DEBUT" %in% names(dt) && "DATE_FIN" %in% names(dt)) {
      # Même nombre de valeurs
      lng_deb <- length(str_remove_all(rmNA(dt$DATE_DEBUT), " "))  # nombre de valeurs
      lng_fin <- length(str_remove_all(rmNA(dt$DATE_FIN), " "))
      if (lng_deb != lng_fin) {  # si le nbre de valeurs est différent
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  DATE_DEBUT et DATE_FIN n'ont pas le même nombre de valeurs.\n"
        )
      }
      # Debut <= Fin
      if (any(stringr::str_remove_all(rmNA(dt$DATE_DEBUT), " ") > stringr::str_remove_all(rmNA(dt$DATE_FIN), " "))) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  DATE_DEBUT doit être plus petit ou égal à DATE_FIN.\n"
        )
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
      if (length(code_rx)) {
        # nbr_NAs <- sum(is.na(code_rx))
        # code_rx <- as.numeric(code_rx)
        # if (sum(is.na(code_rx)) != nbr_NAs) {
        #   if (new_error) {
        #     msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
        #     new_error <- FALSE
        #   }
        #   msg_error <- paste0(msg_error,
        #                       " -  CODE_RX doit contenir des valeurs numériques.\n"
        #   )
        # }
      } else {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  CODE_RX doit contenir au moins une valeur.\n"
        )
      }
    }

    # GROUPER_PAR
    if ("GROUPER_PAR" %in% names(dt)) {
      grpby <- str_remove_all(rmNA(dt$GROUPER_PAR), " ")
      if (length(grpby)) {
        if (!all(grpby %in% vals$GROUPER_PAR)) {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
                              " -  GROUPER_PAR ne contient pas une valeur permise.\n"
          )
        }
      }
    }

    # TYPE_RX_RETRO
    if (any(names(dt) == "TYPE_RX_RETRO")) {
      type_rx_retro <- str_remove_all(rmNA(dt$TYPE_RX_RETRO), " ")  # extraire la valeur du tableau
      if (length(type_rx_retro) && !any(vals$TYPE_RX_RETRO == type_rx_retro)) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  TYPE_RX_RETRO ne contient pas une valeur permise.\n"
        )
      }
    }

    # NJOURS_SANS_CONSO
    if ("NJOURS_SANS_CONSO" %in% names(dt)) {
      no_conso <- stringr::str_remove_all(rmNA(dt$NJOURS_SANS_CONSO), " ")
      if (length(no_conso) > 1) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  NJOURS_SANS_CONSO doit contenir une seule valeur.\n"
        )
      }
      if (length(no_conso)) {
        nbr_NAs <- sum(is.na(no_conso))
        no_conso <- as.numeric(no_conso)
        if (sum(is.na(no_conso)) != nbr_NAs) {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
                              " -  NJOURS_SANS_CONSO doit contenir une valeur numérique.\n"
          )
        }
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

    # AGE_DATE
    if (any(names(dt) == "AGE_DATE")) {
      age_date <- stringr::str_remove_all(rmNA(dt$AGE_DATE), " ")
      if (length(age_date) > 1) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  AGE_DATE doit contenir seulement une valeur.\n")
      } else if (length(age_date)) {
        age_date <- as_date_excel_chr(age_date)
        if (anyNA(age_date)) {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
                              " -  AGE_DATE n'est pas une date au format 'AAAA-MM-JJ'.\n")
        }
      }
    }

    # Vérifier s'il y a eu une erreur et ajouter une séparation au texte s'il y a lieu
    if (init_msg_error != msg_error) {
      msg_error <- paste0(msg_error, format_xl_err_nl())
    }

    return(msg_error)

  }
  verif_sg1 <- function(dt, sh, msg_error) {
    ### Vérification de chaque colonne pour la méthode sg1/stat_gen1/Statistiques générales
    ### lorsque les arguments sont inscrit dans un fichier Excel

    cols <- cols_Excel_file()$sg1  # colonnes nécessaires
    vals <- values_Excel_file()$sg1  # valeurs possible des colonnes
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

    # DATE_DEBUT & DATE_FIN doivent avoir le même nombre de valeurs & Debut <= Fin
    if ("DATE_DEBUT" %in% names(dt) && "DATE_FIN" %in% names(dt)) {
      lng_deb <- length(str_remove_all(rmNA(dt$DATE_DEBUT), " "))  # nombre de valeurs
      lng_fin <- length(str_remove_all(rmNA(dt$DATE_FIN), " "))
      if (lng_deb != lng_fin) {  # si le nbre de valeurs est différent
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  DATE_DEBUT et DATE_FIN n'ont pas le même nombre de valeurs.\n"
        )
      }
      # Debut <= Fin
      if (any(stringr::str_remove_all(rmNA(dt$DATE_DEBUT), " ") > stringr::str_remove_all(rmNA(dt$DATE_FIN), " "))) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  DATE_DEBUT doit être plus petit ou égal à DATE_FIN.\n"
        )
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
      if (length(code_rx)) {
        # nbr_NAs <- sum(is.na(code_rx))
        # code_rx <- as.numeric(code_rx)
        # if (sum(is.na(code_rx)) != nbr_NAs) {
        #   if (new_error) {
        #     msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
        #     new_error <- FALSE
        #   }
        #   msg_error <- paste0(msg_error,
        #                       " -  CODE_RX doit contenir des valeurs numériques.\n"
        #   )
        # }
      } else {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  CODE_RX doit contenir au moins une valeur.\n"
        )
      }
    }

    # GROUPER_PAR
    if ("GROUPER_PAR" %in% names(dt)) {
      grpby <- str_remove_all(rmNA(dt$GROUPER_PAR), " ")
      if (length(grpby)) {
        if (!all(grpby %in% vals$GROUPER_PAR)) {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))  # indiquer nom d'onglet
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
                              " -  GROUPER_PAR ne contient pas une valeur permise.\n"
          )
        }
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

    # AGE_DATE
    if (any(names(dt) == "AGE_DATE")) {
      age_date <- stringr::str_remove_all(rmNA(dt$AGE_DATE), " ")
      if (length(age_date) > 1) {
        if (new_error) {
          msg_error <- paste0(msg_error, format_xl_err_sh(sh))
          new_error <- FALSE
        }
        msg_error <- paste0(msg_error,
                            " -  AGE_DATE doit contenir seulement une valeur.\n")
      } else if (length(age_date)) {
        age_date <- as_date_excel_chr(age_date)
        if (anyNA(age_date)) {
          if (new_error) {
            msg_error <- paste0(msg_error, format_xl_err_sh(sh))
            new_error <- FALSE
          }
          msg_error <- paste0(msg_error,
                              " -  AGE_DATE n'est pas une date au format 'AAAA-MM-JJ'.\n")
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

    return(list(
      naif_switch1 = verif_ns1,
      stat_gen1 = verif_sg1
    ))

  }
  Volumes_path <- function() {
    ### Répertoires disponible sur l'ordinateur où l'on peut sélectionner ou
    ### enregistrer un fichier.

    if (tolower(stringr::str_sub(Sys.info()[["login"]], 1, 2)) == "ms") {
      return(c(
        `Bureau` = paste0("C:/Users/",Sys.info()[["login"]],"/Desktop"),
        `Par défaut` = path_home(),
        R = R.home(),
        getVolumes()()
      ))
    } else {
      return(c(
        `Par défaut` = path_home(),
        R = R.home(),
        getVolumes()()
      ))
    }

  }

  # Datas -------------------------------------------------------------------

  dt_code_list <- create_dt_code_list_med()
  dt_code_serv <- create_dt_code_serv()


  # UI ----------------------------------------------------------------------

  ui <- dashboardPage(

    #### HEADER SECTION
    dashboardHeader(title = paste0("version ", as.character(packageVersion("inesss")))),

    #### SIDEBAR SECTION
    dashboardSidebar(
      sidebarMenu(
        div(style = "margin-top:10px"),
        ### Connexion SQL - tabConn
        menuItem("Connexion", tabName = "tabConn"),

        div(style = "margin-top:30px"),

        ### Méthodes
        menuItem("Requêtes via Excel", tabName = "tabExcel"),
        menuItem("Naïfs / Switch", tabName = "tabNaifsSwitch"),
        menuItem("Statistiques générales", tabName = "tabStatGen1"),

        div(style = "margin-top:30px"),

        ### Tables interactives
        menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        menuItem("V_DEM_PAIMT_MED_CM", tabName = "tabV_DEM_PAIMT_MED_CM"),
        menuItem("V_DENOM_COMNE_MED", tabName = "tabV_DENOM_COMNE_MED"),
        menuItem("V_DES_COD", tabName = "tabV_DES_COD"),
        menuItem("V_PRODU_MED", tabName = "tabV_PRODU_MED")
      ),
      width = 260
    ),

    #### BODY SECTION
    dashboardBody(
      tabItems(  # Contenu de chaque section du sidebar


        # UI - Connexion ------------------------------------------------------------------------------

        tabItem(
          tabName = "tabConn",
          # Informations nécessaires à la connexion
          textInput("sql_user", "Identifiant", value = ""),  # no identifiant
          passwordInput("sql_pwd", "Mot de passe", value = ""),  # mot de passe
          # Établir la connexion ou se déconnecter
          fluidRow(
            column(2, actionButton("sql_conn", "Connexion")),
            column(2, actionButton("sql_deconn", "Déconnexion"))
          ),
          # Indiquer l'état de la connexion
          h5(strong("État de la connexion :")),
          verbatimTextOutput("sql_is_conn", placeholder = TRUE)
        ),


        # UI - Requêtes Excel -------------------------------------------------------------------------

        tabItem(
          tabName = "tabExcel",
          shinyFilesButton(  # bouton pour sélectionner le fichier Excel
            "select_xl_file", "Sélectionner fichier Excel",
            "Sélectionner fichier Excel", multiple = FALSE,
            viewtype = "detail"
          ), p(),  # espace entre le bouton et ce qui suit
          # Indiquer le fichier qui a été sélectionné (répertoire complet)
          textOutput("xl_file_path"),
          # Indiquer les erreurs de chaque onglet s'il y a lieu
          verbatimTextOutput("xl_errors_msg", placeholder = TRUE),
          # Effectuer les extractions s'il n'y a pas d'erreur
          uiOutput("save_xl_file")
        ),



        # UI - naif_switch1 -------------------------------------------------------

        tabItem(
          tabName = "tabNaifsSwitch",

          # ARGUMENTS SECTION
          fluidRow(
            h4(HTML("&nbsp;&nbsp;"), "Arguments"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px"),
          fluidRow(
            column(  # Période d'analyse
              width = 3,
              # Nombre de périodes à afficher
              numericInput("ns1_nb_per", "Nombre de périodes", value = 1),
              uiOutput("ns1_nb_per")
            ),
            column(  # Codes d'analyse
              width = 3,
              # Nombre de codes à afficher pour l'analyse
              numericInput("ns1_nb_codes", "Nombre de Codes Rx", value = 1),
              # Sélection du type de code Rx
              selectInput("ns1_type_Rx", "Type de Code Rx",
                          choices = c("AHFS", "DENOM", "DIN"), selected = "DENOM"),
              # Text inputs où indiquer les codes d'analyse
              uiOutput("ns1_nb_codes")
            ),
            column(  # Codes rétrospectif
              width = 3,
              numericInput("ns1_nb_codes_retro", "Nombre de Codes Rx Rétrospectifs", value = 1),
              # Sélection du type de code Rx
              selectInput("ns1_type_Rx_retro", "Type de Code Rx Rétrospectifs",
                          choices = c("AHFS", "DENOM", "DIN"), selected = "DENOM"),
              # Text inputs où indiquer les codes d'analyse
              uiOutput("ns1_nb_codes_retro")
            ),
            column(
              width = 3,
              # Grouper par
              checkboxGroupInput(
                "ns1_group_by", "Grouper par",
                choices = c("AHFS", "DENOM", "DIN", "CodeServ", "CodeList", "Teneur", "Format", "Age"),
                selected = "DENOM"
              ),
              div(style = "margin-top:-10px"),
              # Date pour calcul de l'âge
              uiOutput("ns1_age_date"),
              # Nombre de jours sans traitement
              numericInput("ns1_njours_sans_conso", "Jours sans Consommation", value = 365),
              # Codes de services
              selectInput("ns1_code_serv_filter", "Codes de Services",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Exclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "ns1_code_serv", "",
                choiceNames = code_serv_choices(dt_code_serv)$ch_name,
                choiceValues = code_serv_choices(dt_code_serv)$value,
                selected = c("1", "AD")
              ),
              # Codes liste médicaments
              selectInput("ns1_code_list_filter", "Codes Liste Médicament",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Inclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "ns1_code_list", "",
                choiceNames = code_list_choices(dt_code_list)$ch_name,
                choiceValues = code_list_choices(dt_code_list)$value
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              actionButton(  # Exécution de la requête SQL
                "ns1_go_extract", "Exécuter Requête",
                style = paste0("color: #ffffff;",
                               "background-color: #006600;",
                               "border-color: #000000;")
              )
            ),
            column(
              width = 3,
              actionButton("ns1_reset_args", "Réinitialiser Arguments", style = reset_button_style())
            )
          ),

          # RESULTATS SECTION
          uiOutput("ns1_html_result_section"),  # En-tête
          fluidRow(
            p(),
            dataTableOutput("ns1_table_req"),  # tableau des résultats
            p()
          ),
          fluidRow(
            # column(
            #   width = 3,
            #   uiOutput("sg1_save") # bouton sauvegarder les résultats de la requête
            # )
          ),
        ),


        # UI - stat_gen1 ------------------------------------------------------------------------------

        tabItem(
          tabName = "tabStatGen1",

          # ARGUMENTS SECTION
          fluidRow(
            h4(HTML("&nbsp;&nbsp;"), "Arguments"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px"),
          fluidRow(
            column(  # Périodes d'analyse
              width = 3,
              # Nombre de périodes à afficher
              numericInput("sg1_nb_per", "Nombre de périodes", value = 1),
              uiOutput("sg1_nb_per")
            ),
            column(  # Codes d'analyse
              width = 3,
              # Nombre de codes à afficher pour l'analyse
              numericInput("sg1_nb_codes", "Nombre de Codes Rx", value = 1),
              # Sélection du type de code Rx
              selectInput("sg1_type_Rx", "Type de Code Rx",
                          choices = c("AHFS", "DENOM", "DIN"), selected = "DENOM"),
              # Text inputs où indiquer les codes d'analyse
              uiOutput("sg1_nb_codes")
            ),
            column(
              width = 2,
              # Grouper par
              checkboxGroupInput(
                "sg1_group_by", "Grouper par",
                choices = c("AHFS", "DENOM", "DIN", "CodeServ", "CodeList", "Teneur", "Format", "Age"),
                selected = "DENOM"
              ),
              div(style = "margin-top:-10px"),
              uiOutput("sg1_age_date")
            ),
            column(
              width = 4,
              # Codes de services
              selectInput("sg1_code_serv_filter", "Codes de Services",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Exclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "sg1_code_serv", "",
                choiceNames = code_serv_choices(dt_code_serv)$ch_name,
                choiceValues = code_serv_choices(dt_code_serv)$value,
                selected = c("1", "AD")
              ),
              # Codes liste médicaments
              selectInput("sg1_code_list_filter", "Codes Liste Médicament",
                          choices = c("Exclusion", "Inclusion"),
                          selected = "Inclusion", multiple = FALSE),
              div(style = "margin-top:-30px"),  # coller le checkBox qui suit
              checkboxGroupInput(
                "sg1_code_list", "",
                choiceNames = code_list_choices(dt_code_list)$ch_name,
                choiceValues = code_list_choices(dt_code_list)$value
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              actionButton(  # Exécution de la requête SQL
                "sg1_go_extract", "Exécuter Requête",
                style = paste0("color: #ffffff;",
                               "background-color: #006600;",
                               "border-color: #000000;")
              )
            ),
            column(
              width = 3,
              actionButton("sg1_reset_args", "Réinitialiser Arguments", style = reset_button_style())
            )
          ),


          # RESULTATS SECTION
          uiOutput("sg1_html_result_section"),  # En-tête
          fluidRow(
            p(),
            dataTableOutput("sg1_table_req"),  # tableau des résultats
            p()
          ),
          fluidRow(
            column(
              width = 3,
              uiOutput("sg1_save") # bouton sauvegarder les résultats de la requête
            )
          ),


          # CODE SQL SECTION
          fluidRow(
            uiOutput("sg1_html_SQL_section"),
            uiOutput("sg1_html_code_SQL")
          )
        ),

        # UI - I_APME_DEM_AUTOR_CRITR_ETEN_CM ---------------------------------------------------------

        tabItem(
          tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM",
          selectInput(inputId = "I_APME_DEM_AUTOR_CRITR_ETEN_CM__data",
                      label = "Élément",
                      choices = names(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM)),
          uiOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__params"),
          dataTableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt")
        ),

        # UI - V_DEM_PAIMT_MED_CM ---------------------------------------------------------------------

        tabItem(
          tabName = "tabV_DEM_PAIMT_MED_CM",
          selectInput(inputId = "V_DEM_PAIMT_MED_CM__data",
                      label = "Élément",
                      choices = names(inesss::V_DEM_PAIMT_MED_CM)),
          uiOutput("V_DEM_PAIMT_MED_CM__params"),
          dataTableOutput("V_DEM_PAIMT_MED_CM__dt")
        ),

        # UI - V_DENOM_COMNE_MED ----------------------------------------------------------------------

        tabItem(
          tabName = "tabV_DENOM_COMNE_MED",
          uiOutput("V_DENOM_COMNE_MED__params"),
          dataTableOutput("V_DENOM_COMNE_MED__dt")
        ),

        # UI - V_DES_COD ------------------------------------------------------------------------------

        tabItem(
          tabName = "tabV_DES_COD",
          uiOutput("V_DES_COD__params"),
          dataTableOutput("V_DES_COD__dt")
        ),

        # UI - V_PRODU_MED ----------------------------------------------------------------------------

        tabItem(
          tabName = "tabV_PRODU_MED",
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


    # General -------------------------------------------------------------------------------------

    ### Fermer l'application lorsque la fenêtre se ferme
    session$onSessionEnded(function() {stopApp()})




    # Connexion -----------------------------------------------------------------------------------

    # Valeurs nécessaires à la connexion de teradata
    conn_values <- reactiveValues(
      uid = NULL, pwd = NULL,  # no utilisateur & mot de passe
      msg = "",  # message d'erreur
      conn = FALSE  # paramètres de connexion. FALSE au lieu de NULL pour éviter des messages d'erreur lors du démarrage
    )

    # Vérifier si les informations entrées sont correctes.
    # Enregistrer les valeurs dans 'conn_values' si c'est le cas.
    observeEvent(input$sql_conn, {
      showNotification("Connexion en cours...", id = "sql_conn", type = "message", duration = NULL)
      if (input$sql_user == "" || input$sql_pwd == "") {
        # Indiquer d'inscrire une valeur dans toutes les cases
        conn_values$msg <- "**Inscrire le numéro d'identifiant ainsi que le mot de passe**"
        conn_values$conn <- NULL  # aucune connexion
      } else {
        conn_values$conn <- SQL_connexion(input$sql_user, input$sql_pwd)  # effectuer une connexion
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

    # Déconnexion à Teradata
    observeEvent(input$sql_deconn, {
      # Supprimer les informations
      conn_values$conn <- NULL  # aucune connexion
      conn_values$msg <- NULL
      conn_values$uid <- NULL
      conn_values$pwd <- NULL
      # Effacer les informations inscrites dans les textInput
      updateTextInput(session, "sql_user", value = "")
      updateTextInput(session, "sql_pwd", value = "")
    })

    # Afficher l'état de la connexion
    output$sql_is_conn <- renderText({ conn_values$msg })


    # Requêtes via Excel --------------------------------------------------------------------------

    # Sélection du fichier Excel
    shinyFileChoose(input, "select_xl_file", roots = Volumes_path())
    select_xl_file <- reactive({ shinyFiles_directories(input$select_xl_file, "file") })  # select_xl_file()datapath indique répertoire + nom du fichier à importer

    # Indiquer le fichier sélectionner (répertoire complet)
    output$xl_file_path <- renderText({
      if (is.null(select_xl_file)) {
        return(NULL)
      } else {
        return(select_xl_file()$datapath)
      }
    })

    # Indiquer les messages d'erreurs une fois le fichier Excel importé
    xl_errors_msg <- eventReactive(select_xl_file(), {  # vérifier le contenu du fichier Excel une fois importé
      showNotification("Vérification en cours...", id = "xl_errors_msg", type = "message", duration = NULL)
      msg_error <- msg_error_from_xlfile(select_xl_file()$datapath)
      removeNotification("xl_errors_msg")
      if (is.null(msg_error)) {
        return("Aucune erreur, exécution possible.")
      } else {
        return(msg_error)
      }
    })
    output$xl_errors_msg <- renderText({ xl_errors_msg() })

    # Bouton pour enregistrer les requêtes via fichier Excel
    output$save_xl_file <- renderUI({
      if (!is.null(attr(conn_values$conn, "info")) && xl_errors_msg() == "Aucune erreur, exécution possible.") {
        return(shinySaveButton(
          "save_xl_file", "Exécuter requêtes", "Enregistrer sous...",
          filetype = list(`Classeur Excel` = "xlsx"),
          viewtype = "list",
          style = saveExcel_button_style()
        ))
      } else {
        return(NULL)
      }
    })
    shinyFileSave(input, "save_xl_file", roots = Volumes_path())  # bouton pour déterminer le répertoire
    save_xl_file <- reactive({ shinyFiles_directories(input$save_xl_file, "save")})
    # Enregistrer les requêtes dans un fichier Excel
    observeEvent(save_xl_file(), {
      if (is.null(conn_values$conn)) {
        showNotification("Exécution impossible. Vérifier la connexion ou corriger les erreurs.")
      } else if (is.logical(conn_values$conn) && conn_values$conn == FALSE) {
        showNotification("Exécution impossible. Vérifier la connexion ou corriger les erreurs.")
      } else {
        showNotification("Exécution en cours...", id = "save_xl_file", type = "message", duration = NULL)
        save_xl_file_queries_method(  # effectuer les requêtes pour chaque onglet du fichier
          conn = conn_values$conn,
          filepath = select_xl_file()$datapath,
          savepath = save_xl_file()$datapath
        )
        removeNotification("save_xl_file")
      }
    }, ignoreInit = TRUE)




    # naif_switch1 ------------------------------------------------------------

    ns1_val <- reactiveValues(
      show_query = FALSE,  # afficher la requête ou pas
      query = NULL,  # message à afficher. NULL = même pas une case où afficher du texte
      show_tab = FALSE  # contient le tableau de la requête/résultats
    )

    # Périodes d'analyse : afficher le bon nombre de dateRangeInput selon valeur
    # de input$ns1_nb_per
    output$ns1_nb_per <- renderUI({
      n <- input$ns1_nb_per  # nb périodes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "ns1_nb_per", value = 1)
      }
      isolate({  # voir commentaire 'output$sg1_nb_codes'
        dates_input <- vector("list", length = n)
        # Créer des dateRangeInput. Possible de conserver les valeurs précédentes
        # si input$ns1_nb_per diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("ns1_date",i)]])) {
            dates_input[[i]] <- dateRangeInput(
              inputId = paste0("ns1_date",i), label = paste("Période", i),
              separator = " au ",
              autoclose = FALSE  # permet d'écrire manuellement la date sans erreur
            )
          } else {
            dates_input[[i]] <- dateRangeInput(
              inputId = paste0("ns1_date",i), label = paste("Période", i),
              start = input[[paste0("ns1_date",i)]][1],
              end = input[[paste0("ns1_date",i)]][2],
              separator = " au ",
              autoclose = FALSE
            )
          }
        }
        return(tagList(dates_input))
      })
    })

    # Codes Rx d'analyse : afficher le bon nombre de textInput selon la valeur
    # de input$ns1_nb_codes
    ns1_nb_codes <- reactive({
      n <- input$ns1_nb_codes  # nb codes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "ns1_nb_codes", value = 1)
      }
      isolate({  # enlève la réactivité de chaque input créé, permet d'écrire
        # dans le textInput sans qu'il y ait de réactivité
        codes_input <- vector("list", length = n)
        # Créer des textInput. Possible de conserver les valeurs précédentes
        # si input$ns1_nb_codes diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("ns1_code",i)]])) {
            codes_input[[i]] <- textInput(inputId = paste0("ns1_code",i),
                                          label = paste("Code Rx", i),
                                          value = "")
          } else {
            codes_input[[i]] <- textInput(inputId = paste0("ns1_code",i),
                                          label = paste("Code Rx", i),
                                          value = input[[paste0("ns1_code",i)]])
          }
        }
        return(tagList(codes_input))
      })
    })
    output$ns1_nb_codes <- renderUI({ ns1_nb_codes() })
    ns1_nb_codes_retro <- reactive({
      n <- input$ns1_nb_codes_retro  # nb codes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "ns1_nb_codes_retro", value = 1)
      }
      isolate({  # enlève la réactivité de chaque input créé, permet d'écrire
        # dans le textInput sans qu'il y ait de réactivité
        codes_input <- vector("list", length = n)
        # Créer des textInput. Possible de conserver les valeurs précédentes
        # si input$ns1_nb_codes diminue
        for (i in 1:n) {
          if (is.null(input[[paste0("ns1_code_retro",i)]])) {
            codes_input[[i]] <- textInput(inputId = paste0("ns1_code_retro",i),
                                          label = paste("Code Rx Rétrospectif", i),
                                          value = "")
          } else {
            codes_input[[i]] <- textInput(inputId = paste0("ns1_code_retro",i),
                                          label = paste("Code Rx Rétrospectif", i),
                                          value = input[[paste0("ns1_code_retro",i)]])
          }
        }
        return(tagList(codes_input))
      })
    })
    output$ns1_nb_codes_retro <- renderUI({ ns1_nb_codes_retro() })

    # Afficher une date pour le calcul de l'âge
    output$ns1_age_date <- renderUI({
      if (any(input$ns1_group_by == "Age")) {
        return(tagList(
          dateInput("ns1_age_date", label = "Date pour calcul Âge", value = input$ns1_date1[1])
        ))
      } else {
        return(NULL)
      }
    })

    # En-tête Résultats - Apparaît seulement s'il y a eu une requête
    output$ns1_html_result_section <- renderUI({
      if (ns1_val$show_tab) {
        return(tagList(
          div(style = "margin-top:15px"),
          fluidRow(
            h4(HTML("&nbsp;&nbsp;"), "Résultats"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px")
        ))
      } else {
        return(NULL)
      }
    })
    # Requete SQL
    ns1_requete_sql <- eventReactive(input$ns1_go_extract, {
      if (any("" %in% str_remove_all(find_code(input, "ns1"), " "))) {
        ns1_val$show_tab <- FALSE
        showNotification("Inscrire un Code Rx dans chaque zone prévu à cet effet.", type = "error")
        return(NULL)
      } else if (!is.null(conn_values$conn)) {
        if (is.logical(conn_values$conn) && conn_values$conn == FALSE) {
          ns1_val$show_tab <- FALSE
          showNotification("Exécution impossible. Connexion requise.", type = "error")
          return(NULL)
        }
        showNotification("Exécution en cours...", id = "ns1_go_extract", type = "message", duration = NULL)
        ns1_val$show_tab <- TRUE
        DT <- ns1_dbGetQuery(input, conn_values$conn)
        removeNotification("ns1_go_extract")
        return(DT)
      } else {
        ns1_val$show_tab <- FALSE
        showNotification("Exécution impossible. Connexion requise.", type = "error")
        return(NULL)
      }
    })
    # Afficher le tableau demandé
    output$ns1_table_req <- renderDataTable(
      {
        DT <- table_format(ns1_requete_sql())
        if (ns1_val$show_tab) {
          return(DT)
        } else {
          return(NULL)
        }
      }, options = renderDataTable_options()  # scrolling si le tableau est plus large que la fenêtre
    )

    # Réinitialiser les arguments comme initialement
    observeEvent(input$ns1_reset_args, {
      # Effacer périodes sauf la 1ere
      updateNumericInput(session, "ns1_nb_per", value = 1)

      # Effacer code et remettre à 1 - Codes Rx + Codes Retro
      n <- input$ns1_nb_codes
      for (i in 1:n) {  # Créer des textInput vide -> efface les valeurs précédentes
        updateTextInput(session, inputId = paste0("ns1_code",i),
                        label = paste("Code Rx", i), value = "")
      }
      updateNumericInput(session, "ns1_nb_codes", value = 1)
      n_retro <- input$ns1_nb_codes_retro
      for (i in 1:n_retro) {  # Créer des textInput vide -> efface les valeurs précédentes
        updateTextInput(session, inputId = paste0("ns1_code_retro_",i),
                        label = paste("Code Rx Rétrospectif", i), value = "")
      }
      updateNumericInput(session, "ns1_nb_codes_retro", value = 1)

      # Mettre à jour les checkboxGroup
      # Mettre à jour les checkboxGroup
      updateCheckboxGroupInput(session, inputId = "ns1_group_by", selected = "DENOM")
      updateNumericInput(session, inputId = "ns1_njours_sans_conso", value = 365)
      updateSelectInput(session, inputId = "ns1_code_serv_filter", selected = "Exclusion")
      updateCheckboxGroupInput(session, inputId = "ns1_code_serv", selected = c("1", "AD"))
      updateSelectInput(session, inputId = "ns1_code_list_filter", selected = "Inclusion")
      updateCheckboxGroupInput(session, inputId = "ns1_code_list", selected = character())

      # Effacer section Résultats et Requête SQL
      ns1_val$show_tab <- FALSE  # variable qui détermine si on affiche les sections
    })




    # stat_gen1 -----------------------------------------------------------------------------------

    # Variables réactives pour sg1
    sg1_val <- reactiveValues(
      show_query = FALSE,  # afficher la requête ou pas
      query = NULL,  # message à afficher. NULL = même pas une case où afficher du texte
      show_tab = FALSE  # contient le tableau de la requête/résultats
    )

    # Périodes d'analyse : afficher le bon nombre de dateRangeInput selon valeur
    # de input$sg1_nb_per
    output$sg1_nb_per <- renderUI({
      n <- input$sg1_nb_per  # nb périodes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "sg1_nb_per", value = 1)
      }
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
    sg1_nb_codes <- reactive({
      n <- input$sg1_nb_codes  # nb codes & déclenche réactivité
      if (is.na(n) || n < 1) {  # forcer valeur 1 si < 1
        updateNumericInput(session, "sg1_nb_codes", value = 1)
      }
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
    output$sg1_nb_codes <- renderUI({ sg1_nb_codes() })

    # Afficher une date pour le calcul de l'âge
    output$sg1_age_date <- renderUI({
      if (any(input$sg1_group_by == "Age")) {
        return(tagList(
          dateInput("sg1_age_date", label = "Date pour calcul Âge", value = input$sg1_date1[1])
        ))
      } else {
        return(NULL)
      }
    })

    # En-tête Résultats - Apparaît seulement s'il y a eu une requête
    output$sg1_html_result_section <- renderUI({
      if (sg1_val$show_tab) {
        return(tagList(
          div(style = "margin-top:15px"),
          fluidRow(
            h4(HTML("&nbsp;&nbsp;"), "Résultats"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px")
        ))
      } else {
        return(NULL)
      }
    })
    # Requête SQL
    sg1_requete_sql <- eventReactive(input$sg1_go_extract, {
      if (any("" %in% str_remove_all(find_code(input, "sg1"), " "))) {
        sg1_val$show_tab <- FALSE
        showNotification("Inscrire un Code Rx dans chaque zone prévu à cet effet.", type = "error")
        return(NULL)
      } else if (!is.null(conn_values$conn)) {
        if (is.logical(conn_values$conn) && conn_values$conn == FALSE) {
          sg1_val$show_tab <- FALSE
          showNotification("Exécution impossible. Connexion requise.", type = "error")
          return(NULL)
        }
        showNotification("Exécution en cours...", id = "sg1_go_extract", type = "message", duration = NULL)
        sg1_val$show_tab <- TRUE
        DT <- sg1_dbGetQuery(input, conn_values$conn)
        removeNotification("sg1_go_extract")
        return(DT)
      } else {
        sg1_val$show_tab <- FALSE
        showNotification("Exécution impossible. Connexion requise.", type = "error")
        return(NULL)
      }
    })
    # Afficher le tableau demandé
    output$sg1_table_req <- renderDataTable(
      {
        DT <- table_format(sg1_requete_sql())
        if (sg1_val$show_tab) {
          return(DT)
        } else {
          return(NULL)
        }
      }, options = renderDataTable_options()  # scrolling si le tableau est plus large que la fenêtre
    )

    # Enregistrer le fichier au format Excel
    output$sg1_save <- renderUI({  # faire apparaître bouton de sauvegarde s'il y a eu une extraction
      if (sg1_val$show_tab) {
        return(shinySaveButton(
          "sg1_save", "Sauvegarder Résultats en Excel",
          "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
          filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
          viewtype = "list",
          style = saveExcel_button_style()
        ))
      } else {
        return(NULL)
      }
    })
    shinyFileSave(input, "sg1_save", roots = Volumes_path())  # détermine les répertoires à afficher pour le bouton sg1_save
    sg1_file_save <- reactive({ shinyFiles_directories(input$sg1_save, "save") })
    observeEvent(sg1_file_save(), {  # sauvegarde de la requête en Excel
      if (nrow(sg1_file_save())) {  # car à la base table de 0 ligne 4 colonnes, 1 ligne si on sélectionne un répertoire
        showNotification("Sauvegarde en cours.", id = "sg1_file_save", type = "message", duration = NULL)
        save_Excel(
          dt = create_dt_data_args_query(
            dt = sg1_requete_sql(),
            args_list = list(
              METHODE = "stat_gen1",
              DATE_DEBUT = find_date(input, "sg1", "deb"),
              DATE_FIN = find_date(input, "sg1", "fin"),
              TYPE_RX = input$sg1_type_Rx, CODE_RX = find_code(input, "sg1"),
              GROUPER_PAR = input$sg1_group_by,
              CODE_SERV_FILTRE = input$sg1_code_serv_filter,
              CODE_SERV = adapt_code_serv(input$sg1_code_serv),
              CODE_LIST_FILTRE = input$sg1_code_list_filter,
              CODE_LIST = input$sg1_code_list,
              AGE_DATE = input$sg1_age_date
            ),
            query = query_stat_gen1(
              debut = find_date(input, "sg1", "deb")[1], fin = find_date(input, "sg1", "fin")[1],
              type_Rx = input$sg1_type_Rx, codes = find_code(input, "sg1"), group_by = input$sg1_group_by,
              code_serv = input$sg1_code_serv, code_serv_filtre = input$sg1_code_serv_filter,
              code_list = input$sg1_code_list, code_list_filtre = input$sg1_code_list_filter,
              age_date = input$sg1_age_date
            )
          ),
          save_path = sg1_file_save()
        )
        removeNotification("sg1_file_save")
      }
    })

    # En-tête SQL - Apparaît seulement s'il y a eu une requête
    output$sg1_html_SQL_section <- renderUI({
      if (sg1_val$show_tab) {
        return(tagList(
          div(style = "margin-top:15px"),
          fluidRow(
            h4(HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"), "Requête SQL"),
            style = "color: #ffffff; background-color: #0086b3;"
          ),
          div(style = "margin-top:10px")
        ))
      } else {
        return(NULL)
      }
    })

    # Code SQL en lien avec les résultats
    sg1_code_SQL <- eventReactive(input$sg1_go_extract, {
      # Code SQL associé à la requête demandée
      query_stat_gen1(
        debut = find_date(input, "sg1", "deb")[1], fin = find_date(input, "sg1", "fin")[1],
        type_Rx = input$sg1_type_Rx,
        codes = ifelse(input$sg1_type_Rx == "AHFS", sort(find_code(input, "sg1"))[1], sort(find_code(input, "sg1"))),
        group_by = input$sg1_group_by,
        code_serv = adapt_code_serv(input$sg1_code_serv), code_serv_filtre = input$sg1_code_serv_filter,
        code_list = sort(input$sg1_code_list), code_list_filtre = input$sg1_code_list_filter,
        age_date = input$sg1_age_date
      )
    })
    output$sg1_code_SQL <- renderText({ sg1_code_SQL() })
    output$sg1_html_code_SQL <- renderUI({  # section affichant le code SQL de la requête
      if (sg1_val$show_tab) {
        verbatimTextOutput("sg1_code_SQL")
      } else {
        return(NULL)
      }
    })

    # Réinitialiser les arguments comme initialement
    observeEvent(input$sg1_reset_args, {
      # Effacer périodes sauf la 1ere
      updateNumericInput(session, "sg1_nb_per", value = 1)

      # Effacer code et remettre à 1
      n <- input$sg1_nb_codes
      for (i in 1:n) {  # Créer des textInput vide -> efface les valeurs précédentes
        updateTextInput(session, inputId = paste0("sg1_code",i),
                        label = paste("Code Rx", i), value = "")
      }
      updateNumericInput(session, "sg1_nb_codes", value = 1)

      # Mettre à jour les checkboxGroup
      updateCheckboxGroupInput(session, inputId = "sg1_group_by", selected = "DENOM")
      updateSelectInput(session, inputId = "sg1_code_serv_filter", selected = "Exclusion")
      updateCheckboxGroupInput(session, inputId = "sg1_code_serv", selected = c("1", "AD"))
      updateSelectInput(session, inputId = "sg1_code_list_filter", selected = "Inclusion")
      updateCheckboxGroupInput(session, inputId = "sg1_code_list", selected = character())

      # Effacer section Résultats et Requête SQL
      sg1_val$show_tab <- FALSE  # variable qui détermine si on affiche les sections
    })



    # I_APME_DEM_AUTOR_CRITR_ETEN_CM --------------------------------------------------------------

    # Paramètres à afficher
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__params <- reactive({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom",
                        "DENOM_DEM")
            ),
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__din",
                        "DIN_DEM")
            )
          ),
          fluidRow(
            column(
              width = 4,
              selectInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnDebut",
                          "Début période - Année",
                          choices = c(max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE):min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE)),
                          selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE))
            ),
            column(
              width = 4,
              selectInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisDebut",
                          "Début période - Mois",
                          choices = 1:12, selected = 1)
            )
          ),
          fluidRow(
            column(
              width = 4,
              selectInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnFin",
                          "Fin période - Année",
                          choices = max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE):min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE),
                          selected = data.table::year(Sys.Date()))
            ),
            column(
              width = 4,
              selectInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisFin",
                          "Fin période - Mois",
                          choices = 1:12, selected = data.table::month(Sys.Date()))
            )
          ),
          fluidRow(
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__search",
                        "DES_COURT_INDCN_RECNU")
            )
          ),
          fluidRow(
            column(4, actionButton("I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("I_APME_DEM_AUTOR_CRITR_ETEN_CM_save",
                                      "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "NO_SEQ_INDCN_RECNU_PME") {
        return(tagList(
          fluidRow(
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__NO_SEQ_INDCN_RECNU",
                        "NO_SEQ_INDCN_RECNU"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_TRAIT_DEM",
                        "DD_TRAIT_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_AUTOR",
                        "DD_AUTOR"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DD_APLIC_AUTOR",
                        "DD_APLIC_AUTOR"),
              actionButton("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__reset",
                           "Réinitialiser",
                           style = reset_button_style()),
              div(style = "margin-top:15px"),
              shinySaveButton("I_APME_DEM_AUTOR_CRITR_ETEN_CM_save",
                              "Sauvegarder Résultats en Excel",
                              "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                              filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                              viewtype = "list",
                              style = saveExcel_button_style())
            ),
            column(
              width = 4,
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DAT_STA_DEM",
                        "DAT_STA_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_TRAIT_DEM",
                        "DF_TRAIT_DEM"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_AUTOR",
                        "DF_AUTOR"),
              textInput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__NO_SEQ_INDCN_RECNU_PME__DF_APLIC_AUTOR",
                        "DF_APLIC_AUTOR")
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
    I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt <- reactive({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        DT <- copy(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU)
        setkey(DT, DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU, ANNEE, MOIS)
        # Rechercher les mots-clés de DES_COURT_INDCN_RECNU
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
        # Rechercher les DENOM_DEM
        denom_words <- unlist(stringr::str_split(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom,
                                                  "\\+"))
        if (length(denom_words) == 1 && denom_words == "") {
          # rien pour le moment...
        } else {
          for (i in 1:length(denom_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(DENOM_DEM), tolower(denom_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
        # Rechercher les DIN_DEM
        din_words <- unlist(stringr::str_split(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__din,
                                                 "\\+"))
        if (length(din_words) == 1 && din_words == "") {
          # rien pour le moment...
        } else {
          for (i in 1:length(din_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(DIN_DEM), tolower(din_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
        # Période d'étude à analyser
        DT <- DT[ANNEE >= as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnDebut) &
                   MOIS >= as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisDebut) &
                   ANNEE <= as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnFin) &
                   MOIS <= as.integer(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisFin)]
        # Regrouper ensemble les dates continues dans le temps
        DT[, DATE := paste0(ANNEE,"-",stringr::str_pad(MOIS, 2, "left", "0")), .(DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU)]
        DT <- DT[
          , .(DEBUT = first(DATE),
              FIN = last(DATE)),
          .(DENOM_DEM, DIN_DEM, DES_COURT_INDCN_RECNU)
        ]
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
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM_dt <- renderDataTable({ I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt() },
                                                                options = renderDataTable_options())
    # Réinitialisation
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__reset, {
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__denom", selected = "")
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__din", selected = "")
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnDebut",
                        selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE))
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisDebut",
                        selected = min(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$MOIS))
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__AnFin",
                        selected = max(inesss::I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$ANNEE))
      updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__DES_COURT_INDCN_RECNU__MoisFin",
                        selected = data.table::month(Sys.Date()))
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
    # Sauvegarde de la table
    shinyFileSave(input, "I_APME_DEM_AUTOR_CRITR_ETEN_CM_save", roots = Volumes_path())
    I_APME_DEM_AUTOR_CRITR_ETEN_CM_file_save <- reactive({
      shinyFiles_directories(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM_save, "save")
    })
    observeEvent(I_APME_DEM_AUTOR_CRITR_ETEN_CM_file_save(), {
      if (nrow(I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt())) {
        showNotification("Sauvegarde en cours.", id = "I_APME_DEM_AUTOR_CRITR_ETEN_CM_save",
                         type = "message", duration = NULL)
        save_Excel(dt = I_APME_DEM_AUTOR_CRITR_ETEN_CM__dt(),
                   save_path = I_APME_DEM_AUTOR_CRITR_ETEN_CM_file_save())
        removeNotification("I_APME_DEM_AUTOR_CRITR_ETEN_CM_save")
      }
    })


    # V_DEM_PAIMT_MED_CM ------------------------------------------------------

    # Paramètres à afficher
    V_DEM_PAIMT_MED_CM__params <- reactive({
      if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_AHFS") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DENOM", "DENOM")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__NOM_DENOM", "NOM_DENOM"))
          ),
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DIN", "DIN")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__MARQ_COMRC", "MARQ_COMRC"))
          ),
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_CLA", "AHFS_CLA")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_SCLA", "AHFS_SCLA")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_SSCLA", "AHFS_SSCLA"))
          ),
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_NOM_CLA", "AHFS_NOM_CLA"))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DEBUT_FIN", "DEBUT - FIN",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_AHFS") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_CLA", "AHFS_CLA")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_SCLA", "AHFS_SCLA")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_SSCLA", "AHFS_SSCLA"))
          ),
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_NOM_CLA", "AHFS_NOM_CLA"))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_AHFS__DEBUT_FIN", "DEBUT - FIN",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_AHFS__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
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
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_DENOM_COMNE__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_DIN__DIN", "DIN")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__COD_DIN__MARQ_COMRC", "MARQ_COMRC"))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN", "DEBUT - FIN",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_DIN__reset", "Réinitialiser",
                                   style = reset_button_style()))

          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
          ),
          div(style = "margin-top:20px")
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_TENEUR_FORME") {
        return(tagList(
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DENOM", "DENOM")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DIN", "DIN")),
          ),
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__TENEUR", "TENEUR")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__NOM_TENEUR", "NOM_TENEUR"))
          ),
          fluidRow(
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__FORME", "FORME")),
            column(4, textInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__NOM_FORME", "NOM_FORME"))
          ),
          fluidRow(
            column(4, sliderInput("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DEBUT_FIN", "DEBUT - FIN",
                                  min = 1996, max = year(Sys.Date()), value = c(1996, year(Sys.Date())),
                                  sep = "", step = 1))
          ),
          fluidRow(
            column(4, actionButton("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
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
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_SERV__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
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
            column(4, actionButton("V_DEM_PAIMT_MED_CM__COD_STA_DECIS__reset", "Réinitialiser",
                                   style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_DEM_PAIMT_MED_CM_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
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
      if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_AHFS") {
        DT <- inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS
        for (col in c("DENOM", "DIN", "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA", "NOM_DENOM", "MARQ_COMRC", "AHFS_NOM_CLA")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__",col)]], "\\+"
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
          input$V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DEBUT_FIN[[2]]
        ]
        return(DT)
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_AHFS") {
        DT <- inesss::V_DEM_PAIMT_MED_CM$COD_AHFS
        for (col in c("AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA", "AHFS_NOM_CLA")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__COD_AHFS__",col)]], "\\+"
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
          input$V_DEM_PAIMT_MED_CM__COD_AHFS__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__COD_AHFS__DEBUT_FIN[[2]]
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
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        DT <- copy(inesss::V_DEM_PAIMT_MED_CM$COD_DIN)
        for (col in c("DIN", "MARQ_COMRC")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__COD_DIN__",col)]], "\\+"
          ))
          for (i in 1:length(search_words)) {
            DT[, paste(i) := stringr::str_detect(tolower(get(col)), tolower(search_words[i]))]
            DT <- DT[get(paste(i)) == TRUE]
            DT[, paste(i) := NULL]
          }
        }
        DT <- DT[
          input$V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN[[2]]
        ]
        return(DT)
      } else if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_TENEUR_FORME") {
        DT <- inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME
        for (col in c("DENOM", "DIN", "TENEUR", "NOM_TENEUR", "FORME", "NOM_FORME")) {
          search_words <- unlist(stringr::str_split(
            input[[paste0("V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__",col)]], "\\+"
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
          input$V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DEBUT_FIN[[1]] <= DEBUT &
            FIN <= input$V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DEBUT_FIN[[2]]
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
      } else {
        return(NULL)
      }
    })
    output$V_DEM_PAIMT_MED_CM__dt <- renderDataTable({ V_DEM_PAIMT_MED_CM__dt() },
                                                     options = renderDataTable_options())
    # Réinitialisation
    observeEvent(input$V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DENOM", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__NOM_DENOM", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DIN", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__MARQ_COMRC", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_CLA", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_SCLA", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_SSCLA", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__AHFS_NOM_CLA", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_AHFS__DEBUT_FIN", value = c(1996, year(Sys.Date())))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_AHFS__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_CLA", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_SCLA", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_SSCLA", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_AHFS__AHFS_NOM_CLA", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_AHFS__DEBUT_FIN", value = c(1996, year(Sys.Date())))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__COD_DIN__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__DIN", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__MARQ_COMRC", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__COD_DIN__DEBUT_FIN", value = c(1996, year(Sys.Date())))
    })
    observeEvent(input$V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__reset, {
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DENOM", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DIN", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__TENEUR", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__NOM_TENEUR", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__FORME", value = "")
      updateTextInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__NOM_FORME", value = "")
      updateSliderInput(session, "V_DEM_PAIMT_MED_CM__DENOM_DIN_TENEUR_FORME__DEBUT_FIN", value = c(1996, year(Sys.Date())))
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
    # Sauvegarde de la table
    shinyFileSave(input, "V_DEM_PAIMT_MED_CM_save", roots = Volumes_path())
    V_DEM_PAIMT_MED_CM_file_save <- reactive({
      shinyFiles_directories(input$V_DEM_PAIMT_MED_CM_save, "save")
    })
    observeEvent(V_DEM_PAIMT_MED_CM_file_save(), {
      if (nrow(V_DEM_PAIMT_MED_CM__dt())) {
        showNotification("Sauvegarde en cours.", id = "V_DEM_PAIMT_MED_CM_save",
                         type = "message", duration = NULL)
        save_Excel(dt = V_DEM_PAIMT_MED_CM__dt(),
                   save_path = V_DEM_PAIMT_MED_CM_file_save())
        removeNotification("V_DEM_PAIMT_MED_CM_save")
      }
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
          column(4, actionButton("V_DENOM_COMNE_MED__reset", "Réinitialiser",
                                 style = reset_button_style()))
        ),
        div(style = "margin-top:15px"),
        fluidRow(
          column(4, shinySaveButton("V_DENOM_COMNE_MED_save", "Sauvegarder Résultats en Excel",
                                    "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                    filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                    viewtype = "list",
                                    style = saveExcel_button_style()))
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
    output$V_DENOM_COMNE_MED__dt <- renderDataTable({ V_DENOM_COMNE_MED__dt() },
                                                    options = renderDataTable_options())
    # Réinitialisation
    observeEvent(input$V_DENOM_COMNE_MED__reset, {
      updateTextInput(session, "V_DENOM_COMNE_MED__DENOM", value = "")
      updateTextInput(session, "V_DENOM_COMNE_MED__NOM_DENOM", value = "")
    })
    # Sauvegarde de la table
    shinyFileSave(input, "V_DENOM_COMNE_MED_save", roots = Volumes_path())
    V_DENOM_COMNE_MED_file_save <- reactive({
      shinyFiles_directories(input$V_DENOM_COMNE_MED_save, "save")
    })
    observeEvent(V_DENOM_COMNE_MED_file_save(), {
      if (nrow(V_DENOM_COMNE_MED__dt())) {
        showNotification("Sauvegarde en cours.", id = "V_DENOM_COMNE_MED_save",
                         type = "message", duration = NULL)
        save_Excel(dt = V_DENOM_COMNE_MED__dt(), save_path = V_DENOM_COMNE_MED_file_save())
        removeNotification("V_DENOM_COMNE_MED_save")
      }
    }, ignoreInit = TRUE)



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
          column(4, actionButton("V_DES_COD__reset", "Réinitialiser", style = reset_button_style()))
        ),
        div(style = "margin-top:15px"),
        fluidRow(
          column(4, shinySaveButton("V_DES_COD_save", "Sauvegarder Résultats en Excel",
                                    "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                    filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                    viewtype = "list",
                                    style = saveExcel_button_style()))
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
    output$V_DES_COD__dt <- renderDataTable({ V_DES_COD__dt() },
                                            options = renderDataTable_options())
    # Réinitialisation
    observeEvent(input$V_DES_COD__reset, {
      updateTextInput(session, "V_DES_COD__CODE", value = "")
      updateTextInput(session, "V_DES_COD__TYPE_CODE", value = "")
      updateTextInput(session, "V_DES_COD__CODE_DESC", value = "")
    })
    # Sauvegarde de la table
    shinyFileSave(input, "V_DES_COD_save", roots = Volumes_path())
    V_DES_COD_file_save <- reactive({
      shinyFiles_directories(input$V_DES_COD_save, "save")
    })
    observeEvent(V_DES_COD_file_save(), {
      if (nrow(V_DES_COD__dt())) {
        showNotification("Sauvegarde en cours.", id = "V_DES_COD_save",
                         type = "message", duration = NULL)
        save_Excel(dt = V_DES_COD__dt(), save_path = V_DES_COD_file_save())
        removeNotification("V_DES_COD_save")
      }
    }, ignoreInit = TRUE)


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
            column(4, actionButton("V_PRODU_MED__NOM_MARQ_COMRC__reset", "Réinitialiser", style = reset_button_style()))
          ),
          div(style = "margin-top:15px"),
          fluidRow(
            column(4, shinySaveButton("V_PRODU_MED_save", "Sauvegarder Résultats en Excel",
                                      "Enregistrer sous...",  # message du haut une fois la fenêtre ouverte
                                      filetype = list(`Classeur Excel` = "xlsx"),  # type de fichier permis
                                      viewtype = "list",
                                      style = saveExcel_button_style()))
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
    output$V_PRODU_MED__dt <- renderDataTable({ V_PRODU_MED__dt() },
                                              options = renderDataTable_options())
    # Réinitialisation
    observeEvent(input$V_PRODU_MED__NOM_MARQ_COMRC__reset, {
      updateTextInput(session, "V_PRODU_MED__NOM_MARQ_COMRC__DENOM", value = "")
      updateTextInput(session, "V_PRODU_MED__NOM_MARQ_COMRC__DIN", value = "")
      updateTextInput(session, "V_PRODU_MED__NOM_MARQ_COMRC__NOM_MARQ_COMRC", value = "")
    })
    # Sauvegarde de la table
    shinyFileSave(input, "V_PRODU_MED_save", roots = Volumes_path())
    V_PRODU_MED_file_save <- reactive({
      shinyFiles_directories(input$V_PRODU_MED_save, "save")
    })
    observeEvent(V_PRODU_MED_file_save(), {
      if (nrow(V_PRODU_MED__dt())) {
        showNotification("Sauvegarde en cours.", id = "V_PRODU_MED_save",
                         type = "message", duration = NULL)
        save_Excel(dt = V_PRODU_MED__dt(), save_path = V_PRODU_MED_file_save())
        removeNotification("V_PRODU_MED_save")
      }
    }, ignoreInit = TRUE)

  }

  # Application -------------------------------------------------------------

  shinyApp(ui, server)

}
