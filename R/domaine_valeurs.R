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


  # DATAS -------------------------------------------------------------------

  fiches_techniques_list <- list(

    I_APME_DEM_AUTOR_CRITR_ETEN_CM = list(
      DES_COURT_INDCN_RECNU = list(
        MaJ = "2023-02-06",
        tab_desc = data.table(
          VARIABLE = c(
            "DENOM_DEM", "DIN_DEM", "DES_COURT_INDCN_RECNU",
            "DebPeriodeDesc", "FinPeriodeDesc"
          ),
          `VARIABLE RAMQ` = c(
            "APME_COD_DENOM_COMNE_DEM", "APME_COD_DIN_DEM", "NPME_DES_COURT_INDCN_RECNU",
            "-",
            "-"
          ),
          DESCRIPTION = c(
            "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
            "Ce code différencie chaque produit. Ce dernier pouvant varier selon la forme pharmaceutique, la marque de commerce, etc.",
            "Description courte complète de l'indication reconnue de PME. Note: Elle peut être dérivée de la fusion de description entre la description qui lui est directement reliée et celle de niveau supérieur (le cas échéant).",
            "Début d'une période continue où la combinaison existe. Format ANNÉE-MOIS.",
            "Fin d'une période continue où la combinaison existe. Format ANNÉE-MOIS."
          )
        )
      )
    ),

    V_CLA_AHF = list(
      MaJ = "2023-02-06",
      tab_desc = data.table(
        VARIABLE = c(
          "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA",
          "NOM_AHFS", "NOM_ANGLAIS_AHFS"
        ),
        `VARIABLE RAMQ` = c(
          "NMED_COD_CLA_AHF", "NMED_COD_SCLA_AHF", "NMED_COD_SSCLA_AHF",
          "NMED_NOM_CLA_AHF", "NMED_NOM_ANGL_CLA_AHF"
        ),
        DESCRIPTION = c(
          "Code identifiant la classe de médicaments telle que déterminée par l'American Hospital Formulary Service (AHFS). Ce code correspond aux deux(2) premières positions de la classification AHFS qui en compte un maximum de six(6).",
          "Code de la sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
          "Code de la sous-sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
          "Nom d'un code de la classification de l'AHFS à un moment donné. Nom valide selon la période d'application.",
          "Nom en anglais d'une Classe, Sous-classe ou Sous-sous-classe AHFS."
        )
      )
    ),

    V_DEM_PAIMT_MED_CM = list(
      MaJ = "2023-02-06",
      tab_desc = data.table(
        VARIABLE = c(
          "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA", "AHFS_NOM_CLA",
          "DENOM", "NOM_DENOM",
          "DIN", "MARQ_COMRC",
          "FORME", "NOM_FORME",
          "TENEUR", "NOM_TENEUR",
          "COD_SERV", "COD_SERV_DESC", "COD_SERV_1", "COD_SERV_2", "COD_SERV_3",
          "COD_STA_DECIS", "COD_STA_DESC",
          "PremierePrescription", "DernierePrescription",
          "DebPeriodPrescripDem", "FinPeriodPrescripDem"
        ),
        `VARIABLE RAMQ` = c(
          "SMED_COD_CLA_AHF", "SMED_COD_SCLA_AHF", "SMED_COD_SSCLA_AHF", "NMED_NOM_CLA_AHF",
          "SMED_COD_DENOM_COMNE", "NMED_NOM_DENOM_COMNE",
          "SMED_COD_DIN", "NMED_NOM_MARQ_COMRC",
          "SMED_COD_FORME_MED", "NMED_NOM_FORME",
          "SMED_COD_TENR_MED", "NMED_NOM_TENR",
          "NMED_COD_SERV_MED", "NMED_DES_SERV_MED", "SMED_COD_SERV_1", "SMED_COD_SERV_2", "SMED_COD_SERV_3",
          "SMED_COD_STA_DECIS", "CODE_DES",
          "-", "-",
          "-", "-"
        ),
        DESCRIPTION = c(
          # AHFS_CLA - SMED_COD_CLA_AHF
          "Code identifiant la classe de médicaments telle que déterminée par l'American Hospital Formulary Service (AHFS). Ce code correspond aux deux(2) premières positions de la classification AHFS qui en compte un maximum de six(6).",
          # AHFS_SCLA - SMED_COD_SCLA_AHF
          "Code de la sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
          # AHFS_SSCLA - SMED_COD_SSCLA_AHF
          "Code de la sous-sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
          # AHFS_NOM_CLA - NMED_NOM_CLA_AHF
          "Nom d'un code de la classification de l'AHFS à un moment donné. Nom valide selon la période d'application.<br>Provient de la vue V_CLA_AHF.",
          # DENOM - SMED_COD_DENOM_COMNE
          "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
          # NOM_DENOM - NMED_NOM_DENOM_COMNE
          "Nom de la dénomination commune du médicament.<br>Provient de la vue V_DENOM_COMNE_MED.",
          # DIN - SMED_COD_DIN
          "Ce code différencie chaque produit. Ce dernier pouvant varier selon la forme pharmaceutique, la marque de commerce, etc.",
          # MARQ_COMRC - NMED_NOM_MARQ_COMRC
          "Cet élément représente le nom sous lequel est commercialisé un produit pharmaceutique. Il sert à désigner, plus précisément, un code DIN. Ce ne sont pas tous les produits qui ont une marque de commerce (fourniture, magistral, solvant....)<br>Provient de la vue V_PRODU_MED.",
          # FORME - SMED_COD_FORME_MED
          "Ce code permet d'identifier de façon unique chacune des formes pharmaceutiques que peuvent prendre les médicaments, que ce soit des médicaments assurés, d'exception ou de patients d'exception.",
          # NOM_FORME - NMED_NOM_FORME
          "Cette propriété est le nom au long d'une forme pharmaceutique. Le nom est inscrit en majuscules et minuscules accentuées.<br>Provient de la vue V_FORME_MED.",
          # TENEUR - SMED_COD_TENR_MED
          "Code de la teneur du médicament auquel fait référence le présent médicament (NO_SEQ_MED) dans une situation de regroupement.<br>Cette propriété sert à identifier les médicaments qui regroupent d'autres médicaments. Cela sert, entres autres, à regrouper les médicaments de longue durée avec ceux de courte durée, pour les besoins du PPB.",
          # NOM_TENEUR - NMED_NOM_TENR
          "Nom de la teneur du médicament.<br>Provient de la vue V_TENR_MED.",
          # COD_SERV - NMED_COD_SERV_MED
          "Les codes de service décrivent les services qui peuvent être facturés dans le cadre du système de Médicaments.<br>Provient de la vue V_PARAM_SERV_MED.",
          # COD_SERV_DESC - NMED_DES_SERV_MED
          "Description du service pour MED de CIP.<br>Provient de la vue V_PARAM_SERV_MED.",
          # COD_SERV_1 - SMED_COD_SERV_1
          "Les codes de services décrivent les services qui ont été fournis. Ces codes peuvent être complétés par des codes d’intervention ou d’exception. Une même réclamation peut contenir jusqu'à trois codes de services.",
          # COD_SERV_2 - SMED_COD_SERV_2
          "Les codes de services décrivent les services qui ont été fournis. Ces codes peuvent être complétés par des codes d’intervention ou d’exception. Une même réclamation peut contenir jusqu'à trois codes de services.",
          # COD_SERV_3 - SMED_COD_SERV_3
          "Les codes de services décrivent les services qui ont été fournis. Ces codes peuvent être complétés par des codes d’intervention ou d’exception. Une même réclamation peut contenir jusqu'à trois codes de services.",
          # COD_STA_DECIS - SMED_COD_STA_DECIS
          "Code de statut de décision.",
          # COD_STA_DESC - CODE_DES
          "Description d'un code.<br>Provient de la vue V_DES_COD.",
          # PremierePrescription
          "Première année que le code ou la combinaison de codes ont été prescrits (inscrits dans V_DEM_PAIMT_MED_CM).",
          # DernierePrescription
          "Dernière année que le code ou la combinaison de codes ont été prescrits (inscrits dans V_DEM_PAIMT_MED_CM).",
          # DebPeriodPrescriptDem
          "Année inscrite dans le sélecteur d'année : Période Prescription (Début).",
          # FinPeriodPrescriptDem
          "Année inscrite dans le sélecteur d'année : Période Prescription (Fin)."
        )
      )
    ),

    V_DENOM_COMNE_MED = list(
      MaJ = "2023-02-06",
      tab_desc = data.table(
        VARIABLE = c(
          "DENOM", "DATE_DEBUT", "DATE_FIN",
          "NOM_DENOM", "NOM_DENOM_SYNON",
          "NOM_DENOM_ANGLAIS", "NOM_DENOM_SYNON_ANGLAIS"
        ),
        `VARIABLE RAMQ` = c(
          "NMED_COD_DENOM_COMNE", "NMED_DD_DENOM_COMNE", "NMED_DF_DENOM_COMNE",
          "NMED_NOM_DENOM_COMNE", "NMED_NOM_DENOM_COMNE_SYNON",
          "NMED_NOM_ANGL_DENOM_COMNE", "NMED_NOM_ANGL_DENOM_SYNON"
        ),
        DESCRIPTION = c(
          "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
          "Date à laquelle cette dénomination commune est apparue pour la première fois sur la liste de médicaments. Cette date correspond à la date de parution d'une liste de médicaments. C'est à partir de cette date que le calcul du 15 ans pour le calcul du PPB, est effectué. Cette date est utile pour identifier les médicaments qui ne sont pas encore assujettis à la méthode de calcul du PPB et qui doivent y être assujettis à l'avenir.",
          "Date à laquelle une dénomination commune a cessé d'être utilisée.",
          "Nom de la dénomination commune du médicament.",
          "Nom dénomination commune synonyme",
          "Nom en anglais d'une dénomination commune. Ce nom est utilisé dans les publications des listes anglaises des médicaments.",
          "Nom synonyme en anglais d'une dénomination commune. Ce nom est utilisé dans les publications des listes anglaises des médicaments."
        )
      )
    ),

    V_PRODU_MED = list(
      NOM_MARQ_COMRC = list(
        MaJ = "2023-02-06",
        tab_desc = data.table(
          VARIABLE = c(
            "DENOM", "DIN",
            "NOM_MARQ_COMRC",
            "DATE_DEBUT", "DATE_FIN"
          ),
          `VARIABLE RAMQ` = c(
            "NMED_COD_DENOM_COMNE", "NMED_COD_DIN", "NMED_NOM_MARQ_COMRC",
            "NMED_DD_PRODU_MED", "NMED_DF_PRODU_MED"
          ),
          DESCRIPTION = c(
            "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
            "Ce code différencie chaque produit. Ce dernier pouvant varier selon la forme pharmaceutique, la marque de commerce, etc.",
            "Cet élément représente le nom sous lequel est commercialisé un produit pharmaceutique. Il sert à désigner, plus précisément, un code DIN. Ce ne sont pas tous les produits qui ont une marque de commerce (fourniture, magistral, solvant....)",
            "Date de début d'une occurrence de cette table. Cette date correspond à la date d'entrée en vigueur de la mise à jour liste (DAT-ENTRE_VIG_MAJ_LISTE) à laquelle est relié l'ajout ou la modification de cette occurrence.",
            "Date de fin d'une occurrence de cette table. Cette date correspond à la date d'entrée en vigueur de la mise à jour liste moins un jour (DAT-ENTRE_VIG_MAJ_LISTE - 1 jour) de l'occurrence suivante."
          )
        ),
        footnote = "Les périodes DÉBUT-FIN ayant la même combinaison DENOM-DIN qui se chevauchent dans le temps ont été combinées en une seule."
      )
    )

  )
  # mois_debut <- mois_debut_fct()  # valeur initiales pour le début


  # USER INTERFACE ----------------------------------------------------------

  ui <- dashboardPage(


    # * Header section ----------------------------------------------------------------------------
    dashboardHeader(title = "Domaine de valeur"),


    # * Sidebar section ---------------------------------------------------------------------------
    dashboardSidebar(
      width = 266,  # ajuster l'espace nécessaire selon le nom de la base de données
      sidebarMenu(

        div(style = "margin-top:10px"),

        p(HTML("&nbsp;&nbsp;"), tags$u("Combinaisons uniques")),
        menuItem("Médicaments d'exception", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        menuItem("Demandes de paiement de médicaments", tabName = "tabV_DEM_PAIMT_MED_CM"),

        div(style = "margin-top:30px"),

        p(HTML("&nbsp;&nbsp;"), tags$u("Dictionnaire")),
        menuItem("Classes AHFS", tabName = "tabV_CLA_AHF"),
        menuItem("Dénomination commune", tabName = "tabV_DENOM_COMNE_MED"),
        menuItem("Produit médicament", tabName = "tabV_PRODU_MED")

        ### ************************************************************************************** #
        ### 2023-01-20  Version initiale où on indiquait le nom de la table plutôt que son contenu
        # div(style = "margin-top:10px"),
        #
        # menuItem("I_APME_DEM_AUTOR_CRITR_ETEN_CM", tabName = "tabI_APME_DEM_AUTOR_CRITR_ETEN_CM"),
        # menuItem("V_DEM_PAIMT_MED_CM", tabName = "tabV_DEM_PAIMT_MED_CM"),
        #
        # div(style = "margin-top:30px"),
        #
        # menuItem("V_CLA_AHF", tabName = "tabV_CLA_AHF"),
        # menuItem("V_DENOM_COMNE_MED", tabName = "tabV_DENOM_COMNE_MED"),
        # # menuItem("V_DES_COD", tabName = "tabV_DES_COD"),
        # menuItem("V_PRODU_MED", tabName = "tabV_PRODU_MED")
        ### ************************************************************************************** #

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
                em(fiches_techniques_list$I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$MaJ)
              ),
              tableOutput("I_APME_DEM_AUTOR_CRITR_ETEN_CM__varDesc")
            )
          )
        ),

        # * * V_DEM_PAIMT_MED_CM --------------------------------------------------
        tabItem(
          tabName = "tabV_DEM_PAIMT_MED_CM",
          fluidRow(
            header_MaJ_datas(attributes(inesss::V_DEM_PAIMT_MED_CM)$MaJ),
            column(
              width = 12,
              strong("Vue : V_DEM_PAIMT_MED_CM"),
              p("Demandes de paiement de médicaments.")
            ),
            column(
              width = ui_col_width(),
              selectInput(  # sélection du domaine de valeur
                inputId = "V_DEM_PAIMT_MED_CM__data",
                label = "Élément",
                choices = names(inesss::V_DEM_PAIMT_MED_CM)
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              div(style = "margin-top:-10px"),
              uiOutput("V_DEM_PAIMT_MED_CM__dataDesc"),
              div(style = "margin-top:20px")
            )
          ),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Domaine de valeur",
              div(style = "margin-top:10px"),
              uiOutput("V_DEM_PAIMT_MED_CM__params"),
              uiOutput("V_DEM_PAIMT_MED_CM__go_reset_button"),
              uiOutput("V_DEM_PAIMT_MED_CM__save_button"),
              div(style = "margin-top:10px"),
              dataTableOutput("V_DEM_PAIMT_MED_CM__dt")
            ),
            tabPanel(
              title = "Fiche technique",
              div(style = "margin-top:20px"),
              column(
                width = 12,
                em(fiches_techniques_list$V_DEM_PAIMT_MED_CM$MaJ)
              ),
              tableOutput("V_DEM_PAIMT_MED_CM__varDesc")
            )
          )
        ),

        # * * V_CLA_AHF ---------------------------------------------------------------
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
                em(fiches_techniques_list$V_CLA_AHF$MaJ)
              ),
              tableOutput("V_CLA_AHF__varDesc")
            )
          )
        ),

        # * * V_DENOM_COMNE_MED --------------------------------------------------
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
                em(fiches_techniques_list$V_DENOM_COMNE_MED$MaJ)
              ),
              tableOutput("V_DENOM_COMNE_MED__varDesc")
            )
          )
        ),


        # * * V_PRODU_MED ----------------------------------------------------------
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
                em(fiches_techniques_list$V_PRODU_MED$NOM_MARQ_COMRC$MaJ)
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

    # * * Fiche Technique ####
    output$I_APME_DEM_AUTOR_CRITR_ETEN_CM__varDesc <- renderTable({
      if (input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__data == "DES_COURT_INDCN_RECNU") {
        return(fiches_techniques_list$I_APME_DEM_AUTOR_CRITR_ETEN_CM$DES_COURT_INDCN_RECNU$tab_desc)
      } else {
        return(NULL)
      }
    })

    # * * Descriptif Data ####
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

    # * * UI ####
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

    # * * Datatable ####
    observeEvent(input$I_APME_DEM_AUTOR_CRITR_ETEN_CM__go, {
      I_APME_DEM_AUTOR_CRITR_ETEN_CM__val$show_tab <- TRUE  # Afficher la table si on clique sur Exécuter
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
        updateTextInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__search", value = "")
        updateSelectInput(session, "I_APME_DEM_AUTOR_CRITR_ETEN_CM__typeRecherche",
                          selected = "keyword")
      }
    }, ignoreInit = TRUE)



    # V_DEM_PAIMT_MED_CM ----------------------------------------------------
    V_DEM_PAIMT_MED_CM__val <- reactiveValues(
      show_tab = FALSE
    )

    # * * Fiche Technique ####
    output$V_DEM_PAIMT_MED_CM__varDesc <- renderTable({
      return(fiches_techniques_list$V_DEM_PAIMT_MED_CM$tab_desc)
    }, sanitize.text.function = identity)

    # * * Descriptif Data ####
    output$V_DEM_PAIMT_MED_CM__dataDesc <- renderUI({
      if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_TENEUR_FORME") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par dénomination commune (DENOM), identification du médicament (DIN), teneur et forme.")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_AHFS") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par dénomination commune (DENOM), identification du médicament (DIN), et classe AHFS.")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DENOM_COMNE") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par dénomination commune (DENOM).")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par identification du médicament (DIN).")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_AHFS") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par classe AHFS.")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par code de service.")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
        return(tagList(
          fluidRow(
            column(
              width = 12,
              p("Combinaisons uniques par code de statut de décision.")
            )
          )
        ))
      }
    })

    # * * UI ####
    output$V_DEM_PAIMT_MED_CM__params <- renderUI({
      if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_AHFS") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__denom", "DENOM"),
              textInput("V_DEM_PAIMT_MED_CM__din", "DIN")
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__nomDenom", "NOM_DENOM"),
              textInput("V_DEM_PAIMT_MED_CM__marqComrc", "MARQ_COMRC")
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "V_DEM_PAIMT_MED_CM__denomTypeRecherche",
                "NOM_DENOM Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              ),
              selectInput(
                "V_DEM_PAIMT_MED_CM__marqTypeRecherche",
                "MARQ_COMRC Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              textInput("V_DEM_PAIMT_MED_CM__ahfsCla", "AHFS_CLA"),
              textInput("V_DEM_PAIMT_MED_CM__ahfsNomCla", "AHFS_NOM_CLA"),
              selectInput(  # Année début
                "V_DEM_PAIMT_MED_CM__AnDebut",
                "Période Prescription (Début)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$PremierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$PremierePrescription),
                selected = min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$PremierePrescription)
              )
            ),
            column(
              width = 3,
              textInput("V_DEM_PAIMT_MED_CM__ahfsScla", "AHFS_SCLA"),
              selectInput(
                "V_DEM_PAIMT_MED_CM__ahfsTypeRecherche",
                "AHFS_NOM_CLA Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              ),
              selectInput(  # Année Fin
                "V_DEM_PAIMT_MED_CM__AnFin",
                "Période Prescription (Fin)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$DernierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$DernierePrescription),
                selected = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$DernierePrescription)
              )
            ),
            column(
              width = 3,
              textInput("V_DEM_PAIMT_MED_CM__ahfsSscla", "AHFS_SSCLA")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DENOM_COMNE") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__denom", "DENOM"),
              selectInput(  # Année début
                "V_DEM_PAIMT_MED_CM__AnDebut", "Période Prescription (Début)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$PremierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$PremierePrescription),
                selected = min(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$PremierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__nomDenom", "NOM_DENOM"),
              selectInput(  # Année fin
                "V_DEM_PAIMT_MED_CM__AnFin", "Période Prescription (Fin)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$DernierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$DernierePrescription),
                selected = max(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$DernierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "V_DEM_PAIMT_MED_CM__typeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__din", "DIN"),
              selectInput(  # Année début
                "V_DEM_PAIMT_MED_CM__AnDebut", "Période Prescription (Début)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$PremierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$PremierePrescription),
                selected = min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$PremierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__marqComrc", "MARQ_COMRC"),
              selectInput(  # Année fin
                "V_DEM_PAIMT_MED_CM__AnFin", "Période Prescription (Fin)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DernierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DernierePrescription),
                selected = max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DernierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "V_DEM_PAIMT_MED_CM__typeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_AHFS") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__ahfsCla", "AHFS_CLA"),
              textInput("V_DEM_PAIMT_MED_CM__ahfsNomCla", "AHFS_NOM_CLA"),
              selectInput(  # Année début
                "V_DEM_PAIMT_MED_CM__AnDebut", "Période Prescription (Début)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$PremierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$PremierePrescription),
                selected = min(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$PremierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__ahfsScla", "AHFS_SCLA"),
              selectInput(
                "V_DEM_PAIMT_MED_CM__typeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              ),
              selectInput(  # Année fin
                "V_DEM_PAIMT_MED_CM__AnFin", "Période Prescription (Fin)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$DernierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$DernierePrescription),
                selected = max(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$DernierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__ahfsSscla", "AHFS_SSCLA")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_TENEUR_FORME") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__denom", "DENOM"),
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__din", "DIN"),
            )
          ),
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__teneur", "TENEUR"),
              textInput("V_DEM_PAIMT_MED_CM__forme", "FORME"),
              selectInput(  # Année début
                "V_DEM_PAIMT_MED_CM__AnDebut", "Période Prescription (Début)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$PremierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$PremierePrescription),
                selected = min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$PremierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__nomTeneur", "NOM_TENEUR"),
              textInput("V_DEM_PAIMT_MED_CM__nomForme", "NOM_FORME"),
              selectInput(  # Année fin
                "V_DEM_PAIMT_MED_CM__AnFin", "Période Prescription (Fin)",
                choices = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$DernierePrescription):min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$DernierePrescription),
                selected = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$DernierePrescription)
              )
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "V_DEM_PAIMT_MED_CM__teneurRecherche", "Teneur Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              ),
              selectInput(
                "V_DEM_PAIMT_MED_CM__formeRecherche", "Forme Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              )
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {
        return(tagList(
          fluidRow(
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__codServ", "COD_SERV"),
              textInput("V_DEM_PAIMT_MED_CM__serv1", "COD_SERV_1")
            ),
            column(
              width = ui_col_width(),
              textInput("V_DEM_PAIMT_MED_CM__codServDesc", "COD_SERV_DESC"),
              textInput("V_DEM_PAIMT_MED_CM__serv2", "COD_SERV_2")
            ),
            column(
              width = ui_col_width(),
              selectInput(
                "V_DEM_PAIMT_MED_CM__typeRecherche", "Type Recherche",
                choices = c("Mot-clé" = "keyword",
                            "Valeur exacte" = "exactWord"),
                selected = "Mot-clé"
              ),
              textInput("V_DEM_PAIMT_MED_CM__serv3", "COD_SERV_3")
            )
          )
        ))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
        return(NULL)
      }
    })
    output$V_DEM_PAIMT_MED_CM__go_reset_button <- renderUI({
      if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
        # Seulement une table à 3 obs, pas besoin de mettre un reset
        return(NULL)
      } else {
        return(button_go_reset("V_DEM_PAIMT_MED_CM"))
      }
    })
    output$V_DEM_PAIMT_MED_CM__save_button <- renderUI({
      button_save("V_DEM_PAIMT_MED_CM", V_DEM_PAIMT_MED_CM__dt())
    })

    # * * Datatable ####
    observeEvent(input$V_DEM_PAIMT_MED_CM__go, {
      V_DEM_PAIMT_MED_CM__val$show_tab <- TRUE  # Afficher la table si on clique sur Exécuter
    }, ignoreInit = TRUE)
    observeEvent(input$V_DEM_PAIMT_MED_CM__data, {
      if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
        V_DEM_PAIMT_MED_CM__val$show_tab <- TRUE  # On veut faire apparaître le data, car petite table qui n'a pas d'input
      } else {
        V_DEM_PAIMT_MED_CM__val$show_tab <- FALSE  # Faire disparaitre la table si on change le data
      }
    }, ignoreInit = TRUE)
    V_DEM_PAIMT_MED_CM__dt <- eventReactive(
      c(input$V_DEM_PAIMT_MED_CM__go, V_DEM_PAIMT_MED_CM__val$show_tab),
      {
        if (V_DEM_PAIMT_MED_CM__val$show_tab) {
          dt <- inesss::V_DEM_PAIMT_MED_CM[[input$V_DEM_PAIMT_MED_CM__data]]

          if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_AHFS") {

            # DENOM
            if (input$V_DEM_PAIMT_MED_CM__denom != "") {
              dt <- search_value_chr(
                dt, col = "DENOM",
                values = input$V_DEM_PAIMT_MED_CM__denom, pad = 5
              )
            }
            if (input$V_DEM_PAIMT_MED_CM__nomDenom != "") {
              if (input$V_DEM_PAIMT_MED_CM__denomTypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "NOM_DENOM",
                  values = input$V_DEM_PAIMT_MED_CM__nomDenom
                )
              } else if (input$V_DEM_PAIMT_MED_CM__denomTypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "NOM_DENOM",
                  values = input$V_DEM_PAIMT_MED_CM__nomDenom
                )
              }
            }
            # DIN
            if (input$V_DEM_PAIMT_MED_CM__din != "") {
              dt <- search_value_num(
                dt, col = "DIN",
                values = input$V_DEM_PAIMT_MED_CM__din
              )
            }
            if (input$V_DEM_PAIMT_MED_CM__marqComrc != "") {
              if (input$V_DEM_PAIMT_MED_CM__marqTypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "MARQ_COMRC",
                  values = input$V_DEM_PAIMT_MED_CM__marqComrc
                )
              } else if (input$V_DEM_PAIMT_MED_CM__marqTypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "MARQ_COMRC",
                  values = input$V_DEM_PAIMT_MED_CM__marqComrc
                )
              }
            }
            # AHFS
            if (input$V_DEM_PAIMT_MED_CM__ahfsCla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_CLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsCla, pad = 2
              )
            }
            if (input$V_DEM_PAIMT_MED_CM__ahfsScla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_SCLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsScla, pad = 2
              )
            }
            if (input$V_DEM_PAIMT_MED_CM__ahfsSscla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_SSCLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsSscla, pad = 2
              )
            }
            if (input$V_DEM_PAIMT_MED_CM__ahfsNomCla != "") {
              if (input$V_DEM_PAIMT_MED_CM__ahfsTypeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "AHFS_NOM_CLA",
                  values = input$V_DEM_PAIMT_MED_CM__ahfsNomCla
                )
              } else if (input$V_DEM_PAIMT_MED_CM__ahfsTypeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "AHFS_NOM_CLA",
                  values = input$V_DEM_PAIMT_MED_CM__ahfsNomCla
                )
              }
            }
            # Période d'étude demandée
            dt <- dt[
              as.integer(input$V_DEM_PAIMT_MED_CM__AnDebut) <= DernierePrescription &
                as.integer(input$V_DEM_PAIMT_MED_CM__AnFin) >= PremierePrescription
            ]
            # Inscrire la période demandée
            dt[
              , `:=` (DebPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnDebut,
                      FinPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnFin)
            ]

          } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DENOM_COMNE") {

            # DENOM
            if (input$V_DEM_PAIMT_MED_CM__denom != "") {
              dt <- search_value_chr(
                dt, col = "DENOM",
                values = input$V_DEM_PAIMT_MED_CM__denom, pad = 5
              )
            }
            # NOM_DENOM
            if (input$V_DEM_PAIMT_MED_CM__nomDenom != "") {
              if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "NOM_DENOM",
                  values = input$V_DEM_PAIMT_MED_CM__nomDenom
                )
              } else if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "NOM_DENOM",
                  values = input$V_DEM_PAIMT_MED_CM__nomDenom
                )
              }
            }
            # Période d'étude demandée
            dt <- dt[
              as.integer(input$V_DEM_PAIMT_MED_CM__AnDebut) <= DernierePrescription &
                as.integer(input$V_DEM_PAIMT_MED_CM__AnFin) >= PremierePrescription
            ]
            # Inscrire la période demandée
            dt[
              , `:=` (DebPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnDebut,
                      FinPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnFin)
            ]

          } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {

            # DIN
            if (input$V_DEM_PAIMT_MED_CM__din != "") {
              dt <- search_value_num(
                dt, col = "DIN",
                values = input$V_DEM_PAIMT_MED_CM__din
              )
            }
            # MARQ_COMRC
            if (input$V_DEM_PAIMT_MED_CM__marqComrc != "") {
              if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "MARQ_COMRC",
                  values = input$V_DEM_PAIMT_MED_CM__marqComrc
                )
              } else if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "MARQ_COMRC",
                  values = input$V_DEM_PAIMT_MED_CM__marqComrc
                )
              }
            }
            # Période d'étude demandée
            dt <- dt[
              as.integer(input$V_DEM_PAIMT_MED_CM__AnDebut) <= DernierePrescription &
                as.integer(input$V_DEM_PAIMT_MED_CM__AnFin) >= PremierePrescription
            ]
            # Inscrire la période demandée
            dt[
              , `:=` (DebPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnDebut,
                      FinPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnFin)
            ]

          } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_AHFS") {

            # AHFS_CLA
            if (input$V_DEM_PAIMT_MED_CM__ahfsCla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_CLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsCla, pad = 2
              )
            }
            # AHFS_SCLA
            if (input$V_DEM_PAIMT_MED_CM__ahfsScla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_SCLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsScla, pad = 2
              )
            }
            # AHFS_SSCLA
            if (input$V_DEM_PAIMT_MED_CM__ahfsSscla != "") {
              dt <- search_value_chr(
                dt, col = "AHFS_SSCLA",
                values = input$V_DEM_PAIMT_MED_CM__ahfsSscla, pad = 2
              )
            }
            # AHFS_NOM_CLA
            if (input$V_DEM_PAIMT_MED_CM__ahfsNomCla != "") {
              if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "AHFS_NOM_CLA",
                  values = input$V_DEM_PAIMT_MED_CM__ahfsNomCla
                )
              } else if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "AHFS_NOM_CLA",
                  values = input$V_DEM_PAIMT_MED_CM__ahfsNomCla
                )
              }
            }
            # Période d'étude demandée
            dt <- dt[
              as.integer(input$V_DEM_PAIMT_MED_CM__AnDebut) <= DernierePrescription &
                as.integer(input$V_DEM_PAIMT_MED_CM__AnFin) >= PremierePrescription
            ]
            # Inscrire la période demandée
            dt[
              , `:=` (DebPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnDebut,
                      FinPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnFin)
            ]

          } else if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_TENEUR_FORME") {

            # DENOM
            if (input$V_DEM_PAIMT_MED_CM__denom != "") {
              dt <- search_value_chr(
                dt, col = "DENOM",
                values = input$V_DEM_PAIMT_MED_CM__denom, pad = 5
              )
            }
            # DIN
            if (input$V_DEM_PAIMT_MED_CM__din != "") {
              dt <- search_value_num(
                dt, col = "DIN",
                values = input$V_DEM_PAIMT_MED_CM__din
              )
            }
            # TENEUR
            if (input$V_DEM_PAIMT_MED_CM__teneur != "") {
              dt <- search_value_num(
                dt, col = "TENEUR",
                values = input$V_DEM_PAIMT_MED_CM__teneur
              )
            }
            # NOM_TENEUR
            if (input$V_DEM_PAIMT_MED_CM__nomTeneur != "") {
              if (input$V_DEM_PAIMT_MED_CM__teneurRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "NOM_TENEUR",
                  values = input$V_DEM_PAIMT_MED_CM__nomTeneur
                )
              } else if (input$V_DEM_PAIMT_MED_CM__teneurRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "NOM_TENEUR",
                  values = input$V_DEM_PAIMT_MED_CM__nomTeneur
                )
              }
            }
            # FORME
            if (input$V_DEM_PAIMT_MED_CM__forme != "") {
              dt <- search_value_num(
                dt, col = "FORME",
                values = input$V_DEM_PAIMT_MED_CM__forme
              )
            }
            # NOM_FORME
            if (input$V_DEM_PAIMT_MED_CM__nomForme != "") {
              if (input$V_DEM_PAIMT_MED_CM__formeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "NOM_FORME",
                  values = input$V_DEM_PAIMT_MED_CM__nomForme
                )
              } else if (input$V_DEM_PAIMT_MED_CM__formeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "NOM_FORME",
                  values = input$V_DEM_PAIMT_MED_CM__nomForme
                )
              }
            }
            # Période d'étude demandée
            dt <- dt[
              as.integer(input$V_DEM_PAIMT_MED_CM__AnDebut) <= DernierePrescription &
                as.integer(input$V_DEM_PAIMT_MED_CM__AnFin) >= PremierePrescription
            ]
            # Inscrire la période demandée
            dt[
              , `:=` (DebPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnDebut,
                      FinPeriodPrescripDem = input$V_DEM_PAIMT_MED_CM__AnFin)
            ]

          } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {

            # COD_SERV
            if (input$V_DEM_PAIMT_MED_CM__codServ != "") {
              dt <- search_value_chr(
                dt, col = "COD_SERV",
                values = input$V_DEM_PAIMT_MED_CM__codServ
              )
            }
            # COD_SERV_DESC
            if (input$V_DEM_PAIMT_MED_CM__codServDesc != "") {
              if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "keyword") {
                dt <- search_keyword(
                  dt, col = "COD_SERV_DESC",
                  values = input$V_DEM_PAIMT_MED_CM__codServDesc
                )
              } else if (input$V_DEM_PAIMT_MED_CM__typeRecherche == "exactWord") {
                dt <- search_value_chr(
                  dt, col = "COD_SERV_DESC",
                  values = input$V_DEM_PAIMT_MED_CM__codServDesc
                )
              }
            }
            # SERV_X
            cols <- c(  # noms input = nom colonne
              V_DEM_PAIMT_MED_CM__serv1 = "COD_SERV_1",
              V_DEM_PAIMT_MED_CM__serv2 = "COD_SERV_2",
              V_DEM_PAIMT_MED_CM__serv3 = "COD_SERV_3"
            )
            for (c in 1:length(cols)) {  # boucle pour filtrer toutes les colonnes
              if (input[[names(cols)[c]]] != "") {
                dt <- search_value_XXXX_YYYY(
                  dt, col = cols[c],
                  values = input[[names(cols)[c]]]
                )
              }
            }
          } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_STA_DECIS") {
            return(dt)
          }
          return(dt)
        } else {
          return(NULL)
        }
      }, ignoreInit = TRUE
    )
    output$V_DEM_PAIMT_MED_CM__dt <- renderDataTable({
      V_DEM_PAIMT_MED_CM__dt()
    }, options = renderDataTable_options())

    # * * Export ####
    output$V_DEM_PAIMT_MED_CM__save <- download_data(
      input,
      datasave = V_DEM_PAIMT_MED_CM__dt(),
      dataname = "V_DEM_PAIMT_MED_CM"
    )

    # * * Update Buttons ####
    observeEvent(input$V_DEM_PAIMT_MED_CM__reset, {
      V_DEM_PAIMT_MED_CM__val$show_tab <- FALSE
      if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_AHFS") {
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__denom", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__din", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__nomDenom", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__marqComrc", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsCla", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsScla", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsSscla", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsNomCla", value = "")
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnDebut",
                          selected = min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$PremierePrescription))
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnFin",
                          selected = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_AHFS$DernierePrescription))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DENOM_COMNE") {
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__denom", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__nomDenom", value = "")
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnDebut",
                          selected = min(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$PremierePrescription))
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnFin",
                          selected = max(inesss::V_DEM_PAIMT_MED_CM$COD_DENOM_COMNE$DernierePrescription))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_DIN") {
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__din", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__marqComrc", value = "")
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnDebut",
                          selected = min(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$PremierePrescription))
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnFin",
                          selected = max(inesss::V_DEM_PAIMT_MED_CM$COD_DIN$DernierePrescription))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_AHFS") {
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsCla", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsScla", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsSscla", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__ahfsNomCla", value = "")
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnDebut",
                          selected = min(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$PremierePrescription))
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnFin",
                          selected = max(inesss::V_DEM_PAIMT_MED_CM$COD_AHFS$DernierePrescription))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "DENOM_DIN_TENEUR_FORME") {
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__denom", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__din", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__teneur", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__nomTeneur", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__forme", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__nomForme", value = "")
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnDebut",
                          selected = min(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$PremierePrescription))
        updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnFin",
                          selected = max(inesss::V_DEM_PAIMT_MED_CM$DENOM_DIN_TENEUR_FORME$DernierePrescription))
      } else if (input$V_DEM_PAIMT_MED_CM__data == "COD_SERV") {
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__codServ", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__codServDesc", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__serv1", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__serv2", value = "")
        updateTextInput(session, "V_DEM_PAIMT_MED_CM__serv3", value = "")
      }
    })

    # * * Erreurs possibles ####
    observeEvent(
      eventExpr = {
        # modifier un des éléments déclenche la vérification
        c(input$V_DEM_PAIMT_MED_CM__AnDebut, input$V_DEM_PAIMT_MED_CM__AnFin)
      },
      handlerExpr = {
        annee_deb <- as.integer(input$V_DEM_PAIMT_MED_CM__AnDebut)
        annee_fin <- as.integer(input$V_DEM_PAIMT_MED_CM__AnFin)
        if (annee_deb >= annee_fin) {
          updateSelectInput(session, "V_DEM_PAIMT_MED_CM__AnDebut", selected = annee_fin)
        }
      },
      ignoreInit = TRUE
    )






    # V_CLA_AHF ------------------------------------------------------------
    V_CLA_AHF__val <- reactiveValues(
      show_tab = FALSE  # afficher la table ou pas
    )

    # * * Fiche Technique ####
    output$V_CLA_AHF__varDesc <- renderTable({
      return(fiches_techniques_list$V_CLA_AHF$tab_desc)
    })

    # * * UI ####
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

    # * * Datatable ####
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

    # * * Export ####
    output$V_CLA_AHF__save <- download_data(
      input,
      datasave = V_CLA_AHF__dt(),
      dataname = "V_CLA_AHF"
    )

    # * * Update Buttons ####
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

    # * * Fiche Technique ####
    output$V_DENOM_COMNE_MED__varDesc <- renderTable({
      return(fiches_techniques_list$V_DENOM_COMNE_MED$tab_desc)
    })

    # * * UI ####
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

    # * * Datatable ####
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

    # * * Export ####
    output$V_DENOM_COMNE_MED__save <- download_data(
      input,
      datasave = V_DENOM_COMNE_MED__dt(),
      dataname = "V_DENOM_COMNE_MED"
    )

    # * * Update buttons ####
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

    # * * Erreurs possibles ####
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

    # V_PRODU_MED ####
    V_PRODU_MED__val <- reactiveValues(
      show_tab = FALSE
    )

    # * * Fiche Technique ####
    output$V_PRODU_MED__varDesc <- renderTable({
      if (input$V_PRODU_MED__data == "NOM_MARQ_COMRC") {
        return(fiches_techniques_list$V_PRODU_MED$NOM_MARQ_COMRC$tab_desc)
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

    # * * Descriptif Data ####
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

    # * * UI ####
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

    # * * Datatable ####
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
                values = input$V_PRODU_MED__denom
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

    # * * Export ####
    output$V_PRODU_MED__save <- download_data(
      input,
      datasave = V_PRODU_MED__dt(),
      dataname = "V_PRODU_MED"
    )

    # * * Update buttons ####
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
