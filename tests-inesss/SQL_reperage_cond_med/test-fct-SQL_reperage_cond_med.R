# conn <- SQL_connexion()



# 2022-06-05 --------------------------------------------------------------

### Modifier la confirmation des diagnostics par la nouvelle fonction confirm_Dx_par_nDx
### qui permet de sélectionner <0, 1, 2, ...> diagnostics qui confirment.
###
### -> nDx = 1 revient au même résultat que la version originale = SUCCESS

# SQL_reperage_cond_med_ORIGIN <- function(
#   conn = SQL_connexion(),
#   debut, fin,
#   Dx_table,
#   CIM = c("CIM9", "CIM10"),
#   by_Dx = TRUE,
#   date_dx_var = "admis",
#   n1 = 30, n2 = 730,
#   ...
# ) {
#
#   ### Arguments qui sont peut-etre dans <...> : les creer si ce n'est pas le cas
#   dotargs <- list(...)
#   if (!any(names(dotargs) == "keep_all")) {
#     keep_all <- FALSE
#   }
#   if (!any(names(dotargs) == "verbose")) {
#     verbose <- TRUE
#   }
#
#   ### Algorithme de repérage d'une condition médicale - étape 1 - MEDECHO
#   # Extraction de tous les Dx selon les critères de l'étape 1
#   if (verbose) {
#     cat("Étape 1 :\n")  # indiquer que l'étape 1 commence
#   }
#   Dx_etape1 <- SQL_diagn(
#     conn = conn, cohort = NULL,
#     debut = debut, fin = fin,
#     Dx_table = Dx_table, CIM = CIM,
#     dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM"),
#     dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO"),
#     date_dx_var = date_dx_var,
#     typ_diagn = c("P", "S", "D"),
#     exclu_diagn = NULL, verbose = verbose
#   )
#   if (verbose) {
#     cat("\n")  # retour de ligne pour l'affichage de l'étape 2
#   }
#   if (!is.null(Dx_etape1)) {
#     # Conserver la dernière date - besoin d'indiquer la date la plus récente plus tard
#     if (by_Dx) {
#       Dx_etape1_last <- Dx_etape1[, .SD[.N], .(ID, DIAGN)][, .(ID, DIAGN, DATE_DX)]
#     } else {
#       Dx_etape1_last <- Dx_etape1[, .SD[.N], .(ID)][, .(ID, DATE_DX)]
#     }
#     # Conserver la 1re observation de chaque individu - 1er Dx
#     if (by_Dx) {
#       Dx_etape1 <- Dx_etape1[, .SD[1], .(ID, DIAGN)]
#       Dx_etape1 <- Dx_etape1[, .(ID, DIAGN, DI_Hospit = DATE_DX)]  # sélection + nommer les colonnes
#     } else {
#       Dx_etape1 <- Dx_etape1[, .SD[1], .(ID)]
#       Dx_etape1 <- Dx_etape1[, .(ID, DI_Hospit = DATE_DX)]  # sélection + nommer les colonnes
#     }
#   }
#
#   ### Algorithme de repérage d'une condition médicale - étape 2
#   # Extraction de tous les Dx selon les critères de l'étape 2
#   if (verbose) {
#     cat("Étape 2 :\n")  # indiquer que l'étape 2 commence
#   }
#   Dx_etape2 <- SQL_diagn(
#     conn = conn, cohort = NULL,
#     debut = debut, fin = fin,
#     Dx_table = Dx_table, CIM = CIM,
#     dt_source = c("V_DIAGN_SEJ_HOSP_CM", "V_SEJ_SERV_HOSP_CM",
#                   "V_EPISO_SOIN_DURG_CM", "I_SMOD_SERV_MD_CM"),
#     dt_desc = list(V_DIAGN_SEJ_HOSP_CM = "MEDECHO", V_SEJ_SERV_HOSP_CM = "MEDECHO",
#                    V_EPISO_SOIN_DURG_CM = "BDCU", I_SMOD_SERV_MD_CM = "SMOD"),
#     date_dx_var = date_dx_var,
#     typ_diagn = c("A", "P", "S", "D"),
#     exclu_diagn = NULL, verbose = verbose
#   )
#   if (verbose) {
#     cat("\n")
#   }
#   if (nrow(Dx_etape2)) {
#     if (by_Dx) {
#       Dx_etape2 <- unique(Dx_etape2[, .(ID, DIAGN, DATE_DX)])
#       Dx_etape2_last <- Dx_etape2[, .SD[.N], .(ID, DIAGN)][, .(ID, DIAGN, DATE_DX)]  # Conserver la dernière date - besoin d'indiquer la date la plus récente plus tard
#     } else {
#       Dx_etape2 <- unique(Dx_etape2[, .(ID, DATE_DX)])
#       Dx_etape2_last <- Dx_etape2[, .SD[.N], .(ID)][, .(ID, DATE_DX)]  # Conserver la dernière date - besoin d'indiquer la date la plus récente plus tard
#     }
#
#     ### Confirmation des Dx de l'étape 2
#     if (verbose) {
#       cat("Confirmation des diagnostics...\n")
#     }
#     Dx_etape2 <- confirm_2Dx(
#       dt = Dx_etape2, ID = "ID", DATE = "DATE_DX",
#       DIAGN = {if (by_Dx) "DIAGN" else NULL},
#       study_start = NULL, study_end = NULL,
#       n1 = n1, n2 = n2,
#       keep_first = TRUE, reverse = FALSE
#     )
#     if (nrow(Dx_etape2)) {
#       setnames(Dx_etape2, c("DATE_REP", "DATE_CONF1"), c("DI_Acte", "DC_Acte"))
#     }
#   }
#
#   if (verbose) {
#     cat("Arrangement de la table finale...\n")
#   }
#   ### Dernière date Dx de chaque ID
#   if (!is.null(Dx_etape1) && !is.null(Dx_etape2)) {
#     Dx_recent <- rbind(Dx_etape1_last, Dx_etape2_last)
#   } else if (!is.null(Dx_etape1)) {
#     Dx_recent <- copy(Dx_etape1_last)
#   } else if (!is.null(Dx_etape2)) {
#     Dx_recent <- copy(Dx_etape2_last)
#   } else {
#     Dx_recent <- NULL
#   }
#
#   if (!is.null(Dx_recent)) {
#     setkey(Dx_recent)
#     if (by_Dx) {
#       Dx_recent <- Dx_recent[, .SD[.N], .(ID, DIAGN)][, .(ID, DIAGN, D_Recent = DATE_DX)]
#     } else {
#       Dx_recent <- Dx_recent[, .SD[.N], .(ID)][, .(ID, D_Recent = DATE_DX)]
#     }
#   }
#
#   ### Combiner les datasets pour table finale
#   if (is.null(Dx_recent)) {
#     return(NULL)
#   } else {
#     dt_final <- copy(Dx_recent)
#     if (by_Dx) {
#       if (!is.null(Dx_etape1)) {
#         dt_final <- Dx_etape1[dt_final, on = .(ID, DIAGN)]
#       }
#       if (!is.null(Dx_etape2)) {
#         dt_final <- Dx_etape2[dt_final, on = .(ID, DIAGN)]
#       }
#     } else {
#       if (!is.null(Dx_etape1)) {
#         dt_final <- Dx_etape1[dt_final, on = .(ID)]
#       }
#       if (!is.null(Dx_etape2)) {
#         dt_final <- Dx_etape2[dt_final, on = .(ID)]
#       }
#     }
#     # Ajouter les colonnes manquantes (si Dx_etape 1 ou 2 n'existe pas)
#     for (col in c("ID", "DIAGN", "DI_Finale", "DI_Hospit", "DI_Acte", "DC_Acte", "D_Recent")) {
#       if (!any(names(dt_final) == col)) {
#         dt_final[, (col) := NA]
#       }
#     }
#     dt_final[, DI_Finale := DI_Acte]
#     dt_final[is.na(DI_Acte), DI_Finale := DI_Hospit]
#     dt_final[DI_Hospit < DI_Acte, DI_Finale := DI_Hospit]
#
#     if (by_Dx) {
#       setcolorder(dt_final, c("ID", "DIAGN", "DI_Finale", "DI_Hospit", "DI_Acte", "DC_Acte", "D_Recent"))
#     } else {
#       dt_final <- dt_final[, .(ID, DI_Finale, DI_Hospit, DI_Acte, DC_Acte, D_Recent)]
#     }
#
#     if (verbose) {
#       cat("FIN.")
#     }
#
#     if (keep_all) {
#       return(dt_final)
#     } else {
#       return(dt_final[!is.na(DI_Finale)])
#     }
#
#   }
#
# }
# dt_orig <- SQL_reperage_cond_med_ORIGIN(
#   conn = conn,
#   debut = "2020-01-01", fin = "2020-12-31",
#   Dx_table = list(
#     diabete = list(
#       CIM9 = c("2500%", "2501%", "2502%"),
#       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#                 "E131%", "E139%", "E140%", "E141%", "E149%")
#     ),
#     diabete_complication = list(
#       CIM9 = paste0(2503:2509, "%"),
#       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#     )
#   ),
#   CIM = c("CIM9", "CIM10"),
#   by_Dx = TRUE,
#   date_dx_var = "admis",
#   n1 = 30, n2 = 730
# )
# dt_new <- SQL_reperage_cond_med_DEV(
#   conn = conn,
#   debut = "2020-01-01",
#   fin = "2020-12-31",
#   Dx_table = list(
#     diabete = list(
#       CIM9 = c("2500%", "2501%", "2502%"),
#       CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#                 "E131%", "E139%", "E140%", "E141%", "E149%")
#     ),
#     diabete_complication = list(
#       CIM9 = paste0(2503:2509, "%"),
#       CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#     )
#   ),
#   CIM = c("CIM9", "CIM10"),
#   by_Dx = TRUE,
#   date_dx_var = "admis",
#   n1 = 30, n2 = 730,
#   nDx = 1
# )


# debut = "2020-06-01"
# fin = "2020-12-31"
# Dx_table = list(
#   diabete = list(
#     CIM9 = c("2500%", "2501%", "2502%"),
#     CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
#               "E131%", "E139%", "E140%", "E141%", "E149%")
#   ),
#   diabete_complication = list(
#     CIM9 = paste0(2503:2509, "%"),
#     CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
#   )
# )
# CIM = c("CIM9", "CIM10")
# by_Dx = TRUE
# date_dx_var = "admis"
# n1 = 30
# n2 = 730
# nDx = 1
# keep_all <- FALSE
# verbose <- TRUE



# Inital ------------------------------------------------------------------


DT <- SQL_reperage_cond_med(
  conn = conn,
  debut = "2020-01-01", fin = "2020-12-31",
  Dx_table = list(
    diabete = list(
      CIM9 = c("2500%", "2501%", "2502%"),
      CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
                "E131%", "E139%", "E140%", "E141%", "E149%")
    ),
    diabete_complication = list(
      CIM9 = paste0(2503:2509, "%"),
      CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
    )
  ),
  CIM = c("CIM9", "CIM10"),
  by_Dx = TRUE,
  date_dx_var = "admis",
  n1 = 30, n2 = 730
)



debut = "2020-01-01"
fin = "2020-12-31"
Dx_table = list(
  diabete = list(
    CIM9 = c("2500%", "2501%", "2502%"),
    CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
              "E131%", "E139%", "E140%", "E141%", "E149%")
  ),
  diabete_complication = list(
    CIM9 = paste0(2503:2509, "%"),
    CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
  )
)
CIM = c("CIM9", "CIM10")
by_Dx = TRUE
date_dx_var = "admis"
n1 = 30
n2 = 730
