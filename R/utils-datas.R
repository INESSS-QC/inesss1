
# Comorbidity_diagn_codes -------------------------------------------------

#' Data - Codes diagnostiques
#'
#' Codes SQL regex (se terminent par un '%') à utiliser lors de l'extraction des codes de diagnostiques pour l'étude de la comorbidité.
#'
#' @encoding UTF-8
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/29578951/}{Validation of the Combined Comorbidity Index of Charlson and Elixhauser to Predict 30-Day Mortality Across ICD-9 and ICD-10}. \href{Ajouter lien}{Voir PDF.}
#' @name Comorbidity_diagn_codes

#' @rdname Comorbidity_diagn_codes
#' @format `list(nom_diagn = list(CIM9, CIM10))`. 32 diagnostics.
"Comorbidity_diagn_codes"

#' @rdname Comorbidity_diagn_codes
#' @format `list(nom_diagn = list(CIM9, CIM10))`. 16 diagnostics.
"Charlson_diagn_codes"

#' @rdname Comorbidity_diagn_codes
#' @format `list(nom_diagn = list(CIM9, CIM10))`. 29 diagnostics.
"Elixhauser_diagn_codes"


# Comorbidity_weights -----------------------------------------------------

#' Data - Poids des codes de diagnostiques
#'
#' @format Tableau de 4 variables et 32 observations.
#' \describe{
#'   \item{DIAGN}{Identification du diagnostique (`chr`).}
#'   \item{DIAGN_CODE}{Code de diagnostique utilisé utilisé dans la programmation (`chr`).}
#'   \item{CIM9}{Poids utilisé pour la *9e* révision du CIM (`int`).}
#'   \item{CIM10}{Poids utilisé pour la *10e* révision du CIM (`int`).}
#' }
#' @usage data("Comorbidity_weights") / MaVariable <- Comorbidity_weights
#' @encoding UTF-8
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/29578951/}{Validation of the Combined Comorbidity Index of Charlson and Elixhauser to Predict 30-Day Mortality Across ICD-9 and ICD-10}. \href{Ajouter lien}{Voir PDF.}
"Comorbidity_weights"


# Pop_QC ------------------------------------------------------------------

#' Data - Estimations et projections de population comparables (1996-2041)
#'
#' Tableau de la population québécoise par niveau géographique.\cr\cr
#' Ce fichier présente une série continue de données populationnelles comparables composée de la série des estimations (1996-2019) et de la série des projections (2020-2041) de population. Ces données tiennent compte de l'évolution de la population selon les plus récentes données observées de naissances, décès et mouvements migratoires.\cr\cr
#' Il est à noter que ces données de population sont présentées sur la base du découpage territorial du réseau de la santé et des services sociaux, soit pour les territoires suivants : le Québec, les réseaux universitaires intégrés de santé et de services sociaux (RUISSS), les régions sociosanitaires (RSS), les réseaux territoriaux de services (RTS), les réseaux locaux de services (RLS) et les centres locaux de services communautaires (CLSC).
#'
#' La classe des colonnes est `character` lorsque c'est du texte ou `integer` lorsque c'est un nombre.\cr\cr
#' **Mise en ligne** : 25 février 2016.\cr
#' **Dernière modification** : 24 avril 2020.\cr
#' **Publication no** : EstimProjComp-ISQ.\cr\cr
#' La fiche d'information et technique de cette base de données est disponible avec le fichier Excel (voir *Source*).
#'
#' @format Tableau de 8 variables et 2 595 320 observations :
#' \describe{
#'   \item{GEO}{Niveau géographique : Québec, RUISSS, RSS, RTS, RLS, CLSC.}
#'   \item{CODE}{Code du territoire.}
#'   \item{AN}{Année.}
#'   \item{TYPE}{Type de données : Estimations ou Projections.}
#'   \item{STATUT}{Donnée révisée ou provisoire. `NA` indique que la donnée n'a pas été changée depuis la dernière publication.}
#'   \item{SEXE}{}
#'   \item{AGE}{}
#'   \item{POP}{Population}
#' }
#'
#' @usage data("Pop_QC") / MaVariable <- Pop_QC.
#' @encoding UTF-8
#' @source \href{https://www.msss.gouv.qc.ca/professionnels/informations-geographiques-et-de-population/donnees-populationnelles/}{MSSS Données de population}.\cr
#' \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/extdata/EstimationProjectionComparable_1996_2041_20200424.xlsx}{Fichier Excel utilisé}.
"Pop_QC"


# RLS_convert -------------------------------------------------------------

#' Data - Correspondance RLS Loi 10
#'
#' Établir la correspondance des RLS avant et après l'adoption de la loi 10.
#'
#' Certains RLS ne peuvent être convertis, car leur valeur se retrouve avant et après l'adoption de la loi 10.\cr\cr
#' `attr(RLS_convert, "RLS_exclus")` indique les quatre (4) RLS exclus : 611, 612, 1611, 1612.\cr\cr
#' `attr(RLS_convert, "RLS_exclus_value")` renvoie un tableau indiquant les valeurs avant et après l'adoption de la loi 10 pour ces quatre (4) RLS.
#'
#' @format Tableau de 2 variables et 84 observations :
#' \describe{
#'   \item{RLS14}{Code de RLS **avant** l'adoption de la loi 10.}
#'   \item{RLS15}{Code de RLS **après** l'adoption de la loi 10.}
#' }
#' @usage data("RLS_convert") / MaVariable <- RLS_convert.
#' @encoding UTF-8
#' @source \href{https://publications.msss.gouv.qc.ca/msss/fichiers/statistiques/decoupage-territorial/Doc1_Correspondance_Etablissement_Public_Loi_10.xls}{Correspondance Etablissement Public Loi 10}.\cr
#' \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/extdata/Doc1_Correspondance_Etablissement_Public_Loi_10.xls}{Fichier Excel utilisé}.
"RLS_convert"


# RLS_list ----------------------------------------------------------------

#' Data - Liste des RLS
#'
#' Vecteur contenant la liste des 93 RLS plus 3 valeurs utiles lors d'analyse : 1001, 1701, 1801.
#'
#' @format Vecteur `integer` de 96 nombres.
#' @encoding UTF-8
#' @usage data("RLS_list") / MaVariable <- RLS_list.
"RLS_list"


# V_DEM_PAIMT_MED_CM.SMED_COD_DIN -----------------------------------------

#' Data - COD_DIN
#'
#' Codes d'identification des médicaments (`SMED_COD_DIN`) qui sont présents dans la vue `V_DEM_PAIMT_MED_CM`.
#'
#' @format Tableau de 3 variables :
#' \describe{
#'   \item{DIN}{Code d'identification du médicament (`SMED_COD_DIN`). `integer`.}
#'   \item{DEBUT}{Première année où le code a été inscrit. `integer`.}
#'   \item{FIN}{Dernière année où le code a été inscrit. `integer`.}
#' }
#' @encoding UTF-8
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}.
"V_DEM_PAIMT_MED_CM.SMED_COD_DIN"


# V_DEM_PAIMT_MED_CM.SMED_COD_SERV ----------------------------------------

#' Data - COD_SERV
#'
#' Codes de services (`SMED_COD_SERV_X`) qui sont présent dans la vue `V_DEM_PAIMT_MED_CM`.
#'
#' @format Tableau de 5 variables :
#' \describe{
#'   \item{COD_SERV}{Codes de services pouvant être isncrit dans les colonnes `SMED_COD_SERV_1`, `SMED_COD_SERV_2` et `SMED_COD_SERV3`. `character`.}
#'   \item{SERV_1}{Première et dernière année que le code de service a été inscrit dans la colonne `SMED_COD_SERV_1`. Si `NA`, le code n'a jamais été inscrit dans cette colonne. `character`.}
#'   \item{SERV_2}{Première et dernière année que le code de service a été inscrit dans la colonne `SMED_COD_SERV_2`. Si `NA`, le code n'a jamais été inscrit dans cette colonne. `character`.}
#'   \item{SERV_3}{Première et dernière année que le code de service a été inscrit dans la colonne `SMED_COD_SERV_3`. Si `NA`, le code n'a jamais été inscrit dans cette colonne. `character`.}
#'   \item{COD_SERV_DESC}{Description du code de service tirée de la variable `COD_SERV_DESC` de la vue `V_PARAM_SERV_MED`. `character`.}
#' }
#' @encoding UTF-8
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}.
"V_DEM_PAIMT_MED_CM.SMED_COD_SERV"


# V_DENOM_COMNE_MED -------------------------------------------------------

#' Data
#'
#' Description des codes de dénomination commune.
#'
#' @format Tableau de 7 variables :
#' \describe{
#'   \item{DENOM}{Code de dénomination commune (`NMED_COD_DENOM_COMNE`). `character`.}
#'   \item{DATE_DEBUT}{Date à laquelle cette dénomination commune est apparue pour la première fois (`NMED_DD_DENOM_COMNE`). `Date`.}
#'   \item{DATE_FIN}{Date à laquelle la dénomination commune a cessé d'être utilisée (`NMED_DF_DENOM_COMNE`). `Date`.}
#'   \item{NOM_DENOM}{Nom de la dénomination commune du médicament (`NMED_NOM_DENOM_COMNE`). `character`.}
#'   \item{NOM_DENOM_SYNON}{Synonyme du nom de la dénomination commune du médicament\cr(`NMED_NOM_DENOM_COMNE_SYNON`). `character`.}
#'   \item{NOM_DENOM_ANGLAIS}{Nom anglais de la dénomination commune du médicament\cr(`NMED_NOM_ANGL_DENOM_COMNE`). `character`.}
#'   \item{NOM_DENOM_SYNON_ANGLAIS}{Synonyme du nom anglais de la dénomination commune du médicament (`NMED_NOM_ANGL_DENOM_SYNON`). `character`.}
#' }
#' @encoding UTF-8
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1082&NomVue=V%5FDENOM%5FCOMNE%5FMED+%28D%E9nomination+commune+m%E9dicament%29}{V_DENOM_COMNE_MED}.
"V_DENOM_COMNE_MED"


# V_DES_COD ---------------------------------------------------------------

#' Data
#'
#' Domaine de valeurs pour les différents codes de l'environnement informationnel.
#'
#' @format Tableau de 5 variables :
#' \describe{
#'   \item{CODE}{Valeurs codifiées que peut prendre un élément (`CODE_VAL_COD`). `character`.}
#'   \item{TYPE_CODE}{Nom identifiant un élément de données (`CODE_NOM_COD`). `character`.}
#'   \item{CODE_DESC}{Description du code (`CODE_DES`). `character`.}
#'   \item{DATE_DEBUT}{Date de début de la période d'application (`CODE_DD_DES_COD`). `Date`.}
#'   \item{DATE_FIN}{Date de fin de la période d'application (`CODE_DF_DES_COD`). `Date`.}
#' }
#' @encoding UTF-8
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=111&NomVue=V%5FDES%5FCOD+%28DESCRIPTIONS+DES+CODES%29}{V_DES_COD}.
"V_DES_COD"


# V_PRODU_MED.NMED_NOM_MARQ_COMRC -----------------------------------------

#' Data - NOM_MARQ_COMRC
#'
#' Nom sous lequel est commercialisé un produit pharmaceutique.
#'
#' @format Tableau de 5 variables :
#' \describe{
#'   \item{DENOM}{Code de dénomination commune (`NMED_COD_DENOM_COMNE`). `character`.}
#'   \item{DIN}{Code d'identification du médicament (`NMED_COD_DIN`). `integer`.}
#'   \item{NOM_MARQ_COMRC}{Nom sous lequel est commercialisé un produit pharmaceutique\cr(`NMED_NOM_MARQ_COMRC`). `character`.}
#'   \item{DATE_DEBUT}{Date de début d'une occurence de cette table (`NMED_DD_PRODU_MED`). `Date`.}
#'   \item{DATE_FIN}{Date de fin d'une occurence de cette table (`NMED_DF_PRODU_MED`). `Date`.}
#' }
#' @encoding UTF-8
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1555&NomVue=V%5FPRODU%5FMED+%28Produit+m%E9dicament%29}{V_PRODU_MED}.
"V_PRODU_MED.NMED_NOM_MARQ_COMRC"
