
# CIM_correspond ----------------------------------------------------------

#' Data - Correspondance entre CIM9 et CIM10
#'
#' Tableau de correspondance entre la CIM-9 et la CIM-10
#'
#' @format Tableau de 4 variables et 25866 observations :
#' \describe{
#'   \item{CIM9}{Code de diagnostic CIM-9. `character`.}
#'   \item{CIM9_DESC}{Description du code de diagnostic. `character`.}
#'   \item{CIM10}{Code de diagnostic CIM-10. `character`.}
#'   \item{CIM10_DESC}{Description du code de diagnostic. `character`.}
#' }
#' @usage data('CIM_correspond')
#' @encoding UTF-8
#' @source \href{https://www.ramq.gouv.qc.ca/fr/professionnels/medecins-omnipraticiens/facturation/repertoire-diagnostics/Pages/repertoire-diagnostics.aspx}{Répertoire des diagnostics}.
"CIM_correspond"


# CIM9 ------------------------------------------------------------------

#' Data - Diagnostics CIM-9
#'
#' Version légèrement modifiée par la RAMQ pour la facturation.
#'
#' @format Tableau de 2 variables et 7184 observations :
#' \describe{
#'   \item{CODE}{Code de diagnostic CIM-9. `character`.}
#'   \item{DIAGNOSTIC}{Description du code de diagnostic. `character`.}
#' }
#' @usage data('CIM9')
#' @encoding UTF-8
#' @source \href{https://www.ramq.gouv.qc.ca/fr/professionnels/medecins-omnipraticiens/facturation/repertoire-diagnostics/Pages/repertoire-diagnostics.aspx}{Répertoire des diagnostics}.
"CIM9"


# CIM10 -----------------------------------------------------------------

#' Data - Diagnostics CIM-10
#'
#' Version légèrement modifiée par la RAMQ pour la facturation.
#'
#' @format Tableau de 2 variables et 15487 observations :
#' \describe{
#'   \item{CODE}{Code de diagnostic CIM-10. `character`.}
#'   \item{DIAGNOSTIC}{Description du code de diagnostic. `character`.}
#' }
#' @usage data('CIM10')
#' @encoding UTF-8
#' @source \href{https://www.ramq.gouv.qc.ca/fr/professionnels/medecins-omnipraticiens/facturation/repertoire-diagnostics/Pages/repertoire-diagnostics.aspx}{Répertoire des diagnostics}.
"CIM10"


# Combine_Dx_CCI_INSPQ18 --------------------------------------------------

#' Data - Codes diagnostics
#'
#' Codes SQL regex (se terminent par un '%') à utiliser lors de l'extraction des codes de diagnostics pour l'étude de la comorbidité.
#'
#' Contient les codes des datas `Charlson_Dx_CCI_INSPQ18` et `Elixhauser_Dx_CCI_INSPQ18`.\cr\cr
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.\cr\cr
#' `aids` : AIDS/HIV\cr
#' `alcohol` : Alcohol abuse\cr
#' `blane` : Blood loss anemia\cr
#' `canc` : Any tumor without metastasis\cr
#' `carit` : Cardiac arrhythmias\cr
#' `cevd` : Cerebrovascular disease\cr
#' `chf` : Congestive heart failure\cr
#' `coag` : Coagulopathy\cr
#' `copd` : Chronic pulmonary disease\cr
#' `dane` : Deficiency anemia\cr
#' `dementia` : Dementia\cr
#' `depre` : Depression\cr
#' `diab` : Diabetes, complicated\cr
#' `diabwc` : Diabetes, uncomplicated\cr
#' `drug` : Drug abuse\cr
#' `fed` : Fluid and electrolyte disorders\cr
#' `hyp` : Hypertension\cr
#' `hypothy` : Hypothyroidism\cr
#' `ld` : Liver disease\cr
#' `metacanc` : Metastatic cancer\cr
#' `mi` : Myocardial infarction\cr
#' `nd` : Neurological disorders\cr
#' `obes` : Obesity\cr
#' `para` : Paralysis\cr
#' `pvd` : Peripheral vascular disorders\cr
#' `psycho` : Psychoses\cr
#' `pcd` : Pulmonary circulation disorders\cr
#' `rend` : Renal disease\cr
#' `rheumd` : Rheumatoid arth./collagen vascular disease\cr
#' `ud` : Ulcer disease\cr
#' `valv` : Valvular disease\cr
#' `wloss` : Weight loss
#'
#' @format `list(Dx = list(CIM9, CIM10))`
#' @encoding UTF-8
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/29578951/}{Validation of the Combined Comorbidity Index of Charlson and Elixhauser to Predict 30-Day Mortality Across ICD-9 and ICD-10}. \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/articles/CCI_INSPQ.pdf}{Voir PDF.}
#' @usage data('Combine_Dx_CCI_INSPQ18')
#' @encoding UTF-8
"Combine_Dx_CCI_INSPQ18"


# Charlson_Dx_CCI_INSPQ18 -------------------------------------------------

#' Data - Codes diagnostics
#'
#' Codes SQL regex (se terminent par un '%') à utiliser lors de l'extraction des codes de diagnostics pour l'étude de la comorbidité.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.\cr\cr
#' `aids` : AIDS/HIV\cr
#' `canc` : Any tumor without metastasis\cr
#' `cevd` : Cerebrovascular disease\cr
#' `chf` : Congestive heart failure\cr
#' `copd` : Chronic pulmonary disease\cr
#' `dementia` : Dementia\cr
#' `diab` : Diabetes, complicated\cr
#' `diabwc` : Diabetes, uncomplicated\cr
#' `ld` : Liver disease\cr
#' `metacanc` : Metastatic cancer\cr
#' `mi` : Myocardial infarction\cr
#' `para` : Paralysis\cr
#' `rend` : Renal disease\cr
#' `rheumd` : Rheumatoid arth./collagen vascular disease\cr
#' `ud` : Ulcer disease\cr
#' `valv` : Valvular disease
#'
#' @format `list(Dx = list(CIM9, CIM10))`
#' @encoding UTF-8
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/29578951/}{Validation of the Combined Comorbidity Index of Charlson and Elixhauser to Predict 30-Day Mortality Across ICD-9 and ICD-10}. \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/articles/CCI_INSPQ.pdf}{Voir PDF.}
#' @usage data('Charlson_Dx_CCI_INSPQ18')
#' @encoding UTF-8
"Charlson_Dx_CCI_INSPQ18"


# Elixhauser_Dx_CCI_INSPQ18 --------------------------------------------------

#' Data - Codes diagnostics
#'
#' Codes SQL regex (se terminent par un '%') à utiliser lors de l'extraction des codes de diagnostics pour l'étude de la comorbidité.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.\cr\cr
#' `aids` : AIDS/HIV\cr
#' `alcohol` : Alcohol abuse\cr
#' `blane` : Blood loss anemia\cr
#' `canc` : Any tumor without metastasis\cr
#' `carit` : Cardiac arrhythmias\cr
#' `chf` : Congestive heart failure\cr
#' `coag` : Coagulopathy\cr
#' `copd` : Chronic pulmonary disease\cr
#' `dane` : Deficiency anemia\cr
#' `depre` : Depression\cr
#' `diab` : Diabetes, complicated\cr
#' `diabwc` : Diabetes, uncomplicated\cr
#' `drug` : Drug abuse\cr
#' `fed` : Fluid and electrolyte disorders\cr
#' `hyp` : Hypertension\cr
#' `hypothy` : Hypothyroidism\cr
#' `ld` : Liver disease\cr
#' `metacanc` : Metastatic cancer\cr
#' `nd` : Neurological disorders\cr
#' `obes` : Obesity\cr
#' `para` : Paralysis\cr
#' `pcd` : Pulmonary circulation disorders\cr
#' `psycho` : Psychoses\cr
#' `pvd` : Peripheral vascular disorders\cr
#' `rend` : Renal disease\cr
#' `rheumd` : Rheumatoid arth./collagen vascular disease\cr
#' `ud` : Ulcer disease\cr
#' `valv` : Valvular disease\cr
#' `wloss` : Weight loss
#'
#' @format `list(Dx = list(CIM9, CIM10))`
#' @encoding UTF-8
#' @source \href{https://pubmed.ncbi.nlm.nih.gov/29578951/}{Validation of the Combined Comorbidity Index of Charlson and Elixhauser to Predict 30-Day Mortality Across ICD-9 and ICD-10}. \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/articles/CCI_INSPQ.pdf}{Voir PDF.}
#' @usage data('Elixhauser_Dx_CCI_INSPQ18')
#' @encoding UTF-8
"Elixhauser_Dx_CCI_INSPQ18"


# Charlson_Dx_UManitoba16 -------------------------------------------------

#' Data - Codes diagnostics
#'
#' Codes SQL regex (se terminent par un '%') à utiliser lors de l'extraction des codes de diagnostics pour l'étude de la comorbidité.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.\cr\cr
#' `aids` : HIV/AIDS\cr
#' `canc` : Cancer\cr
#' `chf` : Congestive Heart Failure\cr
#' `cpd` : Chronic Pulmonary Disease\cr
#' `ctdrd` : Connective Tissue Disease - Rheumatic Disease\cr
#' `cvd` : Cerebrovascular Disease\cr
#' `dementia` : Dementia\cr
#' `diab` : Diabetes with Chronic Complications\cr
#' `diabwc` : Diabetes without Chronic Complications\cr
#' `ld1` : Mild Liver Disease\cr
#' `ld2` : Moderate or Severe Liver Disease\cr
#' `mc` : Metastatic Carcinoma\cr
#' `mi` : Myocardial Infarction\cr
#' `ph` : Paraplegia and Hemiplegia\cr
#' `pud` : Peptic Ulcer Disease\cr
#' `pvd` : Peripheral Vascular Disease\cr
#' `rd` : Renal Disease
#'
#' @format `list(Dx = list(CIM9, CIM10))`
#' @encoding UTF-8
#' @source \href{http://mchp-appserv.cpe.umanitoba.ca/reference/Candata_web_final.pdf#Page=92}{CANCER DATA LINKAGE IN MANITOBA: EXPANDING THE INFRASTRUCTURE FOR RESEARCH} page 72 du document.
#' @usage data('Charlson_Dx_UManitoba16')
#' @encoding UTF-8
"Charlson_Dx_UManitoba16"


# ComorbidityWeights ------------------------------------------------------

#' Data - Poids des codes de diagnostics
#'
#' @details L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format `list` contenant trois (3) tables indiquant la description, le code et le poids :
#' \describe{
#'   \item{CCI_INSPQ_2018_CIM9}{}
#'   \item{CCI_INSPQ_2018_CIM10}{}
#'   \item{UManitoba_2016}{}
#' }
#'
#' @usage data('ComorbidityWeights')
#' @encoding UTF-8
#' @source Voir la source des datas \code{\link{Combine_Dx_CCI_INSPQ18}}, \code{\link{Charlson_Dx_CCI_INSPQ18}}, \code{\link{Elixhauser_Dx_CCI_INSPQ18}} et \code{\link{Charlson_Dx_UManitoba16}}
"ComorbidityWeights"



# I_APME_DEM_AUTOR_CRITR_ETEN_CM ------------------------------------------

#' Data - Demandes d'autorisation de Patient-Médicament d'exceptions.
#'
#' @format `list`
#' \describe{
#'   \item{\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37934&TypOrigElmVue=D&NoSeqElmOrig=37196}{DES_COURT_INDCN_RECNU}}{Valeurs uniques de la description courte complète de l'indication reconnue de PME.\cr - \code{DES_COURT_INDCN_RECNU} : Description courte complète de l'indication reconnue. `character`.\cr - \code{DEBUT} : Première année (\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37531&TypOrigElmVue=T&NoSeqElmOrig=1872}{\code{APME_DAT_STA_DEM_PME}}) où la description courte complète a été inscrite. `integer`.\cr - \code{FIN} : Dernière année (\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37531&TypOrigElmVue=T&NoSeqElmOrig=1872}{\code{APME_DAT_STA_DEM_PME}}) où la description courte complète a été inscrite. `integer`.}
#'   \item{\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37927&TypOrigElmVue=T&NoSeqElmOrig=9127}{NO_SEQ_INDCN_RECNU_PME}}{Indique la première et la dernière année d'utilisation.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37927&TypOrigElmVue=T&NoSeqElmOrig=9127}{\code{NO_SEQ_INDCN_RECNU}} : Numéro de séquence d'indication reconnue - PME. `integer`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37534&TypOrigElmVue=T&NoSeqElmOrig=9381}{\code{DD_TRAIT_DEM}} : Date de début de traitement demandée. `character`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37537&TypOrigElmVue=T&NoSeqElmOrig=9382}{\code{DF_TRAIT_DEM}} : Date de fin de traitement demandée. `character`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37944&TypOrigElmVue=T&NoSeqElmOrig=9367}{\code{DD_AUTOR}} : Date de début de l'autorisation PME. `character`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37946&TypOrigElmVue=T&NoSeqElmOrig=9368}{\code{DF_AUTOR}} : Date de fin de l'autorisation PME. `character`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37943&TypOrigElmVue=T&NoSeqElmOrig=9363}{\code{DD_APLIC_AUTOR}} : Date de début de l'applicabilité de l'autorisation de PME. `character`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37945&TypOrigElmVue=T&NoSeqElmOrig=9364}{\code{DF_APLIC_AUTOR}} : Date de fin de l'applicabilité de l'autorisation de PME. `character`.\cr - \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM&NoSeqElmVue=37531&TypOrigElmVue=T&NoSeqElmOrig=1872}{\code{DAT_STA_DEM}} : Date de création ou de mise à jour du statut d'une demande d'autorisation correspondant à l'attribution du dernier statut de la demande. `character`.}
#' }
#'
#' @usage data('I_APME_DEM_AUTOR_CRITR_ETEN_CM')
#' @encoding UTF-8
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=2325&NomVue=I%5FAPME%5FDEM%5FAUTOR%5FCRITR%5FETEN%5FCM}{Dictionnaire EI}
"I_APME_DEM_AUTOR_CRITR_ETEN_CM"



# Obstetrics_Dx -----------------------------------------------------------

#' Data - Codes diagnostics gestationnels
#'
#' Codes SQL regex (se terminent par un '%') à utiliser lors de l'extraction des codes de diagnostics gestationnels pour l'étude de la comorbidité.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format `list(Dx = list(CIM9, CIM10))`
#' @encoding UTF-8
#' @usage data('Obstetrics_Dx')
"Obstetrics_Dx"



# DBC_TablesV -------------------------------------------------------------

#' Data - Liste des tables de *EI* RAMQ
#'
#' @format `data.table` de 3 variables et 32708 observations
#' \describe{
#'   \item{NOM_BD}{Nom de la base de données. Par exemple *PROD*, *RES_SSS*, *DONNE_INESSS*, etc. `character`.}
#'   \item{NOM_TABLE}{Nom de la table. `character`.}
#'   \item{TYPE_TABLE}{Type de la table. Voir \href{https://docs.teradata.com/r/EcH18cz_mkFcqxA7GeutqQ/kD2HkTfj10WlyDqsbQrc4w}{TableKind Column} pour plus d'infos. `character`.}
#' }
#' @encoding UTF-8
#' @usage data('DBC_TablesV')
#' @keywords internal
"DBC_TablesV"



# Pop_QC ------------------------------------------------------------------

#' Data - Estimations et projections de population comparables (1996-2041)
#'
#' Tableau de la population québécoise par niveau géographique.\cr\cr
#' Ce fichier présente une série continue de données populationnelles comparables composée de la série des estimations (1996-2019) et de la série des projections (2020-2041) de population. Ces données tiennent compte de l'évolution de la population selon les plus récentes données observées de naissances, décès et mouvements migratoires.\cr\cr
#' Il est à noter que ces données de population sont présentées sur la base du découpage territorial du réseau de la santé et des services sociaux, soit pour les territoires suivants : le Québec, les réseaux universitaires intégrés de santé et de services sociaux (RUISSS), les régions sociosanitaires (RSS), les réseaux territoriaux de services (RTS), les réseaux locaux de services (RLS) et les centres locaux de services communautaires (CLSC).
#'
#' **Attention** `AGE = 90` équivaut à *90 ans et plus*.\cr\cr
#' La classe des colonnes est `character` lorsque c'est du texte ou `integer` lorsque c'est un nombre.\cr\cr
#' **Mise en ligne** : 25 février 2016.\cr
#' **Dernière modification** : 24 avril 2020.\cr
#' **Publication no** : EstimProjComp-ISQ.\cr\cr
#' La fiche d'information et technique de cette base de données est disponible avec le fichier Excel (voir *Source*).\cr\cr
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format Tableau de 8 variables et 2 595 320 observations :
#' \describe{
#'   \item{GEO}{Niveau géographique : Québec, RUISSS, RSS, RTS, RLS, CLSC. `character`.}
#'   \item{CODE}{Code du territoire. `integer`.}
#'   \item{AN}{Année. `integer`.}
#'   \item{TYPE}{Type de données : Estimations ou Projections. `character`.}
#'   \item{STATUT}{Donnée révisée ou provisoire. `NA` indique que la donnée n'a pas été changée depuis la dernière publication. `character`.}
#'   \item{SEXE}{`character`.}
#'   \item{AGE}{`integer`.}
#'   \item{POP}{Population. `integer`.}
#' }
#'
#' @usage data('Pop_QC')
#' @encoding UTF-8
#' @source \href{https://www.msss.gouv.qc.ca/professionnels/informations-geographiques-et-de-population/donnees-populationnelles/}{MSSS Données de population}.\cr
#' \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/extdata/EstimationProjectionComparable_1996_2041_20200424.xlsx}{Fichier Excel utilisé}.
"Pop_QC"


# RLS_list ----------------------------------------------------------------

#' Data - Liste des RLS
#'
#' Vecteur contenant la liste des 93 RLS plus 3 valeurs utiles lors d'analyse : 1001, 1701, 1801.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format Vecteur `integer` de 96 nombres.
#' @encoding UTF-8
#' @usage data('RLS_list')
"RLS_list"



# RLS_tab_convert -------------------------------------------------------------

#' Data - Correspondance RLS Loi 10
#'
#' Établir la correspondance des RLS avant et après l'adoption de la loi 10.
#'
#' Certains RLS ne peuvent être convertis, car leur valeur se retrouve avant et après l'adoption de la loi 10.\cr
#' `attr(RLS_tab_convert, "RLS_exclus")` indique les quatre (4) RLS exclus : 611, 612, 1611, 1612.\cr
#' `attr(RLS_tab_convert, "RLS_exclus_value")` renvoie un tableau indiquant les valeurs avant et après l'adoption de la loi 10 pour ces quatre (4) RLS.\cr\cr
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format Tableau de 2 variables et 84 observations :
#' \describe{
#'   \item{RLS14}{Code de RLS **avant** l'adoption de la loi 10. `integer`.}
#'   \item{RLS15}{Code de RLS **après** l'adoption de la loi 10. `integer`.}
#' }
#'
#' @usage data('RLS_tab_convert')
#' @encoding UTF-8
#' @source \href{https://publications.msss.gouv.qc.ca/msss/fichiers/statistiques/decoupage-territorial/Doc1_Correspondance_Etablissement_Public_Loi_10.xls}{Correspondance Etablissement Public Loi 10}.\cr
#' \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/extdata/Doc1_Correspondance_Etablissement_Public_Loi_10.xls}{Fichier Excel utilisé}.
"RLS_tab_convert"


# V_DEM_PAIMT_MED_CM ------------------------------------------------------

#' Data
#'
#' Base de données sur les demandes de paiement de médicaments.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création
#'
#' @format `list` :
#' \describe{
#'   \item{\code{DENOM_DIN_AHFS}}{Valeurs uniques des combinaisons 1) codes de dénomination communes, 2) codes DIN et 3) codes de classe AHFS.\cr - \code{DENOM} : Code de dénomination commune. `character`.\cr - \code{DIN} : Code d'identification du médicament. `integer`.\cr - \code{AHFS_CLA} : Classe AHFS. `character`.\cr - \code{AHFS_SCLA} : Sous-classe AHFS. `character`.\cr - \code{AHFS_SSCLA} : Sous-sous-classe AHFS. `character`.\cr - \code{NOM_DENOM} : Description du code \code{DENOM}. `character`.\cr - \code{MARQ_COMRC} : Nom de la marque commerciale. `character`.\cr - \code{AHFS_NOM_CLA} : Nom de la classe AHFS. `character`.\cr - \code{DEBUT} : Première année où la combinaison a été inscrite. `integer`.\cr - \code{FIN} : Dernière année où la combinaison a été inscrite. `integer`.}
#'   \item{COD_AHFS}{Codes de classe AHFS.\cr - \code{AHFS_CLA} : Classe AHFS. `character`.\cr - \code{AHFS_SCLA} : Sous-classe AHFS. `character`.\cr - \code{AHFS_SSCLA} : Sous-sous-classe AHFS. `character`. - \code{AHFS_NOM_CLA} : Nom de la classe AHFS. `character`.\cr - \code{DEBUT} : Première année où le code a été inscrit. `integer`.\cr - \code{FIN} : Dernière année où le code a été inscrit. `integer`.}
#'   \item{\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29&NoSeqElmVue=30994&TypOrigElmVue=T&NoSeqElmOrig=1233}{COD_DENOM_COMNE}}{Codes de dénominations communes qui existent dans la base de données \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}.\cr - \code{DENOM} : Code de dénomination commune. `character`.\cr - \code{NOM_DENOM} : Description du code \code{DENOM}.\cr - \code{DEBUT} : Première année où le code a été inscrit dans la base de données. `integer`.\cr - \code{FIN} : Dernière année où le code a été inscrit dans la base de données. `integer`.}
#'   \item{\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29&NoSeqElmVue=30939&TypOrigElmVue=T&NoSeqElmOrig=1265}{\code{COD_DIN}}}{Description des codes d'identification du médicament :\cr - \code{DIN} : Code d'identification du médicament. `integer`.\cr - \code{DEBUT} : Première année où le code a été inscrit dans la base de données. `integer`.\cr - \code{FIN} : Dernière année où le code a été inscrit dans la base de données. `integer`.}
#'   \item{COD_SERV}{Description et années d'utilisation des codes de service. `NA` indique que le code n'a pas été utilisé.\cr - \code{COD_SERV} : Code de service. `character`.\cr - \code{SERV_1} : Première et dernière année que le code de service a été inscrit dans la colonne \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29&NoSeqElmVue=30954&TypOrigElmVue=T&NoSeqElmOrig=6295}{SMED_COD_SERV_1}. `character`.\cr - \code{SERV_2} : Première et dernière année que le code de service a été inscrit dans la colonne \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29&NoSeqElmVue=30955&TypOrigElmVue=T&NoSeqElmOrig=6397}{SMED_COD_SERV_2}. `character`.\cr - \code{SERV_3} : Première et dernière année que le code de service a été inscrit dans la colonne \href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29&NoSeqElmVue=30956&TypOrigElmVue=T&NoSeqElmOrig=6398}{SMED_COD_SERV_3}. `character`.\cr - \code{COD_SERV_DESC} : Description du code de service. `character`.}
#'   \item{\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29&NoSeqElmVue=30958&TypOrigElmVue=T&NoSeqElmOrig=6162}{COD_STA_DECIS}}{Codes de statut de décision qui existent dans la base de données \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{V_DEM_PAIMT_MED_CM}.\cr - \code{COD_STA_DECIS}: Code de statut de décision. `character`.\cr - \code{COD_STA_DESC} : Description du code de statut de décision. `character`.\cr - \code{DEBUT} : Première année où le code a été inscrit dans la base de données. `integer`.\cr - \code{FIN} : Dernière année où le code a été inscrit dans la base de données. `integer`.}
#' }
#'
#' @encoding UTF-8
#' @usage data('V_DEM_PAIMT_MED_CM')
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1823&NomVue=V%5FDEM%5FPAIMT%5FMED%5FCM+%28DEMANDES+DE+PAIEMENT+%2D+PROGRAMME+%ABMEDICAMENT%BB%29}{Dictionnaire EI}
"V_DEM_PAIMT_MED_CM"



# V_DENOM_COMNE_MED -------------------------------------------------------

#' Data
#'
#' Description des codes de dénomination commune.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
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
#'
#' @encoding UTF-8
#' @usage data('V_DENOM_COMNE_MED')
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1082&NomVue=V%5FDENOM%5FCOMNE%5FMED+%28D%E9nomination+commune+m%E9dicament%29}{Dictionnaire EI}.
"V_DENOM_COMNE_MED"


# V_DES_COD ---------------------------------------------------------------

#' Data
#'
#' Domaine de valeurs pour les différents codes de l'environnement informationnel.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format Tableau de 5 variables :
#' \describe{
#'   \item{CODE}{Valeurs codifiées que peut prendre un élément (`CODE_VAL_COD`). `character`.}
#'   \item{TYPE_CODE}{Nom identifiant un élément de données (`CODE_NOM_COD`). `character`.}
#'   \item{CODE_DESC}{Description du code (`CODE_DES`). `character`.}
#'   \item{DATE_DEBUT}{Date de début de la période d'application (`CODE_DD_DES_COD`). `Date`.}
#'   \item{DATE_FIN}{Date de fin de la période d'application (`CODE_DF_DES_COD`). `Date`.}
#' }
#'
#' @encoding UTF-8
#' @usage data('V_DES_COD')
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=111&NomVue=V%5FDES%5FCOD+%28DESCRIPTIONS+DES+CODES%29}{Dictionnaire EI}.
"V_DES_COD"



# V_PRODU_MED -------------------------------------------------------------

#' Data
#'
#' Produit qui peut faire l'objet d'une facturation. Règle générale, c'est un médicament conçu par un fabricant.
#'
#' L'attribut `MaJ` indique la dernière mise à jour ou la date de création du tableau.
#'
#' @format Tableau de 5 variables :
#' \describe{
#'   \item{\href{http://intranet/eci/eci2/asp/ECI2P06_ElmSpec.asp?Envir=PROD&min=1&max=10&NomVue=V%5FPRODU%5FMED+%28Produit+m%E9dicament%29&NoSeqElmVue=27099&TypOrigElmVue=T&NoSeqElmOrig=5283}{NOM_MARQ_COMRC}}{Nom sous lequel est commercialisé un produit pharmaceutique.\cr - \code{DENOM} : Code de dénomination commune (`NMED_COD_DENOM_COMNE`). `character`.\cr - \code{DIN} : Code d'identification du médicament (`NMED_COD_DIN`). `integer`.\cr - \code{NOM_MARQ_COMRC} : Nom sous lequel est commercialisé un produit pharmaceutique\cr(`NMED_NOM_MARQ_COMRC`). `character`.\cr - \code{DATE_DEBUT} : Date d'entrée en vigueur de la mise à jour à laquelle est relié l'ajout ou la modification de cette occurrence (`NMED_DD_PRODU_MED`). `Date`.\cr - \code{DATE_FIN} : Date d'entrée en vigueur de la mise à jour **moins un jour** de l'occurrence suivante (`NMED_DF_PRODU_MED`). `Date`.}
#' }
#'
#' @encoding UTF-8
#' @usage data('V_PRODU_MED')
#' @source \href{http://intranet/eci/ECI2/ASP/ECI2P04_DescVue.asp?Envir=PROD&NoVue=1555&NomVue=V%5FPRODU%5FMED+%28Produit+m%E9dicament%29}{Dictionnaire EI}.
"V_PRODU_MED"
