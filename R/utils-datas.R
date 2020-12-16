#' Estimations et projections de population comparables (1996-2041)
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
#'   \item{SEXE}
#'   \item{AGE}
#'   \item{POP}{Population}
#' }
#'
#' @usage data("Pop_QC") / MaVariable <- Pop_QC.
#'
#' @source \href{https://www.msss.gouv.qc.ca/professionnels/informations-geographiques-et-de-population/donnees-populationnelles/}{MSSS Données de population.}
"Pop_QC"


#' Correspondance RLS Loi 10
#'
#' Établir la correspondance des RLS avant et après l'adoption de la loi 10.
#'
#' Certains RLS ne peuvent être convertis, car leur valeur se retrouve avant et après l'adoption de la loi 10.\cr\cr
#' `attr(RLS_convert, "RLS_exclus")` indique les quatre (4) RLS exclus : 611, 612, 1611, 1612.\cr\cr
#' `attr(RLS_convert, "RLS_exclus_value")` renvoie un tableau indiquant les valeurs avant et après l'adoption de la loi 10 pour ces quatre (4) RLS.
#'
#' @format Tableau de 2 variables et 84 observations :
#' \describe {
#'   \item{RLS14}{Code de RLS **avant** l'adoption de la loi 10.}
#'   \item{RLS15}{Code de RLS **après** l'adoption de la loi 10.}
#' }
#'
#' @usage data("RLS_convert") / MaVariable <- RLS_convert.
#'
#' @source \href{https://publications.msss.gouv.qc.ca/msss/fichiers/statistiques/decoupage-territorial/Doc1_Correspondance_Etablissement_Public_Loi_10.xls}{Correspondance Etablissement Public Loi 10.}
"RLS_convert"


#' Liste des RLS
#'
#' Vecteur contenant la liste des 93 RLS plus 3 valeurs utilisées lors d'analyse : 1001, 1701, 1801.
#'
#' @format Vecteur `integer` de 96 nombres.
#'
#' @usage data("RLS_list") / MaVariable <- RLS_list.
"RLS_list"
