% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SQL_stats_SMED_NBR_JR_DUREE_TRAIT.R
\encoding{UTF-8}
\name{SQL_stats_SMED_NBR_JR_DUREE_TRAIT}
\alias{SQL_stats_SMED_NBR_JR_DUREE_TRAIT}
\title{Statistiques}
\usage{
SQL_stats_SMED_NBR_JR_DUREE_TRAIT(
  conn,
  debut,
  fin,
  by_code_serv = TRUE,
  include_dureeTx_0 = FALSE
)
}
\arguments{
\item{conn}{Variable contenant la connexion entre R et Teradata. Voir \code{\link{SQL_connexion}}.}

\item{debut}{Date de début de la période d'étute au format \code{AAAA-MM-JJ}.}

\item{fin}{Date de fin de la période d'étude au format \code{AAAA-MM-JJ}.}

\item{by_code_serv}{\code{TRUE} ou \code{FALSE}. Grouper les résultats par code de services. Par défaut \code{TRUE}.}

\item{include_dureeTx_0}{\code{TRUE} ou \code{FALSE}. Inclure les durées de traitements égale à zéro. Par défaut \code{FALSE}.}
}
\value{
\code{list}
}
\description{
Statistiques descriptives de la variable \code{SMED_NBR_JR_DUREE_TRAIT} de la vue \code{V_DEM_PAIMT_MED_CM}.
}
