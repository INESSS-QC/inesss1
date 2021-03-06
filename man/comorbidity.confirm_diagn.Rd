% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comorbidity.R
\encoding{UTF-8}
\name{comorbidity.confirm_diagn}
\alias{comorbidity.confirm_diagn}
\title{comorbidity}
\usage{
comorbidity.confirm_diagn(dt, n1, n2, confirm_sourc)
}
\value{
\code{data.table} de 6 colonnes :
\itemize{
\item ID : Numéro d'identification de l'usager.
\item DIAGN : Code de diagnostique.
\item DATE_REP : Première date de diagnostique confirmée.
\item SOURCE_REP : Provenance de \code{DATE_REP}.
\item DATE_CONF : Date qui confirme \code{DATE_REP}.
\item SOURCE_CONF : Provenance de \code{SOURCE_CONF}.
}
}
\description{
comorbidity
}
\keyword{internal}
