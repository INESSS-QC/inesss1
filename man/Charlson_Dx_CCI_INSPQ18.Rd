% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-datas.R
\docType{data}
\encoding{UTF-8}
\name{Charlson_Dx_CCI_INSPQ18}
\alias{Charlson_Dx_CCI_INSPQ18}
\title{Data - Codes diagnostics}
\format{
\code{list(Dx = list(CIM9, CIM10))}
}
\source{
\href{https://pubmed.ncbi.nlm.nih.gov/29578951/}{Validation of the Combined Comorbidity Index of Charlson and Elixhauser to Predict 30-Day Mortality Across ICD-9 and ICD-10}. \href{https://github.com/INESSS-QC/inesss1/blob/master/inst/articles/CCI_INSPQ.pdf}{Voir PDF.}
}
\usage{
data('Charlson_Dx_CCI_INSPQ18')
}
\description{
Codes SQL regex (se terminent par un '\%') à utiliser lors de l'extraction des codes de diagnostics pour l'étude de la comorbidité.
}
\details{
L'attribut \code{MaJ} indique la dernière mise à jour ou la date de création du tableau.\cr\cr
\code{aids} : AIDS/HIV\cr
\code{canc} : Any tumor without metastasis\cr
\code{cevd} : Cerebrovascular disease\cr
\code{chf} : Congestive heart failure\cr
\code{copd} : Chronic pulmonary disease\cr
\code{dementia} : Dementia\cr
\code{diab} : Diabetes, complicated\cr
\code{diabwc} : Diabetes, uncomplicated\cr
\code{ld} : Liver disease\cr
\code{metacanc} : Metastatic cancer\cr
\code{mi} : Myocardial infarction\cr
\code{para} : Paralysis\cr
\code{rend} : Renal disease\cr
\code{rheumd} : Rheumatoid arth./collagen vascular disease\cr
\code{ud} : Ulcer disease\cr
\code{valv} : Valvular disease
}
\keyword{datasets}
