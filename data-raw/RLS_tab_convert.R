library(usethis)
library(data.table)
library(readxl)

DT <- read_excel("inst/extdata/Doc1_Correspondance_Etablissement_Public_Loi_10.xls",
                 sheet = "Global")
setDT(DT)  # convertir en data.table

DT <- DT[, .(RLS14 = as.integer(`Code\nRLS 2014`),  # ancien RLS
             RLS15 = as.integer(`Code\nRLS 2015`))]  # nouveau RLS
DT <- unique(DT)  # RLS unique : provient d'un tableau avec les CLSC, RSS, ...

RLS_double <- DT[RLS14 %in% RLS15 | RLS15 %in% RLS14]  # equivalence des RLS qui n'ont pas de sens
RLS_exclus <- sort(unique(RLS_double$RLS14[RLS_double$RLS14 %in% RLS_double$RLS15]))  # liste des RLS à exclure

DT <- DT[!RLS14 %in% RLS_exclus & !RLS15 %in% RLS_exclus]  # exclure les RLS ne pouvant être converti
setkey(DT, RLS15)  # tri

RLS_tab_convert <- copy(DT)
attr(RLS_tab_convert, "RLS_exclus") <- RLS_exclus
attr(RLS_tab_convert, "RLS_exclus_value") <- RLS_double
attr(RLS_tab_convert, "MaJ") <- Sys.Date()

use_data(RLS_tab_convert, overwrite = TRUE)

rm(RLS_tab_convert)
