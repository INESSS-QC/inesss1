library(usethis)
library(readxl)
library(data.table)

Comorbidity_weights <- as.data.table(read_excel(
  "inst/extdata/Comorbidity_weights.xlsx"
))
Comorbidity_weights[, `:=` (CIM9 = as.integer(CIM9),
                            CIM10 = as.integer(CIM10))]
setkey(Comorbidity_weights, DIAGN_CODE)

attr(Comorbidity_weights, "MaJ") <- Sys.Date()
use_data(Comorbidity_weights, overwrite = TRUE)
