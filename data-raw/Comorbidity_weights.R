library(usethis)
library(readxl)
library(data.table)

Comorbidity_weights <- as.data.table(read_excel(
  "inst/extdata/Comorbidity_weights.xlsx"
))
setkey(Comorbidity_weights, DIAGN_CODE)

use_data(Comorbidity_weights, overwrite = TRUE)
