library(usethis)
library(readxl)
library(data.table)

ComorbidityWeights <- list()

# CCI_INSPQ_2018_CIM9 -----------------------------------------------------

ComorbidityWeights$CCI_INSPQ_2018_CIM9 <-
  as.data.table(read_excel("inst/extdata/Comorbidity_weights.xlsx", "CCI_INSPQ_2018_CIM9"))
ComorbidityWeights$CCI_INSPQ_2018_CIM9[, POIDS := as.integer(POIDS)]
setkey(ComorbidityWeights$CCI_INSPQ_2018_CIM9, DIAGN_CODE)


# CCI_INSPQ_2018_CIM10 ----------------------------------------------------

ComorbidityWeights$CCI_INSPQ_2018_CIM10 <-
  as.data.table(read_excel("inst/extdata/Comorbidity_weights.xlsx", "CCI_INSPQ_2018_CIM10"))
ComorbidityWeights$CCI_INSPQ_2018_CIM10[, POIDS := as.integer(POIDS)]
setkey(ComorbidityWeights$CCI_INSPQ_2018_CIM10, DIAGN_CODE)

# UManitoba_2016 ----------------------------------------------------------

ComorbidityWeights$UManitoba_2016 <-
  as.data.table(read_excel("inst/extdata/Comorbidity_weights.xlsx", "UManitoba_2016"))
ComorbidityWeights$UManitoba_2016[, POIDS := as.integer(POIDS)]
setkey(ComorbidityWeights$UManitoba_2016, DIAGN_CODE)



attr(ComorbidityWeights, "MaJ") <- Sys.Date()
use_data(ComorbidityWeights, overwrite = TRUE)
