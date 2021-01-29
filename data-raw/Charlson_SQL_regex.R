library(usethis)
library(inesss)

diagn <- c(
  # AIDS/HIV
  "aids",
  # Acute Myocardial Infarction
  "ami",
  # Cancer (without metastasis)
  "canc",
  # Cerebrovascular disease
  "cevd",
  # Congestive heart failure
  "chf",
  # Chronic Obstructive Pulmonary Disease
  "copd",
  # Dementia
  "dementia",
  # Diabetes without complications
  "diab",
  # Diabetes with complications
  "diabwc",
  # Liver disease
  "ld",
  # Metastatic cancer
  "metacanc",
  # Paralysis
  "para",
  # Renal disease
  "rend",
  # Rheumatoid arthritis/collaged vascular disease
  "rheumd",
  # Ulcer disease
  "ud",
  # Valvular disease
  "valv"
)
Charlson_SQL_regex <- vector("list", length(diagn))
i <- 1L
for (dia in diagn) {
  Charlson_SQL_regex[[i]] <- Comorbidity_SQL_regex[[dia]]
  i <- i + 1L
}
names(Charlson_SQL_regex) <- diagn

use_data(Charlson_SQL_regex, overwrite = TRUE)
