library(usethis)
library(inesss)

diagn <- c(
  # AIDS/HIV
  "aids",
  # Alcohol abuse
  "alcohol",
  # Blood loss anemina
  "blane",
  # Cancer (without metastasis)
  "canc",
  # Cardiac arrhythmias
  "carit",
  # Congestive heart failure
  "chf",
  # Coagulopathy
  "coag",
  # Chronic Obstructive Pulmonary Disease
  "copd",
  # Deficiency anaemia
  "dane",
  # Depression
  "depre",
  # Diabetes without complications
  "diab",
  # Diabetes with complications
  "diabwc",
  # Drug abuse
  "drug",
  # Fluid and electrolyte disorders
  "fed",
  # Hypertension
  "hyp",
  # Hypothyroidism
  "hypothy",
  # Liver disease
  "ld",
  # Metastatic cancer
  "metacanc",
  # Neurological disorders
  "nd",
  # Obesity
  "obes",
  # Paralysis
  "para",
  # Pulmonary circulation disorders
  "pcd",
  # Psychose
  "psycho",
  # Peripheral vascular disease
  "pvd",
  # Renal disease
  "rend",
  # Rheumatoid arthritis/collaged vascular disease
  "rheumd",
  # Ulcer disease
  "ud",
  # Valvular disease
  "valv",
  # Weight loss
  "wloss"
)
Elixhauser_SQL_regex <- vector("list", length(diagn))
i <- 1L
for (dia in diagn) {
  Elixhauser_SQL_regex[[i]] <- Comorbidity_SQL_regex[[dia]]
  i <- i + 1L
}
names(Elixhauser_SQL_regex) <- diagn

use_data(Elixhauser_SQL_regex, overwrite = TRUE)
