library(usethis)
library(inesss)

Obstetrics_diagn_codes <- list(
  # Obstetrics
  obstetric = c(
    paste0(641:676,"%"),
    'O1%', paste0("O",21:95,"%"), 'O98%', 'O99%',
    'V27%', 'Z37'
  )
)

use_data(Obstetrics_diagn_codes, overwrite = TRUE)
