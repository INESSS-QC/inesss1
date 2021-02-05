library(usethis)
library(inesss)

Obstetrics_diagn_codes <- list(
  obstetric = list(
    CIM9 = c(641:676, "V27"),
    CIM10 = c("O1", paste0("O", c(21:95, 98, 99)),
              "Z37")
  )
)

### Ajouter un '%' après chaque code pour pouvoir les utiliser dans du SQL regex
for (i in 1:length(Obstetrics_diagn_codes)) {
  for (j in c("CIM9", "CIM10")) {
    Obstetrics_diagn_codes[[i]][[j]] <- paste0(Obstetrics_diagn_codes[[i]][[j]],"%")
  }
}

attr(Obstetrics_diagn_codes, "MaJ") <- Sys.Date()
use_data(Obstetrics_diagn_codes, overwrite = TRUE)
