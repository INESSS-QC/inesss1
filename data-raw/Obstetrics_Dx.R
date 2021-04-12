library(usethis)
library(inesss)

Obstetrics_Dx <- list(
  obstetric = list(
    CIM9 = c(641:676, "V27"),
    CIM10 = c("O1", paste0("O", c(21:95, 98, 99)),
              "Z37")
  )
)

### Ajouter un '%' après chaque code pour pouvoir les utiliser dans du SQL regex
for (i in 1:length(Obstetrics_Dx)) {
  for (j in c("CIM9", "CIM10")) {
    Obstetrics_Dx[[i]][[j]] <- paste0(Obstetrics_Dx[[i]][[j]],"%")
  }
}

attr(Obstetrics_Dx, "MaJ") <- Sys.Date()
use_data(Obstetrics_Dx, overwrite = TRUE)

rm(Obstetrics_Dx)
