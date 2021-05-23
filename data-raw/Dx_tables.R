############################################################################## #
### IMPORTANT
### Après avoir créée une table :
###   - Ouvrir le script SQL_comorbidity_diagn.R
###   - À la section de la fonction SQL_comorbidity_diagn.select_Dx_table
###   - Ajouter :
###     else if (Dx_table == "NomDeLaTable") {
###         return(inesss::NomDeLaTable)
###     }
############################################################################## #

library(usethis)
library(stringr)


# Coronopathie1 -------------------------------------------------------------------------------

Coronopathie1 <- list(

  # Angine poitrine
  AngPoitrine = list(
    CIM9 = c("413"),
    CIM10 = c("I20")
  ),

  # Infarctus
  Infarc = list(
    CIM9 = c(410:414),
    CIM10 = c(paste0("I", 21:25))
  )

)
attr(Coronopathie1, "MaJ") <- Sys.Date()


# SQL regex ---------------------------------------------------------------

### Ajouter un '%' après chaque code pour pouvoir les utiliser dans du SQL regex
for (k in c("Coronopathie1")) {
  dt <- get(k)
  for (i in 1:length(dt)) {
    for (j in c("CIM9", "CIM10")) {
      dt[[i]][[j]] <- paste0(dt[[i]][[j]], "%")
    }
  }
  assign(k, dt)
}



# Save datas ----------------------------------------------------------------------------------

use_data(Coronopathie1,

         overwrite = TRUE)
rm(Coronopathie1)
