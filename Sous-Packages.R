library(stringr)

# DER.inesss ----------------------------------------------------------------------------------

#* Description ####
file.copy("DESCRIPTION", "DER.inesss/DESCRIPTION", overwrite = TRUE)
desc_file <- readLines("DER.inesss/DESCRIPTION")
desc_file[[1]] <- str_replace(desc_file[[1]], "inesss", "DER.inesss")
desc_file[[9]] <- str_replace(desc_file[[9]], "BDCA", "DER")
writeLines(desc_file, "DER.inesss/DESCRIPTION")

#* Fonction ####
DER.inesss.fcts <- paste0(c(
  "SQL_connexion"
),".R")
for (fct in DER.inesss.fcts) {
  file.copy(paste0("R/",fct), paste0("DER.inesss/R/",fct), overwrite = TRUE)
}
