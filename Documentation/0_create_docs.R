library(inesss)
library(rmarkdown)
conn <- sql_connexion(askpass::askpass("User"), askpass::askpass("Password"))



# README ------------------------------------------------------------------
render(  # github_document
  input = "README.Rmd",
  output_file = "README.md",
  envir = new.env()
)
render(  # pdf_document
  input = "README.Rmd",
  output_format = "pdf_document",
  output_file = paste0("LISEZ-MOI_",Sys.Date(),".pdf"),
  output_dir = "Documentation",
  envir = new.env()
)


# R-Rtools-installation ---------------------------------------------------
render(
  input = "Documentation/source/R-Rtools-installation.Rmd",
  output_file = paste0("R_RTOOLS_INSTALLATION_",Sys.Date(),".pdf"),
  output_dir = "Documentation",
  envir = new.env()
)


# Formulaire --------------------------------------------------------------
render(
  input = "Documentation/source/formulaire.Rmd",
  output_file = paste0("AIDE-FORMULAIRE_",Sys.Date(),".pdf"),
  output_dir = "Documentation",
  params = list(conn = conn),
  envir = new.env()
)


# Supprimer les fichiers superflu -----------------------------------------
files_2_delete <- c(
  paste0("AIDE-FORMULAIRE_",Sys.Date(),".log")
)
for (fil in files_2_delete) {
  if (fil %in% list.files("Documentation/source")) {
    unlink(paste0("Documentation/source/", fil), recursive = TRUE)
  }
}
