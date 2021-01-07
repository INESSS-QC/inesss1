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


# Registre Versions -------------------------------------------------------
render(
  input = "NEWS.Rmd",
  output_format = "md_document",
  output_file = "NEWS.md",
  output_dir = getwd(),
  envir = new.env()
)
render(  # pdf_document
  input = "NEWS.Rmd",
  output_format = "pdf_document",
  output_file = "inesss-REGISTRE-VERSION.pdf",
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
if (paste0("AIDE-FORMULAIRE_",Sys.Date(),".log") %in% list.files("Documentation/source")) {
  unlink(paste0("Documentation/source/AIDE-FORMULAIRE_",Sys.Date(),".log"), recursive = TRUE)
}


# Build Manual ------------------------------------------------------------
devtools::build_manual()
file.copy(paste0("../inesss_",as.character(packageVersion("inesss")),".pdf"),
          paste0("Documentation/inesss_",as.character(packageVersion("inesss")),".pdf"),
          overwrite = TRUE)
file.remove(paste0("../inesss_",as.character(packageVersion("inesss")),".pdf"))
