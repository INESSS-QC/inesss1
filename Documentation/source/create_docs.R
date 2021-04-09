library(inesss)
library(rmarkdown)
library(stringr)
library(writexl)
conn <- SQL_connexion(askpass::askpass("User"), askpass::askpass("Password"))


# Build Manual ------------------------------------------------------------
devtools::build_manual(path = "Documentation")


# Build Vignettes ---------------------------------------------------------
devtools::build_vignettes()
for (file in list.files("doc")) {
  if (stringr::str_sub(file, nchar(file) - 4, nchar(file)) == ".html") {
    file.copy(from = paste0("doc/",file),
              to = paste0("Documentation/Vignettes/",file))
  } else {
    next
  }
}


# Formulaire --------------------------------------------------------------
render(
  input = "Documentation/source/formulaire.Rmd",
  output_file = paste0("AIDE-FORMULAIRE_",Sys.Date(),".pdf"),
  output_dir = "Documentation",
  params = list(conn = conn),
  envir = new.env(), encoding = "UTF-8"
)
if (paste0("AIDE-FORMULAIRE_",Sys.Date(),".log") %in% list.files("Documentation/source")) {
  unlink(paste0("Documentation/source/AIDE-FORMULAIRE_",Sys.Date(),".log"), recursive = TRUE)
}


# README ------------------------------------------------------------------
render(  # github_document
  input = "README.Rmd",
  output_file = "README.md",
  envir = new.env(), encoding = "UTF-8"
)
render(  # pdf_document
  input = "README.Rmd",
  output_format = "pdf_document",
  output_file = paste0("LISEZ-MOI_",Sys.Date(),".pdf"),
  output_dir = "Documentation",
  envir = new.env(), encoding = "UTF-8"
)


# Registre Versions -------------------------------------------------------
render(
  input = "NEWS.Rmd",
  output_format = "md_document",
  output_file = "NEWS.md",
  output_dir = getwd(),
  envir = new.env(), encoding = "UTF-8"
)
render(  # pdf_document
  input = "NEWS.Rmd",
  output_format = "pdf_document",
  output_file = "inesss-REGISTRE-VERSION.pdf",
  output_dir = "Documentation",
  envir = new.env(), encoding = "UTF-8"
)

