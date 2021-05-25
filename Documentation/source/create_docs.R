library(inesss)
library(rmarkdown)
library(stringr)
library(writexl)
# conn <- SQL_connexion()


# Build Manual ------------------------------------------------------------
devtools::build_manual(path = "Documentation")


# Vignettes ---------------------------------------------------------
files <- list.files("vignettes")
files <- files[tolower(stringr::str_sub(files, nchar(files) - 3, nchar(files))) == ".rmd"]
files <- stringr::str_sub(files, 1, nchar(files) - 4)
for (file in files) {
  render(
    input = paste0("vignettes/",file,".Rmd"),
    output_file = paste0(file,".html"),
    output_dir = "Documentation/Vignettes",
    encoding = "UTF-8", envir = new.env()
  )
}

# Formulaire --------------------------------------------------------------
render(
  input = "Documentation/source/formulaire.Rmd",
  output_file = paste0("AIDE-FORMULAIRE.pdf"),
  output_dir = "Documentation",
  # params = list(conn = conn),
  envir = new.env(), encoding = "UTF-8"
)
if (paste0("AIDE-FORMULAIRE.log") %in% list.files("Documentation/source")) {
  unlink(paste0("Documentation/source/AIDE-FORMULAIRE.log"), recursive = TRUE)
}


# README ------------------------------------------------------------------
render(  # github_document
  input = "README.Rmd",
  output_file = "README.md",
  envir = new.env(), encoding = "UTF-8"
)
render(
  input = "README.Rmd",
  # output_format = "pdf_document",
  # output_file = paste0("LISEZ-MOI.pdf"),
  output_format = "html_document",
  output_file = paste0("LISEZ-MOI.html"),
  output_dir = "Documentation",
  envir = new.env(), encoding = "UTF-8"
)
# render(
#   input = "README.Rmd",
#   output_format = "pdf_document",
#   output_file = paste0("LISEZ-MOI.pdf"),
#   output_dir = "Documentation",
#   envir = new.env(), encoding = "UTF-8"
# )


# Registre Versions -------------------------------------------------------
render(
  input = "NEWS.Rmd",
  output_format = "md_document",
  output_file = "NEWS.md",
  output_dir = getwd(),
  envir = new.env(), encoding = "UTF-8"
)
render(
  input = "NEWS.Rmd",
  output_format = "html_document",
  output_file = "inesss-REGISTRE-VERSION.html",
  output_dir = "Documentation",
  envir = new.env(), encoding = "UTF-8"
)
# render(
#   input = "NEWS.Rmd",
#   output_format = "pdf_document",
#   output_file = "inesss-REGISTRE-VERSION.pdf",
#   output_dir = "Documentation",
#   envir = new.env(), encoding = "UTF-8"
# )

