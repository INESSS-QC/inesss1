library(inesss)
library(rmarkdown)
library(stringr)
conn <- SQL_connexion(askpass::askpass("User"), askpass::askpass("Password"))


# Accents dans les .Rd ----------------------------------------------------
### À appliquer après avoir documenté le package devtools::document

files <- list.files("man")  # fichiers à analyser
for (file in files) {
  txt <- readLines(paste0("man/",file))  # chaque ligne du fichier est un élément du vecteur
  err <- FALSE
  for (i in 1:length(txt)) {
    if (str_detect(txt[[i]], "é")) {  # sétection des "é"
      txt[[i]] <- str_replace_all(txt[[i]], "Ã©", "é")
      if (!err) {
        err <- TRUE  # indique qu'il y a eu au moins une erreur
      }
    }
    if (file == "RLS_convert.Rd" && txt[[i]] == "\\describe {") {
      txt[[i]] <- "\\describe{"
      err <- TRUE
    }
  }
  if (err) {
    writeLines(txt, paste0("man/",file))  # réécrire le fichier complet
  }
}

# Build Manual ------------------------------------------------------------
# ATTENTION : INSTALLER LE PACKAGE AVANT DE FAIRE CE CODE
devtools::build_manual(path = "Documentation")
file.copy(paste0("../inesss_",as.character(packageVersion("inesss")),".pdf"),
          paste0("Documentation/inesss_",as.character(packageVersion("inesss")),".pdf"),
          overwrite = TRUE)
file.remove(paste0("../inesss_",as.character(packageVersion("inesss")),".pdf"))




# README ------------------------------------------------------------------
options(encoding = "native.enc")
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
options(encoding = "UTF-8")

# R-Rtools-installation ---------------------------------------------------
options(encoding = "native.enc")
render(
  input = "Documentation/source/R-Rtools-installation.Rmd",
  output_file = paste0("R_RTOOLS_INSTALLATION_",Sys.Date(),".pdf"),
  output_dir = "Documentation",
  envir = new.env()
)
options(encoding = "UTF-8")

# Registre Versions -------------------------------------------------------
options(encoding = "native.enc")
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
options(encoding = "UTF-8")

# Formulaire --------------------------------------------------------------
options(encoding = "native.enc")
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
options(encoding = "UTF-8")

# ----------------------------------------------------------------------- #



