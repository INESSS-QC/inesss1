library(inesss)
library(rmarkdown)
library(stringr)
library(writexl)
conn <- SQL_connexion(askpass::askpass("User"), askpass::askpass("Password"))


# Build Manual ------------------------------------------------------------
devtools::build_manual(path = "Documentation")


# Build Vignettes ---------------------------------------------------------
devtools::build_vignettes()


# Vignettes ---------------------------------------------------------------
# for (file in list.files("vignettes")) {
#   render(
#     input = paste0("vignettes/",file),
#     output_dir = "Documentation/Vignettes"
#   )
# }


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


# BD_descriptif -----------------------------------------------------------

files <- c(
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM",
  "V_DEM_PAIMT_MED_CM",
  "V_DENOM_COMNE_MED",
  "V_DES_COD",
  "V_PRODU_MED"
)
for (f in files) {
  write_xlsx(get(f), paste0("Documentation/BD_descriptif/",f,".xlsx"))
}

