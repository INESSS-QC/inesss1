library(inesss)
library(rmarkdown)
library(stringr)
library(writexl)
# conn <- SQL_connexion()


# Build Manual ------------------------------------------------------------
devtools::build_manual(path = "Documentation")


# Installation - Docu -------------------------------------------------------------------------

# DER - Installation librairie R
render(
  input = "Documentation/source/Install_lib_DER.Rmd",
  output_file = "DER - Installation librairie R.pdf",
  output_dir = "Documentation/Installations",
  encoding = "UTF-8", envir = new.env()
)


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



# PDFs_unique ---------------------------------------------------------------------------------

# Astuce
help("chunk_vec", "inesss", help_type = "pdf")
help("confirm_nDx", "inesss", help_type = "pdf")
help("date_ymd", "inesss", help_type = "pdf")
help("file_directory", "inesss", help_type = "pdf")
help("install_RDCOMClient", "inesss", help_type = "pdf")
help("outlook_mail", "inesss", help_type = "pdf")
help("replace_NA_in_dt", "inesss", help_type = "pdf")
help("RLS_tab_convert", "inesss", help_type = "pdf")
help("rmNA", "inesss", help_type = "pdf")
help("SQL_connexion", "inesss", help_type = "pdf")
help("sunique", "inesss", help_type = "pdf")
for (file in paste0(c(
  "chunk_vec",
  "confirm_nDx",
  "date_ymd",
  "file_directory",
  "install_RDCOMClient",
  "outlook_mail",
  "replace_NA_in_dt",
  "RLS_tab_convert",
  "rmNA",
  "SQL_connexion",
  "sunique"
), ".pdf")) {
  file.copy(file, paste0("Documentation/PDFs_unique/Astuce/",file), overwrite = TRUE)
  file.remove(file)
}
# Domaine de valeur
help("I_APME_DEM_AUTOR_CRITR_ETEN_CM", "inesss", help_type = "pdf")
help("V_DEM_PAIMT_MED_CM", "inesss", help_type = "pdf")
help("V_DENOM_COMNE_MED", "inesss", help_type = "pdf")
help("V_DES_COD", "inesss", help_type = "pdf")
help("V_PRODU_MED", "inesss", help_type = "pdf")
for (file in paste0(c(
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM",
  "V_DEM_PAIMT_MED_CM",
  "V_DENOM_COMNE_MED",
  "V_DES_COD",
  "V_PRODU_MED"
), ".pdf")) {
  file.copy(file, paste0("Documentation/PDFs_unique/Domaine de Valeur/",file), overwrite = TRUE)
  file.remove(file)
}
# Requête Complexe
help("SQL_comorbidity", "inesss", help_type = "pdf")
help("SQL_diagn", "inesss", help_type = "pdf")
help("SQL_reperage_cond_med", "inesss", help_type = "pdf")
for (file in paste0(c(
  "SQL_comorbidity",
  "SQL_diagn",
  "SQL_reperage_cond_med"
), ".pdf")) {
  file.copy(file, paste0("Documentation/PDFs_unique/Requete Complexe/",file), overwrite = TRUE)
  file.remove(file)
}
# Table ou Liste
help("Charlson_Dx_CCI_INSPQ18", "inesss", help_type = "pdf")
help("Charlson_Dx_UManitoba16", "inesss", help_type = "pdf")
help("CIM10", "inesss", help_type = "pdf")
help("CIM9", "inesss", help_type = "pdf")
help("CIM_correspond", "inesss", help_type = "pdf")
help("Combine_Dx_CCI_INSPQ18", "inesss", help_type = "pdf")
help("ComorbidityWeights", "inesss", help_type = "pdf")
help("Elixhauser_Dx_CCI_INSPQ18", "inesss", help_type = "pdf")
help("Obstetrics_Dx", "inesss", help_type = "pdf")
help("RLS_tab_convert", "inesss", help_type = "pdf")
for (file in paste0(c(
  "Charlson_Dx_CCI_INSPQ18",
  "Charlson_Dx_UManitoba16",
  "CIM10",
  "CIM9",
  "CIM_correspond",
  "Combine_Dx_CCI_INSPQ18",
  "ComorbidityWeights",
  "Elixhauser_Dx_CCI_INSPQ18",
  "Obstetrics_Dx",
  "RLS_tab_convert"
), ".pdf")) {
  file.copy(file, paste0("Documentation/PDFs_unique/Table ou Liste/",file), overwrite = TRUE)
  file.remove(file)
}
