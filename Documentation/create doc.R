library(rmarkdown)

# Installation librairie ----------------------------------------------------------------------

# DER.inesss
render(
  input = "Documentation/source/DER_install_lib.Rmd",
  output_file = "DER - Installation librairie R.pdf",
  output_dir = "Documentation/Installations",
  encoding = "UTF-8", envir = new.env()
)
