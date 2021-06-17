library(Rd2md)
library(rmarkdown)

files <- paste0("man/", c(
  "SQL_comorbidity.Rd"
))

for (file in files) {
  ele <- parseRd(tools::parse_Rd(file))
  render(
    input = "Documentation/PDFs_unique/PDFs_unique_model.Rmd",
    params = list(name = ele$name)
  )
}
