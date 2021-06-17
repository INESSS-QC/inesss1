library(Rd2md)
library(rmarkdown)

files <- paste0("man/", c(
  "SQL_comorbidity.Rd"
))

for (file in files) {
  ele <- parseRd(tools::parse_Rd(file))
  render(
    input = "Documentation/PDFs_unique/PDFs_unique_model.Rmd",
    output_file = paste0(ele$name,".pdf"),
    output_dir = "Documentation/PDFs_unique",
    params = list(name = ele$name)
  )
}
