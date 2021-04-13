library(usethis)
library(data.table)
library(readxl)


# CIM9 --------------------------------------------------------------------

CIM9 <- as.data.table(read_xlsx(
  "inst/extdata/CIM9_RAMQ.xlsx",
  col_types = "text"
))
colnames(CIM9) <- c("CODE", "DIAGNOSTIC")
setkey(CIM9)


# CIM10 -------------------------------------------------------------------

CIM10 <- as.data.table(read_xlsx(
  "inst/extdata/CIM10_RAMQ.xlsx",
  col_types = "text"
))
colnames(CIM10) <- c("CODE", "DIAGNOSTIC")
setkey(CIM10)


# CIM_correspond ----------------------------------------------------------

CIM_correspond <- as.data.table(read_xlsx(
  "inst/extdata/CIM_Tableau_de_correspondance.xlsx",
  col_types = "text",
  skip = 1
))
colnames(CIM_correspond) <- c("CIM9", "CIM9_DESC", "CIM10", "CIM10_DESC")
setorder(CIM_correspond, CIM9, CIM10,
         na.last = TRUE)


use_data(CIM9,
         CIM10,
         CIM_correspond,

         overwrite = TRUE)
