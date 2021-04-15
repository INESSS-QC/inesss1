library(inesss)
library(askpass)

conn <- SQL_connexion(askpass("User"))  # connexion teradata

files <- c(
  # "CIM",
  # "Comorbidity_Dx.R",
  # "ComorbidityWeights.R",
  # "DBC_TablesV.R",
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM.R",
  # "internal_datas.R",
  # "Obstetrics_Dx.R",
  # "Pop_QC.R",
  # "RLS_convert.R",
  # "RLS_list.R",
  "V_DEM_PAIMT_MED_CM.R",
  "V_DENOM_COMNE_MED.R",
  "V_DES_COD.R",
  "V_PRODU_MED.R"
)
files <- paste0("data-raw/", files)

t1 <- Sys.time()
for (f in files) {
  source(f, local = TRUE, encoding = "UTF-8")
}
t2 <- Sys.time(); difftime(t2, t1)
