library(inesss)
library(askpass)

conn <- SQL_connexion()  # connexion teradata
send_mail <- TRUE
mail_to <- c(
  "guillaume.boucher@inesss.qc.ca",
  "françois-xavier.houde@inesss.qc.ca"
)

files <- c(
  # "CIM",
  # "Comorbidity_Dx.R",
  # "ComorbidityWeights.R",
  "DBC_TablesV.R",
  "I_APME_DEM_AUTOR_CRITR_ETEN_CM.R",
  # "internal_datas.R",
  # "Obstetrics_Dx.R",
  # "Pop_QC.R",
  # "RLS_list.R",
  # "RLS_tab_convert.R",
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
