library(usethis)
library(readxl)
library(data.table)
library(lubridate)

# fct_values --------------------------------------------------------------
### Valeurs possible des arguments des fonctions

fct_values <- list(

  query_naif_switch1 = list(
    type_rx = c("DENOM", "DIN"),
    group_by = c("AHFS", "DENOM", "DIN", "CodeList", "CodeServ", "Teneur", "Format", "Age"),
    type_Rx_retro = c("AHFS", "DENOM", "DIN"),
    code_serv_filtre = c("Exclusion", "Inclusion"),
    code_list_filtre = c("Exclusion", "Inclusion")
  ),

  query_stat_gen1 = list(
    type_Rx = c("AHFS", "DENOM", "DIN"),
    group_by = c("AHFS", "DENOM", "DIN", "CodeList", "CodeServ", "Teneur", "Format", "Age"),
    code_serv_filtre = c("Exclusion", "Inclusion"),
    code_list_filtre = c("Exclusion", "Inclusion")
  )

)


# Vignettes ---------------------------------------------------------------

vignettes_datas <- list(
  # SQL_comorbidity
  SQL_comorbidity = list(
    `1-unique` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "1-unique"))
      dt[, DATE_INDEX := as_date(DATE_INDEX)]
    },
    `2-filtDx` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "2-filtDx"))
      dt[, DATE_DX := as_date(DATE_DX)]
    },
    `3.1-Obstetric` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "3.1-Obstetric"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX),
                 DATE_INDEX = as_date(DATE_INDEX))]
    },
    `3.2-Obstetric` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "3.2-Obstetric"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX))]
    },
    `4-priorSourc` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "4-priorSourc"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX),
                 DATE_INDEX = as_date(DATE_INDEX))]
    },
    `5-filtConfirm` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "5-filtConfirm"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX),
                 DATE_INDEX = as_date(DATE_INDEX))]
    },
    `6.1-conf` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "6.1-conf"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX),
                 DATE_INDEX = as_date(DATE_INDEX))]
    },
    `6.2-conf` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "6.2-conf"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX),
                 DATE_INDEX = as_date(DATE_INDEX))]
    },
    `6.3-conf` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "6.3-conf"))
      dt[, `:=` (DATE_DX = as_date(DATE_DX),
                 DATE_INDEX = as_date(DATE_INDEX))]
    },
    `7-poids` = as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "7-poids")),
    `8-nDx` = {
      dt <- as.data.table(read_xlsx("vignettes/SQL_comorbidity.xlsx", "8-nDx"))
      dt[, `:=` (DIAGN = as.character(DIAGN),
                 POIDS = as.integer(POIDS),
                 ID = as.integer(ID),
                 DATE_REP = as_date(DATE_REP),
                 SOURCE_REP = as.character(SOURCE_REP),
                 DATE_CONF = as_date(DATE_CONF),
                 SOURCE_CONF = as.character(SOURCE_CONF))]
    }
  )
)

use_data(fct_values,
         vignettes_datas,

         internal = TRUE, overwrite = TRUE)
