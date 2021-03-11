library(usethis)


# fct_values --------------------------------------------------------------
### Valeurs possible des arguments des fonctions

fct_values <- list(

  query_naif_switch1 = list(
    type_rx = c("DENOM", "DIN"),
    grouper_par = c("Codes"),
    code_serv_filtre = c("Exclusion", "Inclusion"),
    code_list_filtre = c("Exclusion", "Inclusion")
  ),

  query_stat_gen1 = list(
    type_Rx = c("DENOM", "DIN"),
    group_by = c("Codes", "Teneur", "Format"),
    code_serv_filtre = c("Exclusion", "Inclusion"),
    code_list_filtre = c("Exclusion", "Inclusion")
  )

)

use_data(fct_values,

         internal = TRUE, overwrite = TRUE)
