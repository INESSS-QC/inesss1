library(usethis)


# fct_values --------------------------------------------------------------
### Valeurs possible des arguments des fonctions

fct_values <- list(

  ### STAT_GEN1
  query_stat_gen1 = list(
    type_Rx = c("DENOM", "DIN"),
    group_by = c("Codes", "Teneur", "Format"),
    code_serv_filtre = c("Exclusion", "Inclusion"),
    code_list_filtre = c("Exclusion", "Inclusion")
  )

)

use_data(fct_values,

         internal = TRUE, overwrite = TRUE)
