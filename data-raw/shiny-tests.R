## code to prepare `DATASET` dataset goes here

library(data.table)

list_fcts <- list(
  Direction1 = c("fct1", "fct2", "fct3"),
  Direction2 = c("fct1", "fct4", "fct5"),
  Direction3 = c("fct8")
)

n = 1e6
data_ex1 <- data.table(
  id = 1:n,
  an = sample(2011:2020, n, T),
  type_var = sample(c("AHFS", "DIN", "DC"), n, T),
  code = sample(1:100, n, T),
  cout = sample(seq(1, 200, 0.1), n, T),
  honor = sample(seq(1, 200, 0.1), n, T)
)
data_ex1[, tot := cout + honor]


usethis::use_data(list_fcts, data_ex1, overwrite = TRUE)
