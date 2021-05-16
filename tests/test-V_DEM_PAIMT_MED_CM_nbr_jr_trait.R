library(inesss)
library(writexl)

conn <- SQL_connexion("ms045")
t1 <- Sys.time()
dt_stats <- SQL_stats_SMED_NBR_JR_DUREE_TRAIT(
  conn = conn, debut = "2020-01-01", fin = "2020-12-31",
  by_code_serv = TRUE, include_dureeTx_0 = FALSE
)
t2 <- Sys.time()

write_xlsx(dt_stats, "tests/stats_SMED_NBR_JR_DUREE_TRAIT.xlsx")
