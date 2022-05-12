library(data.table)
conn <- SQL_connexion()

# Documentation - Exemples ------------------------------------------------

SQL_RCM2 <- SQL_reperage_cond_med(
  conn = conn,
  debut = "2019-01-01", fin = "2021-12-31",
  Dx_table = list(
    diabete = list(
      CIM9 = c("2500%", "2501%", "2502%"),
      CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
                "E131%", "E139%", "E140%", "E141%", "E149%")
    ),
    diabete_complication = list(
      CIM9 = paste0(2503:2509, "%"),
      CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
    )
  ),
  CIM = c("CIM9", "CIM10"),
  by_Dx = TRUE,
  date_dx_var = "admis",
  n1 = 30, n2 = 730,
  keep_all = TRUE, verbose = TRUE
)


SQL_RCM2 <- SQL_reperage_cond_med(
  conn = conn,
  debut = "2019-01-01", fin = "2021-12-31",
  Dx_table = list(
    diabete = list(
      CIM9 = c("2500%", "2501%", "2502%"),
      CIM10 = c("E100%", "E101%", "E109%", "E110%", "E111%", "E119%", "E130%",
                "E131%", "E139%", "E140%", "E141%", "E149%")
    ),
    diabete_complication = list(
      CIM9 = paste0(2503:2509, "%"),
      CIM10 = paste0("E", c(102:108, 112:118, 132:138, 142:148), "%")
    )
  ),
  CIM = c("CIM9", "CIM10"),
  by_Dx = TRUE,
  date_dx_var = "admis",
  n1 = 30, n2 = 730,
  keep_all = TRUE, verbose = TRUE
)

RCM2 <- data.table(

)

save(RCM1, RCM2,
     file = "Documentation/source/datas/SQL_reperage_cond_med.rda")

