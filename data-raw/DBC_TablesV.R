library(data.table)
library(usethis)
library(odbc)
library(inesss)

# conn <- SQL_connexion(askpass::askpass("User"), askpass::askpass("Password"))

DBC_TablesV <- as.data.table(dbGetQuery(conn, statement = paste0(
  "select	DataBaseName as NOM_BD,\n",
  "		    TableName as NOM_TABLE,\n",
  "       TableKind as TYPE_TABLE\n",
  "from   DBC.TablesV;"
)))
setkey(DBC_TablesV)

attr(DBC_TablesV, "MaJ") <- Sys.Date()

use_data(DBC_TablesV, overwrite = TRUE)

rm(DBC_TablesV)
