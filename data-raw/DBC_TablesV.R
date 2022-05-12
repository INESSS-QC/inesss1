library(data.table)
library(usethis)
library(odbc)
library(inesss)

conn <- SQL_connexion(user, pwd)

DBC_TablesV <- as.data.table(dbGetQuery(conn, statement = paste0(
  "select	DataBaseName as NOM_BD,\n",
  "		    TableName as NOM_TABLE,\n",
  "       TableKind as TYPE_TABLE\n",
  "from   DBC.TablesV;"
)))
setkey(DBC_TablesV)

# Fermer la connexion
conn <- odbc::dbDisconnect(conn)

# Indiquer la date de mise à jour
attr(DBC_TablesV, "MaJ") <- Sys.Date()

use_data(DBC_TablesV, overwrite = TRUE)
rm(DBC_TablesV)
