library(inesss)
library(askpass)

package_wd <- getwd()  # project wd
setwd(file_directory())  # répertoire du fichier = wd

files <- list.files()
files <- files[files != "0 - create all.R"]

conn <- sql_connexion(dsn = "PEI_PRD", uid = askpass("Identifiant"),
                      pwd = askpass("Mot de passe"), encoding = "latin1")

t1 <- Sys.time()
for (f in files) {
  source(f, local = TRUE)
}
t2 <- Sys.time(); difftime(t2, t1)
    # 2020-10-29 : 5.85 min

setwd(package_wd)
