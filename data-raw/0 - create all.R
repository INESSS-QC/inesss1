library(inesss)
library(askpass)

conn <- sql_connexion(askpass("User"))  # connexion teradata

package_wd <- getwd()  # project wd
setwd(file_directory())  # répertoire du fichier = wd

files <- list.files()
files <- files[
  !files %in% c("0 - create all.R",
                "Pop_QC.R",
                "RLS_convert.R", "RLS_list.R")
]

t1 <- Sys.time()
for (f in files) {
  source(f, local = TRUE)
}
t2 <- Sys.time(); difftime(t2, t1)
    # 2020-10-29 : 5.85 min

setwd(package_wd)
