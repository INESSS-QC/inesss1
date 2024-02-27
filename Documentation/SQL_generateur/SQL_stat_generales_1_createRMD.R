library(rmarkdown)
library(inesss)
sg1 <- attr(stat_generales_1(), "internal_fcts")

# Header & Options ####
header_options <-
"---
title: 'Test'
date: '`r Sys.Date()`'
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    number_sections: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, eval = TRUE, message = FALSE, comment = '')
library(inesss)
library(stringr)
library(knitr)
sg1_fcts <- attr(stat_generales_1(), 'internal_fcts')
group_age <- sg1_fcts$with.temp.select.groupe_age
```

"

# Requêtes Complètes - Exemples ####
requete.denom <- paste0(
"# Requêtes Complètes - Exemples

## DENOM
```{sql, echo=TRUE, eval=FALSE}
",stat_generales_1(
  fin = '2023-12-31',
  typeRx = 'DENOM',
  codesRx = c(46088, 46342, 46454, 46488, 46580, 47017, 47226, 47284, 47300, 47338, 47483, 47548, 47736),
  catg_liste_med = c(3, 40, 41),
  code_serv = 1, code_serv_filtre = 'Exclusion',
  grp_age = 'Mineur-Majeur'
),"
```

")

# Détails ####
details <- "# Détails

"
## Groupe Âge ####
details.groupe_age <- paste0(
"## Groupe Âge

"
)
# Mineur-Majeur
details.groupe_age.mineur_majeur <-
"### Mineur-Majeur
```{r}
printcode <- group_age('Mineur-Majeur')
printcode <- str_replace(printcode, '        case','case')
printcode <- str_replace(printcode, '             else', '     else')
printcode <- str_replace(printcode, '             end', '     end')
cat(printcode)
```

"
# 5 ans
details.groupe_age.5ans <-
"### 5 ans
```{r}
printcode <- group_age(5)
printcode <- str_replace(printcode, '        case','case')
printcode <- str_replace_all(printcode, '             when', '     when')
printcode <- str_replace(printcode, '             else', '     else')
printcode <- str_replace(printcode, '             end', '     end')
cat(printcode)
```

"
# 10 ans
details.groupe_age.10ans <-
"### 10 ans
```{r}
printcode <- group_age(10)
printcode <- str_replace(printcode, '        case','case')
printcode <- str_replace_all(printcode, '             when', '     when')
printcode <- str_replace(printcode, '             else', '     else')
printcode <- str_replace(printcode, '             end', '     end')
cat(printcode)
```

"

# Requêtes détaillées ####
requete_detail <- "# Requêtes détaillées

"
for (i in names(sg1)) {
  requete_detail <- paste0(requete_detail,
"## ",i,"
```{r}
sg1[['",i,"']]
```

"
  )
}

write2file <- paste0(
  header_options,
  requete.denom,
  details,
  details.groupe_age,
  details.groupe_age.mineur_majeur,
  details.groupe_age.5ans,
  details.groupe_age.10ans,
  requete_detail
)

file.create("Documentation/SQL_generateur/SQL_stat_generales_1.Rmd", overwrite = TRUE)
file.remove("TRUE")
writeLines(write2file, "Documentation/SQL_generateur/SQL_stat_generales_1.Rmd")
render("Documentation/SQL_generateur/SQL_stat_generales_1.Rmd")
