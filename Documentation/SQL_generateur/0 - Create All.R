library(rmarkdown)

file.create("Documentation/SQL_generateur/SQL_statgen1.Rmd", overwrite = TRUE)
txt <- "---
title: 'Test'
author: 'Guillaume Boucher'
date: '2024-02-24'
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
sg1_fcts <- attr(statgen1(), 'internal_fcts')
group_age <- sg1_fcts$with.temp.select.groupe_age
```

# Requêtes - Exemples

## DENOM
```{r, eval=FALSE}
cat(statgen1(
  fin = '2023-12-31',
  typeRx = 'DENOM',
  codesRx = c(46088, 46342, 46454, 46488, 46580, 47017, 47226, 47284, 47300, 47338, 47483, 47548, 47736),
  catg_liste_med = c(3, 40, 41),
  code_serv = 1, code_serv_filtre = 'Exclusion',
  grp_age = 'Mineur-Majeur'
))
```
```{sql, echo=TRUE, eval=FALSE}
with DAT as (
    select
        date '2023-12-31' as MAX_DATE,
        -interval '12' month + (MAX_DATE + interval '1' day) as MIN_DATE
),
DENOM as (
    select distinct(VAL_COD) as COD_PROD
    from COD_DENOM_COMNE
    where VAL_COD in (46088, 46342, 46454, 46488, 46580, 47017, 47226, 47284, 47300, 47338, 47483, 47548, 47736)
),
CATG as (
    select LISTE
    from    (   select 3 as LISTE from (select 1 as T) as T union
                select 40 as LISTE from (select 1 as T) as T union
                select 41 as LISTE from (select 1 as T) as T
            ) as T
),
TEMP as (
    select
        case when SMED_AGE_BEN_AN_SERV between 0 and 17 then '0-17'
             else '18+'
             end as GROUPE_AGE,
        case when month(SMED_DAT_SERV) >= month(DAT.MIN_DATE) then SMED_AN_CIVIL_DAT + 1
             else SMED_AN_CIVIL_DAT
             end as PERIODE_REF,
        year(SMED_DAT_SERV) as ANNEE_CIVILE,
        month(SMED_DAT_SERV) as MOIS,
        SMED_COD_CATG_LISTE_MED as COD_CATG_LISTE,
        SMED_COD_DENOM_COMNE as COD_PRODUIT,
        SMED_COD_TENR_MED as COD_TENR,
        count(distinct SMED_NO_INDIV_BEN_BANLS) as BEN,
        sum(SMED_QTE_MED) as QTE_MED,
        sum(SMED_MNT_AUTOR_MED) as COUTS,
        count(*) as RX,
        sum(SMED_MNT_AUTOR_FRAIS_SERV) as HONORAIRE,
        sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TRAIT,
        sum(SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) as COUTS_TOT
    from V_DEM_PAIMT_MED_CM, DAT, PRODUIT, CATG
    where SMED_DAT_SERV between DAT.MIN_DATE and DAT.MAX_DATE
        and SMED_COD_CATG_LISTE_MED in CATG.LISTE
        and SMED_COD_DENOM_COMNE in PRODUIT.COD_PROD
        and (SMED_COD_SERV_1 not in ('1') or SMED_COD_SERV_1 is null)
        and SMED_NBR_JR_DUREE_TRAIT > 0
        and SMED_COD_STA_DECIS not in ('ANN', 'REF')
    group by GROUPE_AGE, PERIODE_REF, ANNEE_CIVILE, MOIS, COD_CATG_LISTE, COD_PRODUIT, COD_TENR
),
BEN as (
    select
        case when SMED_AGE_BEN_AN_SERV between 0 and 17 then '0-17'
             else '18+'
             end as GROUPE_AGE,
        case when month(SMED_DAT_SERV) >= month(DAT.MIN_DATE) then SMED_AN_CIVIL_DAT + 1
             else SMED_AN_CIVIL_DAT
             end as PERIODE_REF,
        year(SMED_DAT_SERV) as ANNEE_CIVILE,
        month(SMED_DAT_SERV) as MOIS,
        count(distinct SMED_NO_INDIV_BEN_BANLS) as BEN_UNIQUE_MOIS,
    from V_DEM_PAIMT_MED_CM, DAT, PRODUIT, CATG
    where SMED_DAT_SERV between DAT.MIN_DATE and DAT.MAX_DATE
        and SMED_COD_CATG_LISTE_MED in CATG.LISTE
        and SMED_COD_DENOM_COMNE in PRODUIT.COD_PROD
        and (SMED_COD_SERV_1 not in ('1') or SMED_COD_SERV_1 is null)
        and SMED_NBR_JR_DUREE_TRAIT > 0
        and SMED_COD_STA_DECIS not in ('ANN', 'REF')
    group by GROUPE_AGE, PERIODE_REF, ANNEE_CIVILE, MOIS
),
NOM as (
    select
        T.*,
        DC.DES as PRODUIT,
        TENR.DES as TENR,
        CATG.CODE_DES as CATG_LISTE,
        B.BEN_UNIQUE_MOIS
    from TEMP as T
        left join COD_DENOM_COMNE as DC on DC.VAL_COD = T.COD_PRODUIT
        left join COD_TENR_MED as TENR on TENR.VAL_COD = T.COD_TENR
        left join V_DES_COD as CATG on CATG.CODE_VAL_COD = T.COD_CATG_LISTE
                                    and CATG.CODE_NOM_COD = 'COD_CATG_LISTE_MED'
        left join BEN as B on B.GROUPE_AGE = T.GROUPE_AGE
                           and B.PERIODE_REF = T.PERIODE_REF
                           and B.ANNEE_CIVILE = T.ANNEE_CIVILE
                           and B.MOIS = T.MOIS
)
select
    GROUPE_AGE,
    case when month(MIN_DATE) = 1 then concat('JAN',trim(PERIODE_REF-1),'-','DEC',trim(PERIODE_REF-1))
         when month(MIN_Date) = 2 then concat('FEV',trim(PERIODE_REF-1),'-','JAN',trim(PERIODE_REF))
         when month(MIN_Date) = 3 then concat('MAR',trim(PERIODE_REF-1),'-','FEV',trim(PERIODE_REF))
         when month(MIN_Date) = 4 then concat('AVR',trim(PERIODE_REF-1),'-','MAR',trim(PERIODE_REF))
         when month(MIN_Date) = 5 then concat('MAI',trim(PERIODE_REF-1),'-','AVR',trim(PERIODE_REF))
         when month(MIN_Date) = 6 then concat('JUN',trim(PERIODE_REF-1),'-','MAI',trim(PERIODE_REF))
         when month(MIN_Date) = 7 then concat('JUI',trim(PERIODE_REF-1),'-','JUN',trim(PERIODE_REF))
         when month(MIN_Date) = 8 then concat('AOU',trim(PERIODE_REF-1),'-','JUI',trim(PERIODE_REF))
         when month(MIN_Date) = 9 then concat('SEP',trim(PERIODE_REF-1),'-','AOU',trim(PERIODE_REF))
         when month(MIN_Date) = 10 then concat('OCT',trim(PERIODE_REF-1),'-','SEP',trim(PERIODE_REF))
         when month(MIN_Date) = 11 then concat('NOV',trim(PERIODE_REF-1),'-','OCT',trim(PERIODE_REF))
         when month(MIN_Date) = 12 then concat('DEC',trim(PERIODE_REF-1),'-','NOV',trim(PERIODE_REF))
         end as ANNEE,
    ANNEE_CIVILE,
    MOIS,
    BEN_UNIQUE_MOIS,
    COD_CATG_LISTE,
    CATG_LISTE,
    COD_PRODUIT,
    PRODUIT,
    COD_TENR,
    TENR,
    BEN,
    QTE_MED,
    COUTS,
    RX,
    HONORAIRE,
    DUREE_TRAIT,
    COUTS_TOT,
from NOM, DAT
order by GROUPE_AGE, ANNEE, ANNEE_CIVILE, MOIS, COD_CATG_LISTE, COD_PRODUIT;
```

# Requêtes détaillées

```{r}
sg1 <- attr(statgen1(), 'internal_fcts')
```
"

sg1 <- attr(statgen1(), "internal_fcts")
for (i in names(sg1)) {
  txt <- paste0(txt, "## ", i)
  txt <- paste0(txt, "
```{r}
sg1$",i,"
```

")
}

writeLines(txt, "Documentation/SQL_generateur/SQL_statgen1.Rmd")
render("Documentation/SQL_generateur/SQL_statgen1.Rmd")
