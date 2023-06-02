library(usethis)
library(data.table)


# domaine_valeurs.fiches_techniques -------------------------------------------------------------


domaine_valeurs_fiche_technique <- list(

  ## I_APME_DEM_AUTOR_CRITR_ETEN_CM ####
  I_APME_DEM_AUTOR_CRITR_ETEN_CM = list(
    DES_COURT_INDCN_RECNU = list(
      MaJ = "2023-02-06",
      tab_desc = data.table(
        VARIABLE = c(
          "DENOM_DEM", "DIN_DEM", "DES_COURT_INDCN_RECNU",
          "DebPeriodeDesc", "FinPeriodeDesc"
        ),
        `VARIABLE RAMQ` = c(
          "APME_COD_DENOM_COMNE_DEM", "APME_COD_DIN_DEM", "NPME_DES_COURT_INDCN_RECNU",
          "-",
          "-"
        ),
        DESCRIPTION = c(
          "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
          "Ce code différencie chaque produit. Ce dernier pouvant varier selon la forme pharmaceutique, la marque de commerce, etc.",
          "Description courte complète de l'indication reconnue de PME. Note: Elle peut être dérivée de la fusion de description entre la description qui lui est directement reliée et celle de niveau supérieur (le cas échéant).",
          "Début d'une période continue où la combinaison existe. Format ANNÉE-MOIS.",
          "Fin d'une période continue où la combinaison existe. Format ANNÉE-MOIS."
        )
      )
    )
  ),

  ## V_CLA_AHF ####
  V_CLA_AHF = list(
    MaJ = "2023-02-06",
    tab_desc = data.table(
      VARIABLE = c(
        "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA",
        "NOM_AHFS", "NOM_ANGLAIS_AHFS"
      ),
      `VARIABLE RAMQ` = c(
        "NMED_COD_CLA_AHF", "NMED_COD_SCLA_AHF", "NMED_COD_SSCLA_AHF",
        "NMED_NOM_CLA_AHF", "NMED_NOM_ANGL_CLA_AHF"
      ),
      DESCRIPTION = c(
        "Code identifiant la classe de médicaments telle que déterminée par l'American Hospital Formulary Service (AHFS). Ce code correspond aux deux(2) premières positions de la classification AHFS qui en compte un maximum de six(6).",
        "Code de la sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
        "Code de la sous-sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
        "Nom d'un code de la classification de l'AHFS à un moment donné. Nom valide selon la période d'application.",
        "Nom en anglais d'une Classe, Sous-classe ou Sous-sous-classe AHFS."
      )
    )
  ),

  ## V_DEM_PAIMT_MED_CM ####
  V_DEM_PAIMT_MED_CM = list(
    MaJ = "2023-02-06",
    tab_desc = data.table(
      VARIABLE = c(
        "AHFS_CLA", "AHFS_SCLA", "AHFS_SSCLA", "AHFS_NOM_CLA",
        "DENOM", "NOM_DENOM",
        "DIN", "MARQ_COMRC",
        "FORME", "NOM_FORME",
        "TENEUR", "NOM_TENEUR",
        "COD_SERV", "COD_SERV_DESC", "COD_SERV_1", "COD_SERV_2", "COD_SERV_3",
        "COD_STA_DECIS", "COD_STA_DESC",
        "PremierePrescription", "DernierePrescription",
        "DebPeriodPrescripDem", "FinPeriodPrescripDem"
      ),
      `VARIABLE RAMQ` = c(
        "SMED_COD_CLA_AHF", "SMED_COD_SCLA_AHF", "SMED_COD_SSCLA_AHF", "NMED_NOM_CLA_AHF",
        "SMED_COD_DENOM_COMNE", "NMED_NOM_DENOM_COMNE",
        "SMED_COD_DIN", "NMED_NOM_MARQ_COMRC",
        "SMED_COD_FORME_MED", "NMED_NOM_FORME",
        "SMED_COD_TENR_MED", "NMED_NOM_TENR",
        "NMED_COD_SERV_MED", "NMED_DES_SERV_MED", "SMED_COD_SERV_1", "SMED_COD_SERV_2", "SMED_COD_SERV_3",
        "SMED_COD_STA_DECIS", "CODE_DES",
        "-", "-",
        "-", "-"
      ),
      DESCRIPTION = c(
        # AHFS_CLA - SMED_COD_CLA_AHF
        "Code identifiant la classe de médicaments telle que déterminée par l'American Hospital Formulary Service (AHFS). Ce code correspond aux deux(2) premières positions de la classification AHFS qui en compte un maximum de six(6).",
        # AHFS_SCLA - SMED_COD_SCLA_AHF
        "Code de la sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
        # AHFS_SSCLA - SMED_COD_SSCLA_AHF
        "Code de la sous-sous-classe de médicaments selon la codification de l'American Hospital Formulary Service (AHFS).",
        # AHFS_NOM_CLA - NMED_NOM_CLA_AHF
        "Nom d'un code de la classification de l'AHFS à un moment donné. Nom valide selon la période d'application.<br>Provient de la vue V_CLA_AHF.",
        # DENOM - SMED_COD_DENOM_COMNE
        "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
        # NOM_DENOM - NMED_NOM_DENOM_COMNE
        "Nom de la dénomination commune du médicament.<br>Provient de la vue V_DENOM_COMNE_MED.",
        # DIN - SMED_COD_DIN
        "Ce code différencie chaque produit. Ce dernier pouvant varier selon la forme pharmaceutique, la marque de commerce, etc.",
        # MARQ_COMRC - NMED_NOM_MARQ_COMRC
        "Cet élément représente le nom sous lequel est commercialisé un produit pharmaceutique. Il sert à désigner, plus précisément, un code DIN. Ce ne sont pas tous les produits qui ont une marque de commerce (fourniture, magistral, solvant....)<br>Provient de la vue V_PRODU_MED.",
        # FORME - SMED_COD_FORME_MED
        "Ce code permet d'identifier de façon unique chacune des formes pharmaceutiques que peuvent prendre les médicaments, que ce soit des médicaments assurés, d'exception ou de patients d'exception.",
        # NOM_FORME - NMED_NOM_FORME
        "Cette propriété est le nom au long d'une forme pharmaceutique. Le nom est inscrit en majuscules et minuscules accentuées.<br>Provient de la vue V_FORME_MED.",
        # TENEUR - SMED_COD_TENR_MED
        "Code de la teneur du médicament auquel fait référence le présent médicament (NO_SEQ_MED) dans une situation de regroupement.<br>Cette propriété sert à identifier les médicaments qui regroupent d'autres médicaments. Cela sert, entres autres, à regrouper les médicaments de longue durée avec ceux de courte durée, pour les besoins du PPB.",
        # NOM_TENEUR - NMED_NOM_TENR
        "Nom de la teneur du médicament.<br>Provient de la vue V_TENR_MED.",
        # COD_SERV - NMED_COD_SERV_MED
        "Les codes de service décrivent les services qui peuvent être facturés dans le cadre du système de Médicaments.<br>Provient de la vue V_PARAM_SERV_MED.",
        # COD_SERV_DESC - NMED_DES_SERV_MED
        "Description du service pour MED de CIP.<br>Provient de la vue V_PARAM_SERV_MED.",
        # COD_SERV_1 - SMED_COD_SERV_1
        "Les codes de services décrivent les services qui ont été fournis. Ces codes peuvent être complétés par des codes d’intervention ou d’exception. Une même réclamation peut contenir jusqu'à trois codes de services.",
        # COD_SERV_2 - SMED_COD_SERV_2
        "Les codes de services décrivent les services qui ont été fournis. Ces codes peuvent être complétés par des codes d’intervention ou d’exception. Une même réclamation peut contenir jusqu'à trois codes de services.",
        # COD_SERV_3 - SMED_COD_SERV_3
        "Les codes de services décrivent les services qui ont été fournis. Ces codes peuvent être complétés par des codes d’intervention ou d’exception. Une même réclamation peut contenir jusqu'à trois codes de services.",
        # COD_STA_DECIS - SMED_COD_STA_DECIS
        "Code de statut de décision.",
        # COD_STA_DESC - CODE_DES
        "Description d'un code.<br>Provient de la vue V_DES_COD.",
        # PremierePrescription
        "Première année que le code ou la combinaison de codes ont été prescrits (inscrits dans V_DEM_PAIMT_MED_CM).",
        # DernierePrescription
        "Dernière année que le code ou la combinaison de codes ont été prescrits (inscrits dans V_DEM_PAIMT_MED_CM).",
        # DebPeriodPrescriptDem
        "Année inscrite dans le sélecteur d'année : Période Prescription (Début).",
        # FinPeriodPrescriptDem
        "Année inscrite dans le sélecteur d'année : Période Prescription (Fin)."
      )
    )
  ),

  ## V_DENOM_COMNE_MED ####
  V_DENOM_COMNE_MED = list(
    MaJ = "2023-02-06",
    tab_desc = data.table(
      VARIABLE = c(
        "DENOM", "DATE_DEBUT", "DATE_FIN",
        "NOM_DENOM", "NOM_DENOM_SYNON",
        "NOM_DENOM_ANGLAIS", "NOM_DENOM_SYNON_ANGLAIS"
      ),
      `VARIABLE RAMQ` = c(
        "NMED_COD_DENOM_COMNE", "NMED_DD_DENOM_COMNE", "NMED_DF_DENOM_COMNE",
        "NMED_NOM_DENOM_COMNE", "NMED_NOM_DENOM_COMNE_SYNON",
        "NMED_NOM_ANGL_DENOM_COMNE", "NMED_NOM_ANGL_DENOM_SYNON"
      ),
      DESCRIPTION = c(
        "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
        "Date à laquelle cette dénomination commune est apparue pour la première fois sur la liste de médicaments. Cette date correspond à la date de parution d'une liste de médicaments. C'est à partir de cette date que le calcul du 15 ans pour le calcul du PPB, est effectué. Cette date est utile pour identifier les médicaments qui ne sont pas encore assujettis à la méthode de calcul du PPB et qui doivent y être assujettis à l'avenir.",
        "Date à laquelle une dénomination commune a cessé d'être utilisée.",
        "Nom de la dénomination commune du médicament.",
        "Nom dénomination commune synonyme",
        "Nom en anglais d'une dénomination commune. Ce nom est utilisé dans les publications des listes anglaises des médicaments.",
        "Nom synonyme en anglais d'une dénomination commune. Ce nom est utilisé dans les publications des listes anglaises des médicaments."
      )
    )
  ),

  ## V_PRODU_MED ####
  V_PRODU_MED = list(
    NOM_MARQ_COMRC = list(
      MaJ = "2023-02-06",
      tab_desc = data.table(
        VARIABLE = c(
          "DENOM", "DIN",
          "NOM_MARQ_COMRC",
          "DATE_DEBUT", "DATE_FIN"
        ),
        `VARIABLE RAMQ` = c(
          "NMED_COD_DENOM_COMNE", "NMED_COD_DIN", "NMED_NOM_MARQ_COMRC",
          "NMED_DD_PRODU_MED", "NMED_DF_PRODU_MED"
        ),
        DESCRIPTION = c(
          "Ce code identifie de façon unique toutes les dénominations communes qui existent dans les listes de médicaments.",
          "Ce code différencie chaque produit. Ce dernier pouvant varier selon la forme pharmaceutique, la marque de commerce, etc.",
          "Cet élément représente le nom sous lequel est commercialisé un produit pharmaceutique. Il sert à désigner, plus précisément, un code DIN. Ce ne sont pas tous les produits qui ont une marque de commerce (fourniture, magistral, solvant....)",
          "Date de début d'une occurrence de cette table. Cette date correspond à la date d'entrée en vigueur de la mise à jour liste (DAT-ENTRE_VIG_MAJ_LISTE) à laquelle est relié l'ajout ou la modification de cette occurrence.",
          "Date de fin d'une occurrence de cette table. Cette date correspond à la date d'entrée en vigueur de la mise à jour liste moins un jour (DAT-ENTRE_VIG_MAJ_LISTE - 1 jour) de l'occurrence suivante."
        )
      ),
      footnote = "Les périodes DÉBUT-FIN ayant la même combinaison DENOM-DIN qui se chevauchent dans le temps ont été combinées en une seule."
    )
  )

)






# SAVE ----------------------------------------------------------------------------------------

use_data(domaine_valeurs_fiche_technique, overwrite = TRUE)
