select
    sum(SMED_MNT_AUTOR_MED) as MNT_MED, -- montant autorisé pour le médicament
    sum(SMED_MNT_AUTOR_FRAIS_SERV) as MNT_SERV, -- montant autorisé pour le service
    sum(SMED_MNT_AUTOR_MED + SMED_MNT_AUTOR_FRAIS_SERV) as MNT_TOT, -- somme des montants précédents
    count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE, -- nombre de bénéficiaires uniques
    count(*) as NBRE_RX, -- nombre de services
    sum(SMED_QTE_MED) as QTE_MED, -- quantité de médicaments
    sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX -- durée des traitements
from
    V_DEM_PAIMT_MED_CM -- demandes de paiement de médicaments
where
    SMED_DAT_SERV between 'debut' and 'fin' -- début et fin de la période d'étude
    and VariableCodeAnalyse in (...) -- DENOM, DIN, AHFS...
    and SMED_NBR_JR_DUREE_TRAIT > 0; -- analyse des services qui ont au moins une journée de traitement