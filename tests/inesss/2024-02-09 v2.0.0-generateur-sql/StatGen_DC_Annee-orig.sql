WITH dat AS (SELECT

DATE '2024-01-31' AS maxDate, -INTERVAL '1' YEAR + (maxDate + INTERVAL '1' DAY) AS minDate),

denom AS (SELECT DISTINCT(VAL_COD) AS dco FROM cod_denom_comne
WHERE VAL_COD IN (46088, 46342, 46454, 46488, 46580, 47017, 47226, 47284, 47300, 47338, 47483, 47548, 47736)),

cat AS (SELECT liste FROM (SELECT 3 AS liste FROM (SELECT 1 AS "DUMMY") AS "DUAL" UNION ALL SELECT 40 AS liste FROM (SELECT 1 AS "DUMMY") AS "DUAL" UNION ALL SELECT 41 AS liste FROM (SELECT 1 AS "DUMMY") AS "DUAL") AS tab
WHERE liste IN (3,40,41)), /* 3 = liste régulière 40 = patient d'exception 41 = médicament d'exception*/



/* Rien à manipuler à partir d'ici*/

temp AS (
SELECT

CASE WHEN SMED_AGE_BEN_AN_SERV BETWEEN 0 AND 17 THEN '0-17'
	ELSE '18+'
	END AS groupe_age,

CASE WHEN (EXTRACT(MONTH FROM SMED_DAT_SERV)) >= (EXTRACT(MONTH FROM dat.minDate)) THEN SMED_AN_CIVIL_DAT+1
	ELSE SMED_AN_CIVIL_DAT
	END AS periode_ref,
									
SMED_COD_CATG_LISTE_MED,									
	SMED_COD_DENOM_COMNE,								
	SMED_COD_TENR_MED,
	COUNT(DISTINCT SMED_NO_INDIV_BEN_BANLS) AS BEN,	
	SUM (SMED_QTE_MED) AS QTEMED,								
	SUM(SMED_MNT_AUTOR_MED) AS COUT,							
	COUNT(*) AS Rx,								
	SUM (SMED_MNT_AUTOR_FRAIS_SERV) AS HONOR,								
	SUM (SMED_NBR_JR_DUREE_TRAIT) AS DT,								
	SUM (SMED_MNT_AUTOR_FRAIS_SERV + SMED_MNT_AUTOR_MED) AS COUTTOT								
									
FROM  V_DEM_PAIMT_MED_CM, dat, denom, cat						
									
WHERE  	SMED_DAT_SERV BETWEEN dat.minDate AND dat.maxDate								
        AND SMED_COD_DENOM_COMNE IN denom.dco									
		AND (SMED_COD_SERV_1 NOT = '1' OR SMED_COD_SERV_1 IS NULL)							
		AND SMED_NBR_JR_DUREE_TRAIT > 0
		AND SMED_COD_STA_DECIS NOT IN ('REF','ANN')
		AND SMED_COD_CATG_LISTE_MED IN cat.liste		
									
									
GROUP BY 1,2,3,4,5),

ben AS (
SELECT

CASE WHEN SMED_AGE_BEN_AN_SERV BETWEEN 0 AND 17 THEN '0-17'
	ELSE '18+'
	END AS groupe_age,

CASE WHEN (EXTRACT(MONTH FROM SMED_DAT_SERV)) >= (EXTRACT(MONTH FROM dat.minDate)) THEN SMED_AN_CIVIL_DAT+1
	ELSE SMED_AN_CIVIL_DAT
	END AS periode_ref,
	
COUNT(DISTINCT SMED_NO_INDIV_BEN_BANLS) AS BEN

FROM V_DEM_PAIMT_MED_CM, dat, denom, cat

WHERE	SMED_DAT_SERV BETWEEN dat.minDate AND dat.maxDate								
		AND SMED_COD_DENOM_COMNE IN denom.dco									
		AND (SMED_COD_SERV_1 NOT = '1' OR SMED_COD_SERV_1 IS NULL)							
		AND  SMED_NBR_JR_DUREE_TRAIT > 0							
		AND SMED_COD_STA_DECIS NOT IN ('REF','ANN')
		AND SMED_COD_CATG_LISTE_MED IN cat.liste
		
GROUP BY 1,2),
							
nom AS (SELECT 									
									
	temp.*,								
	deco.DES AS PRODUIT,								
	t.DES AS TENEUR,
	ben.BEN AS ben_unique_annee
									
FROM temp									
LEFT JOIN cod_denom_comne AS deco									
ON temp.SMED_COD_DENOM_COMNE = deco.VAL_COD									
LEFT JOIN cod_tenr_med AS t									
ON temp.SMED_COD_TENR_MED = t.VAL_COD
LEFT JOIN ben
ON temp.periode_ref = ben.periode_ref AND temp.groupe_age = ben.groupe_age)						

								
SELECT 									
									
	groupe_age,
	
	CASE WHEN (EXTRACT(MONTH FROM dat.minDate)) = 1 THEN CONCAT('JAN ',TRIM(periode_ref-1),' - ', 'DEC ',TRIM(periode_ref-1)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 2 THEN CONCAT('FEV ',TRIM(periode_ref-1),' - ', 'JAN ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 3 THEN CONCAT('MAR ',TRIM(periode_ref-1),' - ', 'FEV ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 4 THEN CONCAT('AVR ',TRIM(periode_ref-1),' - ', 'MAR ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 5 THEN CONCAT('MAI ',TRIM(periode_ref-1),' - ', 'AVR ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 6 THEN CONCAT('JUN ',TRIM(periode_ref-1),' - ', 'MAI ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 7 THEN CONCAT('JUL ',TRIM(periode_ref-1),' - ', 'JUN ',TRIM(periode_ref))								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 8 THEN CONCAT('AOU ',TRIM(periode_ref-1),' - ', 'JUL ',TRIM(periode_ref))								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 9 THEN CONCAT('SEP ',TRIM(periode_ref-1),' - ', 'AOU ',TRIM(periode_ref))								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 10 THEN CONCAT('OCT ',TRIM(periode_ref-1),' - ', 'SEP ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 11 THEN CONCAT('NOV ',TRIM(periode_ref-1),' - ', 'OCT ',TRIM(periode_ref)) 								
	WHEN (EXTRACT(MONTH FROM dat.minDate)) = 12 THEN CONCAT('DEC ',TRIM(periode_ref-1),' - ', 'NOV ',TRIM(periode_ref)) 								
	END AS annee,
	
	ben_unique_annee,
	
	CASE WHEN SMED_COD_CATG_LISTE_MED = 41 THEN 'Medicament exception'								
	WHEN SMED_COD_CATG_LISTE_MED = 40 THEN 'Patient exception'								
	WHEN SMED_COD_CATG_LISTE_MED = 3 THEN 'Liste reguliere'								
	ELSE SMED_COD_CATG_LISTE_MED END AS LISTE,	
	
	PRODUIT,								
	TENEUR,
	BEN,
	DT,								
	Rx,								
	QTEMED,								
	COUT,								
	HONOR,								
	COUTTOT
	
FROM nom, dat
ORDER BY 1,2,3,4;