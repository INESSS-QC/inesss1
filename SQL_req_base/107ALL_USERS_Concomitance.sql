with RESINE_USERS as (
/* Établir les utilisateurs de résine et indiquer le nombre de traitements reçus */
	select
		SMED_NO_INDIV_BEN_BANLS as ID,
		count(*) as NBRE_RX
	from V_DEM_PAIMT_MED_CM
	where SMED_DAT_SERV between '2020-02-01' and '2021-01-31'
		and SMED_COD_DENOM_COMNE in ('07787', '44931')
		and (SMED_COD_SERV_1 <> '1' or SMED_COD_SERV_1 is null)
		and SMED_NBR_JR_DUREE_TRAIT > 0
	group by ID
	having NBRE_RX >= 4  -- seulement les individus ayant au moins 4 ordonnances/traitements
/*  | ID | NBRE_RX |
	================
	| 1  |		7  |
	| 2  |	   15  |
	| 3  |	    4  |
	|... | 	  ...  |
*/
),
RESINE_USERS4 as (
/* Indiquer le début et la fin de chaque période de traitement/ordonnance des <résineux!> */
	select
		SMED_NO_INDIV_BEN_BANLS as ID,
		SMED_DAT_SERV as DEBUT_RESINE,
		SMED_DAT_SERV + SMED_NBR_JR_DUREE_TRAIT - 1 as FIN_RESINE
	from V_DEM_PAIMT_MED_CM
	where SMED_DAT_SERV between '2020-02-01' and '2021-01-31'
		and SMED_COD_DENOM_COMNE in ('07787', '44931')
		and SMED_NO_INDIV_BEN_BANLS in (select ID from RESINE_USERS)  -- cohorte des <résineux!>
		and (SMED_COD_SERV_1 <> '1' or SMED_COD_SERV_1 is null)
		and SMED_NBR_JR_DUREE_TRAIT > 0
/*	| ID | DEBUT_RESINE | FIN_RESINE |
	| 1  | 2020-02-01   | 2020-02-15 |
	| 1  | 2020-02-10   | 2020-03-09 |
	| 2  | 2021-01-10   | 2021-01-31 |
	| ...|        ...   |        ... |
*/
),
ISRAA_USERS as (
/* Établir les patients qui ont pris au moins un ISRAA et 4 résines ou plus Rx
	Construire la même structure que RESINE_DAT_SERV -> Périodes*/
	select
		SMED_NO_INDIV_BEN_BANLS as ID,
		SMED_DAT_SERV as DEBUT_ISRAA,
		SMED_DAT_SERV + SMED_NBR_JR_DUREE_TRAIT - 1 as FIN_ISRAA
	from V_DEM_PAIMT_MED_CM
	where SMED_DAT_SERV between '2020-02-01' and '2021-01-31'
		and SMED_NO_INDIV_BEN_BANLS in (select distinct ID from RESINE_USERS4)  -- cohorte des <résineux!>
		and SMED_COD_CLA_AHF = '24' and SMED_COD_SCLA_AHF = '32' -- codes AHFS des ISRAA sont 24:32:xx
		and (SMED_COD_SERV_1 <> '1' or SMED_COD_SERV_1 is null)
		and SMED_NBR_JR_DUREE_TRAIT > 0
/* Même structure que RESINE_DAT_SERV */
),
RESINE_ISRAA_USERS as (
/* Établir les patients qui ont au moins un ISRAA qui chevauchent une RÉSINE */
	select
		R.ID,
		R.DEBUT_RESINE,
		R.FIN_RESINE,
		I.DEBUT_ISRAA,
		I.FIN_ISRAA
	from RESINE_USERS4 as R inner join ISRAA_USERS as I
		on R.ID = I.ID
	where I.DEBUT_ISRAA < R.FIN_RESINE
		and I.FIN_ISRAA > R.DEBUT_RESINE
/*	| ID | DEBUT_RESINE | FIN_RESINE | DEBUT_ISRAA | FIN_ISRAA |
	|  1 | 2020-01-10   | 2020-02-09 | 2020-01-10  | 2020-10-01|
	|  1 | 2020-05-05   | 2020-06-06 | 2020-04-01  | 2020-05-15|
	|  2 | 2020-08-23   | 2020-09-23 | 2020-09-01  | 2020-09-30|
	| ...|        ...   |        ... |        ...  |        ...|
*/
)
/* Statistiques pour les patients ayant consommé au moins 4 résines dont
	au moins une de ces consommations chevauche avec un ISRAA */
select
	SMED_COD_DIN as DIN,
	count(distinct SMED_NO_INDIV_BEN_BANLS) as COHORTE,
	count(*) as NBRE_RX,
	sum(SMED_QTE_MED) as QTE_MED,
	sum(SMED_MNT_AUTOR_MED) as MNT_MED,
	sum(SMED_NBR_JR_DUREE_TRAIT) as DUREE_TX
from V_DEM_PAIMT_MED_CM
where SMED_DAT_SERV between '2020-02-01' and '2021-01-31'
	and SMED_NO_INDIV_BEN_BANLS in (select distinct ID from RESINE_ISRAA_USERS) -- cohorte d'étude
	and SMED_COD_DENOM_COMNE in ('07787', '44931')
	and (SMED_COD_SERV_1 <> '1' or SMED_COD_SERV_1 is null)
	and SMED_NBR_JR_DUREE_TRAIT > 0
group by DIN
order by DIN;
