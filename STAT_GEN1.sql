/* #### STATISTIQUES GÉNÉRALES #### */
with RX_PAY as (
	/* Sélectionner informations nécessaires */
	select
		SMED_AN_CIVIL_DAT as ANNEE, -- (1); facultatif
		extract(month from SMED_DAT_SERV) as MOIS, -- (2); facultatif
		SMED_NO_INDIV_BEN_BANLS as ID, -- no d'identification de l'individu/bénéficiaire
		SMED_AGE_BEN_AN_SERV as AGE_SERV, -- (3)
		SMED_COD_DENOM_COMNE as DENOM,
		SMED_MNT_AUTOR_MED as MNT_MED,
		SMED_MNT_AUTOR_FRAIS_SERV as MNT_SERV,
		SMED_QTE_MED as QTE_MED,
		SMED_NBR_JR_DUREE_TRAIT as DUREE_TX
	from
		V_DEM_PAIMT_MED_CM
	where
		SMED_DAT_SERV between '2018-01-01' and '2018-01-31'
		and SMED_COD_DENOM_COMNE in ('00039', '47092', '47135')
		and (SMED_COD_SERV_1 not in ('1', 'AD') or SMED_COD_SERV_1 is null)
), AGE_RX_PAY as (
	/*
		Utiliser cette section seulement si on cherche l'âge d'un ID à une date précise.
		
	*/
	select
		R.*,
		(cast(to_date('2018-01-01') as int) - cast(F.BENF_DAT_NAISS as int)) / 10000 as AGE
	from
		RX_PAY as R left join V_FICH_ID_BEN_CM as F
			on R.ID = F.BENF_NO_INDIV_BEN_BANLS
)
select * from AGE_RX_PAY;

/* #### NOTES #### */

/*(1)
	Année où a eu lieu le service.
	Utile si la période d'étude se déroule sur plusieurs années et qu'on désire grouper par année
	Revient à faire :
*/
	extract(year from SMED_DAT_SERV) as ANNEE
	
/* (2)
	Mois où a eu lieu le service
*/

/* (3)
	SMED_AGE_BEN_AN_SERV : Age du bénéficiaire en années à la date de service.
	***Attention*** un individu pourrait être inclus dans deux classes d'âge 
*/

