Fx_fct <- function(conn) {
  ### Définir une requête SQL

  statementSQL <- "

with

ALL_USER as (

    select

      SMED_NO_INDIV_BEN_BANLS as ID,

      Min(SMED_DAT_SERV) as DATE_INDEX

    from  Prod.V_DEM_PAIMT_MED_CM

    where YEAR(SMED_DAT_SERV) = %s and

          SMED_COD_DENOM_COMNE in ('47386') and

          SMED_NBR_JR_DUREE_TRAIT > 0

    group by 1),

OLD_USER as (

    select

      ALL_USER.ID

    from ALL_USER

    left join Prod.V_DEM_PAIMT_MED_CM as V on ALL_USER.ID = V.SMED_NO_INDIV_BEN_BANLS

    where V.SMED_DAT_SERV between (ALL_USER.DATE_INDEX - 365) and (ALL_USER.DATE_INDEX - 1) and

          V.SMED_COD_DENOM_COMNE in ('47386') and

          V.SMED_NBR_JR_DUREE_TRAIT > 0

    group by 1),

NaifSwitch as (

    select ALL_USER.ID from ALL_USER

    except

    select ID from OLD_USER)

select

  %s as anneeCohorte,

  Extract (year from smed_dat_serv) as annee,

  Count(distinct NaifSwitch.id) as nbIndiv,

  Count(*) as nbRx,

  Sum(smed_qte_med) as nbQte,

  Sum(smed_mnt_autor_med) as coutTotal,

  Sum(smed_nbr_jr_duree_trait) as dureeTotal

from Prod.v_dem_paimt_med_cm as VRx

inner join  NaifSwitch on VRx.SMED_NO_INDIV_BEN_BANLS = NaifSwitch.ID

where VRx.SMED_COD_DENOM_COMNE  in ('47386') and

      VRx.smed_cod_sta_decis = 'PAY' and

      Extract(YEAR FROM VRx.SMED_DAT_SERV) >= %s and

      (SMED_COD_SERV_1 not = '1' OR SMED_COD_SERV_1 is null) and

      VRx.smed_nbr_jr_duree_trait > 0

group by 1,2"

  ### Liste des années durant lesquelles nous voulons appliquer la requête

  listYear <- seq(2022, 2022, by=1)

  ### Créer un objet qui pourra accueillir les données extraites

  output_df <- NULL

  ### Boucle qui itère sur la liste des années

  for (currYear in listYear) {

    tmp_querry <- sprintf(gsub('[\r\t\n]',' ',statementSQL), currYear, currYear, currYear)

    tmp_df <- as.data.frame(dbGetQuery(conn = conn, statement = tmp_querry))

    output_df <- rbind(output_df, tmp_df)

  }

  return(output_df)

}

conn <- SQL_connexion()
library(microbenchmark)
microbenchmark(
  FX = Fx_fct(conn),
  GB = SQL_naif_switch1(
    conn = conn,
    debut = paste0(2022, "-01-01"),
    fin = paste0(2022, "-12-31"),
    type_Rx = "DENOM", codes = 47386,
    group_by = "DENOM", type_Rx_retro = NULL, rx_retrospect_a_exclure = NULL,
    njours_sans_conso = 365, code_serv = "1"
  ),
  times = 100
)
