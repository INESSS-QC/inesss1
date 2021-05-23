#' Extraction Dx
#'
#' Extraction SQL des codes de diagnostics..
#'
#' @inherit SQL_comorbidity_diagn details
#'
#' @inheritParams SQL_comorbidity_diagn
#'
#' @inherit SQL_comorbidity_diagn return
#'
#' @encoding UTF-8
#'
#' @export
SQL_diagn <- function(
  conn, cohort, debut, fin,
  Dx_table = 'Combine_Dx_CCI_INSPQ18', CIM = c('CIM9', 'CIM10'),
  dt_source = c('V_DIAGN_SEJ_HOSP_CM', 'V_SEJ_SERV_HOSP_CM',
                'V_EPISO_SOIN_DURG_CM', 'I_SMOD_SERV_MD_CM'),
  dt_desc = list(V_DIAGN_SEJ_HOSP_CM = 'MEDECHO', V_SEJ_SERV_HOSP_CM = 'MEDECHO',
                 V_EPISO_SOIN_DURG_CM = 'BDCU', I_SMOD_SERV_MD_CM = 'SMOD'),
  date_dx_var = "depar", exclu_diagn = NULL, verbose = TRUE
) {
  ### Même fonction que SQL_comorbidity_diagn
  return(SQL_comorbidity_diagn(
    conn, cohort, debut, fin, Dx_table, CIM,
    dt_source, dt_desc,
    date_dx_var, exclu_diagn, verbose
  ))
}
