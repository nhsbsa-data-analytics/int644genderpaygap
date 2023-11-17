#' NHSBSA employee afc_staff
#'
#' A dataset containing NHSBSA employee hourly pay
#' by reporting period split by gender, AfC band, directorate.
#' Input data frame for gpg_class
#' \itemize{
#'   \item period. 2018/19, 2019/20 etc character
#'   \item gender. Male or Female, character
#'   \item headcount. employee headcount used for aggregation
#'   \item hourly_rate. hourly rate as shown pay slip
#'   \item quartile. split hourly_rate by quartile by gender
#'   \item afc_band. AFC band
#'   \item directorate. NHSBSA directorate
#' }
#'
#' @docType data
#' @keywords datasets
#' @name afc_staff
#' @usage afc_staff
#' @format A data frame with gender pay gap information
#'
#'
"afc_staff"



#' NHSBSA GPG report required objects
#'
#' A S3 class containing aggregated data frames.
#' \itemize{
#'   \item df_hdcnt. overall headcount
#'   \item df_hdcnt_gender. overall headcount by gender
#'   \item df_hdcnt_afc. headcount by gender and AfC
#'   \item df_hdcnt_dir. headcount by gender and directorate
#'   \item df_hrrate Mean and median men and women hourly pay to calculate gender
#'   pay gap
#'   \item df_hrrate_afc Mean and median hourly pay by AfC & paygap
#'   \item df_hrrate_dir Mean and median hourly pay by directorate & paygap
#'   \item df_quartile Quartile pay split by gender
#'   \item reporting_headcount Reporting headcount
#'   \item reporting_latest_year Reporting year#'
#' }
#'
#' @docType data
#' @keywords R3 class
#' @name gpg_class
#' @usage gpg_class
#' @format A data frame with gender pay gap information
#'
#'
"gpg_class"
