#' NHSBSA employee staff_afc
#'
#' A dataset containing NHSBSA employee hourly pay
#' by reporting period split by gender, AFC band, directorate.
#' \itemize{
#'   \item period. 2018/19, 2019/20 etc character
#'   \item gender. Male or Female, character
#'   \item headcount. employee headcount used for aggregation
#'   \item hourly_rate. hourly rate as shown pay slip
#'   \item quartile. split hourly_rate by quartile by gender
#'   \item fte. employee full time or part time info
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


#' NHSBSA employee paygap
#' 
#' A dataset containing NHSBSA employee paygap
#' Directly pulled from ESR dashboard (NHS National Returns) 
#' gender, average hourly rate, median hourly rate and pay gap%
#' 
#' \itemize{
#'    \item period. 2018/19, 2019/20 etc character
#'   \item avg_hr_gpg. Mean gender pay gap % based on male full-pay relevant employees
#'   \item median_hr_gpg. Median gender pay gap % based on male full-pay relevant employees
#'   }
#'   
#' @docType data
#' @keywords datasets
#' @name paygap
#' @usage paygap
#' @format A data frame with paygap information

"paygap"


#' NHSBSA employee quartile
#'
#' A dataset containing NHSBSA employee hourly pay
#' by reporting period from 2018 (eg. snapshot 31/03/2018 for 2018/19 report etc)
#' split by gender, AFC band, directorate
#' 
#' 
#' \itemize{
#'   \item period. 2018/19, 2019/20 etc character
#'   \item quartile. split hourly pay into quartiles
#'   \item female. number of female employees in each quartile
#'   \item male. number of male employees in each quartile
#'   \item quartile. split hourly_rate by quartile by gender
#'   \item female_percent. female employee % in quartile
#'   \item male_percent. male employee % in quartile   
#'  }
#'  @docType data
#'  @keywords datasets
#'  @name quartile
#'  @usage quartile
#'  @format data frame with employee gender pay gap by quartiles

"quartile"