#' @title S3 headcount class to create number of headcount by gender and also
#' gender and AFC pay band.#'
#'
#' @description \code{headcount_data} is the class used for the creation of
#' first two headcount related figures in the GPG report.
#'
#' @details The \code{headcount_data} class expects a \code{data.frame} with at
#' least five columns: FINANCIAL_YEAR, GENDER, PAY_GRADE_NAME, FTE_GROUP, HEADCOUNT. Each
#' row represents aggregated headcount by four columns.
#'
#' Once initiated, the class has seven slots:
#' \code{df}: data frame \n
#' \code{overview_gender}: data frame \n
#' \code{overview_afc}: data frame \n
#' \code{overview_fte}: data frame \n
#' \code{reporting_headcount}: a numeric vector containing reporting financial
#' year's headcount \n
#' \code{diffs}: a numeric vector containing differences from previous \n
#' financial year headcount to current reporting financial year headcount \n
#' \code{ending_fy}: a character vector containing ending reporting period
#' (e.g. 31 March 2022). This uses for introduction paragraph
#'
#'
#' @param x Input data frame.
#' @param log_level keep it WARN
#' @param eda If TRUE base R plot shows in the Viewer
#'
#' @return If the class is not instantiated correctly, nothing is returned.
#'
#' @examples
#'
#' library(nhsbsaGPG)
#'
#' df <- headcount_data(headcount)
#'
#' @export


headcount_data <- function(x,
                           log_level = futile.logger::WARN,
                           eda = FALSE) {

  # Set logger severity threshold, defaults to WARN
  futile.logger::flog.threshold(log_level)


  # Checks
  futile.logger::flog.info("Initiating headcount_data class.
                           \n\nIt expects a data.frame with at
                           least five columns: FINANCIAL_YEAR, gender,
                           PAY_GRADE_NAME, FTE_GROUP and HEADCOUNT.
                           Each row represents an aggregated headcount
                           based on four columns.")



  futile.logger::flog.debug("Checking x is a data.frame...")
  if (!is.data.frame(x)) {
    futile.logger::flog.error("x must be a data.frame",
      x,
      capture = TRUE
    )
  }

  futile.logger::flog.debug("Checking x has correct columns...")
  if (length(colnames(x)) < 5) {
    futile.logger::flog.error("x must have at least five columns:
                              FINANCIAL_YEAR,
                              GENDER, PAY_GRADE_NAME,
                              FTE_GROUP, HEADCOUNT")
  }

  futile.logger::flog.debug("Checking x contains a FINANCIAL_YEAR column...")
  if (!"FINANCIAL_YEAR" %in% colnames(x)) {
    stop("x must contain FINANCIAL_YEAR column")
  }

  futile.logger::flog.debug("Checking x contains a GENDER column...")
  if (!"GENDER" %in% colnames(x)) stop("x must contain GENDER column")

  futile.logger::flog.debug("Checking x contains a PAY_GRADE_NAME column...")
  if (!"PAY_GRADE_NAME" %in% colnames(x)) {
    stop("x must contain PAY_GRADE_NAME column")
  }

  futile.logger::flog.debug("Checking x contains a FTE_GROUP column...")
  if (!"FTE_GROUP" %in% colnames(x)) {
    stop("x must contain FTE_GROUP column")
  }

  futile.logger::flog.debug("Checking x contains a HEADCOUNT column...")
  if (!"HEADCOUNT" %in% colnames(x)) {
    stop("x must contain HEADCOUNT column")
  }

  futile.logger::flog.debug("Checking x does not contain missing values...")
  if (anyNA(x)) stop("x cannot contain any missing values")

  futile.logger::flog.debug("Checking for the correct number of rows...")
  if (nrow(x) < 260) {
    futile.logger::flog.warn("x does not appear to be well formed. nrow(x) should be
                             greater than 180 (5 year * gender * fte * afc) 
                             as of 2021/22 report.")
  }



  # Check sensible range for year
  futile.logger::flog.debug("Checking beginning financial years in a sensible
                            range e.g.(2017:2022)...")


  if (any(as.numeric(stringr::str_sub(x$FINANCIAL_YEAR, 1, 4)) < 2017)) {
    futile.logger::flog.warn("The dates should start from
                             2017/18 financial year. Please check data-raw script.")
  }


  futile.logger::flog.info("...check done..")

  # Message required to pass a test
  message("Checks completed: 'headcount_data' S3 class created.")

  # EDA
  # number of HEADCOUNT per financial year - expect to increase?
  if (eda == TRUE) {
    agg_data <- aggregate(HEADCOUNT ~ FINANCIAL_YEAR, x, sum)
    barplot(agg_data$HEADCOUNT,
      names.arg = agg_data$FINANCIAL_YEAR,
      las = 2,
      ylab = "Financial Year",
      xlab = "Headcount"
    )
  }


  # Calculate the latest and previous years
  # This values are required to add to the interactive document
  start_latest_year <- max(as.numeric(stringr::str_sub(x$FINANCIAL_YEAR, 1, 4)))
  start_prev_year <- start_latest_year - 1
  # Financial year of interest for the report
  latest_fy <-  paste0(
    start_latest_year, "/",
    as.numeric(stringr::str_sub(start_latest_year, 3, 4)) + 1
  )
  previous_fy <- paste0(
    start_prev_year, "/",
    stringr::str_sub(start_latest_year, 3, 4)
  )

  # First aggregate by financial year
  agg_data <- x |>
    dplyr::filter(FINANCIAL_YEAR %in% c(latest_fy, previous_fy)) |>
    dplyr::group_by(FINANCIAL_YEAR) |>
    dplyr::summarise(TOTAL_HEADCOUNT = sum(HEADCOUNT, na.rm = TRUE)) |>
    dplyr::arrange(FINANCIAL_YEAR)

  # Extract the values
  reporting_headcount <-
    agg_data$TOTAL_HEADCOUNT[agg_data$FINANCIAL_YEAR == latest_fy]
  previous_reporting_headcount <-
    agg_data$TOTAL_HEADCOUNT[agg_data$FINANCIAL_YEAR == previous_fy]

  diffs <- reporting_headcount - previous_reporting_headcount

  ending_fy <- as.character(start_latest_year + 1)

  # Attach data frame: headcount by GENDER
  overview_gender <- x |>
    dplyr::group_by(FINANCIAL_YEAR, GENDER) |>
    dplyr::summarise(HEADCOUNT = sum(HEADCOUNT, na.rm = TRUE),
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = GENDER,
                       values_from = HEADCOUNT)

  # Attach data frame: headcount by GENDER & PAY_GRADE_NAME
  overview_afc <- x |>
    dplyr::group_by(FINANCIAL_YEAR, GENDER, PAY_GRADE_NAME) |>
    dplyr::summarise(HEADCOUNT = sum(HEADCOUNT, na.rm = TRUE),
                     .groups = "drop")

  # Attach data frame: headcount by GENDER & FTE
  overview_fte <- x |>
    dplyr::group_by(FINANCIAL_YEAR, GENDER, FTE_GROUP) |>
    dplyr::summarise(HEADCOUNT = sum(HEADCOUNT, na.rm = TRUE),
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = c(GENDER, FTE_GROUP),
                       values_from = HEADCOUNT)


  # Define the class here ----
  # It will use to create highchart line graph

  structure(
    list(
      df = x,
      overview_gender = overview_gender,
      overview_afc = overview_afc,
      overview_fte = overview_fte,
      reporting_headcount = reporting_headcount,
      diffs = diffs,
      ending_fy = ending_fy
    ),
    class = "headcount_data"
  )
}
