#' @title S3 gpg class to create headcount, hourly rate by AFC,
#' directorate level hourly rate.'
#'
#' @description \code{gpg_data} is the class used for the creation of
#' headcount, hourly rate by AFC with without directorate in the GPG report.
#'
#' @details The \code{gpg_data} class expects a \code{data.frame} with at
#' least seven columns: period, gender, hourly_rate, quartile, fte, afc_band,
#' directorate.
#'
#' Once initiated, the class has six slots:
#' \code{df}: raw data frame 
#' \code{df_hdcnt}: data frame contains headcount by period 
#' \code{df_hdcnt_gender}: data frame contains headcount by gender by period 
#' \code{df_hdcnt_afc}: data frame contains headcount by afc band 
#' \code{df_hdcnt_dir}: data frame contains headcount by directorate 
#' \code{df_hrrate}: data frame contains hourly rate by gender for each grade 
#' \code{ending_fy}: a character vector containing ending reporting period
#' (e.g. 31 March 2023). This uses for introduction paragraph
#'
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
#' df <- gpg_data(afc_staff)
#'
#' @export


gpg_data <- function(x,
                     log_level = futile.logger::WARN,
                     eda = FALSE) {
  # Set logger severity threshold, defaults to WARN
  futile.logger::flog.threshold(log_level)


  # Checks
  futile.logger::flog.info("Initiating gpg_data class.
                           \n\nIt expects a data.frame with at
                           least eight columns: period, gender,
                           headcount, hourly_rate, quartile,
                           fte, afc_band, directorate.")



  futile.logger::flog.debug("Checking x is a data.frame...")
  if (!is.data.frame(x)) {
    futile.logger::flog.error("x must be a data.frame",
      x,
      capture = TRUE
    )
  }

  futile.logger::flog.debug("Checking x has correct columns...")

  if (length(colnames(x)) < 7) {
    futile.logger::flog.error("x must have at least eight columns:
                              period, gender, headcount, hourly_rate,
                              quartile, fte, afc_band, directorate.")
  }

  futile.logger::flog.debug("Checking x contains a period column...")
  if (!"period" %in% colnames(x)) {
    stop("x must contain period column")
  }

  futile.logger::flog.debug("Checking x contains a gender column...")
  if (!"gender" %in% colnames(x)) stop("x must contain gender column")

  futile.logger::flog.debug("Checking x contains a headcount column...")
  if (!"headcount" %in% colnames(x)) stop("x must contain headcount column")

  futile.logger::flog.debug("Checking x contains a hourly_rate column...")
  if (!"hourly_rate" %in% colnames(x)) {
    stop("x must contain hourly_rate column")
  }

  futile.logger::flog.debug("Checking x contains a fte column...")
  if (!"fte" %in% colnames(x)) {
    stop("x must contain fte column")
  }

  futile.logger::flog.debug("Checking x contains a afc_band column...")
  if (!"afc_band" %in% colnames(x)) {
    stop("x must contain afc_band column")
  }

  futile.logger::flog.debug("Checking x contains a directorate column...")
  if (!"directorate" %in% colnames(x)) {
    stop("x must contain directorate column")
  }

  futile.logger::flog.debug("Checking x does not contain missing values...")
  if (anyNA(x)) stop("x cannot contain any missing values")

  futile.logger::flog.debug("Checking for the correct number of rows...")
  if (nrow(x) < 16000) {
    futile.logger::flog.warn("x does not appear to be well formed. nrow(x) should be
                             greater than 16000.")
  }

  # Check sensible range for reporting period
  futile.logger::flog.debug("Checking beginning reporting period in a sensible
                            range e.g.(2018:2023)...")


  if (any(as.numeric(stringr::str_sub(x$period, 1, 4)) < 2018)) {
    futile.logger::flog.warn("The dates should start from
                             2018/19 financial year. Please check data-raw script.")
  }


  futile.logger::flog.info("...check done..")

  # Message required to pass a test
  message("Checks completed: 'gpg_data' S3 class created. Good to use for charts")

  # EDA
  # number of HEADCOUNT per financial year - expect to increase?
  agg_data <- aggregate(headcount ~ period, x, sum)

  if (eda == TRUE) {
    barplot(agg_data$headcount,
      names.arg = agg_data$period,
      las = 2,
      ylab = "Reporting period",
      xlab = "Headcount"
    )
  }


  # Calculate the latest reporting year
  # This values are required to add to the introduction text
  # (eg. as of 31 March 2023)
  start_latest_year <- max(as.numeric(stringr::str_sub(x$period, 1, 4)))
  latest_fy <- paste0(
    start_latest_year, "/",
    as.numeric(stringr::str_sub(start_latest_year, 3, 4)) + 1
  )


  # data frame: aggregate headcount by period
  df_hdcnt <- x |>
    dplyr::group_by(period) |>
    dplyr::summarise(headcount = sum(headcount, na.rm = TRUE)) |>
    dplyr::arrange(period)

  # Extract the values
  reporting_headcount <-
    agg_data$headcount[agg_data$period == latest_fy]

  ending_fy <- as.character(start_latest_year + 1)
  
  # data frame: aggregate headcount by gender by period
  df_hdcnt_gender <- x |>
    dplyr::group_by(period,gender) |>
    dplyr::summarise(headcount = sum(headcount, na.rm = TRUE)) |>
    dplyr::arrange(period)
  
  

  # data frame: aggregate headcount by period and AFC band
  df_hdcnt_afc <- x |>
    dplyr::group_by(period, gender, afc_band) |>
    dplyr::summarise(
      headcount = sum(headcount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(period, afc_band) |>
    dplyr::mutate(
      perc = headcount / sum(headcount) * 100
    )

  # data frame: aggregate headcount by period and directorate
  df_hdcnt_dir <- x |>
    dplyr::group_by(period, gender, directorate) |>
    dplyr::summarise(
      headcount = sum(headcount, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(period, directorate) |>
    dplyr::mutate(
      perc = headcount / sum(headcount) * 100
    )

  # data frame: hourly rate by gender for overall, each AFC band
  df_hrrate <- dplyr::bind_rows(
    x |>
      dplyr::group_by(period, gender, afc_band) |>
      dplyr::summarise(
        mean_rate = mean(hourly_rate, na.rm = TRUE),
        median_rate = median(hourly_rate, na.rm = TRUE),
        .groups = "drop"
      ),
    x |>
      dplyr::group_by(period, gender) |>
      dplyr::summarise(
        mean_rate = mean(hourly_rate, na.rm = TRUE),
        median_rate = median(hourly_rate, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(afc_band = "Overall")
  )


  # Define the class here ----
  # This will mainly use for highchart graphs

  structure(
    list(
      df = x,
      df_hdcnt = df_hdcnt,
      df_hdcnt_gender = df_hdcnt_gender,
      df_hdcnt_afc = df_hdcnt_afc,
      df_hdcnt_dir = df_hdcnt_dir,
      df_hrrate = df_hrrate,
      reporting_headcoun = reporting_headcount,
      ending_fy = ending_fy
    ),
    class = "gpg_data"
  )
}