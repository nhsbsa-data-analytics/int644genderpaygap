#' @title Highcharter line chart to show the number of headcount by financial
#' year by gender.
#'
#' @description \code{headcount_data} is the S3 class used for gender related
#' summary of workforce
#'
#'
#' @return Returns a highchart or htmlwidget object.
#'
#' @examples
#'
#' workforce <- nhsbsaGPG::headcount_data(nhsbsaGPG::headcount)
#' nhsbsaGPG::gender_profile(workforce)
#'
#' @export
#' @param x Input data frame from \code{headcount_data} S3 class object.
#' @param xvar "Financial Year", default
#' @param yvars data frame converts to list and each list element to create line
#' @param series_names If user wants to give different series name for
#' highchart legend
#' @param yaxis_title Title of y axis
#' @param yaxis_label Indication of percentage or number
#' @param show_legend TRUE default
#' @param line_style Control line style either Solid or DashDot
#' @param series_alpha Control opacity
#'
#' @import nhsbsa-data-analytics/nhsbsaR
#'
gender_profile <- function(x, xvar = "FINANCIAL_YEAR", yvars, series_names,
  yaxis_title, yaxis_label
) {
  out <- tryCatch(
    expr = {
      # Input data frame convert to list
      data_list <- list(x)

      # create plot object (empty one..)
      plt <- highcharter::highchart() |>
        highcharter::hc_chart(type = "line") |>
        nhsbsaR::theme_nhsbsa_highchart(stack = NA,
                                        palette = c("Blue", "AquaGreen"))


      # It requires minimum two series (male, female) but it could split further
      for (i in seq_along(series_names)) {
        yvar <- yvars[[i]] # take column name (e.g. Female/Male)
        # e.g. Label which will show in legend
        series_name <- series_names[[i]]
        data <- data_list[[1]] # list converted data frame

        plt <- plt |>
          highcharter::hc_add_series(
            data = data,
            type = "line",
            highcharter::hcaes(
              x = .data[[xvar]], # default financial year
              y = .data[[yvar]] # Female for example
            ),
            name = series_name # these labels will show in legend
          )
      }

      plt <- if (yaxis_label == "percentage") {
        plt |>
          highcharter::hc_yAxis(
            title = list(text = yaxis_title),
            labels <- list(format = "{value}"),
            min = 0,
            max = 100
          )
      } else {
        plt |>
          highcharter::hc_yAxis(
            title = list(text = yaxis_title),
            labels <- list(format = "{value:,f}"),
            min = 0
          )

        plt <- plt |>
          highcharter::hc_xAxis(type = "category") |>
          highcharter::hc_legend(
            itemWidth = 600,
            itemMarginTop = 5,
            y = 0
          )
      }
      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gender_profile():", w)
    },
    error = function(e) {
      stop("Error produced running gender_profile():", e)
    },
    finally = {}
  )
}
