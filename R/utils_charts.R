#' @title Highcharter line chart to show the number of headcount by financial
#' year by gender.
#'
#' @description \code{gpg_data} is the S3 class used for trend
#'
#'
#' @return Returns a highchart or htmlwidget object.
#'
#' @export
#' @param x Input data frame from \code{gpg_data} S3 class object.
#' @param xvar "period", default
#' @param yvars data frame converts to list and each list element to create line
#' @param series_names If user wants to give different series name for highchart legend
#' @param yaxis_title Y axis title
#' @param yaxis_label Indication of percentage or number
#' @param colpalette custom palette
#'
#'
gpg_trend <- function(x,
                      xvar = "period",
                      yvars,
                      series_names,
                      yaxis_title,
                      yaxis_label,
                      colpalette) {
  out <- tryCatch(
    expr = {
      # Input data frame convert to list
      data_list <- list(x)

      # create plot object (empty one..)
      plt <- highcharter::highchart() |>
        highcharter::hc_chart(type = "line") |>
        nhsbsaR::theme_nhsbsa_highchart(
          stack = NA,
          palette = colpalette
        )


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
              x = .data[[xvar]], # default period
              y = .data[[yvar]] # Female for example
            ),
            name = series_name # these labels will show in legend
          )
      }


      plt <- if (yaxis_label == "percentage") {
        plt |>
          highcharter::hc_yAxis(
            title = list(text = yaxis_title),
            labels = list(format = "{value}%"),
            min = 0,
            max = 20
          )
      } else {
        plt |>
          highcharter::hc_yAxis(
            title = list(text = yaxis_title),
            labels = list(format = "{value:,f}"),
            min = 0
          )
      }

      plt <- plt |>
        highcharter::hc_xAxis(type = "category") |>
        highcharter::hc_legend(
          itemWidth = 600,
          itemMarginTop = 5,
          y = 0
        )

      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_trend():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_trend():", e)
    },
    finally = {}
  )
}




#' @title Highcharter bar chart to create pyramid chart. This chart
#' shows gender pay gap information by AFC band in NHSBSA
#'
#' @description \code{gpg_data} is the S3 class used for trend
#'
#'
#' @return Returns a highchart or htmlwidget object.
#'
#'
#' @export
#' @param x Input data frame from \code{gpg_data} S3 class object.
#' @param xvar "afc_band" default value
#' @param yvar headcount/mean hourly/median hourly pay
#' @param yaxis_title Y axis title

gpg_pyramid <- function(x, xvar = "afc_band", yvar, yaxis_title) {
  out <- tryCatch(
    expr = {
      data <- x
      # Create chart object
      plt <- data |>
        highcharter::hchart(
          type = "bar",
          highcharter::hcaes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            group = "gender"
          )
        ) |>
        nhsbsaR::theme_nhsbsa_highchart(palette = "gender") |>
        highcharter::hc_yAxis(
          title = list(text = yaxis_title),
          labels = list(
            formatter = highcharter::JS(
              "
                function() {
                  outHTML = this.axis.defaultLabelFormatter.call(this)
                  return outHTML.replace('-', '')
                }
                "
            )
          )
        ) |>
        highcharter::hc_xAxis(
          title = list(text = "AFC band"),
          reversed = FALSE
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            states = list(
              # Disable series highlighting
              inactive = list(enabled = FALSE)
            ),
            events = list(
              # Disables turning the series off
              legendItemClick = htmlwidgets::JS("function () { return false; }")
            )
          )
        )


      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_pyramid():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_pyramid():", e)
    },
    finally = {}
  )
}





#' @title Highcharter column chart to create stacked column chart. This chart
#' shows proportion of males and females in each quartile pay band.
#'
#' @description {quartile} data frame is used for stacked column chart.
#'
#'
#' @return Returns a highchart or htmlwidget object.
#'
#'
#' @export
#' @param x Input quartile data frame.
#' @param xvar "afc_band" default value
#' @param yvar headcount/mean hourly/median hourly pay
#' @param groupvar group by variable
#' @param yaxis_title Y axis title

gpg_stack <- function(x, xvar, yvar, groupvar, yaxis_title) {
  out <- tryCatch(
    expr = {
      data <- x
      # Create chart object
      plt <- data |>
        highcharter::hchart(
          type = "column",
          highcharter::hcaes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            group = .data[[groupvar]]
          )
        ) |>
        nhsbsaR::theme_nhsbsa_highchart(palette = "gender") |>
        highcharter::hc_yAxis(
          title = list(text = yaxis_title),
          max = 100
        ) |>
        highcharter::hc_xAxis(
          title = list(text = "Quartile")
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            states = list(
              # Disable series highlighting
              inactive = list(enabled = FALSE)
            ),
            events = list(
              # Disables turning the series off
              legendItemClick = htmlwidgets::JS("function () { return false; }")
            )
          )
        )


      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_pyramid():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_pyramid():", e)
    },
    finally = {}
  )
}
