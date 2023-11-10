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
                      colpalette,
                      export_filename) {
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
        yvar <- yvars[[i]] # take column name (e.g. Women/Men)
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
        ) |>
        highcharter::hc_credits(enabled = TRUE) |>
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = export_filename,
          buttons = list(
            contextButton = list(
              text = "Export chart",
              menuItems = list("downloadPNG",
                               "downloadSVG",
                               "label")
            )
          )
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
#' @param xvar This can be either afc_band or directorate
#' @param yvar headcount by number or percentage
#' @param yaxis_title Y axis title

gpg_pyramid <- function(x, xvar, yvar, yaxis_title, export_filename) {
  out <- tryCatch(
    expr = {
      data <- x
      xaxis_category <- sort(unique(data[[xvar]]))
      yaxis_max <- if (yvar == "perc") 100 else NULL
      xaxis_title <- if (xvar == "afc_band") "AfC band" else "Directorate"
      data$gender <- factor(data$gender, levels = c("Women", "Men"))

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
        nhsbsaR::theme_nhsbsa_highchart(palette = c("AquaGreen", "Purple")) |>
        highcharter::hc_yAxis(
          max = yaxis_max,
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
          categories = xaxis_category,
          title = list(text = xaxis_title),
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
        ) |>
        highcharter::hc_credits(enabled = TRUE) |>
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = export_filename,
          buttons = list(
            contextButton = list(
              text = "Export chart",
              menuItems = list("downloadPNG",
                               "downloadSVG",
                               "label")
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

gpg_stack <- function(x, xvar, yvar, groupvar, yaxis_title, export_filename) {
  out <- tryCatch(
    expr = {
      data <- x

      # Like other chart, women then follow by men
      data[[groupvar]] <- factor(data[[groupvar]], levels = c("Women", "Men"))

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
        nhsbsaR::theme_nhsbsa_highchart(palette = c("AquaGreen", "Purple")) |>
        highcharter::hc_yAxis(
          title = list(text = yaxis_title),
          max = 100
        ) |>
        highcharter::hc_xAxis(
          min = 0,
          max = 4, # Pad to ensure we can see the 4 label
          categories = c("1: Lower Quartile<br>(Lowest Paid)",
                         "2: Lower Middle Quartile",
                         "3: Upper Middle Quartile",
                         "4: Upper Quartile<br>(Highest Paid)",
                         "Overall"),
          title = list(text = "Quartile")
        ) |>
        highcharter::hc_credits(enabled = TRUE) |>
        # highcharter::hc_legend(ggplot2::element_blank()) |>
        highcharter::hc_plotOptions(
          column = list(dataLabels = list(
            enabled = TRUE,
            format = "{point.y:.1f} %",
            inside = TRUE,
            align = "center",
            color = "#E8EDEE",
            style = list(fontSize = "13px",
                         textOutline = 0)
          )),
          pointPadding = 1,
          groupPadding = 1,
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
        ) |>
        highcharter::hc_tooltip(enabled = FALSE) |>
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = export_filename,
          buttons = list(
            contextButton = list(
              text = "Export chart",
              menuItems = list("downloadPNG",
                               "downloadSVG",
                               "label")
            )
          )
        )

      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_stack():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_stack():", e)
    },
    finally = {}
  )
}


#' @title Highcharter bar chart. This chart will show columns horizontally.
#'
#' @description {df_hrrate_afc} or {df_hrrate_dir} data frame is used for bar chart.
#'
#' @return Returns a highchart or htmlwidget object.
#'
#'
#' @export
#' @param x Input df_hrrate_afc or df_hrrate_dir data frame.
#' @param xvar e.g. mean_paygap, median_paygap
#' @param yvar AfC band, directorate
#' @param yaxis_title Y axis title

gpg_bar <- function(x, xvar, yvar, yaxis_title, export_filename) {
  out <- tryCatch(
    expr = {
      data <- x
      # positive and negative value give different colours.
      data <- data |>
        dplyr::filter(!is.na(.data[[yvar]])) |>
        dplyr::mutate(
          color = ifelse(.data[[yvar]] < 0,
            nhsbsaR::palette_nhsbsa("Pink"),
            nhsbsaR::palette_nhsbsa("LightBlue")
          )
        )

      data$color <- htmltools::parseCssColors(data$color)

      plt <- highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data,
          type = "bar",
          highcharter::hcaes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            color = color
          ),
          name = "Gender pay gap (%)"
        ) |>
        nhsbsaR::theme_nhsbsa_highchart() |>
        highcharter::hc_yAxis(
          title = list(text = yaxis_title),
          max = 50
        ) |>
        highcharter::hc_xAxis(
          type = "category",
          title = list(text = "")
        ) |>
        highcharter::hc_legend(ggplot2::element_blank()) |>
        highcharter::hc_plotOptions(
          bar = list(dataLabels = list(
            enabled = TRUE,
            format = "{point.y:.1f} %",
            inside = FALSE,
            align = "top",
            color = "#425563",
            style = list(fontSize = "13px",
                         textOutline = 0)
          )),
          pointPadding = 1,
          groupPadding = 1
        ) |>
        highcharter::hc_tooltip(
          headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
          pointFormat = '<span style="color:{point.color}">
          \u25CF</span> {series.name}: <b>{point.y} %</b><br/>',
          footerFormat = ""
        ) |>
        highcharter::hc_credits(enabled = TRUE) |>
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = export_filename,
          buttons = list(
            contextButton = list(
              text = "Export chart",
              menuItems = list("downloadPNG",
                               "downloadSVG",
                               "label")
            )
          )
        )

      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_bar():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_bar():", e)
    },
    finally = {}
  )
}



#' @title Highcharter column chart. This chart will show columns vertically
#'
#' @description {paygap} data frame is used for bar chart.
#'
#' @return Returns a highchart or htmlwidget object.
#'
#'
#' @export
#' @param x Input paygap data frame.
#' @param xvar "period" default
#' @param yvar e.g. mean_paygap, median_paygap
#' @param yaxis_title Y axis title

gpg_column <- function(x, xvar = "period", yvar, yaxis_title, export_filename) {
  out <- tryCatch(
    expr = {
      data <- x

      plt <- highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data,
          type = "column",
          highcharter::hcaes(
            x = .data[[xvar]],
            y = .data[[yvar]]
          ),
          name = "Gender pay gap (%)"
        ) |>
        nhsbsaR::theme_nhsbsa_highchart() |>
        highcharter::hc_yAxis(
          title = list(text = yaxis_title),
          max = 20
        ) |>
        highcharter::hc_xAxis(
          type = "category",
          title = list(text = "")
        ) |>
        highcharter::hc_legend(ggplot2::element_blank()) |>
        highcharter::hc_plotOptions(
          column = list(dataLabels = list(
            enabled = TRUE,
            format = "{y} %",
            inside = FALSE,
            align = "center",
            verticalAlign = "bottom",
            y = -5,
            color = "#231f20",
            style = list(fontSize = "12px",
                         textOutline = 0)
          )),
          pointPadding = 0.1,
          groupPadding = 0
        ) |>
        highcharter::hc_credits(enabled = TRUE) |>
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = export_filename,
          buttons = list(
            contextButton = list(
              text = "Export chart",
              menuItems = list("downloadPNG",
                               "downloadSVG",
                               "label")
            )
          )
        )

      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_column():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_column():", e)
    },
    finally = {}
  )
}



#' @title Highcharter dumbbell chart.
#'
#' @description df_hrrate_afc or df_hrrate_dir data frame.
#'
#' @return Returns a highchart or htmlwidget object.
#'
#'
#' @export
#' @param x Input {df_hrrate_afc} or {df_hrrate_dir} data frame.
#' @param xvar "period" default
#' @param yvar e.g. mean_paygap, median_paygap
#' @param yaxis_title Y axis title

gpg_dumbbell <- function(x, low, high, xaxis_category, yaxis_title,
                         export_filename) {
  out <- tryCatch(
    expr = {
      data <- x
      category_text <- data[[xaxis_category]]
      yaxis_max_value <- ifelse(xaxis_category == "afc_band", 60, 90)
      plt <- highcharter::highchart() |>
        highcharter::hc_add_series(
          data = data,
          type = "dumbbell",
          highcharter::hcaes(
            low = .data[[low]],
            high = .data[[high]]
          ),
          lowColor = nhsbsaR::palette_nhsbsa("AquaGreen"),
          color = nhsbsaR::palette_nhsbsa("Purple")
        ) |>
        highcharter::hc_subtitle(
          useHTML = TRUE,
          text =
            "
            <span style = 'color:#00A499; font-size: 30px'> &bull; 
          </span> <b> <span style = font-size: 35px'> Women </span> </b>
            <span style = 'color:#330072; font-size: 30px'> &bull; 
          </span> <b> <span style = font-size: 35px'> Men </span>
            ",
          align = "center"
        ) |>
        highcharter::hc_chart(
          inverted = TRUE,
          marginLeft = 200
        ) |>
        highcharter::hc_xAxis(
          categories = category_text,
          title = list(text = "")
        ) |>
        highcharter::hc_yAxis(
          min = 0,
          max = yaxis_max_value,
          title = list(text = yaxis_title)
        ) |>
        highcharter::hc_scrollbar(enabled = FALSE) |>
        highcharter::hc_legend(enabled = FALSE) |>
        nhsbsaR::theme_nhsbsa_highchart() |>
        highcharter::hc_credits(enabled = TRUE) |>
        highcharter::hc_exporting(
          enabled = TRUE,
          filename = export_filename,
          buttons = list(
            contextButton = list(
              text = "Export chart",
              menuItems = list("downloadPNG",
                               "downloadSVG",
                               "label")
            )
          )
        )

      return(plt)
    },
    warning = function() {
      w <- warnings()
      warning("Warning produced running gpg_dumbbell():", w)
    },
    error = function(e) {
      stop("Error produced running gpg_dumbbell():", e)
    },
    finally = {}
  )
}
