#' headcount UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_headcount_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/app/www/assets/markdown/02_headcount_1.md"),
    # chart 1: five year headcount trend split by gender
    nhs_card_tabstop(
      highcharter::highchartOutput(
        outputId = ns("headcount_all"),
        height = "240px"
      ),
      mod_nhs_download_ui(id = ns("download_headcount_all"))
    ),
    includeMarkdown("inst/app/www/assets/markdown/02_headcount_2.md"),
    # chart 2: headcount split by gender AfC
    nhs_card_tabstop(
      nhs_grid_2_col(
        nhs_selectInput(
          inputId = ns("period"),
          label = "Reporting period",
          choices = unique(nhsbsaGPG::gpg_class$df_hdcnt$period),
          full_width = TRUE,
          selected = max(unique(nhsbsaGPG::gpg_class$df_hdcnt$period))
        ),
        nhs_selectInput(
          inputId = ns("factor"),
          label = "Filter by",
          choices = c("AfC band" = "afc_band", "Directorate" = "directorate"),
          full_width = TRUE,
          selected = "AfC band"
        )
      ),

      highcharter::highchartOutput(
        outputId = ns("headcount_breakdown"),
        height = "500px"
      ),
      mod_radio_button_ui(
        id = ns("hcnt_breakdown_toggle"),
        label = "",
        choices = c(
          "Number" = "headcount",
          "Percentage" = "perc"
        ),
        selected = "headcount"
      ),
      mod_nhs_download_ui(id = ns("download_headcount_afc"))
    )
  )
}

#' headcount Server Functions
#'
#' @noRd
mod_headcount_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_hdcnt_gender <- nhsbsaGPG::gpg_class$df_hdcnt_gender |>
      tidyr::pivot_wider(names_from = gender, values_from = headcount) |>
      dplyr::mutate(
        total = Men + Women,
        dplyr::across(c(Men, Women), ~ round(./total * 100, 1),
                      .names = "{.col}_Proportion")
      ) %>%
      dplyr::select(-total)

    output$headcount_all <- highcharter::renderHighchart({
      plt <- gpg_trend(
        x = df_hdcnt_gender, xvar = "period",
        yvars = c("Women", "Men"),
        series_names = c("Women", "Men"),
        yaxis_title = "Headcount",
        yaxis_label = "number",
        colpalette = "gender"
      )

      plt |>
        highcharter::hc_tooltip(
          shared = FALSE,
          borderColor = "#425563",
          formatter = highcharter::JS(
            "
            function () {
            outHTML =
                '<b>Women: </b>' + Highcharts.numberFormat(this.point.Women, 0) + 
            ' (' + this.point.Women_Proportion + '%)' + '<br/>' +
                '<b>Men: </b>' + Highcharts.numberFormat(this.point.Men, 0) + 
            ' (' + this.point.Men_Proportion + '%)' + '<br/>' 
              return (outHTML)
            }

            "
          )
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
    })


    df_hdcnt_breakdown <- reactive({
      req(input$period)
      req(input$factor)

      if (input$factor == "afc_band") {
        nhsbsaGPG::gpg_class$df_hdcnt_afc |>
          dplyr::filter(period == input$period) |>
          dplyr::mutate(
            headcount = headcount * ifelse(gender == "Men", 1, -1),
            perc = perc * ifelse(gender == "Men", 1, -1),
            tooltip_text = afc_band
          )
      } else {
        nhsbsaGPG::gpg_class$df_hdcnt_dir |>
          dplyr::filter(period == input$period) |>
          dplyr::mutate(
            headcount = headcount * ifelse(gender == "Men", 1, -1),
            perc = perc * ifelse(gender == "Men", 1, -1),
            tooltip_text = directorate
          )
      }

    })

    hcnt_breakdown_toggle <- mod_radio_button_server("hcnt_breakdown_toggle")

    xvar <- reactive({
      req(input$factor)
      ifelse(input$factor == "afc_band", "afc_band", "directorate")
    })

    yvar <- reactive({
      req(hcnt_breakdown_toggle())
      ifelse(hcnt_breakdown_toggle() == "headcount", "headcount", "perc")
    })

    yaxis_title <- reactive({
      req(hcnt_breakdown_toggle())
      ifelse(hcnt_breakdown_toggle() == "headcount", "Headcount", "Percentage")
    })



    output$headcount_breakdown <- highcharter::renderHighchart({
      plt <- gpg_pyramid(
        x = df_hdcnt_breakdown(), xvar = xvar(),
        yvar = yvar(), yaxis_title = yaxis_title()
      )

      plt |>
        highcharter::hc_tooltip(
          shared = FALSE,
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            paste0(
              "
                  function() {
                    outHTML =
                      '<b>Gender: </b>' + this.series.name + '<br>' + '<b>",
              switch(input$factor,
                     "afc_band" = "AfC",
                     "directorate" = "Directorate"),
              ": </b>' + this.point.tooltip_text + '<br/>' + 
               '<b>Number of employees: </b>' +
          Highcharts.numberFormat(Math.abs(this.point.headcount), 0) + '<br>' +
                  '<b>Percentage of employees: </b>' +
          Highcharts.numberFormat(Math.abs(this.point.perc), 0) + '%'
                    return outHTML
                  }
                  "
            )
          )
        )

    })
  })
}
