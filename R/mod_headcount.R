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
    includeMarkdown("inst/markdown/02_headcount_1.md"),
    # chart 1: five year headcount trend split by gender
    nhs_card(
      highcharter::highchartOutput(
        outputId = ns("headcount_all"),
        height = "240px"
      ),
      mod_nhs_download_ui(id = ns("download_headcount_all"))
    ),
    includeMarkdown("inst/markdown/02_headcount_2.md"),
    # chart 2: headcount split by gender AfC
    nhs_card(
      nhs_selectInput(
        inputId = ns("period"),
        label = "Reporting period",
        choices = unique(nhsbsaGPG::gpg_class$df_hdcnt$period),
        full_width = TRUE,
        selected = max(unique(nhsbsaGPG::gpg_class$df_hdcnt$period))
      ),
      highcharter::highchartOutput(
        outputId = ns("headcount_afc"),
        height = "500px"
      ),
      mod_radio_button_ui(
        id = ns("hcnt_afc_toggle"),
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
      tidyr::pivot_wider(names_from = gender, values_from = headcount)

    output$headcount_all <- highcharter::renderHighchart({
      gpg_trend(
        x = df_hdcnt_gender, xvar = "period",
        yvars = c("Female", "Male"),
        series_names = c("Female", "Male"),
        yaxis_title = "Headcount",
        yaxis_label = "number",
        colpalette = "gender"
      )
    })

    df_hdcnt_afc <- reactive({
      req(input$period)

      nhsbsaGPG::gpg_class$df_hdcnt_afc |>
        dplyr::filter(period == input$period) |>
        dplyr::mutate(
          headcount = headcount * ifelse(gender == "Male", 1, -1),
          perc = perc * ifelse(gender == "Male", 1, -1)
        )
    })

    hcnt_afc_toggle <- mod_radio_button_server("hcnt_afc_toggle")

    yvar <- reactive({
      req(hcnt_afc_toggle())
      ifelse(hcnt_afc_toggle() == "headcount", "headcount", "perc")
    })

    yaxis_title <- reactive({
      req(hcnt_afc_toggle())
      ifelse(hcnt_afc_toggle() == "headcount", "Headcount", "Percentage")
    })


    output$headcount_afc <- highcharter::renderHighchart({
      plt <- gpg_pyramid(
        x = df_hdcnt_afc(), xvar = "afc_band",
        yvar = yvar(), yaxis_title = yaxis_title()
      )

      plt |>
        highcharter::hc_tooltip(
          shared = FALSE,
          useHTML = TRUE,
          formatter = htmlwidgets::JS(
            "
              function() {

                outHTML =
                  '<b>Gender: </b>' + this.series.name + '<br>' +
                  '<b>AfC: </b>' + this.point.afc_band + '<br/>' +
                  '<b>Number of employees: </b>' +
          Highcharts.numberFormat(Math.abs(this.point.y), 0) + '<br>' +
                  '<b>Percentage of employees: </b>' +
          Highcharts.numberFormat(Math.abs(this.point.perc), 0) + '%'

                return outHTML

                }
              "
          )
        )
    })
  })
}
