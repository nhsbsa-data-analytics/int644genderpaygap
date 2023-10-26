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
    nhs_card_tabstop(
      heading = "Headcount by gender",
      highcharter::highchartOutput(
        outputId = ns("headcount_all"),
        height = "240px"
      ),
      mod_nhs_download_ui(id = ns("download_headcount_all"))
    ),
    includeMarkdown("inst/markdown/02_headcount_2.md")

  )
}

#' headcount Server Functions
#'
#' @noRd
mod_headcount_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- nhsbsaGPG::gpg_data(nhsbsaGPG::afc_staff)$df_hdcnt_gender |>
      tidyr::pivot_wider(names_from = gender, values_from = headcount)

    output$headcount_all <- highcharter::renderHighchart({

      gpg_trend(x = df, xvar = "period",
                yvars = c("Female", "Male"),
                series_names = c("Female", "Male"),
                yaxis_title = "Headcount",
                yaxis_label = "number",
                colpalette = "gender")
    })

  })
}
