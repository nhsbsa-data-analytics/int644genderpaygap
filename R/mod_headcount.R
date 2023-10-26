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
      heading = "Headcount by gender",
      highcharter::highchartOutput(
        outputId = ns("headcount_all"),
        height = "240px"
      ),
      mod_nhs_download_ui(id = ns("download_headcount_all"))
    ),
    includeMarkdown("inst/markdown/02_headcount_2.md"),
    nhs_card(
      heading = "Headcount by AfC pay band and gender",
      nhs_selectInput(
        inputId = ns("period"),
        label = "Reporting period",
        choices = unique(nhsbsaGPG::gpg_class$df_hdcnt$period),
        full_width = TRUE
      ),
      highcharter::highchartOutput(
        outputId = ns("headcount_afc"),
        height = "500px"

      )
    ),
    # chart 2: reporting period drop down, radio button toggle
    # between count and percentage
    includeMarkdown("inst/markdown/02_headcount_3.md")

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

      gpg_trend(x = df_hdcnt_gender, xvar = "period",
                yvars = c("Female", "Male"),
                series_names = c("Female", "Male"),
                yaxis_title = "Headcount",
                yaxis_label = "number",
                colpalette = "gender")
    })

    df_hdcnt_afc <- reactive({
      req(input$period)

      nhsbsaGPG::gpg_class$df_hdcnt_afc |>
        dplyr::filter(period == input$period) |>
        dplyr::mutate(headcount = headcount * ifelse(gender == "Male", 1, -1))
    })

    output$headcount_afc <- highcharter::renderHighchart({

      gpg_pyramid(x = df_hdcnt_afc(), xvar = "afc_band",
                  yvar = "headcount", yaxis_title = "Headcount")
    })

  })
}
