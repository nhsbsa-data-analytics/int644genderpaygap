#' quartile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_quartile_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/app/www/assets/markdown/04_quartile.md"),
    nhs_card_tabstop(
      nhs_selectInput(
        inputId = ns("period"),
        label = "Reporting period",
        choices = unique(nhsbsaGPG::gpg_class$df_quartile$period),
        full_width = TRUE,
        selected = max(nhsbsaGPG::gpg_class$df_quartile$period)
      ),
      br(),
      highcharter::highchartOutput(
        outputId = ns("quartile"),
        height = "500px"
      ),
      mod_nhs_download_ui(id = ns("download_quartile"))
    )
  )
}

#' quartile Server Functions
#'
#' @noRd
mod_quartile_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_quartile <- reactive({
      req(input$period)

      nhsbsaGPG::quartile |>
        dplyr::filter(period == input$period) |>
        dplyr::mutate(gender = ifelse(gender == "men", "Men", "Women"))
    })

    output$quartile <- highcharter::renderHighchart({
      plt <- gpg_stack(x = df_quartile(),
                       xvar = "quartile",
                       yvar = "percent",
                       groupvar = "gender",
                       yaxis_title = "% of men and women in each pay quartile")

    })

    df_pay_quartile_download <- nhsbsaGPG::quartile |>
      dplyr::mutate(percent = round(percent, 1)) |>
      dplyr::rename(
        `Reporting period` = period,
        Quartile = quartile,
        Gender = gender,
        Count = count,
        Percent = percent
      )

    # data download - quartile
    mod_nhs_download_server(
      id = "download_quartile",
      filename = "pay_quartile.csv",
      export_data = df_pay_quartile_download
    )

  })
}
