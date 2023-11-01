#' paygap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_paygap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/markdown/03_gender_pay_gap_1.md"),
    nhs_card_tabstop(
      nhs_grid_2_col(
        highcharter::highchartOutput(
          outputId = ns("paygap_all_mean"),
          height = "280px"
        ),
        # chart 3: Overall gender pay gap
        highcharter::highchartOutput(
          outputId = ns("paygap_all_median"),
          height = "280px"
        )
      )
    ),
    mod_nhs_download_ui(id = ns("download_paygap_all")),
    br(),
    includeMarkdown("inst/markdown/03_gender_pay_gap_2.md"),
    nhs_card_tabstop(
      nhs_grid_3_col(
        nhs_selectInput(
          inputId = ns("period"),
          label = "Reporting period",
          choices = unique(nhsbsaGPG::gpg_class$df_hrrate_afc$period),
          full_width = TRUE,
          selected = max(unique(nhsbsaGPG::gpg_class$df_hrrate_afc$period))
        ),
        nhs_selectInput(
          inputId = ns("factor"),
          label = "Report by",
          choices = c("AfC band" = "afc_band",
                      "Directorate" = "directorate"),
          full_width = TRUE,
          selected = "AfC band"
        ),
        nhs_selectInput(
          inputId = ns("stats"),
          label = "Gender pay gap by",
          choices = c("Mean" = "mean_paygap",
                      "Median" = "median_paygap"),
          full_width = TRUE,
          selected = "Mean"
        )
      ),
      br(),
      # chart 4: Pay gap by period, AfC, directorate
      highcharter::highchartOutput(
        outputId = ns("paygap_afc_directorate"),
        height = "400px"
      )

    )

  )
}

#' paygap Server Functions
#'
#' @noRd
mod_paygap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_paygap <- nhsbsaGPG::paygap |>
      dplyr::mutate(
        mean_paygap = round(mean_paygap, 1),
        median_paygap = round(median_paygap, 1)
      )

    output$paygap_all_mean <- highcharter::renderHighchart({
      gpg_column(
        x = df_paygap,
        xvar = "period",
        yvar = "mean_paygap",
        yaxis_title = "Mean gender pay gap (%)"
      )
    })

    output$paygap_all_median <- highcharter::renderHighchart({
      gpg_column(
        x = df_paygap,
        xvar = "period",
        yvar = "median_paygap",
        yaxis_title = "Median gender pay gap (%)"
      )
    })

    df_paygap_afc_directorate <- reactive({
      req(input$period)
      req(input$factor)
      req(input$stats)

      if (input$factor == "afc_band") {
        nhsbsaGPG::gpg_class$df_hrrate_afc |>
          dplyr::mutate(mean_paygap = round(mean_paygap, 1),
                        median_paygap = round(median_paygap, 1)) |>
          dplyr::filter(period == input$period) |>
          dplyr::select(period, afc_band, input$stats)

      }else {
        nhsbsaGPG::gpg_class$df_hrrate_dir |>
          dplyr::mutate(mean_paygap = round(mean_paygap, 1),
                        median_paygap = round(median_paygap, 1)) |>
          dplyr::filter(period == input$period) |>
          dplyr::select(period, directorate, input$stats)
      }
    })

    output$paygap_afc_directorate <- highcharter::renderHighchart({
      gpg_bar(
        x = df_paygap_afc_directorate(),
        xvar = input$factor,
        yvar = input$stats,
        yaxis_title = switch(input$stats,
                             "mean_paygap" = "Mean gender pay gap (%)",
                             "median_paygap" = "Median gender pay gap (%)")
      )
    })
  })
}
