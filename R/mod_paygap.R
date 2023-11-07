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
    includeMarkdown("inst/app/www/assets/markdown/03_gender_pay_gap_1.md"),
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
    includeMarkdown("inst/app/www/assets/markdown/03_gender_pay_gap_2.md"),
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
          label = "Filter by",
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
      ),
      mod_nhs_download_ui(id = ns("download_paygap_afc_directorate"))
    )

  )
}

#' paygap Server Functions
#'
#' @noRd
mod_paygap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df_paygap_all <- nhsbsaGPG::gpg_class$df_hrrate |>
      dplyr::mutate(
        dplyr::mutate(dplyr::across(where(is.double), \(x) round(x, digits = 1)))
      )

    output$paygap_all_mean <- highcharter::renderHighchart({
      plt <- gpg_column(
        x = df_paygap_all,
        xvar = "period",
        yvar = "mean_paygap",
        yaxis_title = "Mean gender pay gap (%)"
      )

      plt |>
        highcharter::hc_tooltip(
          headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
          pointFormat = '<span style="color:{point.color}">
          \u25CF</span> {series.name}: <b>{point.y} %</b><br/>
          \u25CF</span> Mean men per hour: £{point.mean_rate_men} <br/>
          \u25CF</span> Mean women per hour: £{point.mean_rate_women}',
          footerFormat = ""
        )
    })

    output$paygap_all_median <- highcharter::renderHighchart({
      plt <- gpg_column(
        x = df_paygap_all,
        xvar = "period",
        yvar = "median_paygap",
        yaxis_title = "Median gender pay gap (%)"
      )

      plt |>
        highcharter::hc_tooltip(
          headerFormat = '<span style="font-size: 10px">{point.key}</span><br/>',
          pointFormat = '<span style="color:{point.color}">
          \u25CF</span> {series.name}: <b>{point.y} %</b><br/>
          \u25CF</span> Median men per hour: £{point.median_rate_men} <br/>
          \u25CF</span> Median women per hour: £{point.median_rate_women}',
          footerFormat = ""
        )
    })





    df_paygap <- reactive({
      req(input$period)
      req(input$factor)

      if (input$factor == "afc_band") {
        nhsbsaGPG::gpg_class$df_hrrate_afc |>
          dplyr::mutate(tooltip_text = .data[[input$factor]]) |>
          dplyr::filter(period == input$period)

      }else {
        nhsbsaGPG::gpg_class$df_hrrate_dir |>
          dplyr::mutate(tooltip_text = .data[[input$factor]]) |>
          dplyr::filter(period == input$period)
      }
    })


    df_paygap_afc_directorate <- reactive({
      req(input$stats)

      df <- if (input$stats == "mean_paygap") {
        df_paygap() |>
          dplyr::select(-dplyr::starts_with("median")) |>
          dplyr::rename(rate_women = mean_rate_women,
                        rate_men = mean_rate_men) |>
          dplyr::mutate(paygap = mean_paygap)

      }else {
        df_paygap() |>
          dplyr::select(-dplyr::starts_with("mean")) |>
          dplyr::rename(rate_women = median_rate_women,
                        rate_men = median_rate_men) |>
          dplyr::mutate(paygap = median_paygap)

      }

      df_paygap_afc_directorate <- df |>
        dplyr::filter(rowSums(is.na(df)) == 0)

    })


    # tooltip heading
    tooltip_title <- reactive({
      req(input$stats)

      lbl <- switch(input$stats,
        "mean_paygap" = "Mean pay gap of",
        "median_paygap" = "Median pay gap of"
      )
    })



    output$paygap_afc_directorate <- highcharter::renderHighchart({
      plt <- gpg_dumbbell(
        x = df_paygap_afc_directorate(),
        low = "rate_women",
        high = "rate_men",
        xaxis_category = input$factor,
        yaxis_title = switch(input$stats,
                             "mean_paygap" = "Mean hour pay (£)",
                             "median_paygap" = "Median hour pay (£)")
      )

      plt |>
        highcharter::hc_tooltip(
          headerFormat = "",
          positioner = htmlwidgets::JS(
            "function(){return {
                 x:this.chart.chartWidth - (this.label.width)*1.01,
                 y:this.chart.chartHeight*0.2}
            }"
          ),
          pointFormat = paste0(
            "<b>{point.tooltip_text}</b> <br><br>",
            "<b>", tooltip_title(), " {point.paygap:,.1f}%</b><br>",
            "<b>Women:</b> £{point.rate_women:,.1f} per hour  <br>",
            "<b>Men:</b> £{point.rate_men:,.1f} per hour"
          )
        )
    })

    df_paygap_all_download <- df_paygap_all |>
      dplyr::rename(`Reporting period` = period,
                    `Men's mean hourly pay` = mean_rate_men,
                    `Women's mean hourly pay` = mean_rate_women,
                    `Mean gender pay gap (%)` = mean_paygap,
                    `Men's median hourly pay` = median_rate_men,
                    `Women's median hourly pay` = median_rate_women,
                    `Median gender pay gap (%)` = median_paygap)

    # data download - gender pay gap for all 
    mod_nhs_download_server(
      id = "download_paygap_all",
      filename = "gender_pay_gap_overall.csv",
      export_data = df_paygap_all_download
    )
    
    
    
    df_paygap_afc_directorate_download <- dplyr::bind_rows(
      nhsbsaGPG::gpg_class$df_hrrate_afc |>
        dplyr::mutate(breakdown = "AfC band") |> 
        dplyr::rename(sub_breakdown = afc_band),
      nhsbsaGPG::gpg_class$df_hrrate_dir |>
        dplyr::mutate(breakdown = "Directorate") |>
        dplyr::rename(sub_breakdown = directorate)
    ) |>
      dplyr::rename(`Reporting period` = period,
                    Breakdown = breakdown,
                    `Sub breakdown` = sub_breakdown,
                    `Men's mean hourly pay` = mean_rate_men,
                    `Women's mean hourly pay` = mean_rate_women,
                    `Mean gender pay gap (%)` = mean_paygap,
                    `Men's median hourly pay` = median_rate_men,
                    `Women's median hourly pay` = median_rate_women,
                    `Median gender pay gap (%)` = median_paygap
      )
                    
                    
    
    # data download - gender pay gap by AfC, directorate
    mod_nhs_download_server(
      id = "download_paygap_afc_directorate",
      filename = "gender_pay_gap_afc_directorate.csv",
      export_data = df_paygap_afc_directorate_download
    )
    
  })
  
  
  
  
  
}
