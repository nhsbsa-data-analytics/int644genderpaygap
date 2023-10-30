#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_headcount_server("headcount_1")


  observeEvent(input$content_main, {
    tag <- switch(input$content_main,
      "Introduction" = "introduction",
      "Gender profile" = "headcount",
      "Mean/Median hourly pay" = "hourly_rate"
    )

    if (input$content_main == "Introduction") {
      shinyjs::runjs("window.scrollTo(0, 0);")
    } else {
      shinyjs::runjs(paste0("document.getElementById('", tag, "').scrollIntoView();"))
    }
  })

  observeEvent(input$top_headcount_intro, {
    # jump to the top of the screen
    shinyjs::runjs("window.scrollTo(0, 0)")

    updateSelectInput(session,
                      inputId = "content_main",
                      selected = "Introduction")
  })

  observeEvent(input$top_hourly_rate_intro, {
    # jump to the top of the screen
    shinyjs::runjs("window.scrollTo(0, 0)")

    updateSelectInput(session,
                      inputId = "content_main",
                      selected = "Introduction")
  })

  mod_hourly_rate_server("hourly_rate_1")

}
