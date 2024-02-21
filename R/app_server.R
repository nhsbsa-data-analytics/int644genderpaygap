#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_headcount_server("headcount")
  mod_paygap_server("paygap")
  mod_quartile_server("quartile")
  mod_action_server("action")

  observeEvent(input$content_main, {
    tag <- switch(input$content_main,
      "Introduction" = "introduction",
      "Gender profile" = "headcount",
      "Gender pay gap" = "paygap",
      "Pay quartile" = "quartile",
      "Action" = "gpg_action",
      "Summary" = "summary"
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

    updateSelectInput(session, inputId = "content_main", selected = "Introduction")
  })


  observeEvent(input$top_paygap_intro, {
    # jump to the top of the screen
    shinyjs::runjs("window.scrollTo(0, 0)")

    updateSelectInput(session, inputId = "content_main", selected = "Introduction")
  })

  observeEvent(input$top_quartile_intro, {
    # jump to the top of the screen
    shinyjs::runjs("window.scrollTo(0, 0)")

    updateSelectInput(session, inputId = "content_main", selected = "Introduction")
  })

  observeEvent(input$top_action_intro, {
    # jump to the top of the screen
    shinyjs::runjs("window.scrollTo(0, 0)")

    updateSelectInput(session, inputId = "content_main", selected = "Introduction")
  })

  observeEvent(input$top_action_summary, {
    # jump to the top of the screen
    shinyjs::runjs("window.scrollTo(0, 0)")

    updateSelectInput(session, inputId = "content_main", selected = "Introduction")
  })

}
