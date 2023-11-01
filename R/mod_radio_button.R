#' radio_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label The label of the radio button group.
#' @param choices Choices for the radio buttons.
#' @param selected The default selected choice.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_radio_button_ui <- function(id, label = "", choices = c(), selected = NULL) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "position:absolute; bottom:10px; left:20px; z-index: 9999;",
      tags$div(
        accessible_radio_button(
          inputId = ns("toggle"),
          label = label,
          inline = TRUE,
          choices = choices,
          selected = selected
        )
      )
    )
  )
}

#' radio_button Server Functions
#'
#' @noRd
mod_radio_button_server <- function(id, filename, export_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    return(reactive({
      input$toggle
    }))
  })
}
