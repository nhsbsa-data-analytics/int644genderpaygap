#' action UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_action_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/markdown/05_action.md")

  )
}

#' action Server Functions
#'
#' @noRd
mod_action_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
