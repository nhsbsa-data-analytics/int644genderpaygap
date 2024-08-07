#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/app/www/assets/markdown/06_summary.md")
  )
}

#' introduction Server Functions
#'
#' @noRd
mod_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
