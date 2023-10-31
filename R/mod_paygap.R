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
      
    )

  )
}

#' paygap Server Functions
#'
#' @noRd
mod_paygap_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
