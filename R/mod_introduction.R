#' introduction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_introduction_ui <- function(id) {

  ns <- NS(id)
  tagList(

    includeMarkdown("inst/markdown/01_introduction.md")

  )
}

#' introduction Server Functions
#'
#' @noRd
mod_introduction_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


  })
}
