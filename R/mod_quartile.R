#' quartile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quartile_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown("inst/markdown/04_quartile_1.md")
 
  )
}
    
#' quartile Server Functions
#'
#' @noRd 
mod_quartile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    

    

