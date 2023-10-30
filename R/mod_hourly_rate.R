#' hourly_rate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_hourly_rate_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' hourly_rate Server Functions
#'
#' @noRd 
mod_hourly_rate_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI

    
## To be copied in the server
# mod_hourly_rate_server("hourly_rate_1")
