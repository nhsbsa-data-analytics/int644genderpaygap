#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_headcount_server("headcount_1")
  
  
  observeEvent(input$nav_main, {
    tag <- switch(input$nav_main,
                  "Introduction" = "introduction",
                  "Gender profile" = "headcount",
                  "Demographics Breakdown" = "demographic_breakdown",
                  "Opt-out Reasons" = "optout_reason",
                  "Geography Breakdown" = "geographic",
                  "Organisation Breakdown" = "vpd"
    )
    
    if(input$nav_main == "Introduction"){
      shinyjs::runjs("window.scrollTo(0, 0);")
    } else {
      # shinyjs::runjs(paste0("location.href='#", tag, "';")) 
      shinyjs::runjs(paste0("document.getElementById('", tag, "').scrollIntoView();")) 
      
    }
  })
}
