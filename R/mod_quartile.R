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
    includeMarkdown("inst/markdown/04_quartile_1.md"),
    nhs_card_tabstop(
      nhs_selectInput(
        inputId = ns("period"),
        label = "Reporting period",
        choices = unique(nhsbsaGPG::quartile$period),
        full_width = TRUE,
        selected = max(unique(nhsbsaGPG::quartile$period))
      ),
      br(),
      highcharter::highchartOutput(
        outputId = ns("quartile"),
        height = "500px"
      ),
      mod_nhs_download_ui(id = ns("download_quartile"))
    )
  )
}
    
#' quartile Server Functions
#'
#' @noRd 
mod_quartile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    df_quartile <- reactive({
      req(input$period)
      
      nhsbsaGPG::quartile |> 
        dplyr::filter(period == input$period) 
    })
    
    output$quartile <- highcharter::renderHighchart({
      plt <- gpg_stack(x = df_quartile() ,
                xvar = "quartile",
                yvar = "percent",
                groupvar = "gender",
                yaxis_title = "% of men and women in each pay quartile")
      
    })
  
  })
}
    

    
