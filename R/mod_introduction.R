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

    h2_tabstop("Introduction"),
    p(
      "Under the Equality Act 2010 (Specific Duties and Public Authorities) 
      Regulations 2017, the NHS Business Services Authority (NHSBSA), 
      along with all public bodies with more than 250 employees, 
      is required to publish gender pay gap information by 30th March each year. 
      This includes information on the mean and median gender gaps in hourly pay, 
      the mean and median gender gaps in bonus pay, 
      the proportion of men and women who received bonuses, 
      and the proportions of male and female employees in each pay quartile."
    ),
    p(
      "The gender pay gap shows the difference in the average pay between 
      all men and women in an organisation. It is different to equal pay, 
      which examines the pay differences between men and women who carry out 
      the same or similar jobs, or work of equal value. It is unlawful to pay 
      people unequally because they are a man or a woman."
    ),
    p(
      "At the NHSBSA, our people are at the centre of our business strategy, 
      and we aspire to be an employer of choice who provides a great place 
      to work and can recruit and retain the right talent with the wide 
      range of knowledge, skills and capabilities we need. We are committed 
      to a diverse and inclusive culture which supports the fair treatment 
      and reward of all colleagues, irrespective of gender, and our pay 
      framework is based on the principles of fairness, transparency, 
      and consistency"
    ),
    p(
      span("This report fulfils our reporting requirements and sets out what we are 
      doing to address the gender pay gap in our organisation. The data is based on a 
      snapshot of all employees as of 31 March ",
           textOutput(ns("reporting_year"), inline = TRUE), style = "font-size:15pt"),
      span(", as this is the date which all public authorities must use each year. 
      The calculations used are those set out in the relevant legislation 
      to ensure the data is calculated consistently across organisations.",
           style = "font-size:15pt")
    )
  )
}

#' introduction Server Functions
#'
#' @noRd
mod_introduction_server <- function(id) {

  moduleServer(id, function(input, output, session) {

    ns <- session$ns


    output$reporting_year <- renderText({
      nhsbsaGPG::gpg_data(afc_staff)$ending_fy
    })


  })
}
