#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    shinyjs::useShinyjs(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    tagList(
      tags$html(lang = "en"),
      bootstrapLib(),
      a(id = "skiplink", "Skip to Main Content", href = "#maincontent"),
      nhs_header(),
      br(),
      div(id = "maincontent"),
      div(
        class = "nhsuk-width-container",
        div(
          class = "nhsuk-main-wrapper",
          role = "main",
          fluidRow(
            id = "mainTabs",
            column(width = 10),
            shinyWidgets::pickerInput(
              inputId = "content_main",
              label = "Content:",
              choices = c(
                "Introduction",
                "Gender profile",
                "Gender pay gap",
                "Pay quartile",
                "Action"
              ),
              selected = "Introduction",
              width = "fit",
              inline = TRUE
            ),
            tags$div(id = "introduction"),
            mod_introduction_ui("introduction"),
            hr(),
            br(),
            tags$div(id = "headcount"),
            mod_headcount_ui("headcount"),
            accessible_action_link("top_headcount_intro", "Go to top page"),
            hr(),
            br(),
            tags$div(id = "paygap"),
            mod_paygap_ui("paygap"),
            accessible_action_link("top_paygap_intro", "Go to top page"),
            hr(),
            br(),
            tags$div(id = "quartile"),
            mod_quartile_ui("quartile"),
            accessible_action_link("top_quartile_intro", "Go to top page"),
            hr(),
            br(),
            tags$div(id = "gpg_action")
          )
        )
      )
    ),
    br(),
    nhs_footer()
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  resources <- app_sys("app/www")
  addResourcePath("www", resources)

  tags$head(
    favicon("assets/favicons/favicon"),
    tags$title("NHSBSA gender pay gap report"),

    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

    # Javascript resources
    htmltools::htmlDependency(
      name = "resources",
      version = "0.0.1",
      src = resources,
      script = list.files(resources, pattern = "\\.js$", recursive = TRUE),
      package = NULL,
      all_files = TRUE
    ),
    # CSS resources
    lapply(
      list.files(resources, pattern = "\\.css$", recursive = TRUE),
      function(x) tags$link(href = file.path("www", x), rel = "stylesheet")
    )
  )
}
