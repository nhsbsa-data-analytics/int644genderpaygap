#' h1_tabstop
#'
#' @param heading Heading text
#' @inheritDotParams shiny::h1
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' h1_tabstop("Heading")
h1_tabstop <- function(heading, ..., tabindex = 0) {
  # create heading as typical heading plus tabindex attribute
  # ensures heading will be stopped at when pressing keyboard tab
  h1_tabstop <- h1(heading, ...) %>%
    htmltools::tagAppendAttributes(`tabindex` = tabindex)
}

#' h2_tabstop
#'
#' @param heading Heading text
#' @inheritDotParams shiny::h1
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' h2_tabstop("Heading")
h2_tabstop <- function(heading, ..., tabindex = 0) {
  # create heading as typical heading plus tabindex attribute
  # ensures heading will be stopped at when pressing keyboard tab
  h2_tabstop <- h2(heading, ...) %>%
    htmltools::tagAppendAttributes(`tabindex` = tabindex)
}

#' h3_tabstop
#'
#' @param heading Heading text
#' @inheritDotParams shiny::h1
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' h3_tabstop("Heading")
h3_tabstop <- function(heading, ..., tabindex = 0) {
  # create heading as typical heading plus tabindex attribute
  # ensures heading will be stopped at when pressing keyboard tab
  h3_tabstop <- h3(heading, ...) %>%
    htmltools::tagAppendAttributes(`tabindex` = tabindex)
}

#' h4_tabstop
#'
#' @param heading Heading text
#' @inheritDotParams shiny::h1
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' h4_tabstop("Heading")
h4_tabstop <- function(heading, ..., tabindex = 0) {
  # create heading as typical heading plus tabindex attribute
  # ensures heading will be stopped at when pressing keyboard tab
  h4_tabstop <- h4(heading, ...) %>%
    htmltools::tagAppendAttributes(`tabindex` = tabindex)
}

#' h5_tabstop
#'
#' @param heading Heading text
#' @inheritDotParams shiny::h1
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' h5_tabstop("Heading")
h5_tabstop <- function(heading, ..., tabindex = 0) {
  # create heading as typical heading plus tabindex attribute
  # ensures heading will be stopped at when pressing keyboard tab
  h5_tabstop <- h5(heading, ...) %>%
    htmltools::tagAppendAttributes(`tabindex` = tabindex)
}

#' h6_tabstop
#'
#' @param heading Heading text
#' @inheritDotParams shiny::h1
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' h6_tabstop("Heading")
h6_tabstop <- function(heading, ..., tabindex = 0) {
  # create heading as typical heading plus tabindex attribute
  # ensures heading will be stopped at when pressing keyboard tab
  h6_tabstop <- h6(heading, ...) %>%
    htmltools::tagAppendAttributes(`tabindex` = tabindex)
}


#' nhs_card_tabstop
#'
#' @param heading Card title
#' @param ... Card content
#' @param tabindex Number for tabindex, default 0
#'
#' @return HTML
#' @export
#'
#' @examples
#' nhs_card_tabstop("A card", shiny::p("Some content"))
nhs_card_tabstop <- function(heading, ..., tabindex = 0) {
  # create nhs_card as typical nhs_card plus tabindex attribute
  # ensures nhs_card will be stopped at when pressing keyboard tab
  htmltools::tagQuery(nhs_card(heading, ...))$
    find(".nhsuk-card__heading")$
    removeAttr("tabindex")$
    addAttr("tabindex" = tabindex)$
    allTags() # Exclude Linting
}



#' accessible_radio_buttons
#'
#' @inheritParams shiny::radioButtons
#'
#' @return HTML
#' @export
#'
#' @examples
#' accessible_radio_buttons("rb", "Choose one:",
#'                         choiceNames = list(
#'                           "apple",
#'                           "banana",
#'                           "cherry"
#'                         ),
#'                         choiceValues = list(
#'                           "A", "B", "C"
#'                         ))
# Begin Exclude Linting
accessible_radio_buttons <- function(inputId, label, choices = NULL, selected = NULL,
                                     inline = FALSE, width = NULL, choiceNames = NULL,
                                     choiceValues = NULL) {
  # End Exclude Linting
  args <- shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)

  selected <- restoreInput(id = inputId, default = selected)

  selected <- if (is.null(selected)) args$choiceValues[[1]] else as.character(selected)
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")

  options <- shiny:::generateOptions(
    inputId, selected, inline, "radio", args$choiceNames, args$choiceValues
  )

  div_class <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) div_class <- paste(div_class, "shiny-input-container-inline")

  tags$fieldset(
    tags$legend(label),
    tags$div(
      id = inputId,
      style = htmltools::css(width = validateCssUnit(width)),
      class = div_class,
      role = "radiogroup",
      options
    )
  )
}


#' accessible_action_link
#'
#' @inheritParams shiny::actionLink
#'
#' @return HTML
#' @export
#'
#' @examples
#' accessible_action_link("infoLink", "Information Link", class = "btn-info")
accessible_action_link <- function(inputId, label, icon = NULL, ...) { # Exclude Linting
  value <- restoreInput(id = inputId, default = NULL)
  tags$a(id = inputId, href = "#", class = "action-button",
         `data-val` = value, style = "color: #004280; text-decoration: underline;",
         list(shiny:::validateIcon(icon), label),
         ...)
}
