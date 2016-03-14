
runApp(list(
  ui = bootstrapPage(

    div(style="display:inline-block",
        tags$small(
          selectInput(inputId="xlimitsmin", label="x-min", width = "100px",
                   choices = c("Short", "A_really_long_variable_name"), selected="Short", multiple = TRUE))),
    div(style="display:inline-block",
        tags$small(selectInput(inputId="xlimitsmax", label="x-max", choices = 1:5, selectize = TRUE, width = "100px")))
  ),
  server = function(input, output) {}
))

smallSelectInput <- function (inputId, label, choices, selected = NULL, multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
{
  choices <- shiny:::choicesWithNames(choices)
  if (is.null(selected)) {
    if (!multiple)
      selected <- shiny:::firstChoice(choices)
  }
  else selected <- shiny:::validateSelected(selected, choices, inputId)
  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }
  selectTag <- tags$select(id = inputId, class = if (!selectize)
    "form-control", size = size, shiny:::selectOptions(choices, selected))
  if (multiple)
    selectTag$attribs$multiple <- "multiple"
  res <- div(class = "form-group shiny-input-container", style = if (!is.null(width))
    paste0("width: ", shiny::validateCssUnit(width), "; height: 200px"), shiny:::controlLabel(inputId,
                                                                 label), div(selectTag))
  if (!selectize)
    return(res)
  shiny:::selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in%
                                                              choices))
}

runApp(shinyApp(
  ui = fluidPage(
    tags$style(type='text/css', ".select-input {padding-right: 0px; padding-left: 0px; padding-top: 0px; font-size: 10px; line-height: 10px;} .selectize-dropdown { font-size: 9px; line-height: 9px; }"),
    tags$style(type='text/css', "btn-mini {height: 100px; line-height: 44px; margin-left: -20px;}"),
    tags$style(type='text/css', ".selectize {font-color: red;"),
        tags$small(smallSelectInput("test","Test", 1:5, width = "200px"))
  ),
  server = function(input, output, session) {
  }
))
