#' Simple graphical user interface helper for ggplot2
#'
#' Based on gagdets
#'
#' @param data a data frame ready for graphing
#' @param formula a formula to set the frame initially
#' @param ... specify additional data tables for layers
#' @examples
#' \dontrun{gghelper(wage ~ age, data = CPS85)
#' gghelper( ~ wage, data = CPS85) # for a density plot
#' gghelper(data = CPS85) # for a map}
#' @import shiny
#' @import miniUI
#' @import ggmap
#' @import ggplot2
#'
#' @export
gghelper <- function(data = NULL, formula=NULL, ...) {
  requireNamespace("shiny")
  requireNamespace("miniUI")
  vars <- head(names(data), 20)
  categorical_vars <- vars[unlist(lapply(data, FUN = function(x){ !inherits(x, "numeric")}))]
  formula_vars <- all.vars(formula)
  data_table_name <- unlist(as.character(substitute(data)))
  other_data_tables <- lazyeval::lazy_dots(...)
  # All the data tables given to the function
  all_data_table_names <-
    as.character(c(data_table_name,
      unlist(lapply(other_data_tables, FUN=function(x) x$expr ))))


  # Make this about whether to do a density, a scatter, or a map.
  if (missing(formula)) { # it's a map
    plot_mode <- "map"
  } else if (length(formula) == 2) { # it's a scatterplot
    x_start <- formula_vars[1]
    plot_mode <- "density"
  } else {
    y_start <- formula_vars[1]
    x_start <- formula_vars[2]
    plot_mode <- "scatter"
  }

  ui <- miniPage(
    tags$style(type='text/css',
               ".select-input {padding-right: 0px; padding-left: 0px; padding-top: 0px; font-size: 10px; line-height: 10px;} .selectize-dropdown { font-size: 9px; line-height: 9px; }"),

    gadgetTitleBar("ggplot helper"),
    miniTabstripPanel(
      tabPanel("Frame", icon = icon("square-o"), # "object-group"
               fillRow(flex = c(1,2),
                       fillCol(
                         fillRow(
                           div(style="display:inline-block",
                               tags$small(
                                 selectInput(inputId="xframe", label="x-axis", choices = ".", selectize = TRUE, width = "100px"))),
                           div(style="display:inline-block",
                               tags$small(selectInput(inputId="yframe", label="y-axis", choices = ".", selectize = TRUE, width = "100px")))
                         ),
                         div(style="display:inline-block",
                             tags$small(selectInput(inputId="facet", label="facet by", choices = ".", selectize = TRUE, width = "100px"))),
                         div(style="display:inline-block",
                             tags$small(selectInput(inputId="logaxes", label="log axes", choices = ".", selectize = TRUE, width = "100px"))),
                         div(style="display:inline-block",
                             tags$small(selectInput(inputId="color", label="color", choices = ".", selectize = TRUE, width = "100px"))),tags$hr(),
                         div(style="display:inline-block",
                             tags$small(selectInput(inputId="legend", label="legend position", choices = ".", selectize = TRUE, width = "100px")))
                         # wellPanel(
                         #   HTML(frame_controls)
                         # )
                       ),
                       plotOutput("ggframe", height="90%", width="90%")
               )
      ),
      tabPanel("Scatter", icon = icon("line-chart"),
               fillRow(flex = c(1,2),
                       fillCol(checkboxInput("scatter_go",
                                             "Activate scatter layer",
                                             value = plot_mode == "scatter"),
                               wellPanel(HTML(scatter_controls))),
                       plotOutput("ggscatter", height="90%", width="90%")
               )
      ),
      tabPanel("Density", icon = icon("bar-chart"),
               fillRow(flex = c(1,2),
                       fillCol(checkboxInput("density_go",
                                             "Activate density layer",
                                             value = plot_mode == "density"),
                               selectInput("density_type", "Type",
                                           choices = c(density = "density",
                                                       # note: just tail part of name of e.g. geom_density
                                                       histogram = "histogram",
                                                       polygon = "freqpoly")),
                               selectInput("density_position", "Position",
                                           choices = c(stack = "stack", overlap = "dodge", conditional = "fill")),
                               selectInput("density_color", "Color",
                                           choices = c("black", "red", "green", "blue", categorical_vars), selected="black"),
                               checkboxInput("density_fill", "Fill", value = TRUE)
                       ),
                       plotOutput("ggdensity", height="90%", width="90%")
               )
      ),
      tabPanel("Map", icon = icon("map-o"),
               fillRow(flex = c(1,2),
                       fillCol(checkboxInput("map_go",
                                             "Activate map layer",
                                             value = plot_mode == "map"),
                               #HTML(map_controls),
                               textInput("map_location", "Location", value = ""),
                               selectInput("map_source", "Choose a map source:",
                                           choices = list("None", "stamen", "google", "osm"), selected = ""),
                               selectInput("map_type", "Map type:", choices = ""),
                               sliderInput("map_zoom", "Scale", min = 1, max = 21, value = 10),
                               p("Scale: 3 (continent) to 21 (building).")
                               ),
                       plotOutput("ggmap", height="90%", width="90%")
               )
      )
    )
  )


  server <- function(input, output, session) {
    # set the y-axis depending on the mode of plot

    if(plot_mode == "density") {
      y_possibilities <- "..Computed.."
      x_possibilities <- vars
      y_selected <- NULL
      x_selected <- x_start
    } else if (plot_mode == "map") {
      y_possibilities <- "..Latitude.."
      x_possibilities <- "..Longitude.."
      y_selected <- NULL # first item on the list
      x_selected <- NULL # first item on the list
    } else { # plot_mode is "scatter"
      y_possibilities <- vars
      x_possibilities <- vars
      y_selected <- y_start
      x_selected <- x_start
    }

    updateSelectInput(session = session, "yframe",
                      choices = y_possibilities, selected=y_selected)
    updateSelectInput(session = session, "xframe",
                      choices = x_possibilities, selected=x_selected)





    updateSelectInput(session = session, "logaxes",
                      choices = c("none", "x", "y", "both"))
    updateSelectInput(session = session, "model",
                      choices = c("none", "linear", "smoother", "linear+bands", "smoother+bands"))
    updateSelectInput(session = session, "color",
                      choices = c("black", "red", "green", "blue", vars), selected="black")
    updateSelectInput(session = session, "shape",
                      choices = add_NA(categorical_vars), selected = "")
    updateSelectInput(session = session, "facet",
                      choices = add_NA(categorical_vars), selected = "")
    updateSelectInput(session = session, "legend",
                      choices = c(none="", top="top", left="left", right="right"),
                      selected = "right")
    updateSelectInput(session = session, "scatterglyph",
                      choices = c("point", "line", "path", "boxplot")
                      )
    output$ggframe <- renderPlot({
      if (input$xframe == '.') NULL # skip initialization step
      make_whole_plot()
    })

    output$ggscatter <- renderPlot({
      if (input$xframe == '.') NULL # skip initialization step
      make_whole_plot()
    })

    output$ggdensity <- renderPlot({
      if (input$xframe == '.') NULL # skip initialization step
      make_whole_plot()
    })

    output$ggmap <- renderPlot({
      if (input$xframe == '.') NULL # skip initialization step
      make_whole_plot()
    })

    string_for_frame <- reactive({
      req(input$xframe, input$yframe)
      frame_string(data_table_name, input$xframe, input$yframe)
    })

    string_for_density <- reactive({
      req(input$density_type, input$xframe, input$density_position, input$density_color, input$density_fill)
      res <- density_string(data_table_name,
                            names(data),
                            input$density_type,
                            input$xframe,
                            input$density_position,
                            input$density_color,
                            ifelse(input$density_fill, input$density_color, ""))
      res

    })

    string_for_map <- reactive({
      req(input$map_location, input$map_zoom, input$map_source, input$map_type)
      map_string(input$map_location, as.integer(input$map_zoom), input$map_source, input$map_type)
    })

    string_for_scatter_layer <- reactive({
      res <- layer_string(names(data), geom = input$scatterglyph, color = input$color,
                   shape = input$shape)
      paste0("+ ",res)
    })

    string_for_facets <- reactive({
      facet_string(input$facet)
    })

    string_for_legend_position <- reactive({
      legend_position_string(input$legend)
    })

    string_for_log_axes <- reactive({
      log_axes_string(input$logaxes)
    })

    string_for_model <- reactive({
      model_string(input$model)
    })

    ggplot_string <- reactive({
      frame_str <- if (input$map_go) string_for_map()
      else if (input$density_go) string_for_density()
      else string_for_frame()
      res <- paste(frame_str,
                   ifelse(input$scatter_go, string_for_scatter_layer(), ""),
                   # ifelse(input$density_go, string_for_density(), ""),
                   string_for_facets(),
                   string_for_legend_position(),
                   string_for_log_axes(),
                   ifelse(input$scatter_go, string_for_model(), "")
      )
      res
    })

    make_whole_plot <- reactive({
      if (input$xframe == ".") return(NULL) # initialization dodge

      eval(parse(text = ggplot_string()))
    })

    # To choose from different maps
    observe({
      stamen <- list("terrain", "toner", "watercolor")
      google <- list ("roadmap", "terrain", "satellite", "hybrid")

      relevant <- switch(input$map_source,
                         "stamen" = stamen,
                         "google" = google
      )

      updateSelectInput(session, inputId = "map_type",
                        choices = relevant
      )
    })

    # When the Done button is clicked, return a value
    observeEvent(input$done, {
      returnValue <- ggplot_string()
      returnValue <- gsub("data = data",
           paste0("data = ", data_table_name),
           returnValue)
      stopApp(returnValue)
    })
  }

  runGadget(ui, server)
}

# initial value for xframe and yframe is "."
frame_controls <- '<table border="0px">
<td><label for="xframe">x</label></td>
<td><select id="xframe"><option value = "." selected>A</option></select>
<script type="application/json" data-for="xframe" data-nonempty="">{}</script></td></tr>
</tr><tr>
<td><label for="yframe">y</label></td>
<td><select id="yframe"><option value = "." selected>A</option></select>
<script type="application/json" data-for="yframe" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="facet">facet</label></td>
<td><select id="facet"><option value = "." selected>A</option></select>
<script type="application/json" data-for="facet" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="logaxes">log axes</label></td>
<td><select id="logaxes"><option value = "." selected>A</option></select>
<script type="application/json" data-for="logaxes" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="legend">legend</label></td>
<td><select id="legend"><option value = "A" selected>A</option></select>
<script type="application/json" data-for="legend" data-nonempty="">{}</script></td>
</tr></table>'

scatter_controls <- '<table><tr>
<td><label for="model">model</label></td>
<td><select id="model"><option value = "A" selected>A</option></select>
<script type="application/json" data-for="model" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="color">color</label></td>
<td><select id="color"><option value = "A" selected>A</option></select>
<script type="application/json" data-for="color" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="shape">shape</label></td>
<td><select id="shape"><option value = "A" selected>A</option></select>
<script type="application/json" data-for="shape" data-nonempty="">{}</script></td>
</tr></table>
<td><label for="glyph">scatterglyph</label></td>
<td><select id="scatterglyph"><option value = "point" selected>point</option></select>
<script type="application/json" data-for="scatterglyph" data-nonempty="">{}</script></td>
</tr>
</table>'


