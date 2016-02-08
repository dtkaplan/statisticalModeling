#' Simple graphical user interface helper for ggplot2
#'
#' Based on gagdets
#'
#' @param data a data frame ready for graphing
#' @param formula a formula to set the frame initially
#' @export
gghelper <- function(data = NULL, formula=NULL) {
  vars <- head(names(data), 20)
  formula_vars <- all.vars(formula)
  data_table_name <- as.character(substitute(data))

  if (length(formula) == 2) {
    xstart <- formula_vars[1]
  } else {
    ystart <- formula_vars[1]
    xstart <- formula_vars[2]
  }

  ui <- miniPage(
    gadgetTitleBar("ggplot helper"),
    miniTabstripPanel(
     tabPanel("Frame", icon = icon("area-chart"),
              fillRow(flex = c(1,2),
                fillCol(wellPanel(HTML(frame_controls))),
                plotOutput("ggframe", height="90%", width="90%")
              )

     ),
      tabPanel("geom_point", icon = icon("sliders"),
                   miniContentPanel(

                     sliderInput("year", "Year", 1978, 2010, c(2000, 2010), sep = "")
                   )
      )
    )
  )

  server <- function(input, output, session) {
    updateSelectInput(session = session, "xframe", choices = vars, selected=xstart)
    updateSelectInput(session = session, "yframe", choices = vars, selected=ystart)
    updateSelectInput(session = session, "color",
                      choices = c("black", "red", "green", "blue", vars), selected="black")
    updateSelectInput(session = session, "glyph",
                      choices = c(point = "geom_point", line = "geom_line")
                      )
    output$ggframe <- renderPlot({
      if (input$xframe == '.') NULL
      else eval(parse(text = ggplot_string() ))
    })

    ggplot_string <- reactive({
      res <- paste0("ggplot(data = data, aes(x=",
             input$xframe, ", y=", input$yframe, ")) + ", input$glyph, "(color ='",
             input$color, "')"  )

      res
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
<caption>Frame aesthetics</caption>
<tr>
<td><label for="xframe">x</label></td>
<td><select id="xframe"><option value = "." selected>A</option></select>
<script type="application/json" data-for="xframe" data-nonempty="">{}</script></td></tr>
</tr><tr>
<td><label for="yframe">y</label></td>
<td><select id="yframe"><option value = "." selected>A</option></select>
<script type="application/json" data-for="yframe" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="color">color</label></td>
<td><select id="color"><option value = "A" selected>A</option></select>
<script type="application/json" data-for="color" data-nonempty="">{}</script></td>
</tr><tr>
<td><label for="glyph">glyph</label></td>
<td><select id="glyph"><option value = "A" selected>A</option></select>
<script type="application/json" data-for="glyph" data-nonempty="">{}</script></td>
</tr>
</table>'
