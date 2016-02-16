#' Formula interface to ggplot2
#'
#' Provides a model formula interface to ggplot2
#'
#' @details Use \code{formula_gg()} to create the plot. \code{formula_to_gg()} translates from
#' formula notation to an equivalent ggplot2 command.
#'
#' The formula will, generically, look like
#' y ~ x for 2-variable graphics, and
#' ~ x for 1-variable graphics (e.g. density, histogram)
#' For both 2- and 1-variable graphics, you can add terms to the formula to handle other aesthetics
#' y ~ x + color:blue + shape:age + alpha:0.5
#' Names like <blue> that are not variables in the data frame will be
#' set to the corresponding attribute.
#' Variables and words (e.g. <blue> in color:blue) should **not** be quoted
#' in the formulas
#'
#' Some geoms take additional arguments, like \code{bins = } in histograms.  Add those arguments
#' as you would in the geom.
#' @return A character string containing the ggplot2 command equivalent of the formula
#'
#' @examples
#' data(CPS85, package = "mosaicData")
#' require(ggplot2)
#' formula_gg(wage ~ age, data = CPS85)
#' formula_gg(wage ~ age + color:sex + size:3, data = CPS85) + facet_wrap(~ sector)
#' formula_gg(wage ~ age, data = CPS85, position = "jitter")
#' formula_gg( ~ age + color:married + fill:married + alpha:0.5, data = CPS85,
#'               geom = "histogram", bins = 10)
#' @param data a data frame
#' @param formula a formula constructed from the variables in the data frame
#' @param add if TRUE, create just the layer
#' @param geom the name of the geom, e.g. "point", "line", "path". You don't need to use this.
#' @param ... additional arguments for the ggplot2 geom, e.g. \code{bins = 10} for histogram

#' @rdname formula_gg
#' @export
formula_to_gg <- function(data=NULL, formula=NULL, add=FALSE, geom = NULL, ...) {
  if (is.null(formula)) stop("Must provide a graphing formula, e.g. y ~ x")
  if (is.null(data)) stop("Must provide a data frame for graphing")
  data_name <- as.character(substitute(data))
  extras <- list(...)

  # Pull out the frame and aesthetic info from the formula
  # and put it in list form suitable for ggstring functions
  nms <- all.vars(formula, unique = FALSE)

  # generate the frame.
  # Default geoms are density (1-var) and point (2-vars)
  for_frame <-
    if (length(formula) == 2) {
      if (is.null(geom)) geom <- "density"
      frame_string(data_name, nms[1])
    } else {
      if (is.null(geom)) geom <- "point"
      frame_string(data_name, nms[1], nms[2])
    }

  extras <- capture_extras(...) # get the extra arguments (if any) to the geom

  aes_and_set_list <- pairs_in_formula(formula) # get the pairs (e.g. color:red)

  for_layer_fun <-
    function(...) layer_string(names(data), geom = geom, extras = extras, ...)
  for_layer <- do.call(for_layer_fun, aes_and_set_list)

  gg_command_string <-
    if ( ! add) {
      paste(for_frame, for_layer, sep=" +  ")
    } else {
      for_layer
    }

  gg_command_string
}

#' @rdname formula_gg
#' @return The ggplot object itself
#' @export
formula_gg <- function(data=NULL, formula=NULL, add=FALSE, geom = NULL, ...) {
  gg_command_string <- formula_to_gg(data=data, formula=formula, add=add, geom=geom, ...)
  p <- eval(parse(text = gg_command_string))

  p
}

