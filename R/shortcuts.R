# Helper function to build the specific types
.ffactory <- function(type) {
  function(data=NULL, formula=NULL, add=FALSE, verbose = TRUE, ...) {
    data_name <- as.character(substitute(data))
    gg_command_string <-
      formula_to_gg(data=data, formula=formula, add=add,
                    geom=type, .use_name = data_name, ...)
    if (verbose) cat(gsub("+", "+\n", gg_command_string, fixed = TRUE), "\n")
    eval(parse(text = gg_command_string))
    }
}


#' Shortcuts for formula_gg
#'
#' These are intended for user convenience, just a small set of plots.
#'
#'
#'
#' @rdname fplot

#' @param data A data frame with the variables to be plotted
#' @param formula A formula describing the x and y (if any) variables and other aesthetics in
#' a form like \code{y ~ x + color:red + shape:sex + alpha:0.5}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param ... Other arguments such as \code{position="dodge"},
#' @rdname fplot
#' @export
fframe <- .ffactory(type = "blank")
#' @rdname fplot
#' @export
fpoint <- .ffactory(type = "point")
#' @rdname fplot
#' @export
fline <- .ffactory(type = "line")
#' @rdname fplot
#' @export
fdensity <- .ffactory(type = "density")
#' @rdname fplot
#' @export
fboxplot <- .ffactory(type = "boxplot")
#' @rdname fplot
#' @export
fhistogram <- .ffactory(type = "histogram")

