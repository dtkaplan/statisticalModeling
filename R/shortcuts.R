# Helper function to build the specific types
.ffactory <- function(type, details=NULL) {
  res <-
    function(data=NULL, formula=NULL, add=FALSE, verbose = TRUE, system = "ggplot2", ...) {
    data_name <- as.character(substitute(data))

    # Eventually, test whether <system> is ggplot2 and translate accordingly

    gg_command_string <-
      formula_to_gg(data=data, formula=formula, add=add,
                    geom=type, .use_name = data_name,
                    details = details, ...)
    if (verbose) cat(gsub("+", "+\n", gg_command_string, fixed = TRUE), "\n")
    eval(parse(text = gg_command_string))
    }

  res
}


#' Shortcuts for formula_gg
#'
#' These are intended for user convenience, just a small set of plots.
#'
#'
#'
#' @rdname fchart

#' @param data A data frame with the variables to be plotted
#' @param formula A formula describing the x and y (if any) variables and other aesthetics in
#' a form like \code{y ~ x + color:red + shape:sex + alpha:0.5}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param system Which graphics system to use, e.g. ggplot2, and so on.
#' @param ... Other arguments such as \code{position="dodge"},
#' @rdname fchart
#' @export
fframe <- .ffactory(type = "blank")
#' @rdname fchart
#' @export
fpoint <- .ffactory(type = "point")
#' @rdname fchart
#' @export
fline <- .ffactory(type = "line")
#' @rdname fchart
#' @export
fdensity <- .ffactory(type = "density")
#' @rdname fchart
#' @export
fboxplot <- .ffactory(type = "boxplot")
#' @rdname fchart
#' @export
fhistogram <- .ffactory(type = "histogram")
# Separate functions for a count-type bar chart and a value-based bar chart.
#' @rdname fchart
#' @export
fcounts <- .ffactory(type = "bar", details = list(stat = '"count"'))
#' @rdname fchart
#' @export
fbar <- .ffactory(type = "bar", details = list(stat = '"identity"'))

