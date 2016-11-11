#' gf_ plotting functions
#'
#' These functions provide a formula interface to \code{ggplot2} and 
#' various geoms. For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and mosaic notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#' 
#' @rdname gf_functions
#' 
#' @aliases gf_abline gf_bar gf_boxplot gf_counts gf_density 
#' @aliases gf_density_2d gf_frame gf_freqpoly gf_hex gf_histogram 
#' @aliases gf_hline gf_jitter gf_line gf_path gf_point gf_text

#'
#' @param placeholder Ignore this argument. See details.
#' @param data A data frame with the variables to be plotted
#' @param formula A formula describing the x and y (if any) variables and other aesthetics in
#' a form like \code{y ~ x + color:red + shape:sex + alpha:0.5}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
# Not current: @param system Which graphics system to use, e.g. ggplot2, and so on.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"},
#' 
#' @details These \code{gf_} functions are written to interact with ggplot objects. The \code{placeholder} 
#' argument is part of this interaction system; the end user can ignore it.
#' 
#' @examples 
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars)
#' gf_line(mpg ~ hp + group:cyl, data = mtcars) + facet_grid(~ am)
#' gf_histogram(~ Sepal.Length + fill:Species, data = iris)
#' gf_text(Sepal.Length ~ Sepal.Width + label:Species + color:Species , data = iris)
#' @rdname gf_functions
#' @aliases gf_frame
#' @export
gf_frame <- gf_factory(type = "blank")
# see the other gf_ functions in file gf_function_helpers.R
