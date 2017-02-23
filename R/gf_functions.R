
#' bivariate gf_ plotting functions
#'
#' These functions provide a formula interface to \code{ggplot2} and
#' various geoms. For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and mosaic notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' @rdname gf_functions
#'
#' @param placeholder Ignore this argument. See details.
#' @param data A data frame with the variables to be plotted
#' @param formula A formula describing the x and y variables and other aesthetics in
#' a form like \code{y ~ x + color:red + shape:sex + alpha:0.5}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
# Not current: @param system Which graphics system to use, e.g. ggplot2, and so on.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#'
#' @details These \code{gf_} functions are written to interact with ggplot objects.
#' The \code{placeholder}
#' argument is part of this interaction system; the end user can ignore it.
#'
#' @examples
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars, verbose = TRUE)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars) %>%
#'   gf_abline(~ color:"red" + slope:-0.10 + intercept:35)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars) %>%
#'   gf_abline(color = "red", slope = -0.10, intercept = 35)
#' gf_point(mpg ~ hp + color:cyl + size:wt, data = mtcars) %>%
#'   gf_abline(color = "red", slope = -0.10, intercept = 33:36) %>%
#'   gf_hline(color = "navy", yintercept = c(20, 25)) %>%
#'   gf_vline(color = "brown", xintercept = c(200, 300))
#' # use %>% for gf_* but + when returning to native ggplot functions
#' gf_line(mpg ~ hp + group:cyl, data = mtcars) %>%
#'   gf_point(mpg ~ hp + group:cyl) +
#'   facet_grid(~ am)
#' gf_text(Sepal.Length ~ Sepal.Width + label:Species + color:Species , data = iris)
#'
#' @rdname gf_functions
#' @export
gf_frame <- gf_factory(type = "blank")

#' @rdname gf_functions
#' @export
gf_point <- gf_factory(type = "point")

#' @rdname gf_functions
#' @export
gf_jitter <- gf_factory(type = "jitter")

#' @rdname gf_functions
#' @export
gf_line <- gf_factory(type = "line")

#' @rdname gf_functions
#' @export
gf_path <- gf_factory(type = "path")

#' @rdname gf_functions
#' @export
gf_smooth <- gf_factory(type = "smooth")

#' @rdname gf_functions
#' @export
gf_spline <- gf_factory(type = "spline")

#' @rdname gf_functions
#' @export
gf_rug <- gf_factory(type = "rug")

#' @rdname gf_functions
#' @export
gf_quantile <- gf_factory(type = "quantile")

#' @rdname gf_functions
#' @export
gf_density_2d <- gf_factory(type = "density_2d")

#' @rdname gf_functions
#' @export
gf_density2d <- gf_factory(type = "density2d")

#' @rdname gf_functions
#' @export
gf_contour <- gf_factory(type = "contour")

#' @rdname gf_functions
#' @export
gf_hex <- gf_factory(type = "hex")

#' @rdname gf_functions
#' @export
gf_boxplot <- gf_factory(type = "boxplot")

#' @rdname gf_functions
#' @export
gf_text <- gf_factory(type = "text")

#' @rdname gf_functions
#' @export
gf_label <- gf_factory(type = "label")

#' @rdname gf_functions
#' @export
gf_area <- gf_factory(type = "area")

#' @rdname gf_functions
#' @export
gf_violin <- gf_factory(type = "violin")

#' @rdname gf_functions
#' @export
gf_spoke <- gf_factory(type = "spoke")

#' @rdname gf_functions
#' @export
gf_step <- gf_factory(type = "step")

#' @rdname gf_functions
#' @export
gf_tile <- gf_factory(type = "tile")

#' Univariate gf_ plotting functions
#'
#' These functions provide a formula interface to \code{ggplot2} and
#' various geoms. For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and mosaic notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' @rdname gf_functions1
#'
#' @param placeholder Ignore this argument. See details.
#' @param data A data frame with the variables to be plotted
#' @param formula A formula describing the x variable and other aesthetics in
#' a form like \code{ ~ x + color:red + fill:gray50 + alpha:0.5}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#'
#' @details These \code{gf_} functions are written to interact with ggplot objects.
#' The \code{placeholder}
#' argument is part of this interaction system; the end user can ignore it.
#' @seealso \code{\link{gf_point}()}, \code{\link{gf_abline}()}, \code{\link{gf_pointrange}()}.
#
#' @examples
#' gf_histogram(~ Sepal.Length + fill:Species, data = iris)
#' gf_density(~ Sepal.Length + color:Species, data = iris)
#' gf_dens(~ Sepal.Length + color:Species, data = iris)
# gf_ash(~ Sepal.Length + color:Species, data = iris)
#' gf_freqpoly(~ Sepal.Length + color:Species, data = iris)
#' gf_dotplot(~ Sepal.Length + fill:Species, data = iris)
#' gf_counts(~ substance, data = HELPrct)

# Separate functions for a count-type bar chart and a value-based bar chart.
#' @rdname gf_functions1
#' @export
gf_counts <- gf_factory(type = "bar",
                        extras = list(stat = "count"), aes_form = ~ x)
#' @rdname gf_functions1
#' @export
gf_bar <- gf_factory(type = "bar",
                     extras = list(stat = "identity"), aes_form = ~ x)

#' @rdname gf_functions1
#' @export
gf_freqpoly <- gf_factory(type = "freqpoly", aes_form = ~ x)

# #' @rdname gf_functions1
# #' @export
# gf_ash <- gf_factory(type = "ash", aes_form = ~ x)

#' @rdname gf_functions1
#' @export
gf_density <- gf_factory(type = "density", aes_form = ~ x)

#' @rdname gf_functions1
#' @export
gf_histogram <- gf_factory(type = "histogram", aes_form = ~x)

#' @rdname gf_functions1
#' @export
gf_dotplot <- gf_factory(type = "dotplot", aes_form = ~x)

# modified version of density plot without line along bottom and sides
#' @rdname gf_functions1
#' @export
gf_dens <- gf_factory(type = "line", extras = list(stat = "density"),
                      aes_form = ~ x)

#'
#' @rdname gf_functions1
#' @export
gf_qq <- gf_factory(type = "qq", aes_form = ~ x)

#' Multivariate gf_ plotting functions
#'
#' These functions provide a formula interface to \code{ggplot2} and
#' various geoms. For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and mosaic notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' @rdname gf_functions3
#' @param placeholder Ignore this argument. See details.
#' @param data A data frame with the variables to be plotted
#' @param formula A formula describing the manditory aesthetics and possibly other
#' aesthetics in a form like \code{ y + ymin + ymax ~ x + color:red + fill:gray50 + alpha:0.5}
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#' @seealso \code{\link{gf_point}()}, \code{\link{gf_histogram}()}, \code{\link{gf_abline}()}.
#' @export

#' @rdname gf_functions3
#' @export
gf_curve <- gf_factory(type = "curve", aes_form = y + yend ~ x + xend)

#' @rdname gf_functions3
#' @export
gf_segment <- gf_factory(type = "segment", aes_form = y + yend ~ x + xend)

#' @rdname gf_functions3
#' @export
gf_ribbon <- gf_factory(type = "ribbon", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_linerange <- gf_factory(type = "linerange", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_pointrange <- gf_factory(type = "pointrange", aes_form = y + ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_crossbar <- gf_factory(type = "crossbar", aes_form = y + ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_errorbar <- gf_factory(type = "errorbar", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions3
#' @export
gf_errorbarh <- gf_factory(type = "errorbarh", aes_form = y ~ x + xmin + xmax)

#' @rdname gf_functions3
#' @export
gf_rect <- gf_factory(type = "rect", aes_form = ymin + ymax ~ xmin + xmax)


#' gf_ functions with no formula part
#'
#' These functions provide a formula interface to \code{ggplot2} and
#' various geoms. For plots with just one layer, the formula interface
#' is more compact and is consistent with modeling and mosaic notation.
#' The functions generate a \code{ggplot} command string which can be displayed by
#' setting \code{verbose = TRUE} as an argument.
#'
#' @rdname gf_functions0
#' @param placeholder Ignore this argument. See details.
#' @param data A data frame with the variables to be plotted
#' @param formula ignored.
#' @param add If \code{TRUE} then construct just the layer with no frame.  The result
#' can be added to an existing frame.
#' @param verbose If \code{TRUE} print the ggplot2 command in the console.
#' @param geom A way to specify ggplot geoms that are not aliased to gf functions.
#' @param ... Other arguments such as \code{position="dodge"}.
#' @seealso \code{\link{gf_point}()}, \code{\link{gf_histogram}()}, \code{\link{gf_pointrange}()}
#' @export
#' @rdname gf_functions0
#' @export
gf_hline <- gf_factory(type = "hline", aes_form = NULL)

#' @rdname gf_functions0
#' @export
gf_vline <- gf_factory(type = "vline", aes_form = NULL)

#' @rdname gf_functions0
#' @export
gf_abline <- gf_factory(type = "abline", aes_form = NULL)

#' @rdname gf_functions0
#' @export
gf_coefline <- function(placeholder = NULL, formula = NULL, coef, ...) {
  gf_abline(placeholder = placeholder, formula = formula,
            intercept = coef[1], slope = coef[2], ...)
}
