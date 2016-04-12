#' Extract parts of models
#'
#' Functions for extracting data and names of explanatory and response variables from a model object
#' @rdname extract_from_model
#' @param object the model you are extracting features from.
#' @param ... additional arguments (not used)
#' @export
data_from_model <- function(object, ...) {
  UseMethod("data_from_model")
}
#' @rdname extract_from_model
#' @export
explanatory_vars <- function(object, ...) {
  UseMethod("explanatory_vars")
}
#' @export
explanatory_vars.lm <-
  explanatory_vars.groupwiseModel <-
  explanatory_vars.rpart <-
  explanatory_vars.glm <- function(object, ...) all.vars(object$terms[[3]])
#' @rdname extract_from_model
#' @export
response_var <- function(object, ...) {
  UseMethod("response_var")
}
#' @export
response_var.lm <-
  response_var.groupwiseModel <-
  response_var.rpart <-
  response_var.glm <- function(object, ...) { deparse(object$terms[[2]])}
#' @export
data_from_model.groupwiseModel <-
  data_from_model.glm <- function(object, ...){
  object$data
}
#' @export
data_from_model.lm <- function(object, ...) {
  object$model
}
#' @export
data_from_model.rpart <- function(object, ...) {
  stop("Can't extract data from models of class rpart. Provide the data in another way, e.g. via the argument data =")
}

#' Compute sensible values from a data set for use as a baseline
#'
#'
#' @param data a data frame
#' @param n number of values for specified variables: could be a single number or
#' a list assigning a number of levels for individual variables
#' @param at optional values at which to set values: a list whose names are the
#' variables whose values are to be set.
#'
#' @details Variables not listed in \code{at} will be assigned levels using these principles:
#' Categorical variables: the most populated levels.
#' Quantitative variables: central quantiles, e.g. median for n=1, 0.33 and 0.67 for n=2, and so on.

#' @export
reference_values <- function(data, n = 1, at = list()) {
  var_names <- names(data)
  # n might be a list.  If so, the default should be 1
  n_default <- ifelse(inherits(n, "list"), 1, n)
  n_values <- as.list(rep(n_default, length(var_names)))
  names(n_values) <- var_names
  if (inherits(n, "list")) # override any appearing in the n-list
    n_values[names(n)] <- n

  get_range <- function(var_name, n) {
    # get an appropriate set of levels
    values <- data[[var_name]]
    conversion <- NA
    value_set <-
      if (n == Inf) {
        if (is.numeric(values)) {
          if (length(unique(values)) < 100) unique(values)
          else seq(min(values), max(values), length = 100)
        } else {
          as.character(unique(values))
        }
      } else {
        if (is.numeric(values)) {
          conversion <- "as.discrete"
          quant_levels <- seq(0,1, length = n + 2)[c(-1, -(n + 2))]
          quantile(values, quant_levels)
        } else {
          level_names <- names(sort(table(values), decreasing = TRUE))
          level_names[1:pmin(n, length(level_names))]
        }
      }
    # will the variable ultimately be represented as discrete
    attr(value_set, "convert") <- conversion
    value_set
  }
  ranges <- conversions <- as.list(rep(NA, length(var_names)))
  names(ranges) <- var_names
  for (k in 1:length(var_names)) {
    # get the appropriate number of levels for each variable
    if (var_names[k] %in% names(at)) {
      ranges[[k]] <- at[[var_names[k]]]
      conversions[[k]] <- ifelse(is.numeric(ranges[[k]]), "as.discrete", NA)

    } else {
      ranges[[k]] <- get_range(var_names[k], n_values[[ var_names[k] ]])
      conversions[[k]] <- attr(ranges[[k]], "convert")
    }
  }
  res <- do.call(expand.grid, ranges)
  attr(res, "convert") <- conversions

  res
}

# arrange numerical variables that have just a few levels to be represented as discrete
convert_to_discrete <- function(data) {
  convert_to_what <- attr(data, "convert")
  if (is.null(convert_to_what)) stop("Conversion to discrete not supported. See <reference_values> function.")
  for (k in 1:ncol(data)) {
    tmp <- convert_to_what[[k]]
    if( (! is.na(tmp)) && tmp == "as.discrete")
      data[[k]] <- as.character(data[[k]])
  }

  data
}



