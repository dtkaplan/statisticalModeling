#' Extract component parts of models
#'
#' Functions for extracting data and names of explanatory and response variables from a model object
#' @rdname extract_from_model
#' @param object the model you are extracting features from.
#' @param ... additional arguments (not used)
data_from_model <- function(object, ...) {
  UseMethod("data_from_model")
}
#' @rdname extract_from_model
explanatory_vars <- function(object, ...) {
  UseMethod("explanatory_vars")
}
explanatory_vars.lm <-
  explanatory_vars.gwm <-
  explanatory_vars.groupwiseModel <-
  explanatory_vars.rpart <-
  explanatory_vars.randomForest <-
  # Need to fix this so that the items as stored in the model, 
  # e.g., as.factor(month), get returned.
  explanatory_vars.glm <- function(object, ...) all.vars(object$terms[[3]])

explanatory_vars.bootstrap_ensemble <- function(object, ...) {
  explanatory_vars(object$original_model, ...)
}
explanatory_vars.gbm <- function(object, ...) all.vars(object$Terms[[3]])
#' @rdname extract_from_model

response_var <- function(object, ...) {
  UseMethod("response_var")
}

response_var.lm <-
  response_var.groupwiseModel <-
  response_var.rpart <-
  response_var.randomForest <-
  response_var.glm <- function(object, ...) { deparse(object$terms[[2]])}

response_var.bootstrap_ensemble <- function(object, ...) {
  response_var(object$original_model, ...)
}

response_var.gbm <- function(object, ...) { deparse(object$Terms[[2]]) }
# CHANGE THE ABOVE to draw on formula_from_mod()

response_values <- function(model) {
  Data <- data_from_model(model)
  eval(parse(text = response_var(model)), envir = Data) 
}
formula_from_mod <- function(object, ...) {
  UseMethod("formula_from_mod")
}

formula_from_mod.lm <-
  formula_from_mod.groupwiseModel <-
  formula_from_mod.rpart <-
  formula_from_mod.randomForest <-
  formula_from_mod.glm <- function(object, ...) {formula(object$terms)}

formula_from_mod.gbm <- function(object, ...) {formula(object$Terms) }

data_from_model.lm <-
  data_from_model.groupwiseModel <-
  data_from_model.glm <-
  data_from_model.randomForest <- 
  data_from_model.gbm <-
  data_from_model.rpart <- function(object, ...) {
    dots <- list(...)
    if ("data" %in% names(dots) && ! is.null(dots$data))
      return(dots$data)
    # The above is NOT YET IMPLEMENTED
    # When/If I add the train function ...
    # if the object has a data attribute added by train, use that
    data_in_call <- which("data" == names(object$call))
    if (length(data_in_call) == 1) {
      the_data <- eval(object$call[[data_in_call]], envir = parent.frame(3))
      if (is.data.frame(the_data)) return(the_data)
    }
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

  ranges <- conversions <- as.list(rep(NA, length(var_names)))
  names(ranges) <- var_names
  for (k in 1:length(var_names)) {
    # get the appropriate number of levels for each variable
    if (var_names[k] %in% names(at)) {
      ranges[[k]] <- at[[var_names[k]]]
      conversions[[k]] <- ifelse(is.numeric(ranges[[k]]), "as.discrete", NA)

    } else {
      ranges[[k]] <- get_range(var_names[k], n_values[[ var_names[k] ]], data)
      conversions[[k]] <- attr(ranges[[k]], "convert")
    }
  }
  res <- do.call(expand.grid, c(ranges, stringsAsFactors = FALSE))
  attr(res, "convert") <- conversions
  
  vnames <- names(res)
  for (name in vnames) {
    if (inherits(data[[name]], "factor") && !inherits(res[[name]], "factor"))
      res[[name]] <- factor(res[[name]], levels = levels(data[[name]]))
  }

  res
}

# arrange numerical variables that have just a few levels to be represented as discrete
convert_to_discrete <- function(data) {
  convert_to_what <- attr(data, "convert")
  if (is.null(convert_to_what)) stop("Conversion to discrete not supported. See <reference_values> function.")
  for (k in 1:ncol(data)) {
    tmp <- convert_to_what[[k]]
    if( (! is.na(tmp)) && tmp == "as.discrete")
      data[[k]] <- factor(data[[k]], levels = as.character(sort(unique(data[[k]]))))
  }

  data
}
