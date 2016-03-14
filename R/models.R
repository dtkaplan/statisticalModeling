#' Plot out model values
#'
#' @param ... arguments to predict()
#' @export
fmodel <- function(model=NULL, formula = NULL, data = NULL, nlevels = 3, ...) {
  if (is.null(model)) {
    stop("Must provide a model for graphing.")
  } else if (inherits(model, c("rpart", "glm", "lm", "groupwiseModel"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for fmodel().")
  }

  # try to figure out what are the possible levels of variables
  if (is.null(data)) data <- data_from_model(model)
  response_var <- response_var(model)


  if (is.null(formula)) explan_vars <- explanatory_vars(model)
  else explan_vars <- all.vars(mosaic::rhs(formula))

  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>

  how_many <- as.list(c(Inf, rep(nlevels, length(explan_vars) - 1)))
  names(how_many) <- explan_vars
  eval_levels <- reference_values(data, n = how_many )
  model_vals <- predict(model, newdata = eval_levels, ...)
  # pre_processing
  if (inherits(model, "groupwiseModel") &&
      inherits(model_vals, "data.frame"))
    model_vals <- model_vals$model_value

  eval_levels[[response_var]] <- model_vals

  # TO DO
  # 1. change "model_vals" to the actual name of the reponse variable
  # 2. for quantitative response, use geom_line() instead of geom_path()
  # 3. check for probability vector in response from gwm and create separate facets based on that



  # Use geom_path() when the x-axis is categorical
  ggplot(data = convert_to_discrete(eval_levels), aes_string(explan_vars[1], response_var, colour = explan_vars[2] )) +
    geom_line() +
    facet_wrap(explan_vars[3])
}

#' @export
data_from_model <- function(object, ...) {
  UseMethod("data_from_model")
}
#' @export
explanatory_vars <- function(object, ...) {
  UseMethod("explanatory_vars")
}
explanatory_vars.lm <-
  explanatory_vars.groupwiseModel <-
  explanatory_vars.rpart <-
  explanatory_vars.glm <- function(object, ...) all.vars(object$terms[[3]])


#' @export
response_var <- function(object, ...) {
  UseMethod("response_var")
}
response_var.lm <-
  response_var.groupwiseModel <-
  response_var.rpart <-
  response_var.glm <- function(object, ...) { all.vars(object$terms[[2]])}
#'
data_from_model.groupwiseModel <-
  data_from_model.glm <- function(object, ...){
  object$data
}
#'
data_from_model.lm <- function(object, ...) {
  object$model
}
#'
data_from_model.rpart <- function(object, ...) {
  stop("Can't extract data from models of class rpart.")
}


#' @export
reference_values <- function(data, n = 1, convert_fun = FALSE) {
  var_names <- names(data)
  # n might be a list.  If so, the default should be 1
  n_default <- ifelse(inherits(n, "list"), 1, n)
  n_values <- as.list(rep(n_default, length(var_names)))
  names(n_values) <- var_names
  if (inherits(n, "list")) # override any appearing in the n-list
    n_values[names(n)] <- n

  get_range <- function(var_name, n = 1) {
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
    ranges[[k]] <- get_range(var_names[k], n_values[[ var_names[k] ]])
    conversions[[k]] <- attr(ranges[[k]], "convert")
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
