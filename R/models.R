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
  response_var.glm <- function(object, ...) { all.vars(object$terms[[2]])}
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
  stop("Can't extract data from models of class rpart.")
}

#' @param data a data frame
#' @param n number of values for specified variables: a list
#' @param at optional values at which to set values

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
      conversions[[k]] <- ifelse(is.numeric(ranges[[k]]), "as.descrete", NA)

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

#' Calculate effect sizes
#'
#' Like a derivative or finite-difference

#' @param at center values for evaluating the model
#' @param step list of numbers for numerical finite-difference, or comparison groups for
#' @param raw for 2nd-order effects, show the sets of first-order effects
#' @param ... additional arguments for \code{predict()}
#' categorical variables.
#'

#' @export
D.lm <- function(model, formula, at = NULL, step = NULL, raw = FALSE, ... ) {
  # grab the explanatory variable to use for the difference
  change_vars <- all.vars(mosaic::rhs(formula))

  data <- fchart:::data_from_model.lm(model)
  response <- fchart:::response_var.lm(model)
  explan_vars <- fchart:::explanatory_vars.lm(model)

  centers <- as.list(rep(NA, length(explan_vars)))
  names(centers) <- explan_vars
  for (var_name in explan_vars) {
    values <- data[[var_name]]
    centers[[var_name]] <-
      if (is.numeric(values)) {
        median(values, na.rm = TRUE)
      } else {
        # get the most popular level
        names(table(sort(values, decreasing = TRUE)))[1]
      }
  }

  # loop over <at> and update any nominal values
  for (k in seq_along(at)) {
    nm <- names(at)[k]
    if ( ! nm %in% explan_vars) stop(nm, "isn't an explanatory variable.")
    centers[[nm]] <- at[[k]]
  }

  change_ranges <- as.list(rep(NA, length(change_vars)))
  names(change_ranges) <- change_vars
  change_ranges[names(step)] <- step
  for (vname in change_vars) {
    if ( ! vname %in% names(step)) {
      vals <- data[[vname]]
      if (is.numeric(vals)) change_ranges[[vname]] <- sd(vals, na.rm = TRUE)
      else {
        center_category <- centers[[vname]]
        new_categories <- names(sort(table(vals)))
        new_categories <- setdiff(new_categories, center_category)
        change_ranges[[vname]] <- new_categories[1:pmin(length(new_categories), 10)]
      }
    }

    if (is.numeric(centers[[vname]]))
      centers[[vname]] <- centers[[vname]] + c(-1, 1) * change_ranges[[vname]]
    else
      centers[[vname]] <- c(centers[[vname]], change_ranges[[vname]])
  }

  input_data <- reference_values(data, at = centers)

  mod_vals <- predict(model, newdata = input_data)
  input_data[[response]] <- mod_vals
  # Set the response variable
  # PROCESS
  # for the variables in formula, each has a high and low value
  # set the nominals for that variable to be high value
  #  - calculate the model output
  # set the nominals for the variable to be the low value(s) --- plural if categorical
  #  - calculate the model output
  # subtract the low value(s) from the high values

  # If there is a second variable
  # perform the process for the high and low values of the second variable
  # just let there be 2 values for the second variable

  one_change <- function(in_data, var_name, response_names) {
    var_levels <- in_data[[var_name]]
    ref_level <- var_levels[1]
    remaining_levels <- var_levels[-1]
    response <- in_data[response_names]
    ref_val <- response[1,]
    others <- response[-1,]
    effect <- ref_val - others
    if (is.numeric(ref_level)) {
      res <- effect / (ref_level - remaining_levels)
      names(res) <- paste0("delta_", response_names, "_by_", var_name)
    } else {
      res <- - effect
      names(res) <- paste0(response_names, "..from..", ref_level, "..to..", remaining_levels)
    }

    res
  }

  if (length(change_vars) == 1) {
    if (raw) effects <- input_data
    else effects <- one_change(input_data, change_vars, response)
  } else if (length(change_vars) == 2) {
    effects <- NULL
    vals <- unique(input_data[[change_vars[[2]]]])
    if (is.factor(vals)) vals <- as.character(vals)
    input_data <- split(input_data, input_data[[change_vars[2]]])
    for (k in 1:length(input_data)) {
      res <- one_change(input_data[[k]], change_vars[[1]], response)
      effect <- data.frame(as.list(res))
      row.names(effect) <- NULL
      names(effect) <- names(res)
      effects <- rbind(effects, data.frame(effect, vals[k], stringsAsFactors = FALSE))
    }
    effects <- data.frame(effects, stringsAsFactors = FALSE)
    names(effects)[ncol(effects)] <- change_vars[[2]]
    if ( ! raw) effects <- one_change(effects, change_vars[2], names(effects)[-length(names(effects))])
  } else {
    stop("Only 1st and 2nd-order interactions supported.")
  }

  effects
}


