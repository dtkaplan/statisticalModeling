#' Plot out model values
#'
#' @param ... arguments to predict()
fmodel <- function(model=NULL, formula = NULL, data = NULL, nlevels = 3, ...) {
  if (is.null(model)) {
    stop("Must provide a model for graphing.")
  } else if (inherits(model, c("rpart", "glm", "lm", "groupwiseModel"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for fmodel().")
  }

  # try to figure out what are the possible levels of variables
  if (is.null(data)) {
    if (inherits(model, "rpart")) stop("Must provide a dataset with the explanatory variables.")
    if (inherits(model, c("glm", "groupwiseModel"))){
      data <- model$data
    } else if (inherits(model, "lm")) {
      data <- model$model
    } else {
      # stop("Not set up for extracting data from this model")
    }
  }

  if (is.null(formula)) {
    # select the first few explanatory variables
    if (inherits(model, c("rpart", "glm", "lm", "groupwiseModel"))) {
        formula <- model$terms
    } else {
      stop("Not set up for extracting explanatory variables from this model.")
    }
  }

  # Use the first var as <x>
  # Use the second var as <color>
  # Use the third var as <facet>
  explan_vars <- all.vars(formula[[3]])
  get_range <- function(var_name, type = Inf) {
    # get an appropriate set of levels
    values <- data[[var_name]]
    return_values <-
      if (type == Inf) {
        if (is.numeric(values)) return(seq(min(values), max(values), length = 100))
        else as.character(unique(values))
      } else {
        if (is.numeric(values)) {
          quant_levels <- seq(0,1, nlevels + 2)[c(-1, -(nlevels + 2))]
          quantile(values, quant_levels)
        } else {
          level_names <- names(sort(table(values), decreasing = TRUE))
          level_names[1:pmin(nlevels, length(level_names))]
        }
      }
    return_values
  }
  ranges <- as.list(rep(NA, length(explan_vars)))
  names(ranges) <- explan_vars
  for (k in 1:length(explan_vars)) {
    # get the appropriate number of levels for each variable
    type_val <- ifelse(k == 1, Inf, ifelse(k > 3, 1, nlevels))
    ranges[[k]] = get_range(explan_vars[k], type = type_val)
  }

  eval_levels <- do.call(expand.grid, ranges)
  model_vals <- predict(model, newdata = eval_levels, ...)
  # pre_processing
  if (inherits(model, "groupwiseModel") &&
      inherits(model_vals, "data.frame"))
    model_vals <- model_vals$model_value

  eval_levels$model_vals <- model_vals

  # TO DO
  # 1. change "model_vals" to the actual name of the reponse variable
  # 2. for quantitative response, use geom_line() instead of geom_path()
  # 3. check for probability vector in response from gwm and create separate facets based on that



  # Use geom_path() when the x-axis is categorical
  ggplot(data = eval_levels, aes_string(explan_vars[1], "model_vals", colour = explan_vars[2]) ) +
    geom_path(aes_string(group = NA, color = explan_vars[2])) + geom_point(position = position_dodge(width = .2))#+
    #facet_wrap(explan_vars[3])
}
