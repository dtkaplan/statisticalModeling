#' Calculate effect sizes in a model
#'
#' Like a derivative or finite-difference

#' @param model the model from which the effect size is to be calculated
#' @param formula a formula whose right-hand side is the variable with respect
#' to which the effect size is to be calculated.
#' @param at center values for evaluating the model
#' @param from optional: the value to use as baseline for the variable specified
#' in the formula
#' @param step the numerical stepsize for the change var, or a comparison category
#' for a categorical change var.
#' @param ... additional arguments for \code{predict()}. For instance, for a glm, perhaps you
#' want \code{type = "response"}.
#'
#' @examples
#' mod1 <- lm(wage ~ age * sex * educ + sector, data = mosaicData::CPS85)
#' effect_size(mod1, ~ sex)
#' effect_size(mod1, ~ sector)
#' effect_size(mod1, ~ age, at = list(sex = "M"))
#' effect_size(mod1, ~ age, at = list(sex = "F"))

#' @export
effect_size <- function(model, formula, from = NULL, at = NULL, step = NULL, to = step, data = NULL, ... ) {
  extras <- list(...)
  # set up so that glms are evaluated, by default, as the response rather than the link
  if (inherits(model, "glm") && (! "type" %in% names(extras))) {
    extras$type = "response"
  }
  # grab the explanatory variable to use for the difference
  change_var <- all.vars(mosaic::rhs(formula))

  data <- data_from_model(model, data = data)
  response <- response_var(model)
  explan_vars <- explanatory_vars(model)

  centers <- as.list(rep(NA, length(explan_vars)))
  names(centers) <- explan_vars
  for (var_name in explan_vars) {
    values <- data[[var_name]]
    centers[[var_name]] <-
      if (is.numeric(values)) {
        median(values, na.rm = TRUE)
      } else {
        # get the most popular level
        names(sort(table(values), decreasing = TRUE))[1]
      }
  }
  if ( ! is.null(from)) centers[[change_var]] <- from

  # loop over <at> and update any nominal values
  for (k in seq_along(at)) {
    nm <- names(at)[k]
    if ( ! nm %in% explan_vars) stop(nm, "isn't an explanatory variable.")
    centers[[nm]] <- at[[k]]
  }
  # input values for the explanatory variables
  input_data <- reference_values(data[,explan_vars, drop = FALSE], at = centers)

  if (is.null(step)) { # Need to set the step
    vals <- data[[change_var]]

    if (is.numeric(vals)) step <- sd(vals, na.rm = TRUE)
    else {
      if ( ! is.null(from)) 
        vals <- vals[vals != from]
      else {
        vals <- vals[vals != centers[[change_var]]]
      }
      step <- names(sort(table(vals), decreasing = TRUE))[1] 
    }
  }
    
  base_vals <- do.call(predict, c(list(model, newdata = input_data), extras))
  from_levels <- input_data[[change_var]]
    if (is.numeric(step)) {
    input_data[[change_var]] = input_data[[change_var]] + step
  } else {
    input_data[[change_var]] <- step
  }
  to_levels <- input_data[[change_var]]
  offset_vals <- do.call(predict, c(list(model, newdata = input_data), extras))
   
  if (is.numeric(step)) {
    res <- data.frame(slope =  (offset_vals - base_vals) / step)
    names(res) <- paste0("slope")
  } else {
    res <- - data.frame(change = base_vals - offset_vals)
    names(res) <- paste0("change")
  }
  
  input_data[[change_var]] <- NULL # remove it temporarily
  input_data <- rev(input_data)
  input_data[[paste0("to:", change_var)]] <- to_levels
  input_data[[change_var]] <- from_levels

  return(cbind(res, rev(input_data)))
}  
  
