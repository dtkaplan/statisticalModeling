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
  data <- data_from_model(model, data = data)
  change_var <- all.vars(mosaic::rhs(formula))
  # set up so that glms are evaluated, by default, as the response rather than the link
  if (inherits(model, "glm") && (! "type" %in% names(extras))) {
    extras$type = "response"
  }
  input_data <- create_eval_levels(model, formula, from=from, at=at, data = data, ...)
  step <- get_step(input_data, change_var, data)
  
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
  
