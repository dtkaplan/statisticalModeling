#' Evaluate a model for specified inputs
#' 
#' Find the model outputs for specified inputs. This is equivalent to the 
#' generic \code{predict()} function, except it will choose sensible values
#' by default.  This simplifies getting a quick look at model values.
#' 
#' @return A dataframe containing both the explanatory variable inputs and
#' the resulting model output (in the `model_value` field). This differs from the output
#' of \code{predict()}, which for many model classes/architectures may be a vector or matrix.
#'
#' @param model the model to display graphically
#' @param data optional data set from which to extract levels for explanatory variables
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. \code{pretty()} will determine 
#' @param at named list giving specific values at which to hold the variables. Use this to 
#' override the automatic generation of levels for any or all explanatory variables.
#' @param ... arguments to predict()
#'
#'
#' @examples
#' \dontrun{mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' evaluate_model(mod1)
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' evaluate_model(mod3, nlevels = 2, type = "response")
#' }
#' @export
evaluate_model <- function(model=NULL, data = NULL, 
                   nlevels = 3, at = list(), ...) {
  extras <- list(...)
  if (is.null(model)) {
    stop("Must provide a model to evaluate.")
  } else if (inherits(model, 
                      c("rpart", "glm", "lm", "groupwiseModel",
                        "randomForest", "gbm"))) {
    # nothing to do
  } else {
    stop("Model of type", class(model)[1], "not set up for evaluate_model().")
  }
  if( inherits(model, "gbm")) stop("gbm models still not working.")
  
  eval_levels <- typical_levels(model = model, data = data, 
                                nlevels = nlevels, at = at)

  model_vals <- 
    do.call(predict,c(list(model, newdata = eval_levels), 
                      extras))

  eval_levels$model_output <- model_vals

  eval_levels
}

