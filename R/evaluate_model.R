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
#' @param on_training flag whether to use the training data for evaluation. Only needed
#' when there are random terms, e.g. from \code{rand()}, \code{shuffle()}, .... See details.
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. \code{pretty()} will determine 
#' @param at named list giving specific values at which to hold the variables. Use this to 
#' override the automatic generation of levels for any or all explanatory variables.
#' @param ... arguments to predict()
#'
#'
#' @details There are two ways to evaluate the model on the training data. The first is
#' to set the \code{data} argument to the same data frame used to train the model. The second
#' is to use the \code{on_training = TRUE} argument. These are equivalent unless there is
#' some random component among the explanatory terms, as with `mosaic::rand()`, `mosaic::shuffle()` and so on.
#' 
#'
#' @examples
#' \dontrun{mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' evaluate_model(mod1)
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' evaluate_model(mod3, nlevels = 2, type = "response")
#' evaluate_model(mod3, nlevels = 2, type = "response", at = list(sex = "F"))
#' }
#' @export
evaluate_model <- function(model=NULL, data = NULL, on_training = FALSE,
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
  
  eval_levels <- if (is.null(data)) 
    typical_levels(model = model, data = data, nlevels = nlevels, at = at)
  else data

  model_vals <- 
    if (on_training) { 
      do.call(predict, c(list(model), extras))
    } else {
        do.call(predict,c(list(model, newdata = eval_levels), extras))
    }
  
  eval_levels$model_output <- model_vals

  eval_levels
}

