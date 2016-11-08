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
#' @importFrom stats formula lm median model.matrix predict quantile sd
#' @importFrom ggplot2 aes_string facet_grid facet_wrap geom_errorbar geom_line geom_point geom_ribbon ggplot label_both ylab
#' @importFrom utils data
#' 
#' @param model the model to display graphically
#' @param data optional set of cases from which to extract levels for explanatory variables
#' @param on_training flag whether to use the training data for evaluation. Only needed
#' when there are random terms, e.g. from \code{rand()}, \code{shuffle()}, .... See details.
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. \code{pretty()} will refine your choice. (default: 3) 
#' @param at named list giving specific values at which to hold the variables. Use this to 
#' override the automatic generation of levels for any or all explanatory variables. 
#' Unlike \code{data =} the variables given in \code{at =} or \code{...} will be crossed, so that
#' the evaluation will occur at all combinations of the various levels.
#' @param ... arguments about or values at which to evaluate the model or the kind of output to be passed along to predict().
#'
#' @return A data frame containing both the inputs to the model and the corresponding outputs.
#'
#' @details This function is set up to let you look easily at typical outputs. The function 
#' will choose "typical" levels of the explanatory variables at which to evaluate the model. 
#' (See the examples.) The \code{nlevels} controls 
#' how many levels of these levels to use. If you wish to choose your own levels for one or more
#' explanatory variables, give those variables as named arguments assigned to the levels you want. If you have
#' a data frame with the desired inputs for some or all of the explanatory variables, 
#' use the \code{data} argument to pass those values.
#' 
#' There are two ways to evaluate the model on the training data. The first is
#' to set the \code{data} argument to the same data frame used to train the model. The second
#' is to use the \code{on_training = TRUE} argument. These are equivalent unless there is
#' some random component among the explanatory terms, as with `mosaic::rand()`, `mosaic::shuffle()` and so on.
#' When you want to restrict/force the evaluation at specified values of an explanatory variable,
#' include a vector of those variables in ... for instance \code{sex = "F"} will restrict the evaluation
#' to females.
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
                   nlevels = 3, at = NULL,...) {
  dots <- handle_dots_as_variables(model, ...)
  extras <- dots$extras
  inline_values <- dots$at
  # Override the values in <at> with any set as inline arguments.
  at[names(inline_values)] <- NULL
  at <- c(at, inline_values)
  
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
  
  eval_levels <- 
    if (on_training) {
      data_from_model(model)
    } else {
      if (is.null(data)) {
        typical_levels(model = model, data = data, nlevels = nlevels, at = at)
      } else {
        data
      }
    }
  
  model_vals <- 
    if (on_training) { 
      do.call(predict, c(list(model), extras))
    } else {
        do.call(predict,c(list(model, newdata = eval_levels), extras))
    }
  
  eval_levels$model_output <- model_vals

  eval_levels
}

