#' Find typical levels of explanatory variables in a model/dataset.
#' 
#' This function tries to choose sensible values of the explanatory variables 
#' from the data used to build a model or any other specified data. 
#' (or from data specified with the \code{data =} argument.)
#' 
#' @details For categorical variables, the most populated levels are used. For quantitative 
#' variables, a sequence of \code{pretty()} values is generated. 
#' 
#' @return A dataframe containing all combinations of the selected values for 
#' the explanatory variables. If there are p explanatory variables, 
#' there will be about \code{nlevels^p} cases.
#'
#' @param model the model to display graphically
#' @param data optional data frame from which to extract levels for explanatory variables
#' @param nlevels how many levels to construct for input variables.
#' For quantitative variables, this is a suggestion. \code{pretty()} will determine 
#' @param at named list giving specific values at which to hold the variables. Use this to 
#' override the automatic generation of levels for any or all explanatory variables.
#' @param ... a more concise mechanism to passing desired values for variables
#'
#' @examples
#' \dontrun{mod1 <- lm(wage ~ age * sex + sector, data = mosaicData::CPS85)
#' typical_levels(mod1)
#' mod3 <- glm(married == "Married" ~ age + sex * sector,
#'             data = mosaicData::CPS85, family = "binomial")
#' typical_levels(mod3, nlevels = 2)
#' }
#' @export
typical_levels <- function(model=NULL, data = NULL, 
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

  explan_vars <- explanatory_vars(model)
  # try to figure out what are the possible levels of variables
  if (is.null(data)) data <- data_from_model(model)

  # Set a large number of levels for the first explanatory variable,
  # then nlevels for the remaining ones.
  how_many <- as.list(c(rep(nlevels, length(explan_vars))))
  names(how_many) <- explan_vars
  eval_levels <- reference_values(data[explan_vars], n = how_many, at = at )
  vnames <- names(eval_levels)
  for (name in vnames) {
    if (inherits(data[[name]], "factor") && !inherits(eval_levels[[name]], "factor"))
      eval_levels[[name]] <- factor(eval_levels[[name]], levels = levels(data[[name]]))
  }

  eval_levels
}

