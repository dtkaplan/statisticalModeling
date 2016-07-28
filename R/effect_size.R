#' Calculate effect sizes in a model
#'
#' Like a derivative or finite-difference

#' @param model the model from which the effect size is to be calculated
#' @param formula a formula whose right-hand side is the variable with respect
#' to which the effect size is to be calculated.
#' @param at center values for evaluating the model. These should be in the form of a 
#' list or data.frame so that the variables involved can be named. Any variables that you do not set
#' will be set for you automatically.
#' @param step the numerical stepsize for the change var, or a comparison category
#' for a categorical change var. This will be either a character string or a number,
#' depending on the type of variable specified in the formula.
#' @param bootstrap If \code{TRUE}, calculate a standard error using bootstrapping. Alternatively, you can 
#' specify the number of bootstrap replications (default:100).
#' @param to a synonym for step. (In English, "to" is more appropriate for a 
#' categorical input, "step" for a quantitative. But you can use either.)
#' @param ... additional arguments for \code{predict()}. For instance, for a glm, perhaps you
#' want \code{type = "response"}.
#'
#' @examples
#' mod1 <- lm(wage ~ age * sex * educ + sector, data = mosaicData::CPS85)
#' effect_size(mod1, ~ sex)
#' effect_size(mod1, ~ sector)
#' effect_size(mod1, ~ age, at = list(sex = "M"))
#' effect_size(mod1, ~ age, at = list(sex = "F", age = 34), step = 1)
#' effect_size(mod1, ~ sex, at = list(age = 35, sex = "M"), to = "F" )

#' @export
effect_size <- function(model, formula, at = NULL, step = NULL, bootstrap = FALSE, to = step, data = NULL, ... ) {
  extras <- list(...)
  
  if (inherits(model, "bootstrap_ensemble")) {
    ensemble <- model$replications # that is, the list of bootstrapped models
    original_model <- model$original_model
    ensemble_flag  <- TRUE
  } else {
    ensemble <- list(model) # Just the one model to be evaluated
    original_model <- model
    ensemble_flag <- FALSE
  }
  
  # If data not explicitly provided, get from model
  if(is.null(data)) data <- data_from_model(original_model)
  
  change_var <- all.vars(mosaic::rhs(formula))[1]
  # set up so that glms are evaluated, by default, as the response rather than the link
  if (inherits(model, "glm") && (! "type" %in% names(extras))) {
    extras$type = "response"
  }
  from_inputs <- to_inputs <- 
    create_eval_levels(original_model, formula, at=at, data = data, ...)
  step <- get_step(from_inputs, change_var, data, step = unlist(to))
  
  # construct inputs for step from baseline
  if (is.numeric(step)) {
    to_inputs[[change_var]] <- from_inputs[[change_var]] + step
  } else {
    to_inputs[[change_var]] <- step
  }
  
  # Create the output data frame with vars in desired order.
  # change_var and to:change_var should be first two columns

  output_form <- from_inputs
  output_form[[change_var]] <- NULL # remove it temporarily
  output_form <- rev(output_form) 
  output_form[[paste0("to:", change_var)]] <- to_inputs[[change_var]]
  output_form[[change_var]] <- from_inputs[[change_var]]
  output_form <- rev(output_form) 
  
  # set up for accumulated output 

  Result <- NULL
  # if the "model" input was a single model, rather than an ensemble,
  # that single model is being stored as an ensemble of 1, so that
  # the same loop covers both cases.
  for (k in 1:length(ensemble)) {
    base_vals <-   do.call(predict, c(list(ensemble[[k]], newdata = from_inputs), extras))
    # model output after the step (or "offset")
    offset_vals <- do.call(predict, c(list(ensemble[[k]], newdata = to_inputs),   extras))
   
    res <- if (is.numeric(step)) {
      data.frame(slope =  (offset_vals - base_vals) / step)
    } else {
      data.frame(change = offset_vals - base_vals)
    }
    if (ensemble_flag) output_form$bootstrap_rep <- k
    
    Result <- rbind(Result, cbind(res, output_form))
  }

  if ( ! ensemble_flag && bootstrap) {
    if (is.logical(bootstrap)) bootstrap = 100  # set the default
    model <- ensemble(model, bootstrap)
    Bootstrap_reps <- effect_size(model, formula, at = at, step = step, to = to, data = data, ...)
    # find the sd for each set of replications
    set_number <- rep(1:(nrow(Bootstrap_reps) / bootstrap), length_out = nrow(Bootstrap_reps))
    std_errors <- mosaic::sd(Bootstrap_reps[[1]] ~ set_number)
    Result <- cbind(Result[1], stderr_effect = signif(std_errors,2), Result[2:length(Result)])
  }
  Result
}  
  
