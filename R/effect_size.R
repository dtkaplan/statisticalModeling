#' Calculate effect sizes in a model
#'
#' Like a derivative or finite-difference

#' @param model the model from which the effect size is to be calculated
#' @param formula a formula whose right-hand side is the variable with respect
#' to which the effect size is to be calculated.
#' @param step the numerical stepsize for the change var, or a comparison category
#' for a categorical change var. This will be either a character string or a number,
#' depending on the type of variable specified in the formula.
#' @param bootstrap If \code{TRUE}, calculate a standard error using bootstrapping. Alternatively, you can 
#' specify the number of bootstrap replications (default:100).
#' @param to a synonym for step. (In English, "to" is more appropriate for a 
#' categorical input, "step" for a quantitative. But you can use either.)
#' @param ... additional arguments for evaluation levels of explanatory 
#' variables or to be passed to  \code{predict()}. For instance, for a glm, perhaps you
#' want \code{type = "response"}.
#' @param at similar to \code{...} but expects a list or dataframe of the values you want to set.
#' Like \code{...}, all combinations of the values specified will be used as inputs.
#' @param data Specifies exactly the cases at which you want to calculate the effect size.
#' Unlike \code{...} or \code{at}, no new combinations will be created.
#' 
#'
#' @details 
#' When you want to force or restrict the effect size calculation to specific values for 
#' explanatory variables, list those variables and levels as a vector in ...
#' For example, \code{educ = c(10, 12, 16)} will cause the effect size to be calculated
#' at each of those three levels of education. Any variables whose levels are not specified in 
#' ... will have values selected automatically.
#'
#' @examples
#' mod1 <- lm(wage ~ age * sex * educ + sector, data = mosaicData::CPS85)
#' effect_size(mod1, ~ sex)
#' effect_size(mod1, ~ sector)
#' effect_size(mod1, ~ age, sex = "M", educ = c(10, 12, 16), age = c(30, 40))
#' effect_size(mod1, ~ age, sex = "F", age = 34, step = 1)
#' effect_size(mod1, ~ sex, age = 35, sex = "M", to = "F" )

#' @export
effect_size <- function(model, formula, step = NULL, 
                        bootstrap = FALSE, to = step, data = NULL, at = NULL, ... ) {
  dots <- handle_dots_as_variables(model, ...)
  inline_vals <- dots$at
  # combine values set inline with those set in <at>, overriding the ones in <at>
  at[names(inline_vals)] <- NULL
  at <- c(at, inline_vals)
  extras <- dots$extras
  
  if (inherits(model, "bootstrap_ensemble")) {
    ensemble <- model$replications # that is, the list of bootstrapped models
    original_model <- model$original_model
    ensemble_flag  <- TRUE
  } else {
    ensemble <- list(model) # Just the one model to be evaluated
    original_model <- model
    ensemble_flag <- FALSE
  }

  
  change_var <- all.vars(mosaic::rhs(formula))[1]
  # set up so that glms are evaluated, by default, as the response rather than the link
  if (inherits(model, "glm") && (! "type" %in% names(extras))) {
    extras$type = "response"
  }
  if (length(at) > 0 && !is.null(data)) {
    warning("You specified 'at' levels. Using these instead of raw data.") 
  }
  from_inputs <- to_inputs <- 
    if (is.null(data) || length(at) > 0) {
      data <- data_from_model(original_model)
      create_eval_levels(original_model, formula, at=at, data = data)
    } else {
      # if data explicitly stated, use exactly those inputs
      data
    }
  step <- get_step(from_inputs, change_var, data, step = unlist(to))
  
  # deal with the architectures written only for factors by
  # converting the step to a factor
  if (inherits(from_inputs[[change_var]], "factor")) {
    levels_for_change_var <- unique(as.character(data[[change_var]]))
    from_inputs[[change_var]] = factor(from_inputs[[change_var]], 
                                       levels = levels_for_change_var)
    step <- factor(step, levels = levels_for_change_var)
  }
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
  
