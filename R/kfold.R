#' Compare models with k-fold cross-validation
#'
#' @export 
cv_pred_error <- function(..., k = 10, ntrials = 5, 
                     output = c("mse", "likelihood", "error_rate", "class")) {
  output <- match.arg(output)
  
  # Get just the names of the models
  full_names <- as.character(lapply(lazyeval::lazy_dots(...), FUN = function(x) x$expr))
  # Now for the models themselves
  models <- list(...)
  # model can be a list. If so, repeat over all the models.
  
  # Just a first stab at the problem.
  type <- switch(output, 
                 "mse" = "response",
                 "likelihood" = "likelihood",
                 "error_rate" = "class",
                 "class" = "class" # same as error_rate
                 )
  
  
  result = NULL
  for (counter in 1:length(models)) {
    this_mod <- models[[counter]]
    truth <- response_values(this_mod)
    pred_error_results <- numeric(ntrials)
    for (this_trial in 1:ntrials) {
      # get the model outputs for each test group against
      # the rest of the data
      mod_output <- kfold_trial(this_mod, type = type)
      pred_error_results[this_trial] <-
        if( type == "class") {
          mean(truth != mod_output, na.rm = TRUE)
        } else if (type == "likelihood") {
          sum(log(mod_output), na.rm = FALSE )
        } else mean((truth - mod_output)^2, na.rm = TRUE)
    }
    from_this_mod <- data.frame(pred_error_results, model = full_names[counter],
                                stringsAsFactors = FALSE)
    names(from_this_mod)[1] <- output # e.g. "mse", "likelihood", etc.

    result <- rbind(result, from_this_mod)
  }

  
  result
}

#' returns a vector of predictions or likelihoods
#' @export
kfold_trial <- function(mod, 
                        k=10, 
                        type = c("response", "prob", "likelihood", "class")) {
  # This isn't working for null models
  # null_mod <- lm(net ~ 1, data = Runners)
  # null_output <- kfold_trial(null_mod)
  
  
  
  type <- match.arg(type)
  # Grab the data and the call from the model.
  data <- data_from_model(mod)
  # For cross validation, we don't want the constructed terms
  constructed <- grep("\\(.*\\)", names(data))
  if (length(constructed) > 0) data[[constructed]] <- NULL # get rid of them
  
  # set up the call for fitting the model to the training data
  architecture <- mod$call[[1]]
  fit_call <- mod$call
  fit_call[["data"]] <- as.name("training")
  # construct the groups for the k-fold divisions
  groups <- sample(rep(1:k, length.out = nrow(data) ))
  # Create a holder for the result
  # output <- evaluate_model(mod, data = data, type = type)
  output <- numeric(nrow(data))
  
  for (group in 1:k) {
    training <- data[group != groups, , drop = FALSE ]

    testing  <- data[group == groups, , drop = FALSE ]
    
    this_model <- eval(fit_call)

    output[group == groups] <- 
      if (type == "likelihood") {
        likelihood_helper(this_model, data = testing)
      } else if (type == "class") {
        class_helper(this_model, data = testing)
      } else {
        mse_helper(this_model, data = testing)
      }
  }
  
  output
}

class_helper <- function(mod, data) {
  # find the classifier output for each case
  if (inherits(mod, "rpart")) {
    which_class <- predict(mod, newdata = data, type = "class")
    which_class <- as.character(which_class)
  } else if (inherits(mod, "glm")) {
    probs <- predict(mod, newdata = data, type = "response")
    which_class <- probs >= 0.5
  } else {
    warning("No likelihood_helper function for models of class", class(mod))
    which_class <- NA
  }
  
  which_class
}


likelihood_helper <- function(mod, data) {
  # calculate likelihood of each model output
  actual <- eval(parse(text = response_var(mod)), envir = data)
  if (inherits(mod, "rpart")) {
    # class_preds <- predict(mod, newdata = data, type = "vector")
    probs <- predict(mod, newdata = data, type = "prob")
    L <- probs[cbind(1:nrow(data), actual)]
  } else if (inherits(mod, "glm")) {
    probs <- predict(mod, newdata = data, type = "response")
    L <- ifelse(actual, probs, 1 - probs)
  } else {
    warning("No likelihood_helper function for models of class", class(mod))
    L <- NA
  }
  
  L
}

mse_helper <- function(mod, data) {
  # calculate likelihood of each model output
  actual <- eval(parse(text = response_var(mod)), envir = data)
  if ( ! is.numeric(actual)) stop("Can't calculate MSE on a classifier.")
  
  if (inherits(mod, "rpart")) {
    L <- predict(mod, newdata = data, type = "vector")
  } else {
    L <- predict(mod, newdata = data, type = "response")
  } 
  
  L
}