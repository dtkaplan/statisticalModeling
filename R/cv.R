#' Compare models with k-fold cross-validation
#'
#' @param ... one or more models on which to perform the cross-validation
#' @param k the k in k-fold. cross-validation will use k-1/k of the data for training.
#' @param ntrials how many random partitions to make. Each partition will be one case in the 
#' output of the function
#' @param output The kind of output to produce from each cross-validation. See details.
#' 
#' @details The purpose of cross-validation is to provide "new" data on which to test a model's 
#' performance. In k-fold cross-validation, the data set used to train the model is broken into 
#' new training and testing data. This is accomplished simply by using most of the data for training while 
#' reserving the remaining data for evaluating the model: testing. Rather than training a single model, k models
#' are trained, each with its own particular testing set. The testing sets in the k models are arranged to cover the
#' whole of the data set. On each of the k testing sets, a performance output is calculated. Which output is 
#' most appropriate depends on the kind of model: regression model or classifier. The most basic measure is the mean square error: the 
#' difference between the actual response variable in the testing data and the output of the model 
#' when presented with inputs from the testing data. This is appropriate in many regression models.
#'
#' For classification models, two different outputs are appropriate. The first is the error rate: the frequency
#' with which the classifier produces an incorrect output when presented with inputs from the testing data. This 
#' is a rather course measure. A more graded measure is the likelihood: the probability of the response values
#' from the test data given the model. (The "class" method is exactly the same as "error rate", but provided 
#' for compatibility purposes with other software under development.)  
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