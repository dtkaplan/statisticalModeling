#' Create bootstrapped ensembles  of a model
#' 
#' @export
ensemble <- function(mod, nreps = 2, data = NULL) {
  # not yet dealing with poly(x,2) situations
  # does this have to be something where mod was built by train()?
  if (is.null(data)) 
    data <- data_from_model(mod)
  architecture <- mod$call[[1]]
  res <- list()
  res$original_model <- mod
  res$data <- data
  fit_call <- res$call <- mod$call
  fit_call[[3]] <- as.name("train_data")
  res$replications <- list()
  res$oob <-list() # out of bag cases for each replication
  for (k in 1:nreps) {
    this_rep <- list()
    inds <- sample(nrow(data), size = nrow(data), replace = TRUE)
    train_data <- data[inds, ]
    # Find the cases not used in training the model.
    # These are the out-of-bag (oob) cases, useful for cross-validation
    oob <- 1:nrow(data)
    oob <- oob[ ! oob %in% inds]
    res$oob[[k]] <- oob
    res$replications[[k]] <- eval(fit_call)
  }
  
  class(res) <- "bootstrap_ensemble"
  res
}

#' @export
print.bootstrap_ensemble <- function(object) {
  cat(
    paste("An ensemble of", length(object$replications), "bootstrap replications", 
        "of\n"))
    print(object$original_model$call)
    invisible(object)
}


