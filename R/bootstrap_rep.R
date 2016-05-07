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
  for (k in 1:nreps) {
    this_rep <- list()
    inds <- sample(nrow(data), size = nrow(data), replace = TRUE)
    train_data <- data[inds, ]

    oob <- 1:nrow(data)
    oob <- oob[ ! oob %in% inds]
    this_rep$oob <- oob
    this_rep$mod <- eval(fit_call)
    res$replications[[k]] <- this_rep
  }
  
  class(res) <- "bootstrap_ensemble"
  res
}

