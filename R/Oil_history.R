#' Historical production of crude oil, worldwide 1880-2014
#' 
#' Annual production of crude oil, in millions of barrels (mbbl).
#' 
#' @docType data
#' @name Oil_history
#' @usage data(Oil_history)
#'
#' @keywords datasets
#'
#' @format
#'   A data frame with 47 cases, each of which is a US state, with observations on the following variables.
#'   \itemize{
#'     \item{\code{year}} {the year for which production is reported}
#'     \item{\code{mbbl}} {oil production in millions of barrels}
#'   }
#'   
#' @source Assembled from older information from  RH Romer (1976) "Energy: An Introduction to Physics" and more recent
#' data from \code{data.oecd.org}.
#' 
#' @examples
#' model <- lm(log(mbbl) ~ year, data = Oil_history)
#' fmodel(model)
"Oil_history"
