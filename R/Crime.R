#' Data from the US FBI Uniform Crime Report, 1960
#' 
#' A report of the number of offenses reported to police per million
#' population, and many other social and demographic variables. Each
#' case corresponds to a state in the US.
#' 
#' @docType data
#' @name Crime
#' @usage data(Crime)
#'
#' @keywords datasets
#'
#' @format
#'   A data frame with 47 cases, each of which is a US state, with observations on the following variables.
#'   \itemize{
#'     \item{\code{R}} {Crime rate: number of offenses reported to police per million population.}
#'     \item{\code{Age}} {Number of males aged 14-24 per 1000 population}
#'     \item{\code{N}} {State population (in 100,000s)}
#'     \item{\code{W}} {State-wise median value of transferable goods and assets or family income in tens of dollars.}
#'     \item{\code{X}} {Number of families per 1000 earning below half the median income.}
#'     \item{\code{ExDiff}} {Change in per capita expenditure on police by state and local government from 1950 to 1960}
#'     \item{\code{Ex0}} {1960 per capita expenditures on police.}
#'   }
#'   
#' @source FBI Uniform Crime Report via \href{http://dasl.datadesk.com/data/view/114}{DASL: Data and Story Library}
#' 
#' @examples
#' mod_1 <- lm(R ~ W, data = Crime)
#' mod_2 <- lm(R ~ X, data = Crime)
#' mod_3 <- lm(R ~ W + X, data = Crime)
#' effect_size(mod_1, ~ W)
#' effect_size(mod_3, ~ W)
#' effect_size(mod_2, ~ X)
#' effect_size(mod_3, ~ X)
"Crime"
