#' Earnings of workers at a trucking company.
#' 
#' A dataset from a mid-western US trucking company on annual earnings of its employees in 2007. 
#' Datasets like this are used in audits by the Federal Government to look for signs of discrimination.
#' 
#' @format A data frame \code{Trucking_jobs} with 129 rows and 11 variables. 
#'   \itemize{
#'     \item{\code{sex}} {The employee's sex: M or F}
#'     \item{\code{earnings}} {Annual earnings, in dollars. Hourly wages have been converted to a full-time basis.}
#'     \item{\code{age}} {The employee's age, in years.}
#'     \item{\code{title}} {The employee's job title.}
#'     \item{\code{hiredyears}} {How long the employee has been working for the company.}
#'   }
#'   
#' @docType data
#' @name Trucking_jobs
#' @usage data(Trucking_jobs)
#'
#' @keywords datasets
#'

#' @examples
#' mod_1 = lm(earnings ~ age + hiredyears + sex, data = Trucking_jobs)
"Trucking_jobs"
