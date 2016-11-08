#' Prices of used Ford automobiles in 2009
#'
#' These data were compiled by Macalester College students for a class project.
#' Then-undergraduates Elise delMas, Emiliano Urbina, and Candace Groth collected the data from 
#' \code{cars.com}


#' 
#' @format A data frame \code{Used_Fords} with 635 rows and 7 variables. 
#'   \itemize{
#'     \item{\code{Price}} {The asking price for the car in USD.}
#'     \item{\code{Year}} {The model year of the car.}
#'     \item{\code{Mileage}} {The reported odometer reading.}
#'     \item{\code{Location}} {Which of the several regions the car was marketed in.}
#'     \item{\code{Color}} {The car's body color.}
#'     \item{\code{Age}} {The age of the car at the time the data were collected in 2009. This is directly related to \code{Year}}
#'   }
#'
#' @docType data
#' @name Used_Fords
#' @usage data(Used_Fords)
#'
#' @keywords datasets
#'

#' @examples
#' mod_1 <- lm(Price ~ Mileage, data = Used_Fords)
#' mod_2 <- lm(Price ~ Mileage + Age, data = Used_Fords)
"Used_Fords"
