#' Simulated data bearing on school vouchers
#' 
#' In the US, there have been controversial proposals to provide vouchers 
#' to students in failing public schools. The vouchers would allow the students to attend 
#' private schools. There are arguments pro and con that are often rooted in political 
#' philosophy (free choice!) and politics. The presumption behind the *pro* arguments 
#' is that attending private schools would create better outcomes for students.
#'
#' A reasonable way to test this presumption is to compare test scores for students 
#' in public and private schools. 
#' One famous analysis (Howell and Peterson, 2003, "The Education Gap: Vouchers and Urban Schools")
#' found that voucher schools are most helpful for African-American students, 
#' and not so much for white or Hispanic students.
#' 
#' The \code{School_data} data frame comes from a simulation designed by the package author to 
#' replicate the overall results but supporting a very different policy recommendation. WARNING: This is 
#' just a simulation, reflecting one hypothesis about how the world might work. Don't be tempted
#' to draw conclusions about the actual factors involved in school performance from such simulated data.
#' 

#' 
#' @docType data
#' @name School_data
#' @usage data(School_data)
#'
#' @keywords datasets
#'
#' @format
#'   A data frame with 500 cases, each of which is a simulated student, with observations on the following variables.
#'   \itemize{
#'     \item{\code{test_score}} {a simulated test score for the student}
#'     \item{\code{school}} {whether the student attended public or private school}
#'     \item{\code{lottery}} {whether the student was entered into a lottery for a private-school voucher}
#'     \item{\code{group}} {the racial/ethnic group of the student}
#'     \item{\code{acad_motivation}} {the overall level of involvement and concern of the student's parents for the student's academic performance}
#'     \item{\code{relig_motivation}} {the overall level of interest motivated by religion. This is 
#'     potentially an issue because a large majority of urban private schools are Catholic.}
#'   }
#'   

#' 
#' @examples 
#' lm(test_score ~ school, data = School_data)
#' # the simulation mechanism itself:
#' nstudents <- 500
#' acad_motivation <- rnorm(nstudents)
#' group <- sample(c("black", "hispanic", "white"), replace = TRUE, size = nstudents)
#' relig_motivation <- ifelse( group == "black", -1, ifelse(group == "white", 0, 1))
#' relig_motivation <- rnorm(nstudents, mean = relig_motivation)
#' lottery <- (acad_motivation + relig_motivation) > 0
#' school <- ifelse( (runif(nstudents) + .8* lottery ) > 1, "private", "public")
#' test_score <- rnorm(nstudents, mean = 100 - 5 * (school == "private") + 20 * acad_motivation)
#' School_data <- data.frame(test_score, acad_motivation, group, relig_motivation, lottery, school)
"School_data"
