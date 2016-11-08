#' Metabolism of zebra mussels
#'
#' Zebra mussels are a small, fast reproducing species of freshwater mussel 
#' native to the lakes of southeast Russia. They have accidentally been introduced in other 
#' areas, competing with native species and creating problems for people as they cover the 
#' undersides of docks and boats, clog water intakes and other underwater structures. 
#' Zebra mussels even attach themselves to other mussels, sometimes starving those mussels.
#' 
#' Ecologists Shirley Baker and Daniel Hornbach examined whether zebra mussels gain an advantage by attaching to other mussels rather than to rocks.(baker-hornbach-2008) The ecologists collected samples of small rocks and Amblema plicata mussels, each of which had a collection of zebra mussels attached. The samples were transported to a laboratory where the group of mussels from each individual rock or Amblema were removed and placed in an aquarium equipped to measure oxygen uptake and ammonia excretion. After these physiological measurements were made, the biochemical composition of the mussel tissue was determined: the percentage of protein, lipid, carbohydrate, and ash.

#' Baker and Hornbach found that zebra mussels attached to Amblema had greater 
#' physiological activity than those attached to rocks as measured by oxygen uptake and 
#' ammonia excretion. But this appears to be a sign of extra effort for the Amblema-attached 
#' zebra mussels, since they had lower carbohydrate and lipid levels. In other words, attaching to 
#' Amblema appears to be disadvantageous to the zebra mussels compared to attaching to a rock.

#' 
#' @format A data frame \code{Mussels} with 30 rows and 11 variables. 
#'   \itemize{
#'     \item{\code{GroupID}} {ID for the cluster of mussels growing on a substrate. }
#'     \item{\code{dry.mass}} {The mass of the mussels (as a group) after dehydration.}
#'     \item{\code{count}} {How many mussels were in the cluster.}
#'     \item{\code{attachment}} {The substrate to which the mussels were attached.}
#'     \item{\code{lipid}} {Percentage of dry mass that is lipid.}
#'     \item{\code{protein}} {Percentage of dry mass that is protein.}
#'     \item{\code{carbo}} {Percentage of dry mass that is carbohydrate.}
#'     \item{\code{ash}} {Percentage of dry mass that is ash.}
#'     \item{\code{ammonia}} {Nitrogen excretion measured as ammonia in mg per hour for the group.}
#'     \item{\code{Kcal}} {Total calorific value of the tissue in kilo-calories per gram.}
#'     \item{\code{O2}} {Oxygen uptake in mg per hour for the group.}
#'   }
#'   
#' @docType data
#' @name Mussels
#' @usage data(Mussels)
#'
#' @keywords datasets
#'

#' @examples
#' Mussels$ind.mass <- with(Mussels, dry.mass/count)
#' mod_1 <- lm(O2/count ~ attachment, data = Mussels)
#' mod_2 <- lm(ammonia/count ~ attachment, data = Mussels)
#' mod_3 <- lm(O2/count ~ ind.mass + attachment, data = Mussels)
#' mod_4 <- lm(ammonia/count ~ ind.mass + attachment, data = Mussels)
"Mussels"
