#' @fdname fmaps
#' @export
fmap <- function(data, formula, location = "Saint Paul, MN", zoom = 10, verbose = TRUE,
                 source = c("google", "stamen"),
                 type = c("roadmap", "terrain", "sattellite", "hybrid", "watercolor", "toner"),
                 ...) {
  source <- match.arg(source)
  type <- match.arg(type)
  gg_command_string <- map_string(location, zoom, source, type)
  gg_command_string_2 <-
    if (!missing(data) && !missing(formula)) {
      data_name <- as.character(substitute(data))
      paste0(" + ", formula_to_gg(data=data, formula=formula, add=TRUE,
                                  geom="point", .use_name = data_name, ...))
    } else {
      "" # blank
    }
  if (verbose) cat(gsub("+", "+\n",
                        paste(gg_command_string, gg_command_string_2), fixed = TRUE), "\n")
  eval(parse(text = gg_command_string))
}
#' @fdname fmaps
#' @export
fborders <- function(data, formula, ...) {

}

TO DO: NEED TO PUT THE AES STUFF IN THE GEOM WHEN ADD IS TRUE.
