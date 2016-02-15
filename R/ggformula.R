#' Formula interface to ggplot2
#'
#' Uses a formula to map and set aesthetics in ggplot2
#'
#' @details The formula will, generically, look like
#' y ~ x
#' You can add in terms to handle other aesthetics
#' y ~ x + color:blue + shape:age + alpha:sex
#' Names like <blue> that are not variables in the data frame will be
#' set to the corresponding attribute.
#'
#'
#' @param data a data frame
#' @param formula a formula constructed from the variables in the data frame
#' @param add if TRUE, create just the layer
#' @param geom the name of the geom, e.g. "point", "line", "path"
#' @export
line_plot <- function(data=parent.env(), formula=NULL, add=FALSE, geom="line"){
  point_plot(data=data, formula=formula, add=add, geom=geom)
}
#' @export
path_plot <- function(data=parent.env(), formula=NULL, add=FALSE, geom="path"){
  point_plot(data=data, formula=formula, add=add, geom=geom)
}
#' @export
point_plot <- function(data=parent.env(), formula=NULL, add=FALSE,
                         geom = "point") {
  if (is.null(formula)) stop("Must provide a graphing formula, e.g. y ~ x")

  # Pull out the frame and aesthetic info from the formula
  # and put it in list form suitable for ggstring functions
  nms <- all.vars(formula, unique = FALSE)
  mapping_list <- list(data = "data")
  if (length(formula) == 2) {
    nms["x"] = nms[1]
    nms["y"] = NULL
    nms <- nms[-1]
  } else {
    mapping_list["y"] <- nms[1]
    mapping_list["x"] <- nms[2]
    nms <- nms[-(1:2)]
  }

  # generate a frame (if needed)
  for_frame <- do.call(frame_string, mapping_list[c("data", "x", "y")])

  mapping_list <- pairs_in_formula(formula)

  for_layer_fun <-
    function(...) layer_string(names(data), geom = geom, ...)
  for_layer <- do.call(for_layer_fun,
                       mapping_list)
  gg_command_string <-
    if ( ! add) {
      paste(for_frame, for_layer, sep=" +  ")
    } else {
      for_layer
    }


  p <- eval(parse(text = gg_command_string ))

  p
}

# pull out the pairs from a formula like color::red + alpha:0.5
# return them as a named list
pairs_in_formula <- function(formula) {
  fc <- as.character(formula)
  parts <- unlist(strsplit(fc, "+", fixed = TRUE))
  # trim leading blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)
  # identify the pairs
  pairs <- parts[grep(":+", parts)]
  res <- list()
  for (pair in pairs) {
    this_pair <- unlist(strsplit(pair, ":+"))
    res[this_pair[1] ] <- this_pair[2]
  }
  res
}
