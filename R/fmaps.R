#' Drawing a data map underlayed by a map type
#'
#' This function creates a ggplot2 frame. 
#' You can add other layers on top of them but you cannot add this frame to an existing one. (Nor should you need to.)
#' 
#' @rdname fmaps

#' @param data data to use in drawing glyphs on the map
#' @param formula a formula specifying which variables from data are to be latitude ~ longitude
#' @param map one way to specify a map
#' @param extent the way to handle the frame when overplotting glyphs on the map. See \code{ggmap()}
#' @param location another way to specify a map: name of a country, city, etc.
#' @param zoom if using \code{location}, then set the scale of the map.
#' 3 = continent, 18 = city block. Each additional step zooms in by a factor of 2 in linear dim.
#' @param verbose whether to print the ggplot2 statement (default: \code{TRUE})
#' @param source if using \code{location}, the source of the map tiles: Google or Stamen
#' @param type if using \code{location}, the style of the map tiles
#' @param ... additional arguments to be passed to the layer overlaying the map
#'
#' @details The styles are, for google, \code{"roadmap", "terrain", "hybrid", "satellite"},
#' for stamen, \code{"terrain", "toner", "watercolor"}.
#'
#' @examples
#' \dontrun{
#' world <- ggmap::get_map(location = 'World', zoom = 3,
#'                         source = 'google', maptype = 'roadmap', crop=FALSE)
#' data(CountryCentroids, package = "DataComputing")
#' fmap(CountryCentroids, lat ~ long, map = world)
#' }


#' @rdname fmaps
#' @export
fmap <- function(data = NULL, formula = NULL, map = NULL, extent = c("panel", "device", "normal"),
                 location = "Saint Paul, MN", zoom = 10, verbose = TRUE,
                 source = c("google", "stamen"),
                 type = c("roadmap", "terrain", "satellite", "hybrid", "watercolor", "toner"),
                 ...) {
  source <- match.arg(source)
  type <- match.arg(type)
  extent <- match.arg(extent)
  map_name <- as.character(substitute(map))
  gg_command_string <- map_string(map_name, location, zoom, source, type, extent = extent)
  gg_command_string_2 <-
    if (!missing(data) && !missing(formula)) {
      data_name <- as.character(substitute(data))
      paste0(" + ", formula_to_gg(data=data, formula=formula, add=TRUE,
                                  geom="point", .use_name = data_name, ...))
    } else {
      "" # blank
    }
  full_command <- paste0(gg_command_string, gg_command_string_2)
  if (verbose) cat(gsub("+", "+\n",
                        full_command, fixed = TRUE), "\n")
  eval(parse(text = full_command))
}
#' @rdname fmaps
#' @param color color for the border
#' @param size width of border
#' @param add produce as a layer to another plot
#' @export
fborders <- function(data=NULL, formula=NULL, map = "USA_state_20m",
                     color = "grey", size = 1,
                     verbose = TRUE, add = FALSE, ...) {
  if ( ! requireNamespace("mosaicMapShapes")) stop("missing mosaicMapShapes package.")

  command_str <- if (add) {
    sprintf("geom_path(data = %s_shapes, aes(x = long, y = lat, group = group), color = '%s', size = %g)", map, color, size)
  } else {
    sprintf("ggplot(%s_shapes, aes(x = long, y = lat, group = group)) + geom_path(color = '%s', size = %g)", map, color, size)
  }
  full_command <- paste(command_str) # and the other stuff

  if (verbose) cat(gsub("+", "+\n",
                      full_command, fixed = TRUE), "\n")
  eval(parse(text = full_command))
}

# TO DO:
# 1. Document this.
# 2. Create a system to handle, e.g. sat ~ state, by merging
#    the data with [map]_data and then merging that with [map_shapes]
#    and adding a polygon() method with the fill set to "sat" (that is,
#    the lhs of the formula.)
