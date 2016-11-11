# The actual graphing functions are created dynamically.
#  See the functions at the bottom of this file

# These are unexported helper functions to create the gf_ functions. The gf_ functions
# themselves are at the end of this file....

.add_arg_list_to_function_string <- function(S, extras) {
  empty <- grepl("\\(\\s?\\)$", S)
  res <- if (length(extras) == 0 ) { 
    S
  } else {
    more <- paste0(names(extras), " = ", unlist(extras), collapse = ", ")
    S <- gsub("\\)$", "", S)
    paste0(S, ifelse(empty, "", ", "), more, ")")
  }
  
  res
}

gf_generic <- function(placeholder = NULL, formula = NULL, data = NULL, 
                       extras = list(), geom = "geom_point", ... ) {
  data_name <- as.character(substitute(data))
  if (inherits(placeholder, c("gg", "ggplot"))) {
    # things are already set up
  } else if (inherits(placeholder, "formula")) {
    formula <- placeholder
    placeholder <- NULL
  }
  
  gg_string <- gf_master(formula = formula, data = data, 
                         geom = geom, gg_object = placeholder, 
                         extras = extras, data_name = data_name)
  gg_string
}

gf_factory <- function(type, extras = NULL){
  # this is a copy of the body of gf_generic() with some of the 
  # arguments Curried.
  function(placeholder = NULL, formula = NULL, 
           data = NULL, geom = type, verbose = FALSE, 
           add = FALSE,...) {
    extras <- list(...)
    data_name <- as.character(substitute(data))
    if (inherits(placeholder, c("gg", "ggplot"))) {
      add <- TRUE
    } else if (inherits(placeholder, "formula")) {
      formula <- placeholder
      placeholder <- NULL
    }
    
    gg_string <- gf_master(formula = formula, data = data, 
                           geom = geom, gg_object = placeholder,
                           add = add,
                           extras = extras, data_name = data_name)
    if (verbose) cat(gsub("+", "+\n", gg_string, fixed = TRUE), "\n")

    P <- eval(parse(text = gg_string))
    if (add) return(placeholder + P)
    else return(P)
  }
}

gf_master <- function(formula = NULL, data = NULL, add = FALSE, 
                      data_name = NULL, 
                      geom = "geom_point", extras = list(),
                      gg_object = NULL) {
  
  data_string <- 
    if (is.null(data)) ""
  else paste("data =", data_name)
  
  if ( (! add) && is.null(data) )
    stop("Must provide a frame or a data argument for a frame.")
  
  var_names <- 
    if (is.null(data)) {
      if (is.null(gg_object)) {
        character(0)
      } else {
        names(gg_object$data)
      }
    } else {
      names(data)
    }
  # arguments for the frame or, if add == TRUE, for the geom
  main_arguments <- 
    formula_to_aesthetics(formula, var_names, 
                          prefix = data_string)
  
  from_formula <- formula_to_df(formula, var_names)
  
  gg_string <-
    if (add) { # don't need the ggplot() call
      main_arguments <- 
        df_to_aesthetics(from_formula, 
                         var_names, prefix = data_string)
      .add_arg_list_to_function_string(
        paste0("geom_", geom, main_arguments),
        extras)
    } else {
      main_arguments <- 
        df_to_aesthetics(subset(from_formula, from_formula$map),
                         var_names, prefix = data_string)
      geom_arguments <-
        df_to_aesthetics(subset(from_formula, ! from_formula$map), var_names)
      paste0("ggplot", main_arguments, " + ", 
             # always add extras to geom string
             .add_arg_list_to_function_string(
               paste0("geom_", geom, geom_arguments),
               extras
             ))
    } 
  
  gg_string
}

formula_to_df <- function(formula = NULL, data_names = character(0)) {
  if (is.null(formula)) 
    return(data.frame(role = character(0), 
                      var = character(0), 
                      map = logical(0)))
  fc <- as.character(formula)
  parts <- unlist(strsplit(fc, "+", fixed = TRUE))
  # trim leading blanks
  parts <- gsub("^\\s+|\\s+$", "", parts)
  # identify the pairs
  pairs <- parts[grepl(":+", parts)]
  nonpairs <- parts[ ! grepl(":+", parts)] # the x- and y-part of the formula
  if (length(nonpairs) > 3) {
    warning("No role specified for ", 
            paste(nonpairs[-(1:3)], collapse = " and "))
  }
  res <- if (length(nonpairs) == 3) {
    list(y = nonpairs[[2]], x = nonpairs[[3]])
  } else if (length(nonpairs)) {
    list(x = nonpairs[[2]])
  }
  for (pair in pairs) {
    this_pair <- unlist(strsplit(pair, ":+"))
    res[this_pair[1] ] <- this_pair[2]
  }
  
  res <- data.frame(role = names(res), 
                    var = unlist(res), 
                    map = unlist(res) %in% data_names)
  row.names(res) <- NULL
  
  res
}

df_to_aesthetics <- function(formula_df, data_names = NULL, prefix = "") {
  aes_substr <- 
    if (is.null(data_names) || nrow(formula_df) == 0) {
      ""
    } else {
      paste0("aes(", 
             with(subset(formula_df, formula_df$map), 
                  paste(role, var, sep = " = ", collapse = ", ")),
             ")",
             ifelse(any( ! formula_df$map), ", ", "") # prepare for more args
      )
    }
  S <- paste0("(", prefix, 
              ifelse(nchar(prefix) > 0, ", ", ""),
              aes_substr, 
              with(subset(formula_df, ! formula_df$map), 
                   paste(role, var, sep = " = ", collapse = ", ")),
              ")")
  S
}


formula_to_aesthetics <- function(formula, 
                                  data_names = NULL, 
                                  prefix = "") {
  df <- formula_to_df(formula, data_names)
  df_to_aesthetics(df, data_names = data_names, prefix = prefix)
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
  xy <- parts[ ! grepl(":", parts)][-1] # logic for x:, y: explicit
  res <- list()
  for (pair in pairs) {
    this_pair <- unlist(strsplit(pair, ":+"))
    res[this_pair[1] ] <- this_pair[2]
  }
  # more logic for x:, y: explicit.
  stop("Haven't yet updated logic in frame_string. See comment.")
  # BUT ... not yet replaced explicit "x" and "y" arguments in 
  # frame_string()
  if (length(xy) == 2) {
    if ("y" %in% names(res)) 
      warning("duplicate specification of y aesthetic")
    else res["y"] <- xy[1]

    
    if ("x" %in% names(res))
      warning("duplicate specification of x aesthetic")
    else res["x"] <- xy[2]
  } else if (length(xy) == 1) {
    if ("y" %in% names(res)) {
      if ("x" %in% names(res))
        warning("duplicate specification of x aesthetic")
      else res["x"] <- xy
    } else if ("x" %in% names(res)) {
      if ("y" %in% names(res))
        warning("duplicate specification of y aesthetic")
      else res["y"] <- xy
    }
  }

  res
}


#' @rdname gf_functions
#' @export
gf_point <- gf_factory(type = "point")
#' @rdname gf_functions
#' @export
gf_jitter <- gf_factory(type = "jitter")
#' @rdname gf_functions
#' @export
gf_line <- gf_factory(type = "line")
#' @rdname gf_functions
#' @export
gf_path <- gf_factory(type = "path")
#' @rdname gf_functions
#' @export
gf_density <- gf_factory(type = "density")
#' @rdname gf_functions
#' @export
gf_density_2d <- gf_factory(type = "density_2d")
#' @rdname gf_functions
#' @export
gf_hex <- gf_factory(type = "hex")
#' @rdname gf_functions
#' @export
gf_hline <- gf_factory(type = "hline")
#' @rdname gf_functions
#' @export
gf_abline <- gf_factory(type = "abline")
#' @rdname gf_functions
#' @export
gf_hex <- gf_factory(type = "hex")
#' @rdname gf_functions
#' @export
gf_boxplot <- gf_factory(type = "boxplot")
#' @rdname gf_functions
#' @export
gf_freqpoly <- gf_factory(type = "freqpoly")
#' @rdname gf_functions
#' @export
gf_histogram <- gf_factory(type = "histogram")
#' @rdname gf_functions
#' @export
gf_text <- gf_factory(type = "text")
#
# Separate functions for a count-type bar chart and a value-based bar chart.
#' @rdname gf_functions
#' @export
gf_counts <- gf_factory(type = "bar", extras = list(stat = '"count"'))
#' @rdname gf_functions
#' @export
gf_bar <- gf_factory(type = "bar", extras = list(stat = '"identity"'))
