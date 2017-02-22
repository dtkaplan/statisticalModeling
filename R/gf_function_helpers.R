
# The actual graphing functions are created dynamically.
#  See the functions at the bottom of this file

# These are unexported helper functions to create the gf_ functions. The gf_ functions
# themselves are at the end of this file....

# add quotes to character elements of list x and returns a vector of character
.quotify <- function(x) {
  as.character(
    lapply(x, function(e) if(is.character(e)) paste0('"', e, '"') else e)
  )
}

.add_arg_list_to_function_string <- function(S, extras) {
  empty <- grepl("\\(\\s?\\)$", S)
  res <- if (length(extras) == 0 ) {
    S
  } else {
    more <- paste0(names(extras), " = ", .quotify(extras), collapse = ", ")
    S <- gsub("\\)$", "", S)
    paste0(S, ifelse(empty, "", ", "), more, ")")
  }

  res
}

# This doen't appear to be used anywhere.

gf_generic <- function(placeholder = NULL, formula = NULL, data = NULL,
                       extras = list(), geom = "point", aes_form = y ~ x,
                       ... ) {
  data_name <- as.character(substitute(data))
  if (inherits(placeholder, c("gg", "ggplot"))) {
    # things are already set up
    add <- TRUE
  } else if (inherits(placeholder, "formula")) {
    formula <- placeholder
    placeholder <- NULL
    add <- FALSE
  }

  gg_string <- gf_master(formula = formula, data = data, add = add,
                         geom = geom, gg_object = placeholder,
                         extras = extras, aes_form = aes_form,
                         data_name = data_name)
  gg_string
}

gf_factory <- function(type, extras = NULL, aes_form = y ~ x) {
  # this is a copy of the body of gf_generic() with some of the
  # arguments Curried.
  function(placeholder = NULL, formula = NULL,
           data = NULL, geom = type, verbose = FALSE,
           add = inherits(placeholder, c("gg", "ggplot")),
           ...) {
    extras <- list(...)
    data_name <- as.character(substitute(data))
    # if (inherits(placeholder, c("gg", "ggplot"))) {
    #   add <- TRUE
    if (inherits(placeholder, "formula")) {
      formula <- placeholder
      placeholder <- NULL
    }


    if (!inherits(placeholder, c("gg", "ggplot"))) {
      add <- FALSE  # can't add if we don't have a plot to add to
    }
    gg_string <- gf_master(formula = formula, data = data,
                           geom = geom, gg_object = placeholder,
                           add = add, extras = extras,
                           aes_form = aes_form,
                           data_name = data_name)
    if (verbose) cat(gsub("+", "+\n", gg_string, fixed = TRUE), "\n")

    P <- eval(parse(text = gg_string))
    if (add)  #  don't need this part: && inherits(placeholder, c("gg", "ggplot")))
      return(placeholder + P)
    else
      return(P)
  }
}

gf_master <- function(formula = NULL, data = NULL, add = FALSE,
                      data_name = NULL,
                      geom = "geom_point", extras = list(),
                      gg_object = NULL,
                      aes_form = y ~ x) {

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
                          prefix = data_string,
                          aes_form = aes_form)

  from_formula <- formula_to_df(formula, var_names, aes_form = aes_form)

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

formula_to_df <- function(formula = NULL, data_names = character(0),
                          aes_form = y ~ x) {
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

  pair_list <- list()
  for (pair in pairs) {
    this_pair <- unlist(strsplit(pair, ":+"))
    pair_list[this_pair[1]] <- this_pair[2]
  }

  nonpair_list <- nonpairs[-1]  # remove ~
  # remove items specified explicitly
  aes_names <- setdiff(all.vars(aes_form), names(pair_list))
  names(nonpair_list) <- head(aes_names, length(nonpair_list))

  if (length(nonpair_list) > length(aes_names)) {
    stop("Formula too large.  I'm looking for ", format(aes_form),
         call. = FALSE)
  }
  if (length(nonpair_list) < length(aes_names)) {
    stop("Formula too small.  I'm looking for ", format(aes_form),
         call. = FALSE)
  }

  res <- c(nonpair_list, pair_list)

    # switch(
    #   length(nonpairs),
    #   list(),
    #   list(x = nonpairs[[2]]),
    #   list(y = nonpairs[[2]], x = nonpairs[[3]])
    # )


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
                                  prefix = "",
                                  aes_form = y ~ x) {
  df <- formula_to_df(formula, data_names, aes_form = aes_form)
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
gf_rug <- gf_factory(type = "rug")

#' @rdname gf_functions
#' @export
gf_quantile <- gf_factory(type = "quantile")

#' @rdname gf_functions
#' @export
gf_qq <- gf_factory(type = "qq", aes_form = ~ x)

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
gf_smooth <- gf_factory(type = "smooth")

#' @rdname gf_functions
#' @export
gf_spline <- gf_factory(type = "spline")

#' @rdname gf_functions
#' @export
gf_curve <- gf_factory(type = "curve", aes_form = y + yend ~ x + xend)

#' @rdname gf_functions
#' @export
gf_segment <- gf_factory(type = "segment", aes_form = y + yend ~ x + xend)

#' @rdname gf_functions
#' @export
gf_density <- gf_factory(type = "density", aes_form = ~ x)

#' @rdname gf_functions
#' @export
gf_density_2d <- gf_factory(type = "density_2d")

#' @rdname gf_functions
#' @export
gf_density2d <- gf_factory(type = "density2d")

#' @rdname gf_functions
#' @export
gf_contour <- gf_factory(type = "contour")

#' @rdname gf_functions
#' @export
gf_hex <- gf_factory(type = "hex")

#' @rdname gf_functions
#' @export
gf_hline <- gf_factory(type = "hline", aes_form = NULL)

#' @rdname gf_functions
#' @export
gf_vline <- gf_factory(type = "vline", aes_form = NULL)

#' @rdname gf_functions
#' @export
gf_abline <- gf_factory(type = "abline", aes_form = NULL)

#' @rdname gf_functions
#' @export
gf_hex <- gf_factory(type = "hex")

#' @rdname gf_functions
#' @export
gf_boxplot <- gf_factory(type = "boxplot")

#' @rdname gf_functions
#' @export
gf_histogram <- gf_factory(type = "histogram", aes_form = ~x)

#' @rdname gf_functions
#' @export
gf_dotplot <- gf_factory(type = "dotplot", aes_form = ~x)

#' @rdname gf_functions
#' @export
gf_text <- gf_factory(type = "text")

#' @rdname gf_functions
#' @export
gf_label <- gf_factory(type = "label")

#' @rdname gf_functions
#' @export
gf_ribbon <- gf_factory(type = "ribbon", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions
#' @export
gf_area <- gf_factory(type = "area")
#
# Separate functions for a count-type bar chart and a value-based bar chart.
#' @rdname gf_functions
#' @export
gf_counts <- gf_factory(type = "bar",
                        extras = list(stat = '"count"'), aes_form = ~ x)
#' @rdname gf_functions
#' @export
gf_bar <- gf_factory(type = "bar",
                     extras = list(stat = '"identity"'), aes_form = ~ x)

#' @rdname gf_functions
#' @export
gf_freqpoly <- gf_factory(type = "freqpoly", aes_form = ~ x)

#' @rdname gf_functions
#' @export
gf_ash <- gf_factory(type = "ash", aes_form = ~ x)

#' @rdname gf_functions
#' @export
gf_linerange <- gf_factory(type = "linerange", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions
#' @export
gf_pointrange <- gf_factory(type = "pointrange", aes_form = y + ymin + ymax ~ x)

#' @rdname gf_functions
#' @export
gf_crossbar <- gf_factory(type = "crossbar", aes_form = y + ymin + ymax ~ x)

#' @rdname gf_functions
#' @export
gf_errorbar <- gf_factory(type = "errorbar", aes_form = ymin + ymax ~ x)

#' @rdname gf_functions
#' @export
gf_errorbarh <- gf_factory(type = "errorbarh", aes_form = y ~ x + xmin + xmax)

# modified version of density plot without line along bottom and sides
#' @rdname gf_functions
#' @export
gf_dens <- gf_factory(type = "line", extras = list(stat = "density"),
                      aes_form = ~ x)

#' @rdname gf_functions
#' @export
gf_violin <- gf_factory(type = "violin")

#' @rdname gf_functions
#' @export
gf_spoke <- gf_factory(type = "spoke")

#' @rdname gf_functions
#' @export
gf_step <- gf_factory(type = "step")

#' @rdname gf_functions
#' @export
gf_rect <- gf_factory(type = "rect", aes_form = ymin + ymax ~ xmin + xmax)

#' @rdname gf_functions
#' @export
gf_tile <- gf_factory(type = "tile")
