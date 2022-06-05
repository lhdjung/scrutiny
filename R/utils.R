
#' @include import-reexport.R

utils::globalVariables(c(
  ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
  "frac"
))



# Do NOT export any of these! ---------------------------------------------


wrong_spec_string <- function(x) {
  if (is.character(x)) {
    return(glue::glue("\"{x}\""))
  } else {
    return(glue::glue("`{x}` (not a string)"))
  }
}



reconstruct_sd_scalar <- function(formula, x, n, group_0, group_1) {
  x <- as.numeric(x)

  if (formula == "mean_n") {
    sd_rec <- sd_binary_mean_n(mean = x, n = n)
  } else if (formula == "0_n") {
    sd_rec <- sd_binary_0_n(group_0 = group_0, n = n)
  } else if (formula == "1_n") {
    sd_rec <- sd_binary_1_n(group_1 = group_1, n = n)
  } else if (formula == "groups") {
    sd_rec <- sd_binary_groups(group_0 = group_0, group_1 = group_1)
  } else {
    cli::cli_abort(c(
      "`formula` was given as {wrong_spec_string(formula)}.",
      "i" = "Please specify it as \"mean_n\", \"0_n\", \"1_n\", or \\
      \"groups\" instead. Default is \"mean_n\"."
    ))
  }

  return(sd_rec)
}


reconstruct_sd <- Vectorize(reconstruct_sd_scalar, USE.NAMES = FALSE)



# Used in unit testing:
integer_places <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed("\\.", n = 2) %>%
    .[, 1] %>%
    stringr::str_length()
}



# A helper for tidy evaluation used within `is_subset_of_vecs()`,
# `is_superset_of_vecs()`, and `is_equal_set_vecs()`:
straighten_out <- function(...) {
  y <- rlang::enexprs(...)
  return(purrr::flatten(purrr::map(y, rlang::eval_bare)))
}



an_a <- function(x) {
  dplyr::if_else(stringr::str_detect(x, "^[aeiou]"), "an", "a")
}


an_a_type <- function(x) {
  type <- dplyr::if_else(is.double(x), "double (numeric value)", typeof(x))
  glue::glue("{an_a(typeof(x))} {type}")
}


# This function is essentially taken from the Examples section of the
# documentation for `integer()`, where it's called `is.wholenumber()`:
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}



# Super-short functions used within `decimal_places()` -- because named
# functions are somewhat faster than anonymous ones:
is_length_1_and_not_na <- function(x) length(x) == 1 && !is.na(x)
is_length_greater_1 <- function(x) length(x) > 1
set_to_0 <- function(x) 0



# Subset every `n`th element from a vector `x`:
parcel_nth_elements <- function(x, n, from = 1) {
  x[seq(from = from, to = length(x), by = n)]
}



# This one is for GRIMMER. It differs from `dplyr::distinct()` mainly insofar as
# it doesn't take the order of values within a row into account. Thus, it only
# checks which values are present in a row how many times; hence "equivalent"
# (see example below the function):
remove_equivalent_rows <- function(data) {
  data_array <- apply(data, 1, sort)

  data[!duplicated(data_array, MARGIN = 2), ]
}

# For example, consider this data frame:

# df <- tibble::tribble(
#   ~a, ~b, ~c,
#   1, 2, 2,
#   2, 1, 2,
#   1, 2, 3
# )

# No two rows are identical here, so `dplyr::distinct()` preserves all of them.
# However, `remove_equivalent_rows()` cuts the second row. That is because the
# first two rows contain all the same values. They only differ in terms of the
# order of values, which the function ignores.


# Also for GRIMMER:
reverse_column_order <- function(data) {
  col_numbers_reversed <- ncol(data):1

  data[, order(col_numbers_reversed)]
}



# Censoring is used in some of scrutiny's unit tests:
censor <- function(x, left, right) {
  dplyr::case_when(
    x < left   ~ left,
    x > right  ~ right,
    TRUE       ~ x
  )
}



# Conveniently add one or more classes to an object:
add_class <- function(x, new_class) {
  class(x) <- c(new_class, class(x))
  x
}


# Mapped within `row_to_colnames()`:
remove_na <- function(x) {
  x[!is.na(x)]
}



check_rounding_singular <- function(x, bad, good_1, good_2) {
  if (bad %in% x) {
    cli::cli_abort(c(
      "`rounding` given as \"{bad}\" plus others.",
      "x" = "If `rounding` has length > 1, only single rounding procedures \\
      are supported, such as \"{good_1}\" and \"{good_2}\".",
      "i" = "You can still concatenate multiple of them; just leave out \\
      those with \"_or_\"."
    ))
  }
}

check_rounding_singular_all <- function(x) {
  if (length(x) > 1) {
    check_rounding_singular(x, "up_or_down", "up", "down")
    check_rounding_singular(x, "up_from_or_down_from", "up_from", "down_from")
    check_rounding_singular(x, "ceiling_or_floor", "ceiling", "floor")
  }
}




# `check_lengths_congruent()` is called within a function `f()` and takes a list
# of arguments to `f()` supplied by the user (`var_list`). It checks if two or
# more of those arguments have lengths that are greater than 1. If at least two
# of these lengths are also different from each other and the `error` argument
# is `TRUE` (the default), the function will throw a precisely informative
# error. If they have the same > 1 length and the `warn` argument is `TRUE` (the
# default), there will be an informative warning. The only dependencies of this
# function are {rlang} and {cli}. As these are tidyverse backend packages that
# most users have installed already, the function might conceivably be used more
# widely.

check_lengths_congruent <- function(var_list, error = TRUE, warn = TRUE) {
  var_names <- rlang::enexprs(var_list)
  var_lengths <- vapply(var_list, length, integer(1))
  var_list_gt1 <- var_list[var_lengths > 1]

  # Condition of checking for error and warning:
  if (length(var_list_gt1) > 1) {
    var_names <- var_names[[1]][-1]
    var_names <- as.character(var_names)
    var_names_gt1 <- var_names[var_lengths > 1]
    vnames_gt1_all <- var_names_gt1   # for the warning

    length_dup <- duplicated(var_lengths)
    var_list_gt1 <- var_list_gt1[!length_dup]
    var_names_gt1 <- var_names_gt1[!length_dup]

    # Error condition, checking if there is more than one element of `var_list`
    # with a unique length greater than one (the duplicated lengths were
    # filtered out from `var_list_gt1` right above):
    if (error & (length(var_list_gt1) > 1)) {

      x <- var_list_gt1[[1]]
      y <- var_list_gt1[[2]]
      x_name <- var_names_gt1[[1]]
      y_name <- var_names_gt1[[2]]

      residues_names <- var_names[!var_names %in% c(x_name, y_name)]
      msg_need <-
        "Both need to have the same length unless either has length 1."

      # Append-to-error-message condition:
      if (length(residues_names) > 0) {
        residues_names <- paste0("`", residues_names, "`")
        msg_need <- paste(
          msg_need,
          "This also applies to {residues_names}."
        )
      }

      # Throw error:
      cli::cli_abort(c(
        "Lengths of `{x_name}` and `{y_name}` are not congruent.",
        "x" = "`{x_name}` has length {length(x)}.",
        "x" = "`{y_name}` has length {length(y)}.",
        "!" = msg_need
      ))
    }

    # Warning condition, triggered if more than one element of `var_list` has
    # length > 1, it's the same length for all (hence no error), and the `warn`
    # argument is `TRUE` (the default):
    if (warn) {
      x_name <- vnames_gt1_all[[1]]
      y_name <- vnames_gt1_all[[2]]

      l_vnames <- length(vnames_gt1_all)

      if (l_vnames > 2) {
        msg_example <- ", for example,"
      } else {
        msg_example <- ""
      }

      if (l_vnames == 2) {
        one_both_all <- "one or both"
        var_count <- ""
      } else {
        one_both_all <- "all (or all but one)"
        var_count <- l_vnames
      }

      vnames_gt1_all <- paste0("`", vnames_gt1_all, "`")

      # Throw warning:
      cli::cli_warn(c(
        "Values of {vnames_gt1_all} get paired.",
        "!" = "Are you sure that{msg_example} each `{x_name}` value \\
        should correspond to a different `{y_name}` value?",
        ">" = "It might be better if {one_both_all} of these {var_count} \\
        variables have length 1."
      ))
    }
  }
}





# Make sure a vector `x` has length `l`, otherwise throw an informative error.
# For example, if a vector called `vals` needs to have length 1, run:
# `check_length(vals, 1)`.
check_length <- function(x, l) {
  if (length(x) != l) {
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "`{name}` has length {length(x)}.",
      "x" = "It needs to have length {l}."
    ))
  }
}


# Much the same as `check_length()`, but for object types rather than lengths.
# An object `x` needs to have one of the types in `t`, or else there will be an
# error.
check_type <- function(x, t) {
  if (!typeof(x) %in% t) {
    msg_name <- deparse(substitute(x))
    if (length(t) == 1) {
      msg_object <- "be of type"
    } else {
      msg_object <- "be one of these types:"
    }
    cli::cli_abort(c(
      "`{msg_name}` is of type {typeof(x)}.",
      "x" = "It needs to {msg_object} {t}."
    ))
  }
}



check_class <- function(x, cl) {
  if (!inherits(x, cl)) {
    msg_name <- deparse(substitute(x))
    cli::cli_abort(c(
      "Required class missing.",
      "x" = "`{msg_name}` doesn't inherit class \"{cl}\"."
    ))
  }
}



split_into_groups <- function(x, group_size) {
  check_length(group_size, 1)
  remainder <- length(x) %% group_size

  if (remainder != 0) {
    if (!is_whole_number(group_size)) {
      cli::cli_abort(c(
        "`group_size` is `{group_size}`.",
        "x" = "It needs to be a whole number."
      ))
    }
    name_x <- deparse(substitute(x))
    msg_el <- if (remainder == 1) "element" else "elements"
    cli::cli_warn(c(
      "!" = "`x` (`{name_x}`) can't be evenly divided into \\
      groups of {group_size}.",
      "x" = "It has length {length(x)}, so the last group has \\
      {remainder} {msg_el}, not {group_size}."
    ))
  }

  split(x, ceiling(seq_along(x) / group_size))
}


split_into_rows <- function(data) {
  split_into_groups(x = t(data), group_size = ncol(data))
}



step_size_scalar <- function(x) {
  digits <- decimal_places_scalar(x)
  1 / (10 ^ digits)
}

step_size <- function(x) {
  digits <- max(decimal_places(x))
  1 / (10 ^ digits)
}



manage_string_output_seq <- function(out, from, string_output, digits) {
  if (string_output == "auto") {
    if (is.character(from)) {
      out <- restore_zeros(out, width = digits)
    } else {
      out <- methods::as(out, typeof(from))
    }
    return(out)
  } else if (!is.logical(string_output)) {
    if (is.character(string_output)) {
      string_output <- paste0("\"", string_output, "\"")
    } else {
      string_output <- paste0("`", string_output, "`")
    }
    cli::cli_abort(c(
      "`string_output` given as {string_output}.",
      "x" = "It must be logical or \"auto\"."
    ))
  } else if (string_output) {
    out <- restore_zeros(out, width = digits)
  } else if (typeof(from) != "character") {
    out <- methods::as(out, typeof(from))
  }
  return(out)
}


commas_and <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    collapse <- " "
    and <- " and "
  } else {
    collapse <- ", "
    and <- ", and "
  }
  out <- stringr::str_flatten(x[-length(x)], collapse = collapse)
  out <- paste0(out, and, x[length(x)])
  return(out)
}




check_non_negative <- function(x) {
  offenders <- x[x < 0]
  if (length(offenders) > 0) {
    if (length(offenders) > 3) {
      offenders <- offenders[1:3]
      msg_among_others <- ", among others"
    } else {
      msg_among_others <- ""
    }
    offenders <- paste0("`", offenders, "`")
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "`{name}` contains {offenders}{msg_among_others}.",
      "x" = "It can't be negative."
    ))
  }
}



is_even <- function(x) {
  x %% 2 == 0
}



check_length_disperse_n <- function(n, msg_single) {
  if (length(n) != 1) {
    if (length(n) == 2) {
      msg_single <- paste(
        msg_single, "Did you mean to call `disperse2(n = c({n[1]}, {n[2]}))`?"
      )
    }
    cli::cli_abort(c(
      "`n` has length {length(n)}.",
      "x" = msg_single,
      ">" = "See documentation under `?disperse`."
    ))
  }
}



# Three helpers for `function_map_seq()` as well as its assorted `reverse_` and
# `summarize_` functions:

# 1. MAYBE I ONLY NEED THIS ONE?! This function relies on `x` being a double,
# integer, or string vector of length > 1 that consists of a numeric sequence
# with exactly one missing link, such that it's possible to interpolate the
# missing value in a deterministic way.
index_case_interpolate <- function(x, index_case_only = TRUE,
                                   index_itself = FALSE) {
  x_orig <- x
  x <- as.numeric(x)

  index_seq <- purrr::map_dbl(seq_along(x), ~ x[.] - x[. + 1])
  # This doesn't seem to work: `index_seq <- x[x] - x[x + 1]`
  index_seq <- stats::na.omit(abs(index_seq))

  index_target <- match(max(index_seq), index_seq)

  if (index_itself) {
    return(index_target)
  }

  index_case <- x[index_target] + x[index_target + 1]
  index_case <- index_case / 2
  index_case <- methods::as(index_case, typeof(x_orig))

  if (is.character(index_case)) {
    x_orig_around_target <- c(x_orig[index_target], x_orig[index_target + 1])
    dp_orig <- max(decimal_places(x_orig_around_target))
    index_case <- restore_zeros(index_case, width = dp_orig)
  }

  if (index_case_only) {
    return(index_case)
  }

  # The rest only gets run if the entire sequence was required:
  out <- append(x, index_case, after = index_target)
  out <- methods::as(out, typeof(x_orig))

  if (is.character(out)) {
    out <- restore_zeros(out)
  }

  return(out)
}


# # 2. MAYBE I DON'T EVEN NEED THIS ONE?!
# index_case_recover <- function(x, index_case_only = TRUE,
#                                index_itself = FALSE) {
#   lx <- length(x)
#
#   if (is_even(lx)) {
#     index_target <- lx / 2
#     from <- x[index_target]
#   } else {
#     ico <- index_case_only
#     ii  <- index_itself
#     out <- index_case_interpolate(x, index_case_only = ico, index_itself = ii)
#     return(out)
#   }
#
#   if (index_itself) {
#     return(index_target)
#   }
#
#   index_case <- seq_distance(
#     from = from, length_out = 1, offset_from = 1, string_output = "auto"
#   )
#
#   if (index_case_only) {
#     return(index_case)
#   } else {
#     out <- append(x, index_case, after = index_target)
#     return(out)
#   }
#
# }


# 3.
index_case_diff <- function(data) {
  var <- data$var[[1]]
  data_var <- data[var][[1]]
  index <- index_case_interpolate(data_var, index_itself = TRUE)
  index_diff <- 1:nrow(data) - index

  # out <- dplyr::mutate(
  #   data,
  #   index = !!var,
  #   index = index_case_recover(index, index_itself = TRUE),
  #   diff = 1:nrow(data) - index
  # )

  # out$diff[out$diff < 1] <-
  #   out$diff[out$diff < 1] - 1

  index_diff[index_diff < 1] <-
    index_diff[index_diff < 1] - 1

  index_diff <- as.integer(index_diff)

  dplyr::mutate(data, index_diff)
}



# Helper for `split_by_parens()`:
transform_split_parens_object <- function(data) {

  class_d <- class(data)

  end1 <- class_d[stringr::str_detect(class_d, "scr_end1_")]
  end1 <- stringr::str_remove(end1, "scr_end1_")

  end2 <- class_d[stringr::str_detect(class_d, "scr_end2_")]
  end2 <- stringr::str_remove(end2, "scr_end2_")

  uscore_end1 <- paste0("_", end1)
  uscore_end2 <- paste0("_", end2)

  cols_1 <- data %>%
    dplyr::select(contains(uscore_end1)) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = ".origin",
      values_to = end1
    )

  cols_1 <- cols_1 %>%
    dplyr::mutate(key = 1:nrow(cols_1))

  cols_2 <- data %>%
    dplyr::select(contains(uscore_end2)) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = ".origin_2",
      values_to = end2
    )

  cols_2 <- cols_2 %>%
    dplyr::mutate(key = 1:nrow(cols_2))

  out <- dplyr::left_join(cols_1, cols_2, by = "key")

  out$key <- NULL
  out$.origin_2 <- NULL

  out <- out %>%
    dplyr::mutate(.origin = stringr::str_remove(.data$.origin, uscore_end1)) %>%
    dplyr::arrange(.data$.origin)

  return(out)
}



select_key_columns <- function(data, before = "consistency") {
  index_last_key_col  <- match(before, colnames(data)) - 1
  data[1:index_last_key_col]
}



get_rounding_class <- function(x) {
  x_cl <- class(x)
  x_cl[stringr::str_detect(x_cl, "scr_rounding_")]
}

get_rounding_class_arg <- function(x) {
  out <- get_rounding_class(x)
  stringr::str_remove(out, "scr_rounding_")
}


backticks <- function(x) {
  paste0("`", x, "`")
}



# Custom function as a workaround to replace `tidyr::unnest_wider()` within
# mapper functions -- specifically, `grim_map()` -- because `unnest_wider()` has
# become too slow for that job:
unnest_consistency_cols <- function(results, col_names) {
  n_cols <- length(col_names)

  consistency_list <- results$consistency %>%
    purrr::map(unlist)

  consistency_df <- consistency_list %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    t() %>%
    tibble::as_tibble(.name_repair = ~ paste0("V", 1:n_cols)) %>%
    dplyr::mutate(V1 = as.logical(V1))

  colnames(consistency_df) <- col_names

  index_consistency <- results %>%
    select_key_columns() %>%
    ncol() %>%
    `+`(1)

  results <- results %>%
    dplyr::select(-consistency) %>%
    dplyr::bind_cols(consistency_df) %>%
    dplyr::relocate(ratio, .after = index_consistency + n_cols)

  return(results)
}



