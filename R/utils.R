
#' @include import-reexport.R

utils::globalVariables(c(
  ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
  "frac", "distance", "both_consistent", "fun", "var", "dispersion", "out_min",
  "out_max", "include_reported", "n", "times", "value", "name", "setNames",
  "rounding", "case", "n_sum", "V1", "consistency", "ratio", "scr_index_case",
  "dust"
))



# Do NOT export any of these! ---------------------------------------------


wrong_spec_string <- function(x) {
  if (is.character(x)) {
    glue::glue("\"{x}\"")
  } else {
    glue::glue("`{x}` (not a string)")
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



# A helper for tidy evaluation used within `is_subset_of_vecs()` and friends
# (i.e., other functions documented on that page):
straighten_out <- function(...) {
  y <- rlang::enexprs(...)
  purrr::flatten(purrr::map(y, rlang::eval_bare))
}



an_a <- function(x) {
  dplyr::if_else(stringr::str_detect(x, "^[aeiou]"), "an", "a")
}


an_a_type <- function(x) {
  type <- dplyr::if_else(is.double(x), "double (numeric value)", typeof(x))
  glue::glue("{an_a(typeof(x))} {type}")
}


# This function was adapted from the examples of `?is.integer()`, where it's
# called `is.wholenumber()`. The default for the `tolerance` argument is close
# to 1 / (10 ^ 8), which is reasonable but still user-imposed. To test if R
# itself considers a vector integer-like, use `rlang::is_integerish()` instead.
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



# This function differs from `dplyr::distinct()` mainly insofar as it doesn't
# take the order of values within a row into account. Thus, it only checks which
# values are present in a row how many times; hence "equivalent" (see example
# below the function):
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

check_length_or_null <- function(x, l) {
  if (length(x) != l) {
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "`{name}` has length {length(x)}.",
      "x" = "It needs to have length {l} or to be `NULL`."
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


# Test if a vector `x` is numeric or coercible to numeric:
is_numericish <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  x <- x[!is.na(x)]
  x <- suppressWarnings(as.numeric(x))
  !any(is.na(x))
}


# This function expects an `x` vector like the one described above for
# `index_seq()`, with the additional expectation that continuous sequences have
# an odd length. That is because an index case needs to be identified, and
# without a gap in the sequence, this has to be a single median value. If the
# index case is missing, it is reconstructed and returned. If the sequence is
# continuous, the index case is identical to the median, so this metric is
# returned. All of that works independently of the step size:
index_case_interpolate <- function(x, index_case_only = TRUE,
                                   index_itself = FALSE) {
  x_orig <- x
  x <- as.numeric(x)

  index_seq_x <- index_seq(x)
  index_target <- match(max(index_seq_x), index_seq_x)

  # For continuous `x` sequences, the index case is already present in the
  # sequence as its median. It is here identified, coerced into the original
  # type of `x`, and then returned:
  if (is_seq_linear(x)) {
    index_case <- stats::median(x)
    index_case <- methods::as(index_case, typeof(x_orig))
    if (index_itself) {
      index_target <- match(index_case, x)
      return(index_target)
    }
    return(index_case)
  }

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




index_case_diff <- function(data) {
  var <- data$var[[1]]
  data_var <- data[var][[1]]
  index <- index_case_interpolate(data_var, index_itself = TRUE)
  index_diff <- 1:nrow(data) - index

  if (is_even(length(index_diff))) {
    index_diff[index_diff < 1] <-
      index_diff[index_diff < 1] - 1
  }

  index_diff <- as.integer(index_diff)

  dplyr::mutate(data, index_diff)
}



index_central <- function(x) {
  ((length(x) - 1) / 2) + 1
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



select_tested_cols <- function(data, before = "consistency") {
  index_last_key_col <- match(before, colnames(data)) - 1
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


# For error messages and similar, wrap objects into backticks or quotes:
wrap_in_backticks <- function(x) {
  paste0("`", x, "`")
}

wrap_in_quotes <- function(x) {
  paste0("\"", x, "\"")
}

wrap_in_quotes_if_string <- function(x) {
  if (is.character(x)) {
    x <- paste0("\"", x, "\"")
  }
  x
}

wrap_in_quotes_or_backticks <- function(x) {
  if (is.character(x)) {
    x <- paste0("\"", x, "\"")
  } else {
    x <- paste0("`", x, "`")
  }
  x
}




# When testing for equality, strict equality as assessed by `identical()` would
# be asking too much from numeric values, so `dplyr::near()` is used instead.
# Since `near()` is vectorized and `identical()` is not, their results are not
# on par with each other, so `near()` needs to be wrapped in `all()`, which
# makes sure that there are no differences beyond the tolerance:
about_equal <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    all(dplyr::near(x, y))
  } else {
    identical(x, y)
  }
}



# This little helper checks `data`'s column names and removes all columns the
# names of which include the string specified via `drop_with`:
drop_cols_with <- function(data, drop_with) {
  data[!stringr::str_detect(names(data), drop_with)]
}



# Get the name of a function that is being called. By default (`n = 1`), that's
# the function within which `name_caller_call()` is called. Also by default
# (`wrap = TRUE`), the name is coerced to string, suffixed with parentheses, and
# wrapped in backticks. Example: f --> `f()`
name_caller_call <- function(n = 1, wrap = TRUE) {
  name <- rlang::caller_call(n = n)
  name <- name[[1]]
  if (wrap) {
    name <- paste0("`", name, "()`")
  }
  name
}


# "Dust" variables were used by Nick Brown and later by Lukas Wallrich in
# rsprite2. They get rid of spurious precision in reconstructed decimal numbers:
dust <- 1e-12

# This applies the global `dust` variable to a number `x`. The idea is to catch
# very minor variation from `x` introduced by spurious precision in floating
# point numbers, so that such purely accidental deviations don't lead to false
# assertions of significant numeric difference when there is none:
dustify <- function(x) {
  c(x - dust, x + dust)
}



