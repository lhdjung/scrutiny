
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



# Super-short functions used within `decimal_places()`:
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
  return(x)
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




# `check_lengths_congruent()` takes a list of user-supplied arguments
# (`var_list`) and checks if two or more of them have lengths that are greater
# than 1. If at least two of these lengths are also different from each other,
# the function throws a precisely informative error. If they have the same > 1
# length and the `warn` argument is `TRUE` (the default), there will be an
# informative warning. The only dependencies of this function are {rlang} and
# {cli}. As these are tidyverse backend packages most users have installed
# already, the function might conceivably be used more widely.

check_lengths_congruent <- function(var_list, warn = TRUE) {
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
    if (length(var_list_gt1) > 1) {

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



