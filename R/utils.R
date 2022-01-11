
#' @include import-reexport.R

utils::globalVariables(c(
  ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
  "frac"
))



# Do NOT export any of these! ---------------------------------------------


wrong_spec_string <- function(x) {
  dplyr::if_else(
    is.character(x),
    glue::glue("\"{x}\""),
    glue::glue("`{x}` (not a string)")
  )
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

  purrr::flatten(purrr::map(y, rlang::eval_bare))
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


str_atomize <- function(string) {
  out <- stringr::str_split(string, "")

  if (length(string) == 1) {
    unlist(out)
  } else {
    out
  }

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



proto_rounding_singular <- function(x, bad, good_1, good_2) {
  if (bad %in% x) {
    cli::cli_abort(c(
      "`rounding` given as \"{bad}\" plus others",
      "x" = "If `rounding` has length > 1, only single rounding procedures \\
      are supported, such as \"{good_1}\" and \"{good_2}\".",
      "i" = "You can still concatenate multiple of them; just leave out \\
      those with \"_or_\"."
    ))
  }
}

check_rounding_singular <- function(x) {
  if (length(x) > 1) {
    proto_rounding_singular(x, "up_or_down", "up", "down")
    proto_rounding_singular(x, "up_from_or_down_from", "up_from", "down_from")
    proto_rounding_singular(x, "ceiling_or_floor", "ceiling", "floor")
  }
}


# reround_to_fraction(
#   x = c(4.23, 6.29, 2.74),
#   denominator = c(20, 60),
#   digits = 1:20,
#   rounding = c("down", "even", "ceiling")
# )


proto_lengths_congruent <- function(x, y, residues,
                                    x_name, y_name, residues_names) {

  if (length(x) > 1 && length(y) > 1) {
    if (length(x) != length(y)) {
      msg_need <-
        "Both need to have the same length unless either has length 1."
      if (length(residues) > 0) {
        residues_names <- paste0("`", residues_names, "`")
        msg_need <- paste(
          msg_need,
          "This also applies to {residues_names}."
        )
      }
      cli::cli_abort(c(
        "Lengths of `{x_name}` and `{y_name}` are not congruent",
        "x" = "`{x_name}` has length {length(x)}.",
        "x" = "`{y_name}` has length {length(y)}.",
        "!" = msg_need
      ))
    } else {
      cli::cli_warn(c(
        "`{x_name} ` and `{y_name}` values get paired",
        "!" = "Are you sure each `{x_name}` value should correspond to a \\
        different `{y_name}`?",
        ">" = "It might be better if at least one of `{x_name}` and \\
        `{y_name}` has length 1."
      ))
    }
  }

}


check_lengths_congruent <- function(var_list) {
  var_names <- as.character(rlang::enexprs(var_list)[[1]][-1])
  var_lengths <- purrr::map_int(var_list, length)
  var_list <- var_list[var_lengths > 1]

  if (length(var_list) > 1) {
    var_names <- var_names[var_lengths > 1]
    length_dup <- duplicated(var_lengths)
    var_list <- var_list[!length_dup]
    var_names <- var_names[!length_dup]
    residues_list <- var_list[-(1:2)]
    residues_names <- var_names[-(1:2)]

    proto_lengths_congruent(
      var_list[[1]], var_list[[2]], residues_list,
      var_names[[1]], var_names[[2]], residues_names
    )
  }

}


