
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


reconstruct_sd <- Vectorize(reconstruct_sd_scalar)



# Used in unit testing:
integer_places <- function(x) {
  x <- stringr::str_split_fixed(stringr::str_trim(x), "\\.", n = 2)[, 1]
  stringr::str_length(x)
}



# # Unclear if this one is ever needed:
# mantissa_only_has_digits <- function(x, sep = "\\.") {
#   x <- stringr::str_remove(x[!is.na(x)], sep)
#
#   !stringr::str_detect(x, "[^[:digit:]]")
# }



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
# checks which values are present in a row how many times; hence "equivalent":
remove_equivalent_rows <- function(data) {
  data_array <- apply(data, 1, sort)

  data[!duplicated(data_array, MARGIN = 2), ]
}


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



