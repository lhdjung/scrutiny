
#' @include import-reexport.R

utils::globalVariables(c(
  ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
  "frac", "distance", "both_consistent", "fun", "var", "dispersion", "out_min",
  "out_max", "include_reported", "n", "times", "value", "name", "setNames",
  "rounding", "case", "n_sum", "V1", "consistency", "ratio", "scr_index_case",
  "dust", "starts_with", "value_duplicated", "variable", "sd_lower",
  "sd_incl_lower", "sd_upper", "sd_incl_upper", "x_lower", "x_upper",
  "dupe_count"
))



# Do NOT export any of these! ---------------------------------------------


#' Mark a string as wrong
#'
#' @param x Object that should have been a string (it isn't; that's why the
#'   function is called.)
#'
#' @return String.
#'
#' @noRd
wrong_spec_string <- function(x) {
  if (is.character(x)) {
    glue::glue("\"{x}\"")
  } else {
    glue::glue("`{x}` (not a string)")
  }
}



#' DEBIT helper for SD reconstruction
#'
#' @param formula String. For now, this has to be `"mean_n"`.
#' @param x,n String. Binary mean and sample size.
#' @param group_0,group_1 Numeric. Number of values coded 0 and 1, respectively.
#'
#' @return Numeric.
#'
#' @noRd
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


# Vectorized version of `reconstruct_sd_scalar()`:
reconstruct_sd <- Vectorize(reconstruct_sd_scalar, USE.NAMES = FALSE)



#' Count integer places
#'
#' Used in unit testing. Analogous to `decimal_places()`.
#'
#' @param x Numeric (or string that can be coerced to numeric). Object with
#'   integer places to count.
#'
#' @return Integer.
#'
#' @noRd
integer_places <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed("\\.", n = 2) %>%
    .[, 1] %>%
    stringr::str_length()
}




#' Collect dots-arguments in a list
#'
#' A helper for tidy evaluation used within `is_subset_of_vecs()` and friends
#' (i.e., other functions documented on that page).
#'
#' @param ... Any number of values.
#'
#' @return List.
#'
#' @noRd
straighten_out <- function(...) {
  y <- rlang::enexprs(...)
  purrr::flatten(purrr::map(y, rlang::eval_bare))
}



#' Write "an" or "a", depending on the preceding word
#'
#' @param x String. A string value that ends on a vowel letter returns `"an"`;
#'   else, it returns `"a"`.
#'
#' @return String.
#'
#' @noRd
an_a <- function(x) {
  dplyr::if_else(stringr::str_detect(x, "^[aeiou]"), "an", "a")
}


#' Prefix an object's type with "an" or "a"
#'
#' This uses `an_a()` to prepend the type of `x` with "an" or "a".
#'
#' @param x Any object.
#'
#' @return String.
#'
#' @noRd
an_a_type <- function(x) {
  type <- dplyr::if_else(is.double(x), "double (numeric value)", typeof(x))
  glue::glue("{an_a(typeof(x))} {type}")
}



#' Check if numbers are whole
#'
#' This function was adapted from the examples of `?is.integer()`, where it's
#' called `is.wholenumber()`. To test if R itself considers a vector
#' integer-like, use `rlang::is_integerish()` instead.
#'
#' @param x Numeric.
#' @param tolerance Numeric. The default is close to `1 / (10 ^ 8)`, which is
#'   reasonable but still user-imposed.
#'
#' @return Boolean vector of length `length(x)`.
#'
#' @noRd
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}



#' Subset every `n`th element from a vector `x`:
#'
#' @param x Vector from which the `n`th element should be subsetted.
#' @param n Numeric. Distance between two consecutive elements that will be
#'   subsetted.
#' @param from Numeric. Index of `x` where subsetting will start. Default is
#'   `1`.
#'
#' @return Vector containing some (or, in theory, all) elements of `x`.
#'
#' @noRd
parcel_nth_elements <- function(x, n, from = 1) {
  x[seq(from = from, to = length(x), by = n)]
}



#' Drop rows with equivalent values, regardless of their order
#'
#' This function differs from `dplyr::distinct()` mainly insofar as it doesn't
#' take the order of values within a row into account. Thus, it only checks
#' which values are present in a row how many times; hence "equivalent" (see
#' example below the function).
#'
#' @param data Data frame.
#'
#' @return Data frame. Each row is now unique.
#'
#' @examples
#' df <- tibble::tribble(
#'   ~a, ~b, ~c,
#'   1, 2, 2,
#'   2, 1, 2,
#'   1, 2, 3
#' )
#'
#' No two rows are identical here, so `dplyr::distinct()` preserves all of them.
#' However, `remove_equivalent_rows()` cuts the second row. That is because the
#' first two rows contain all the same values. They only differ in terms of the
#' order of values, which the function ignores.
#' remove_equivalent_rows(df)
#'
#' @noRd
remove_equivalent_rows <- function(data) {
  data_array <- apply(data, 1, sort)

  data[!duplicated(data_array, MARGIN = 2), ]
}



#' Switch back and front columns
#'
#' @param data Data frame
#'
#' @return Data frame, like `data` but with the column order reversed.
#'
#' @noRd
reverse_column_order <- function(data) {
  col_numbers_reversed <- ncol(data):1
  data[, order(col_numbers_reversed)]
}



#' Censor left and right
#'
#' `censor()` is used in some of scrutiny's unit tests. The `left` and `right`
#' arguments should only be length 1, although this is not checked.
#'
#' @param x Numeric.
#' @param left Numeric. Lower bound. Any elements of `x` that are less than
#'   `left` will be replaced by `left`.
#' @param right Numeric. Upper bound. Any elements of `x` that are greater than
#'   `right` will be replaced by `right`.
#'
#' @return Numeric vector of length `length(x)`.
#'
#' @noRd
censor <- function(x, left, right) {
  x[x < left] <- left
  x[x > right] <- right
  x
}



#' Conveniently add classes to an object
#'
#' @param x Some object. In scrutiny, always a tibble.
#' @param new_class String. One or more classes that will be prepended to the
#'   classes of `x`.
#'
#' @return `x` but with new classes.
#'
#' @noRd
add_class <- function(x, new_class) {
  class(x) <- c(new_class, class(x))
  x
}



#' Remove any `NA` elements
#'
#' Mapped within `row_to_colnames()`.
#'
#' @param x Vector.
#'
#' @return `x` but without any `NA` elements.
#'
#' @noRd
remove_na <- function(x) {
  x[!is.na(x)]
}




#' Check if lengths are congruent
#'
#' `check_lengths_congruent()` is called within a function `f()` and takes a
#' list of arguments to `f()` supplied by the user (`var_list`). It checks if
#' two or more of those arguments have lengths that are greater than 1.
#'
#' If at least two of these lengths are also different from each other and the
#' `error` argument is `TRUE` (the default), the function will throw a precisely
#' informative error. If they have the same > 1 length and the `warn` argument
#' is `TRUE` (the default), there will be an informative warning.
#'
#' The only dependencies of this function are {rlang} and {cli}. As these are
#' tidyverse backend packages that most users have installed already, the
#' function might conceivably be used more widely.
#'
#' @param var_list List of variables that were passed to the enclosing function
#'   as arguments.
#' @param error Boolean (length 1). Should an error be thrown if lengths are not
#'   congruent? Default is `TRUE`.
#' @param warn Boolean (length 1). If no error is thrown, should a warning be
#'   issued if appropriate (see description)? Default is `TRUE`.
#'
#' @return No return value; might throw error or warning.
#'
#' @noRd
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



#' Check length
#'
#' Make sure a vector `x` has length `l`, otherwise throw an informative error.
#' For example, if a vector called `vals` needs to have length 1, run:
#' `check_length(vals, 1)`.
#'
#' @param x Vector.
#' @param l Numeric. Length that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_length <- function(x, l) {
  if (length(x) != l) {
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "`{name}` has length {length(x)}.",
      "x" = "It needs to have length {l}."
    ))
  }
}



#' Check length (with `NULL`-related message)
#'
#' Same as `check_length()` except the error message says that `x` might be
#' `NULL` instead of a vector of length `l`. However, the function doesn't check
#' this, so it should only be called in a context when this condition was
#' checked already.
#'
#' @param x Vector.
#' @param l Numeric. Length that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_length_or_null <- function(x, l) {
  if (length(x) != l) {
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "`{name}` has length {length(x)}.",
      "x" = "It needs to have length {l} or to be `NULL`."
    ))
  }
}



#' Check type
#'
#' Much the same as `check_length()`, but for object types rather than lengths.
#' An object `x` needs to have one of the types in `t`, or else there will be an
#' informative error.
#'
#' @param x Vector.
#' @param t Numeric. Type that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
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



#' Check class
#'
#' Much the same as `check_length()` or `check_type()`, but for classes. An
#' object `x` needs to have one of the types in `t`, or else there will be an
#' informative error.
#'
#' @param x Vector.
#' @param cl Numeric. Class that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_class <- function(x, cl) {
  if (!inherits(x, cl)) {
    msg_name <- deparse(substitute(x))
    cli::cli_abort(c(
      "Required class missing.",
      "x" = "`{msg_name}` doesn't inherit class \"{cl}\"."
    ))
  }
}



#' Split into groups
#'
#' Split up a vector `x` into groups that each consist of a number of elements
#' equal to `group_size` -- or, if the division has a remainder, the final group
#' will have fewer elements, and the function will issue an informative warning.
#'
#' @param x Vector.
#' @param group_size Number of elements in each resulting group (except,
#'   perhaps, the last group).
#'
#' @return Named list of groups. The names are equal to the indices.
#'
#' @noRd
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


#' Split a data frame into rows
#'
#' Each row becomes a list-element.
#'
#' @param data Data frame or matrix.
#'
#' @return Named list of rows. The names are equal to the row numbers.
#'
#' @noRd
split_into_rows <- function(data) {
  split_into_groups(x = t(data), group_size = ncol(data))
}



#' Step size (stride) of a single decimal number
#'
#' Computes the smallest possible difference between two numbers on the lowest
#' decimal level of `x`.
#'
#' @param x Numeric (or string coercible to numeric).
#'
#' @return Numeric.
#'
#' @noRd
step_size_scalar <- function(x) {
  digits <- decimal_places_scalar(x)
  1 / (10 ^ digits)
}



#' Lowest step size (stride) of decimal numbers
#'
#' Like `step_size_scalar()` above, but `x` can have any length. The function
#' will compute the step size of the one element of `x` with the most decimal
#' numbers.
#'
#' @param x Numeric (or string coercible to numeric).
#'
#' @return Numeric.
#'
#' @noRd
step_size <- function(x) {
  digits <- max(decimal_places(x))
  1 / (10 ^ digits)
}



#' Sequence typing helper
#'
#' This helper is called within the sequence-generating functions
#' `seq_distance()`, `seq_endpoint()`, and `seq_disperse()`. It processes their
#' `string_output` argument and coerces the preliminary return sequence, `out`,
#' to the desired type. Depending on how `string_output` was specified by the
#' user, this type might be the type of the argument that was the original
#' starting point of the sequence, `from`.

#' The function coerces `out` to string (and pads it with trailing zeros using
#' `restore_zeros()`) in either of these two cases, and to the type of `from()`
#' otherwise:
#' - `string_output` is `TRUE`.
#' - `string_output` is `"auto"` and `from` is string.

#' @param out Numeric (or string coercible to numeric). Preliminary sequence
#'   output of the calling `seq_*()` function.
#' @param from Numeric (or string coercible to numeric). Argument of the calling
#'   function. The `out` sequence was generated starting from this point.
#' @param string_output Boolean (or a string that says `"auto"`).
#' @param digits Numeric. Number of digits to which `out` will be padded if it's
#'   coerced to string.
#'
#' @return Numeric or string (see above).
#'
#' @noRd
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



#' Paste and enumerate with commas and `"and"`
#'
#' - If `x` is length 1, it is returned unchanged.
#' - If `x` is length 2, its elements are pasted together but separated by word
#'   "and".
#' - If `x` is length > 2, all of its elements will be pasted into a string, but
#' separated by commas within this string. The word "and" is inserted before the
#' last element.
#'
#' @param x String (or coercible to string).
#'
#' @return String (length 1).
#'
#' @noRd
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



#' Check that no element of a numeric vector is negative
#'
#' Throws error if any element of `x` is less than 0.
#'
#' @param x Numeric.
#'
#' @return No return value; might throw error.
#'
#' @noRd
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



#' Test for even parity
#'
#' `TRUE` for every even element of `x`, `FALSE` for every odd one.
#'
#' @param x Numeric.
#'
#' @return Boolean vector of length `length(x)`.
#'
#' @noRd
is_even <- function(x) {
  x %% 2 == 0
}



#' Check for length-1 sample size in dispersion functions
#'
#' @description Only used within `disperse()` and `disperse_total()`. In these
#'   functions, the `n` argument needs to be length 1. This is in contrast to
#'   `disperse2()` where it needs to be length 2, so a length-2 `n` will trigger
#'   an error message that specifically points to `disperse2()`.
#'
#'   All `n` values with a length other than 1 will trigger an error that refers
#'   the user to `?disperse()`.
#'
#' @param n Argument from `disperse()` or `disperse_total()` by the same name.
#' @param msg_single String (length 1). Error message specific to the calling
#'   function, i.e., `disperse()` or `disperse_total()`: Their reasons for
#'   requiring a length-1 `n` differ from each other.
#'
#' @return No return value; might throw error.
#'
#' @noRd
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



#' Test if a vector is numeric or coercible to numeric
#'
#' @description `is_numericish()` returns `TRUE` if `x` is a non-factor vector
#'   in which at least one element can be coerced to a non-`NA` numeric value,
#'   and `FALSE` otherwise.
#'
#'   This is meant to implement the common notion of an R vector being
#'   "coercible to numeric". However, factors are excluded here because treating
#'   them like numeric variables is not useful in the context of scrutiny: The
#'   package often deals with number-strings, as in `restore_zeros_df()`, which
#'   uses `is_numericish()` as a selector. Accepting factors there would make no
#'   sense.
#'
#'   `check_type_numericish()` throws an informative error if `is_numericish()`
#'   returns `FALSE`.
#'
#' @param x Any object.
#'
#' @return Boolean (length 1) for `is_numericish()`; none for
#'   `check_type_numericish()` which might throw an error.
#'
#' @details `is_numericish()` resulted in Hadley Wickham liking one of my
#'   tweets! :D https://twitter.com/lukasjung_hd/status/1571852033996595200
#'
#' @noRd
is_numericish <- function(x) {
  if (!rlang::is_vector(x)) {
    return(FALSE)
  }
  if (is.factor(x)) {
    return(FALSE)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  x <- suppressWarnings(as.numeric(x))
  !any(is.na(x))
}


check_type_numericish <- function(x) {
  if (isFALSE(is_numericish(x))) {
    name <- deparse(substitute(x))
    if (rlang::is_vector(x)) {
      length_non_na <- length(x[!is.na(x)])
      if (length_non_na == 1L) {
        msg_values <- "a non-`NA` value"
        msg_elements <- "element"
      } else {
        msg_values <- "non-`NA` values"
        msg_elements <- "elements"
      }
      if (is.factor(x)) {
        msg_factor <- " In the present context, it must also not be a factor."
      } else {
        msg_factor <- ""
      }
      cli::cli_abort(c(
        "`{name}` must be numeric or coercible to numeric.",
        ">" = "(This means that converting it to numeric \\
        must return {msg_values} for its {length_non_na} \\
        non-`NA` {msg_elements}.{msg_factor})"
      ))
    } else {
      cli::cli_abort(c(
        "`{name}` is {an_a_type(x)}.",
        "x" = "It must be numeric or coercible to numeric."
      ))
    }
  }
}



#' Test if numeric-like vectors contain at least some decimal places
#'
#' For numeric-like `x` inputs (as determined by `is_numericish()`),
#' `has_decimals_if_numericish()` checks if at least one element of `x` has at
#' least one decimal place. If so, or if `x` is not numeric-like, the function
#' returns `TRUE`. Otherwise, it returns `FALSE`.
#'
#' @param x Object to test.
#' @param sep Separator between the integer and decimal parts. Passed on to
#'   `decimal_places()`. Default is `"\\."`, a decimal point.
#'
#' @return Boolean (length 1).
#'
#' @noRd
has_decimals_if_numericish <- function(x, sep = "\\.") {
  !is_numericish(x) || !all(decimal_places(x, sep = sep) == 0L)
}



#' Interpolate the index case
#'
#' @description This function expects an `x` vector like the one described
#'   elsewhere for `index_seq()`, with the additional expectation that
#'   continuous sequences have an odd length. That is because an index case
#'   needs to be identified; and without a gap in the sequence, this has to be a
#'   single median value. If the index case is missing, it is reconstructed and
#'   returned.
#'
#'   If the sequence is continuous, the index case is identical to the median,
#'   so this metric is returned. All of that works independently of the step
#'   size.
#'
#' @param x Numeric (or coercible to numeric).
#' @param index_case_only Boolean. If `TRUE` (the default), only the
#'   reconstructed index case is returned. If `FALSE`, the entire `x` sequence
#'   is returned, with the index case inserted at the center.
#' @param index_itself If set to `TRUE`, the index of the "index case" is
#'   returned, as opposed to the index case itself.
#'
#' @return Numeric (or string coercible to numeric).
#'
#' @noRd
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



#' Compute difference to index case in `audit_seq()`
#'
#' @description Mapped within `audit_seq()`, this helper wraps
#'   `index_case_interpolate()` to calculate the difference between an index
#'   case and the next consistent value within a dispersed sequence.
#'
#'   The function should likely not be used in any other context.
#'
#' @param data Data frame created as an intermediate product within
#'   `audit_seq()`.
#'
#' @return Data frame.
#'
#' @noRd
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



#' Compute central index
#'
#' @param x Vector of an odd length (!).
#'
#' @return Index of the central value in `x`.
#'
#' @noRd
index_central <- function(x) {
  ((length(x) - 1) / 2) + 1
}



#' Transformation helper for `split_by_parens()`
#'
#' @description Only called within `split_by_parens()`, and only if the latter
#'   function's `.transform` argument is set to `TRUE`.
#'
#'   `transform_split_parens()` pivots the data into a longer format using
#'   `tidyr::pivot_longer()`. It lumps values from all original columns into two
#'   new columns named after the two split-column endings (`"x"` and `"sd"` by
#'   default), but preserves the information about their origin by storing it in
#'   a `.origin` column.
#'
#' @param data Data frame created as an intermediate product within
#'   `split_by_parens()`.

#' @return Data frame with these columns:
#' - `.origin`: Names of the original columns of the data frame that
#'   `split_by_parens()` took as an input.
#' - Two columns named after the values of `split_by_parens()`'s `.col1` and
#'   `.col2` arguments. Default are `"x"` and `"sd"`.
#'
#' @noRd
transform_split_parens <- function(data, end1, end2) {

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

  out %>%
    dplyr::mutate(.origin = stringr::str_remove(.data$.origin, uscore_end1)) %>%
    dplyr::arrange(.data$.origin)
}



#' Select columns before `"consistency"`
#'
#' Useful helper for selecting all "tested" columns in the sense of
#' `vignette("consistency-tests")`; i.e., those columns that factored into a
#' consistency test applied by a mapper function like `grim_map()`.
#'
#' @param data Data frame resulting from a consistency test mapper function,
#'   such as `grim_map()`.
#' @param before String (length 1). Name of the first column that will not be
#'   selected. Default is `"consistency"`, which should hardly be changed.
#'
#' @return Data frame with a number of columns equal to $k - 1$, where $k$ is
#'   the index of `before` in `data`.
#'
#' @noRd
select_tested_cols <- function(data, before = "consistency") {
  index_last_key_col <- match(before, colnames(data)) - 1
  data[1:index_last_key_col]
}



#' Extract rounding class from an object
#'
#' @description In scrutiny, information about the rounding method used to
#'   compute some or all values in a tibble is conveyed via a class which starts
#'   on `"scr_rounding_"` and which is inherited by that tibble.
#'
#'   This function extracts any such rounding classes from `x`. If it returns
#'   multiple classes, something went wrong with `x` earlier on.
#'
#' @param x Any object, but typically a tibble.
#'
#' @return String. Should be length 1, or else there is a problem.
#'
#' @noRd
get_rounding_class <- function(x) {
  x_cl <- class(x)
  x_cl[stringr::str_detect(x_cl, "scr_rounding_")]
}



#' Extract bare rounding string
#'
#' Wrapper around `get_rounding_class()` that removes the `"scr_rounding_"`
#' prefix from its output. This might be useful for re-processing the original
#' `rounding` argument's value in `reround()` or in a function that calls
#' `reround()` internally. Not currently used.
#'
#' @param x Any object, but typically a tibble.
#'
#' @return String. Should be length 1, or else there is a problem. See
#'   `get_rounding_class()`.
#'
#' @noRd
get_rounding_class_arg <- function(x) {
  out <- get_rounding_class(x)
  stringr::str_remove(out, "scr_rounding_")
}



#' Wrap into backticks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_backticks <- function(x) {
  paste0("`", x, "`")
}



#' Wrap into quotation marks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes <- function(x) {
  paste0("\"", x, "\"")
}



#' Wrap into quotation marks if string
#'
#' For error messages and similar. `x` is returned unchanged unless it's a
#' string, in which case it's treated as in `wrap_in_quotes()`.
#'
#' @param x Any object.
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes_if_string <- function(x) {
  if (is.character(x)) {
    x <- paste0("\"", x, "\"")
  }
  x
}



#' Wrap into quotation marks if string, else in backticks
#'
#' For error messages and similar. Like `wrap_in_quotes_if_string()` except a
#' non-string `x` is wrapped into backticks (rather than being returned
#' unchanged).
#'
#' @param x Any object.
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes_or_backticks <- function(x) {
  if (is.character(x)) {
    x <- paste0("\"", x, "\"")
  } else {
    x <- paste0("`", x, "`")
  }
  x
}



#' Test numbers for near-equality, other objects for identity
#'
#' When testing for equality, strict equality as assessed by `identical()` would
#' be asking too much from numeric values, so `dplyr::near()` is used if both
#' `x` and `y` are numeric. `identical()` is used otherwise.
#'
#' @param x,y Two objects to be compared.
#'
#' @return Boolean (length 1).
#'
#' @details Since `near()` is vectorized and `identical()` is not, their results
#'   are not on par with each other, so `near()` needs to be wrapped in `all()`,
#'   which makes sure that there are no differences beyond the tolerance.
#'
#' @noRd
about_equal <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    all(dplyr::near(x, y))
  } else {
    identical(x, y)
  }
}



#' Drop all columns with specific string in name
#'
#' All column with names that include `drop_with` are removed.
#'
#' @param data Data frame.
#' @param drop_with String (length 1).
#'
#' @return Data frame.
#'
#' @noRd
drop_cols_with <- function(data, drop_with) {
  data[!stringr::str_detect(names(data), drop_with)]
}



# Get the name of a function that is being called. By default (`n = 1`), that's
# the function within which `name_caller_call()` is called. Also by default
# (`wrap = TRUE`), the name is coerced to string, suffixed with parentheses, and
# wrapped in backticks. Example: f --> `f()`


#' Get name of function being called
#'
#' Returns the name of the function within which `name_caller_call()` is called
#' (by default of `n`).
#'
#' @param n The number of callers to go back. See `?rlang::caller_call()`.
#' @param wrap Boolean. If `TRUE` (the default), the output is wrapped into
#'   backticks and appended with `()`.
#'
#' @return String (length 1).
#'
#' @noRd
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



#' Subtle variations to numbers
#'
#' @description Reduplicate a numeric vector, subtly varying it below and above
#'   the original. This avoids issues of spurious precision in floating-point
#'   arithmetic.
#'
#'   The difference is the global variable `dust`.
#'
#' @param x Numeric.
#'
#' @return Numeric vector of length `2 * length(x)`.
#'
#' @details The idea is to catch very minor variation from `x` introduced by
#'   spurious precision in floating point numbers, so that such purely
#'   accidental deviations don't lead to false assertions of substantively
#'   important numeric difference when there is none.
#'
#' @noRd
dustify <- function(x) {
  c(x - dust, x + dust)
}



#' Count `NA` elements
#'
#' Mapped within `summarize_audit_special()`.
#'
#' @param x Vector.
#' @param ... The dots are merely pro forma; their purpose is to swallow up the
#'   `na.rm = TRUE` specification in a for loop with `dplyr::across()`.
#'
#' @return Integer (length 1).
#'
#' @noRd
na_count <- function(x, ...) {
  length(x[is.na(x)])
}



#' Remove scrutiny classes
#'
#' Strip any and all scrutiny classes from `x`: those classes that start on
#' `"scr_"`. The function's name follows `base::unclass()`.
#'
#' @param x Any object, but typically a tibble.
#'
#' @return `x`, but without `"scr_"` classes.
#'
#' @noRd
unclass_scr <- function(x) {
  class(x) <- class(x)[!stringr::str_detect(class(x), "^scr_")]
  x
}


#' Check for ggplot2 versions
#'
#' These two functions negotiate a breaking change in ggplot2 since version
#' 3.4.0:
#'
#' - `check_ggplot2_size()` checks whether the default for the deprecated `size`
#' aesthetic was changed by the user. Call it if
#' `utils::packageVersion("ggplot2") >= 3.4` is `TRUE`.
#'
#' - `check_ggplot2_linewidth()` checks whether the default for the
#' not-yet-implemented `linewidth` aesthetic was changed by the user. Call it if
#' the `utils::packageVersion()` call above returns `FALSE`.
#'
#' As of now, these two functions are only used within `debit_plot()`.
#'
#' @param arg_old,default_old `size`-like parameter and its default value.
#' @param arg_new,default_new `linewidth`-like parameter and its default value.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_ggplot2_size <- function(arg_old, default_old) {

  if (arg_old != default_old) {
    msg1 <- paste0(
      "That's because your ggplot2 version is >= 3.4.0 (actually, ",
      utils::packageVersion("ggplot2"), ")."
    )
    msg2 <- paste(
      "In ggplot2, the `size` aesthetic has been deprecated since",
      "version 3.4.0."
    )
    msg3 <- "See https://www.tidyverse.org/blog/2022/11/ggplot2-3-4-0/#hello-linewidth"
    cli::cli_abort(c(
      paste0("`", arg_old, "` is deprecated for you."),
      "x" = msg1,
      "i" = msg2,
      "i" = msg3
    ))
  }

}


check_ggplot2_linewidth <- function(arg_new, default_new) {

  if (arg_new != default_new) {
    msg1 <- paste0(
      "That's because your ggplot2 version is < 3.4.0 (actually, ",
      utils::packageVersion("ggplot2"), ")."
    )
    msg2 <- paste(
      "In ggplot2, the `size` aesthetic has been deprecated since",
      "version 3.4.0. The `linewidth` aesthetic is used as a replacement,",
      "but it's not accessible for versions lower than 3.4.0."
    )
    msg3 <- "See https://www.tidyverse.org/blog/2022/11/ggplot2-3-4-0/#hello-linewidth"
    cli::cli_abort(c(
      paste0("You can't use `", arg_new, "`."),
      "x" = msg1,
      "i" = msg2,
      "i" = msg3
    ))
  }

}
