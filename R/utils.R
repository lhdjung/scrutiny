
#' @include import-reexport.R is-numeric-like.R

utils::globalVariables(c(
  ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
  "frac", "distance", "both_consistent", "fun", "var", "dispersion", "out_min",
  "out_max", "include_reported", "n", "times", "value", "name", "setNames",
  "rounding", "case", "n_sum", "consistency", "ratio", "scr_index_case",
  "dust", "starts_with", "value_duplicated", "variable", "sd_lower",
  "sd_incl_lower", "sd_upper", "sd_incl_upper", "x_lower", "x_upper",
  "dupe_count", "fun_name",
  # Added after rewriting the function factories using `rlang::new_function()`:
  "!!", "!!!", "constant", "constant_index", "include_consistent",
  "n_min", "n_max",
  # Added for `function_duplicate_cols()`, which uses `rlang::new_function()`:
  "colname_end", "ignore", "numeric_only"
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
    paste0("\"", x, "\"")
  } else {
    paste0("`", x, "` (not a string)")
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
      "!" = "`formula` must be \"mean_n\", \"0_n\", \"1_n\", or \\
      \"groups\".",
      "x" = "It is {wrong_spec_string(formula)}."
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
    stringr::str_split_fixed("\\.", n = 2L) %>%
    .[, 1L] %>%
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



#' Write "an" or "a", depending on the next word
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
#' This uses `an_a()` to prepend the type of `x` with "an" or "a". Because the
#' function meant to be used in messages, it replaces "double" by "double
#' (numeric value)" and "character" by "string".
#'
#' @param x Any object.
#'
#' @return String.
#'
#' @noRd
an_a_type <- function(x) {
  type <- typeof(x)
  if (type == "double") {
    type <- "double (numeric value)"
  } else if (type == "character") {
    type <- "string"
  }
  paste(an_a(typeof(x)), type)
}



#' Check whether numbers are whole
#'
#' @description For each element of a numeric vector `x`, `is_whole_number()`
#'   checks whether that element is a whole number.
#'
#'   This is not the same as the integer data type, so doubles and integers are
#'   tested the same way. See the note in `?integer`. To test if R itself
#'   considers a vector integer-like, use `rlang::is_integerish()` instead.
#'
#' @param x Numeric.
#' @param tolerance Numeric. Any difference between `x` and a truncated version
#'   of `x` less than `tolerance` (in the absolute value) will be ignored. The
#'   default is close to `1 / (10 ^ 8)`. This avoids errors due to spurious
#'   precision in floating-point arithmetic.
#'
#' @return Logical vector of the same length as `x`.
#'
#' @details This function was adapted (with naming modifications) from the
#'   examples of `?integer`, where a very similar function is called
#'   `is.wholenumber()`.
#'
#' @author R Core Team, Lukas Jung
#'
#' @noRd
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tolerance
}



#' Subset every `n`th element
#'
#' @param x Vector from which the `n`th element should be subsetted.
#' @param n Numeric. Distance between two consecutive elements that will be
#'   subsetted.
#' @param from Numeric. Index of `x` where subsetting will start. Default is
#'   `1L`.
#'
#' @return Vector containing some (or, in theory, all) elements of `x`.
#'
#' @noRd
parcel_nth_elements <- function(x, n, from = 1L) {
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
  data_array <- apply(data, 1L, sort)
  data[!duplicated(data_array, MARGIN = 2L), ]
}



#' Switch back and front columns
#'
#' @param data Data frame
#'
#' @return Data frame, like `data` but with the column order reversed.
#'
#' @noRd
reverse_column_order <- function(data) {
  if (ncol(data) == 0L) {
    return(data)
  }
  # Don't mind sequence linting here; the early return above takes care of the
  # empty edge case already!
  col_numbers_reversed <- ncol(data):1L
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
#' `add_class()` is pipeable, unlike the replacement function it wraps.
#'
#' @param x Some object. In scrutiny, always a tibble.
#' @param new_class String. One or more classes that will be added to the
#'   `class(x)` attribute. They are prepended before the classes of `x`, so that
#'   subclasses that are added later take precedence over existing -- and more
#'   generic -- base classes.
#'
#' @return `x` but with new classes.
#'
#' @noRd
add_class <- function(x, new_class) {
  `class<-`(x, value = c(new_class, class(x)))
}



#' Check whether lengths are congruent
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
#' @param error Logical (length 1). Should an error be thrown if lengths are not
#'   congruent? Default is `TRUE`.
#' @param warn Logical (length 1). If no error is thrown, should a warning be
#'   issued if appropriate (see description)? Default is `TRUE`.
#'
#' @return No return value; might throw error or warning.
#'
#' @noRd
check_lengths_congruent <- function(var_list, error = TRUE, warn = TRUE) {
  var_names <- rlang::enexprs(var_list)
  var_lengths <- vapply(var_list, length, integer(1L), USE.NAMES = FALSE)
  var_list_gt1 <- var_list[var_lengths > 1L]

  # Condition of checking for error and warning:
  if (length(var_list_gt1) > 1L) {
    var_names <- var_names[[1L]][-1L]
    var_names <- as.character(var_names)
    var_names_gt1 <- var_names[var_lengths > 1L]
    vnames_gt1_all <- var_names_gt1   # for the warning

    length_dup <- duplicated(var_lengths)
    var_list_gt1 <- var_list_gt1[!length_dup]
    var_names_gt1 <- var_names_gt1[!length_dup]

    # Error condition, checking if there is more than one element of `var_list`
    # with a unique length greater than one (the duplicated lengths were
    # filtered out from `var_list_gt1` right above):
    if (error && (length(var_list_gt1) > 1L)) {

      x <- var_list_gt1[[1L]]
      y <- var_list_gt1[[2L]]
      x_name <- var_names_gt1[[1L]]
      y_name <- var_names_gt1[[2L]]

      residues_names <- var_names[!var_names %in% c(x_name, y_name)]

      msg_error <- c(
        "`{x_name}` and `{y_name}` must have the same length \\
        unless either has length 1.",
        "*" = "`{x_name}` has length {length(x)}.",
        "*" = "`{y_name}` has length {length(y)}."
      )

      # Append-to-error-message condition:
      if (length(residues_names) > 0L) {
        residues_names <- paste0("`", residues_names, "`")
        msg_error <- append(
          msg_error, c("i" = "This also applies to {residues_names}.")
        )
      }

      # Throw error:
      cli::cli_abort(msg_error)
    }

    # Warning condition, triggered if more than one element of `var_list` has
    # length > 1, it's the same length for all (hence no error), and the `warn`
    # argument is `TRUE` (the default):
    if (warn) {
      x_name <- vnames_gt1_all[[1L]]
      y_name <- vnames_gt1_all[[2L]]

      l_vnames <- length(vnames_gt1_all)

      if (l_vnames > 2L) {
        msg_example <- ", for example,"
      } else {
        msg_example <- ""
      }

      if (l_vnames == 2L) {
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
#' For example, if a vector called `vals` must have length 1, run:
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
      "!" = "`{name}` must have length {l}.",
      "x" = "It has length {length(x)}."
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
      "!" = "`{name}` must have length {l} unless it's `NULL`.",
      "x" = "It has length {length(x)}."
    ))
  }
}



#' Check type
#'
#' Much the same as `check_length()`, but for object types rather than lengths.
#' An object `x` must have one of the types in `t`, or else there will be an
#' informative error.
#'
#' @param x Vector.
#' @param t Numeric. Type that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_type <- function(x, t) {
  if (!any(typeof(x) == t)) {
    msg_name <- deparse(substitute(x))
    if (length(t) == 1L) {
      msg_object <- "be of type"
    } else {
      msg_object <- "be one of these types:"
    }
    cli::cli_abort(c(
      "!" = "`{msg_name}` must {msg_object} {t}.",
      "x" = "It is {an_a_type(x)}."
    ))
  }
}



#' Check class
#'
#' Much the same as `check_length()` or `check_type()`, but for classes. An
#' object `x` must have one of the types in `t`, or else there will be an
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
      "!" = "`{msg_name}` must inherit class \"{cl}\".",
      "x" = "It doesn't."
    ))
  }
}



#' Check whether an object is a tibble
#'
#' Note: This assumes the name of `x` within the user-calles function is `data`.
#'
#' @param x A user-supplied data frame.
#'
#' @return Logical (length 1).
#'
#' @noRd
check_tibble <- function(x) {
  if (!tibble::is_tibble(x)) {
    cli::cli_abort(c(
      "!" = "`data` must be a tibble.",
      "i" = "Convert it with `tibble::as_tibble()`."
    ))
  }
}



#' Check that `rounding` values for two procedures are not mixed
#'
#' @description In `reround()` and the many functions that call it internally,
#'   valid specifications of the `rounding` argument include the following:
#'
#' - `"up_or_down"` (the default)
#' - `"up_from_or_down_from"`
#' - `"ceiling_or_floor"`
#'
#'   If `rounding` includes any of these, it must not include any other values.
#'   `check_rounding_singular()` is called within `reround()` if `rounding` has
#'   length > 1 and throws an error if any of these strings are part of it.
#'
#' @param rounding String (length > 1).
#' @param bad String (length 1). Any of `"up_or_down"` etc.
#' @param good1,good2 String (length 1). Two singlular rounding procedures that
#'   are combined in `bad`, and that can instead be specified individually;
#'   like, e.g., `rounding = c("up", "down")`.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_rounding_singular <- function(rounding, bad, good1, good2) {
  if (any(bad == rounding)) {
    cli::cli_abort(c(
      "!" = "If `rounding` has length > 1, only single rounding procedures \\
      are supported, such as \"{good1}\" and \"{good2}\".",
      "x" = "`rounding` was given as \"{bad}\" plus others.",
      "i" = "You can still concatenate multiple of them; just leave out \\
      those with \"_or_\"."
    ))
  }
}



#' Check whether a rounding threshold was specified
#'
#' @description `check_threshold_specified()` is called within curly braces
#'   inside of the switch statement in `reconstruct_rounded_numbers_scalar()` if
#'   `rounding` includes `"_from"` and therefore requires specification of a
#'   threshold.
#'
#'   It should always be followed by the respective rounding function.
#'
#' @param rounding_threshold
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_threshold_specified <- function(threshold) {
  if (threshold == 5) {
    cli::cli_abort(c(
      "You need to specify `threshold`.",
      "x" = "If `rounding` is \"up_from\", \"down_from\", or \\
      \"up_from_or_down_from\", set `threshold` to a number \\
      other than 5. The `x` argument will then be rounded up or down from \\
      that number.",
      "i" = "To round up or down from 5, just set `rounding` to \\
      \"up\", \"down\", or \"up_or_down\" instead."
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
  check_length(group_size, 1L)
  remainder <- length(x) %% group_size

  if (remainder != 0L) {
    if (!is_whole_number(group_size)) {
      cli::cli_abort(c(
        "!" = "`group_size` must be a whole number.",
        "x" = "It is `{group_size}`."
      ))
    }
    name_x <- deparse(substitute(x))
    msg_el <- if (remainder == 1L) "element" else "elements"
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
#' @param string_output Logical (or a string that says `"auto"`).
#' @param digits Numeric. Number of digits to which `out` will be padded if it's
#'   coerced to string.
#'
#' @return Numeric or string (see above).
#'
#' @noRd
manage_string_output_seq <- function(out, from, string_output, digits) {
  if (string_output == "auto") {
    if (is.character(from)) {
      return(restore_zeros(out, width = digits))
    } else {
      return(methods::as(out, typeof(from)))
    }
  } else if (!is.logical(string_output)) {
    if (is.character(string_output)) {
      string_output <- paste0("\"", string_output, "\"")
    } else {
      string_output <- paste0("`", string_output, "`")
    }
    cli::cli_abort(c(
      "!" = "`string_output` must be logical or \"auto\".",
      "x" = "It is {string_output}."
    ))
  } else if (string_output) {
    return(restore_zeros(out, width = digits))
  } else if (typeof(from) != "character") {
    return(methods::as(out, typeof(from)))
  }
  out
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
  if (length(x) == 1L) {
    return(x)
  }
  if (length(x) == 2L) {
    collapse <- " "
    and <- " and "
  } else {
    collapse <- ", "
    and <- ", and "
  }
  out <- stringr::str_flatten(x[-length(x)], collapse = collapse)
  paste0(out, and, x[length(x)])
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
  if (length(offenders) > 0L) {
    if (length(offenders) > 3L) {
      offenders <- offenders[1:3]
      msg_among_others <- ", among others"
    } else {
      msg_among_others <- ""
    }
    offenders <- paste0("`", offenders, "`")
    name <- deparse(substitute(x))
    cli::cli_abort(c(
      "!" = "`{name}` can't be negative.",
      "x" = "It contains {offenders}{msg_among_others}."
    ))
  }
}



#' Test for even parity
#'
#' `TRUE` for every even element of `x`, `FALSE` for every odd one.
#'
#' @param x Numeric.
#'
#' @return Logical vector of length `length(x)`.
#'
#' @noRd
is_even <- function(x) {
  x %% 2 == 0
}



#' Check for length-1 sample size in dispersion functions
#'
#' @description Only used within `disperse()` and `disperse_total()`. In these
#'   functions, the `n` argument must be length 1. This is in contrast to
#'   `disperse2()` where it must be length 2, so a length-2 `n` will trigger
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
#' @return No return value; might throw an error.
#'
#' @noRd
check_length_disperse_n <- function(n, msg_single) {
  if (length(n) != 1L) {
    if (length(n) == 2L) {
      msg_single <- paste(
        msg_single, "Did you mean to call `disperse2(n = c({n[1L]}, {n[2L]}))`?"
      )
    }
    cli::cli_abort(c(
      "`n` has length {length(n)}.",
      "x" = msg_single,
      "i" = "See documentation under `?disperse`."
    ))
  }
}



#' Check if a vector is numeric or coercible to numeric
#'
#' `check_type_numeric_like()` throws an informative error if `is_numeric_like()`
#'   returns `FALSE`. This means it tolerates `NA`, not just `TRUE.`
#'
#' @param x Object to be tested.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_type_numeric_like <- function(x) {
  if (isFALSE(is_numeric_like(x))) {
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
      cli::cli_abort(c(
        "!" = "`{name}` must be numeric or coercible to numeric.",
        "i" = "(This means that converting it to numeric \\
        must return {msg_values} for its {length_non_na} \\
        non-`NA` {msg_elements}.)"
      ))
    } else {
      cli::cli_abort(c(
        "!" = "`{name}` must be numeric or coercible to numeric.",
        "x" = "It is {an_a_type(x)}."
      ))
    }
  }
}



#' Interpolate the index case
#'
#' @description This function expects an `x` vector like the one described
#'   elsewhere for `index_seq()`, with the additional expectation that
#'   continuous sequences have an odd length. That is because an index case
#'   must be identified; and without a gap in the sequence, this has to be a
#'   single median value. If the index case is missing, it is reconstructed and
#'   returned.
#'
#'   If the sequence is continuous, the index case is identical to the median,
#'   so this metric is returned. All of that works independently of the step
#'   size.
#'
#' @param x Numeric (or coercible to numeric).
#' @param index_case_only Logical. If `TRUE` (the default), only the
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

  index_case <- x[index_target] + x[index_target + 1L]
  index_case <- index_case / 2
  index_case <- methods::as(index_case, typeof(x_orig))

  if (is.character(index_case)) {
    x_orig_around_target <- c(x_orig[index_target], x_orig[index_target + 1L])
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
    restore_zeros(out)
  } else {
    out
  }

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



#' Check for arguments with or via dots
#'
#' @description `check_old_args_split_by_parens()` checks a call to
#'   `split_by_parens()` or `restore_zeros_df()` for certain kinds of errors
#'   that used to be part of the design of these functions, but no longer are:
#'
#'   1. Column names are selected via the dots, `...`.
#'   2. Argument names are prefixed with a dot, like `.transform` or
#'   `.check_decimals`.
#'   3. `col1` or `col2` are specified. (After losing their prefix dots, these
#'   arguments of `split_by_parens()` were renamed to `end1` and `end2`.)
#'
#'   If any of these cases, a precisely informative error is thrown. There is
#'   also a more generic error if any other argument is passed through the dots,
#'   `...`. This used to be checked within `split_by_parens()` and
#'   `restore_zeros_df()` themselves.

#' @param data Input data frame of the main function itself.
#' @param dots Captures in the main function with `rlang::enquos(...)`.
#' @param old_args String vector with the old, dot-prefixed arguments.
#' @param name_fn String. Name of the main function.
#'
#' @details Error 2 also points the user to the shift from `col*` to `end*` if
#'   `.col1` or `.col2` were specified, much like error 3 does.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_new_args_without_dots <- function(data, dots, old_args, name_fn) {

  if (length(dots) == 0L) {
    return(invisible(NULL))
  }

  dots_names <- names(purrr::map(dots, rlang::as_label))

  # Error 1: Column names are selected via the dots, `...`.
  offenders1 <- dots_names[dots_names %in% colnames(data)]
  if (length(offenders1) > 0L) {
    if (length(offenders1) == 1L) {
      msg_cols <- glue::glue("{offenders1}")
    } else {
      msg_cols <- stringr::str_flatten(as.character(offenders1), ", ")
      msg_cols <- paste0("c(", msg_cols, ")")
    }
    cli::cli_abort(c(
      "!" = "`{name_fn}()` no longer uses the dots, `...`, \\
      for column selection.",
      "i" = "Use the `cols` argument instead, like `cols = {msg_cols}`.",
      "*" = "Apologies for the inconvenience."
    ))
  }

  arg_names <- names(rlang::caller_call())

  # Error 2: Argument names are prefixed with a dot, like `.transform`.
  offenders2 <- arg_names[arg_names %in% old_args]
  if (length(offenders2) > 0L) {
    if (length(offenders2) == 1L) {
      msg_was_were <- "was"
      msg_dot_dots <- "a dot"
    } else {
      msg_was_were <- "were"
      msg_dot_dots <- "dots"
    }
    msg_new_args <- stringr::str_remove(offenders2, ".")

    if (name_fn == "split_by_parens" &&
        any(c("col1", "col2") %in% msg_new_args)) {
      msg_new_args[msg_new_args == "col1"] <- "end1"
      msg_new_args[msg_new_args == "col2"] <- "end2"
      msg_switch_end <- " Note the shift from `col*` to `end*`."
    } else {
      msg_switch_end <- ""
    }
    msg_new_args <- wrap_in_backticks(msg_new_args)
    offenders2 <- wrap_in_backticks(offenders2)
    cli::cli_abort(c(
      "!" = "In `{name_fn}()`, {offenders2} {msg_was_were} \\
      renamed to {msg_new_args} (without {msg_dot_dots}).{msg_switch_end}",
      "*" = "Apologies for the inconvenience."
    ))
  }

  if (name_fn == "split_by_parens") {
    # Error 3: `col1` or `col2` are specified (only in `split_by_parens()`).
    offenders3 <- arg_names[arg_names %in% c("col1", "col2")]
    if (length(offenders3) > 0L) {
      if (length(offenders3) == 1L) {
        msg_no_args <- "is not an argument"
        msg_dot_dots <- "with a dot"
      } else {
        msg_no_args <- "are not arguments"
        msg_dot_dots <- "with dots"
      }
      msg_offenders_old <- paste0(".", offenders3)
      msg_offenders_old <- wrap_in_backticks(msg_offenders_old)
      msg_new_args <- stringr::str_replace(offenders3, "col", "end")
      msg_new_args <- wrap_in_backticks(msg_new_args)
      offenders3 <- wrap_in_backticks(offenders3)
      cli::cli_abort(c(
        "!" = "{offenders3} {msg_no_args} of `{name_fn}()`.",
        "i" = "You're right not to use {msg_offenders_old} anymore \\
        ({msg_dot_dots}), but also note that it says {msg_new_args} now.",
        "*" = "Apologies for the inconvenience."
      ))
    }
  }

  # Finally, check that no other arguments are passed through the dots:
  rlang::check_dots_empty(env = rlang::caller_env(n = 1L))
}



#' Transformation helper for `split_by_parens()`
#'
#' @description Only called within `split_by_parens()`, and only if the latter
#'   function's `transform` argument is set to `TRUE`.
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
#' - Two columns named after the values of `split_by_parens()`'s `end1` and
#'   `end2` arguments. Default are `"x"` and `"sd"`.
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
    dplyr::mutate(key = seq_len(nrow(cols_1)))

  cols_2 <- data %>%
    dplyr::select(contains(uscore_end2)) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = ".origin_2",
      values_to = end2
    )

  cols_2 <- cols_2 %>%
    dplyr::mutate(key = seq_len(nrow(cols_2)))

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
#' `vignette("consistency-tests-in-depth")`; i.e., those columns that factored
#' into a consistency test applied by a mapper function like `grim_map()`.
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
  index_last_key_col <- match(before, colnames(data)) - 1L
  data[1L:index_last_key_col]
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
  x_cl[stringr::str_detect(x_cl, "^scr_rounding_")]
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
  stringr::str_remove(out, "^scr_rounding_")
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
#' @return Logical (length 1).
#'
#' @details Since `near()` is vectorized and `identical()` is not, their results
#'   are not on par with each other, so `near()` must be wrapped in `all()`,
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



#' Get name of function being called
#'
#' Returns the name of the function within which `name_caller_call()` is called
#' (by default of `n`).
#'
#' @param n The number of callers to go back. See `?rlang::caller_call()`.
#' @param wrap Logical. If `TRUE` (the default), the output is wrapped into
#'   backticks and appended with `()`.
#'
#' @return String (length 1).
#'
#' @noRd
name_caller_call <- function(n = 1L, wrap = TRUE) {
  name <- rlang::caller_call(n = n)
  name <- name[[1L]]
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
#' `utils::packageVersion("ggplot2") >= "3.4"` is `TRUE`.
#'
#' - `check_ggplot2_linewidth()` checks whether the default for the
#' not-yet-implemented `linewidth` aesthetic was changed by the user. Call it if
#' the `utils::packageVersion()` comparison above returns `FALSE`.
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



#' Remove the integer part, keeping the decimal part
#'
#' `trunc_reverse()` reduces a number to its decimal portion. It is the opposite
#' of `trunc()`: Whereas `trunc(3.45)` returns `3,` `trunc_reverse(3.45)`
#' returns `0.45`.
#'
#' This is used in some unit tests.
#'
#' @param x Decimal number.
#'
#' @return Decimal part of `x`.
#'
#' @noRd
trunc_reverse <- function(x) {
  x - trunc(x)
}



#' Conventional summary statistics for `audit()` methods
#'
#' @description `audit_summary_stats()` takes a tidyselect spec and uses it to
#'   compute statistics like mean, SD, and median by column.
#'
#'   This is used in many `audit()` methods, such as those following up on
#'   `duplicate_*()` functions, as well as on `audit_seq()` and
#'   `audit_total_n()`. (The latter two have their own `audit()` methods to
#'   summarize their results even further.)
#'
#' @param data Data frame.
#' @param selection Tidyselect specification to select the columns from `data`
#'   to operate on. It is spliced into `dplyr::across()`.
#' @param total Logical. Should there be a `.total` row that summarizes across
#'   all values in `data`, regardless of their original columns? If `TRUE`,
#'   `.total` will be the last row of the output tibble. Default is `FALSE`.
#'
#' @return Tibble with summary statistics.
#'
#' @noRd
audit_summary_stats <- function(data, selection, total = FALSE) {

  selection <- rlang::enexprs(selection)

  if (total && any(".total" == colnames(data))) {
    cli::cli_abort(c(
      "`.total` can't be a column name.",
      "!" = "Please rename the `.total` column, then try again.",
      "i" = "You could use `dplyr::rename()` for this."
    ))
  }

  # The dots are merely pro forma; their purpose is to swallow up the `na.rm =
  # TRUE` specification in a for loop below.
  na_count <- function(x, ...) {
    length(x[is.na(x)])
  }

  fun_names <- c(  "mean",      "sd",      "median", "min", "max", "na_count")
  funs      <- list(mean, stats::sd, stats::median,   min,   max,   na_count)

  out <- tibble::tibble()

  # Applying each summarizing function individually, compute the output tibble
  # row by row:
  for (i in seq_along(funs)) {
    temp <- dplyr::summarise(data, dplyr::across(
      .cols = c(!!!selection),
      .fns  = function(x) funs[[i]](x, na.rm = TRUE)
    ))
    out <- dplyr::bind_rows(out, temp)
  }

  if (total) {
    total_summary <- vector("list", length(funs))
    values_all <- data %>%
      dplyr::select(c(!!!selection)) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::pull("value")
    for (i in seq_along(funs)) {
      total_summary[[i]] <- funs[[i]](values_all, na.rm = TRUE)
    }
    total_summary <- c(".total", total_summary)
    names(total_summary) <- c("term", fun_names)
  } else {
    total_summary <- NULL
  }

  out %>%
    t() %>%
    tibble::as_tibble(.name_repair = function(x) fun_names) %>%
    dplyr::mutate("term" = names(out), .before = 1L) %>%
    dplyr::bind_rows(total_summary) %>%
    dplyr::mutate(na_rate = na_count / nrow(data), .after = "na_rate")
}



#' List of minimal-distance functions for `audit_seq()`
#'
#' @description The functions collected in `list_min_distance_functions` are
#'   mapped in one particular place within `audit_seq()` and shouldn't really be
#'   used elsewhere.
#'
#'   Instead of being individually defined as named functions or being used as
#'   anonymous functions directly inside of `audit_seq()`, they are stored in a
#'   list for greater efficiency -- in terms of both speed and memory.
#'
#'   The `x` parameter in all three functions is an integer vector measuring the
#'   number of dispersion steps between inconsistent reported values and their
#'   consistent neighbors. The notion of "steps" is the same as in, e.g.,
#'   `grim_map_seq()`.
#'
#' @return List of three functions.
#'
#' @noRd
list_min_distance_functions <- list(
  # Absolute distance:
  function(x) {
    vapply(
      x, function(x) {
        if (any(!is.numeric(x))) {
          return(NA_real_)
        }
        min(abs(x), na.rm = TRUE)
      },
      numeric(1L), USE.NAMES = FALSE
    )
  },
  # Positive distance:
  function(x) {
    vapply(
      x, function(x) {
        if (any(!is.numeric(x))) {
          return(NA_real_)
        }
        min(x[x > 0L], na.rm = TRUE)
      },
      numeric(1L), USE.NAMES = FALSE
    )
  },
  # Negative distance:
  function(x) {
    vapply(
      x, function(x) {
        if (any(!is.numeric(x))) {
          return(NA_real_)
        }
        max(x[x < 0L], na.rm = TRUE)
      },
      numeric(1L), USE.NAMES = FALSE
    )
  }
)



#' Check for linearly increasing dispersion in sequence mapper output
#'
#' @description This throws an error if a data frame returned by a sequence
#'   mapper (i.e., a function such as `grim_map_seq()`) was computed with the
#'   `dispersion` argument of that sequence mapper specified as anything other
#'   than a linearly increasing sequence.
#'
#'   For example, the default `1:5` is linearly increasing, but `5:1` and `c(3,
#'   7, 2)` are not.
#'
#' @param data Data frame returned by a function made by `function_map_seq()`.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_dispersion_linear <- function(data) {
  if (inherits(data, "scr_map_seq_disp_nonlinear")) {
    name_mapper <- class(data)[grepl("_map_seq$", class(data))]
    name_mapper <- name_mapper[name_mapper != "scr_map_seq"]
    name_mapper <- sub("scr_*", "", name_mapper)
    cli::cli_abort(c(
      "Invalid for data with this dispersion.",
      "!" = "`audit_seq()` is only applicable if `dispersion` \\
      in `{name_mapper}()` is a linearly increasing sequence.",
      "i" = "This limitation may be removed in a future version of scrutiny."
    ))
  }
}



#' Generate code to process the `"consistency"` column
#'
#' @description Call `write_code_col_key_result()` within a function factory
#'   such as `function_map()`. It returns an expression to be unquoted at the
#'   end of the factory-made function's body using ``!!!`()`.
#'
#'   This will insert code into the body that may process the `"consistency"`
#'   column of the output data frame, `out`, in one or both of these two ways:
#'
#'   - If the `name_key_result` argument is not the default `"consistency"`, the
#'   column will be renamed accordingly. This makes sense when applying tests
#'   that are not consistency tests. (As of now, these other procedures also
#'   need to return logical values.)
#'   - If the column is still a list, it is transformed into a logical vector
#'   using `unlist()`.
#'
#' @param name_key_result String (length 1). The `.name_key_result` argument of
#'   the function factory, passed to the present function.
#'
#' @return Expression.
#'
#' @noRd
write_code_col_key_result <- function(name_key_result = "consistency") {
  # Enable renaming the `"consistency"` column for binary procedures that are
  # not consistency tests:
  code_rename <- if (name_key_result == "consistency") {
    NULL
  } else {
    rlang::expr({
      out <- dplyr::rename(out, `!!`(name_key_result) := consistency)
    })
  }
  # Generate code to process the (possibly renamed) key result column:
  rlang::expr({
    `!!!`(code_rename)
    if (!is.list(out$`!!`(name_key_result))) {
      return(out)
    }
    `$<-`(
      out, `!!`(name_key_result),
      unlist(out$`!!`(name_key_result), use.names = FALSE)
    )
  })
}

