#' Count duplicate values
#'
#' @description `duplicate_count()` returns a frequency table of the values in
#'   a vector or data frame. For a data frame, it pools the values of all
#'   columns and records which columns each value appears in.
#'
#'   This is a blunt tool for initial checks. If many values have few characters
#'   each, duplicates occur by chance and mean little.
#'
#'   Call [`audit()`] on the output for summary statistics.
#'
#' @param x Vector or data frame.
#' @param ignore Optionally, a vector of values not to count.
#' @param locations_type String. `"character"` (the default) pastes the column
#'   names in `locations` into one string; `"list"` keeps them as a vector per
#'   value, which is better for programming.
#'
#' @return A tibble with these columns, or only the first two if `x` is not
#'   named:
#'
#' - `value`: the distinct values of `x`.
#' - `frequency`: how often each occurs, in descending order.
#' - `locations`: the names of the columns of `x` in which `value` appears.
#' - `locations_n`: the number of such columns.
#'
#' The tibble has the class `scrutiny_dup_count`, which is recognized by
#'   [`audit()`].
#'
#' @section Summaries with [`audit()`]: `audit()` on `duplicate_count()`
#'   output returns summary statistics for `frequency` and `locations_n` (or
#'   `frequency` alone if `x` is not named).
#'
#' @seealso
#' - [`duplicate_count_colpair()`] for duplicates between pairs of columns.
#' - [`duplicate_tally()`] for counts next to each value.
#' - [`janitor::get_dupes()`] for duplicate rows.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' iris |>
#'   duplicate_count()
#'
#' iris |>
#'   duplicate_count() |>
#'   audit()
#'
#' # Ignore some values:
#' iris |>
#'   duplicate_count(ignore = c("setosa", "versicolor", "virginica"))

duplicate_count <- function(
  x,
  ignore = NULL,
  locations_type = c("character", "list")
) {
  locations_type <- rlang::arg_match(locations_type)

  check_duplicate_input(x)
  x_was_named <- rlang::is_named(x)

  # Long format: one row per value and the name of its location. The names of
  # a named atomic vector are its locations; `tibble::as_tibble()` would put all
  # its values into one column named `value` instead:
  if (x_was_named && is.atomic(x)) {
    names_orig <- unique(names(x))
    x <- tibble::tibble(
      name = as.factor(names(x)),
      value = value_to_factor(unname(x))
    )
  } else {
    if (!x_was_named || !is.data.frame(x)) {
      x <- tibble::as_tibble(
        x,
        .name_repair = if (x_was_named) {
          function(x) x
        } else {
          function(x) paste0("col", seq_along(x))
        }
      )
    } else if (!tibble::is_tibble(x)) {
      x <- tibble::as_tibble(x)
    }

    names_orig <- colnames(x)

    # Groups are meaningless for a pooled frequency table, and `across()` would
    # skip a grouping column, which then could not be pivoted with the factors:
    x <- x |>
      dplyr::ungroup() |>
      dplyr::mutate(dplyr::across(everything(), value_to_factor)) |>
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "name",
        values_to = "value"
      ) |>
      dplyr::mutate("name" = as.factor(.data$name))
  }

  if (is.null(ignore)) {
    x <- x |>
      dplyr::filter(!is.na(.data$value))
  } else {
    x <- x |>
      dplyr::filter(
        !is.na(.data$value) & !.data$value %in% value_to_string(ignore)
      )
  }

  # `tibble::as_tibble()` can't convert a table with no values, so the columns
  # are built by hand. `table()` counts every level of a factor, including
  # those of the values filtered out above, so only values that occur are kept:
  counts <- table(x$value)
  out <- tibble::tibble(
    value = as.character(names(counts)),
    frequency = as.integer(counts)
  ) |>
    dplyr::filter(.data$frequency > 0L) |>
    dplyr::arrange(dplyr::desc(.data$frequency)) |>
    add_class("scrutiny_dup_count")

  # Unnamed input has no locations:
  if (!x_was_named) {
    return(out)
  }

  # Location names in their order in `x`. Split once, rather than scanning `x`
  # once per value, which took quadratic time:
  names_by_value <- split(as.character(x$name), as.character(x$value))
  locations <- lapply(
    names_by_value[match(out$value, names(names_by_value))],
    function(names) {
      names <- unique(names)
      names[order(match(names, names_orig))]
    }
  )
  names(locations) <- NULL
  locations_n <- lengths(locations)

  if (locations_type == "character") {
    locations <- vapply(
      locations,
      function(x) paste(x, collapse = ", "),
      character(1L),
      USE.NAMES = FALSE
    )
  }

  out |>
    dplyr::mutate(locations, locations_n)
}


# Like `as.factor()`, with the levels in the same order -- numbers by size, so
# that values with the same frequency keep their order -- but labeled by
# `value_to_string()`, so that `100000` is `"100000"` rather than `"1e+05"`:
value_to_factor <- function(x) {
  levels <- x |> unique() |> sort() |> value_to_string() |> unique()
  factor(value_to_string(x), levels = levels)
}


# `x` must be a vector, and the elements of a list are columns. A list or
# data frame with no columns, or with a `NULL` one, would fail in tibble or
# tidyr with an error about their own arguments. `tibble::as_tibble()` recycles
# an element of length 1 to the length of the others, which would count its
# value several times, so the columns must have the same length, too:
check_duplicate_input <- function(x) {
  if (!rlang::is_vector(x)) {
    abort_in_export(
      "`x` must be a data frame or other type of vector.",
      "x" = "It is {.obj_type_friendly {x}}."
    )
  }
  # Raw bytes have no order, and `sort()` fails on them:
  is_raw <- if (is.list(x)) {
    vapply(x, is.raw, logical(1L), USE.NAMES = FALSE)
  } else {
    is.raw(x)
  }
  if (any(is_raw)) {
    abort_in_export(
      "`x` must not contain raw bytes.",
      "i" = "Convert them with `as.integer()` or `as.character()` first."
    )
  }
  if (!is.list(x)) {
    return(invisible(NULL))
  }
  if (length(x) == 0L) {
    abort_in_export(
      "`x` must have at least one column.",
      "x" = "It has none."
    )
  }
  index_null <- which(vapply(x, is.null, logical(1L), USE.NAMES = FALSE))
  if (length(index_null) > 0L) {
    abort_in_export(
      "The elements of `x` can't be `NULL`.",
      "x" = "{cli::qty(length(index_null))}Element{?s} {as.character(index_null)} \\
      {?is/are}."
    )
  }
  check_atomic_cols(x, "x")
  check_unique_names(x, "x")
  if (is.data.frame(x)) {
    return(invisible(NULL))
  }
  lengths_x <- lengths(x)
  if (length(unique(lengths_x)) > 1L) {
    abort_in_export(
      "The elements of `x` must have the same length.",
      "x" = "Their lengths are {lengths_x}.",
      "i" = "A list is read as columns, one per element."
    )
  }
}


# Values are compared as strings, which a list-column has no single one of, so
# `1:3` would become `"1:3"`, nor a matrix column, which has one per column.
# Shared with `duplicate_count_colpair()`:
check_atomic_cols <- function(x, name) {
  is_vector <- function(col) is.atomic(col) && is.null(dim(col))
  index_bad <- which(!vapply(x, is_vector, logical(1L), USE.NAMES = FALSE))
  if (length(index_bad) > 0L) {
    abort_in_export(
      "The columns of `{name}` must be atomic vectors.",
      "x" = "{cli::qty(length(index_bad))}Column{?s} {as.character(index_bad)} \\
      {?is not one/are not}.",
      "i" = "Lists and matrices can't be compared value by value."
    )
  }
}


# Columns are reported by name, and tibble would reject a repeated one with an
# error of its own. Unnamed columns are named by position later. Shared with
# `duplicate_count_colpair()`:
check_unique_names <- function(x, name) {
  names_x <- names(x)
  repeated <- unique(names_x[duplicated(names_x) & names_x != ""])
  if (length(repeated) > 0L) {
    abort_in_export(
      "The columns of `{name}` must have unique names.",
      "x" = "{cli::qty(length(repeated))}{wrap_in_backticks(repeated)} \\
      occur{?s/} more than once."
    )
  }
}
