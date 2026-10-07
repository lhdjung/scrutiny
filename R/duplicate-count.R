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

  x_was_named <- rlang::is_named(x)

  # Bring `x` into a long format with one row per value and the name of its
  # location. The names of an atomic vector are its locations, so they go
  # straight into the `name` column. `tibble::as_tibble()` would otherwise put
  # all of its values into one column named `value`, and each of them would be
  # located there. Other inputs are converted to a data frame if needed
  # (`x_was_named` will also be checked further below), and pivoted:
  if (x_was_named && is.atomic(x)) {
    names_orig <- unique(names(x))
    x <- tibble::tibble(
      name = as.factor(names(x)),
      value = as.factor(unname(x))
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

    x <- x |>
      dplyr::mutate(dplyr::across(everything(), as.factor)) |>
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
      dplyr::filter(!is.na(.data$value) & !.data$value %in% ignore)
  }

  # `tibble::as_tibble()` can't convert a table with no values, so the columns
  # are built by hand. `table()` counts every level of a factor, including
  # unused ones and those of the values filtered out above, so only values that
  # occur are kept:
  counts <- table(x$value)
  out <- tibble::tibble(
    value = as.character(names(counts)),
    frequency = as.integer(counts)
  ) |>
    dplyr::filter(.data$frequency > 0L) |>
    dplyr::arrange(dplyr::desc(.data$frequency)) |>
    add_class("scrutiny_dup_count")

  # All code below is about the `locations` and `locations_n` columns, but they
  # are only meant for data frames and other named vectors. If the `x` input was
  # not named, there is nothing more to do:
  if (!x_was_named) {
    return(out)
  }

  # In the original `x` input data frame, count the columns in which each unique
  # value appears. Store the names of those columns, and sort them by their
  # order of appearance in the input data frame:
  x$name <- as.character(x$name)
  locations <- vector("list", nrow(out))
  locations_n <- integer(nrow(out))
  for (i in seq_along(locations)) {
    temp <- unique(x[x$value == out$value[i], ]$name)
    locations[i] <- list(temp[order(match(temp, names_orig))])
    locations_n[i] <- length(locations[[i]])
  }

  # By default (`locations_type == "character"`), collapse each list element --
  # i.e., each vector of location names -- into a string. The user may specify
  # `locations_type` to remain a list:
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
