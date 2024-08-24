
#' Count duplicate values
#'
#' @description `duplicate_count()` returns a frequency table. When searching a
#'   data frame, it includes values from all columns for each frequency count.
#'
#'   This function is a blunt tool designed for initial data checking. It is not
#'   too informative if many values have few characters each.
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @param x Vector or data frame.
#' @param ignore Optionally, a vector of values that should not be counted.
#' @param locations_type String. One of `"character"` or `"list"`. With
#'   `"list"`, each `locations` value is a vector of column names, which is
#'   better for further programming. By default (`"character"`), the column
#'   names are pasted into a string, which is more readable.

#' @return If `x` is a data frame or another named vector, a tibble with four
#'   columns. If `x` isn't named, only the first two columns appear:
#'
#' - `value`: All the values from `x`.
#' - `frequency`: Absolute frequency of each value in `x`, in descending order.
#' - `locations`: Names of all columns from `x` in which `value` appears.
#' - `locations_n`: Number of columns named in `locations`.
#'
#' The tibble has the `scr_dup_count` class, which is recognized by the
#' [`audit()`] generic.

#' @section Summaries with [`audit()`]: There is an S3 method for the
#'   [`audit()`] generic, so you can call [`audit()`] following
#'   `duplicate_count()`. It returns a tibble with summary statistics for the
#'   two numeric columns, `frequency` and `locations_n` (or, if `x` isn't named,
#'   only for `frequency`).
#'
#' @seealso
#' - [`duplicate_count_colpair()`] to check each combination of columns for
#' duplicates.
#' - [`duplicate_tally()`] to show instances of a value next to each instance.
#' - [`janitor::get_dupes()`] to search for duplicate rows.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # Count duplicate values...
#' iris %>%
#'   duplicate_count()
#'
#' # ...and compute summaries:
#' iris %>%
#'   duplicate_count() %>%
#'   audit()
#'
#' # Any values can be ignored:
#' iris %>%
#'   duplicate_count(ignore = c("setosa", "versicolor", "virginica"))


duplicate_count <- function(x, ignore = NULL,
                            locations_type = c("character", "list")) {

  locations_type <- rlang::arg_match(locations_type)

  # Convert `x` to a data frame if needed (`x_was_named` will also be checked
  # further below):
  x_was_named <- rlang::is_named(x)
  if (!x_was_named || !is.data.frame(x)) {
    x <- tibble::as_tibble(
      x, .name_repair = if (x_was_named) {
        function(x) x
      } else {
        function(x) paste0("col", seq_along(x))
      }
    )
  } else if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }

  names_orig <- colnames(x)

  x <- x %>%
    dplyr::mutate(dplyr::across(everything(), as.factor)) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "name",
      values_to = "value"
    ) %>%
    dplyr::mutate("name" = as.factor(.data$name))

  if (is.null(ignore)) {
    x <- dplyr::filter(x, !is.na(.data$value))
  } else {
    x <- dplyr::filter(x, !is.na(.data$value) & !.data$value %in% ignore)
  }

  out <- x$value %>%
    table() %>%
    tibble::as_tibble(.name_repair = function(x) c("value", "frequency")) %>%
    dplyr::filter(!.data$value %in% ignore) %>%
    dplyr::arrange(dplyr::desc(.data$frequency)) %>%
    add_class("scr_dup_count")

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

  # The user may specify `locations_type` to remain a list:
  if (locations_type == "list") {
    return(dplyr::mutate(out, locations, locations_n))
  }

  # By default (`locations_type == "character"`), collapse each list element --
  # i.e., each vector of location names -- into a string:
  dplyr::mutate(
    out,
    locations = vapply(
      locations, function(x) paste(x, collapse = ", "),
      character(1L), USE.NAMES = FALSE
    ),
    locations_n
  )
}
