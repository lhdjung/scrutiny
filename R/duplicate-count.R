
#' Count duplicate values
#'
#' @description `duplicate_count()` returns a frequency table. When searching a
#'   data frame, it includes values from all columns for each frequency count.
#'
#'   This function is a blunt tool designed for initial data checking. It is not
#'   too informative if many values have few characters each.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @param x Vector or data frame.
#' @param ignore Optionally, a vector of values that should not be counted.
#' @param locations_type String. One of `"character"` or `"list"`. With
#'   `"list"`, each `locations` value is a vector of column names, which is
#'   better for further programming. By default (`"character"`), the column
#'   names are pasted into a string, which is more readable.
#' @param numeric_only [[Deprecated]] No longer used: All values are coerced to
#'   character.

#' @return If `x` is a data frame or another named vector, a tibble with four
#'   columns. If `x` isn't named, only the first two columns appear:
#'
#' - `value`: All the values from `x`.
#' - `count`: The frequency of each value in `x`, in descending order.
#' - `locations`: The names of all columns from `x` in which `value` appears.
#' - `locations_n`: The number of columns named in `locations`.

#' The tibble has the `scr_dup_count` class, which is recognized by the
#' `audit()` generic.

#' @details Don't use `numeric_only`. It no longer has any effect and will be
#'   removed in the future. The only reason for this argument was the risk of
#'   errors introduced by coercing values to numeric. This is no longer an issue
#'   because all values are now coerced to character, which is more appropriate
#'   for checking reported statistics.

#' @section Summaries with `audit()`: There is an S3 method for the `audit()`
#'   generic, so you can call `audit()` following `duplicate_count()`. It
#'   returns a tibble with summary statistics for the two numeric columns,
#'   `count` and `locations_n` (or only for `count`; see above).
#'
#' @seealso
#' - `duplicate_count_colpair()` to check each combination of columns for
#' duplicates.
#' - `duplicate_tally()` to show instances of a value next to each instance.
#' - `janitor::get_dupes()` to search for duplicate rows.
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
                            locations_type = c("character", "list"),
                            numeric_only = deprecated()) {

  locations_type <- rlang::arg_match(locations_type, c("character", "list"))

  if (lifecycle::is_present(numeric_only)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "duplicate_count(numeric_only)",
      details = "It no longer has any effect because all input \
      values are now coerced to character strings."
    )
  }

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
    tibble::as_tibble(.name_repair = function(x) c("value", "count")) %>%
    dplyr::filter(!.data$value %in% ignore) %>%
    dplyr::arrange(dplyr::desc(.data$count)) %>%
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
    locs <- unique(x[x$value == out$value[i], ]$name)
    locations[i] <- list(locs[order(match(locs, names_orig))])
    locations_n[i] <- length(locations[[i]])
  }
  rm(x)

  # By default, collapse each vector of location names into a string:
  if (locations_type == "character") {
    locations <- vapply(
      locations, function(x) paste(x, collapse = ", "), character(1L)
    )
  }

  dplyr::mutate(out, locations, locations_n)
}
