
#' Detect duplicate values
#'
#' @description For every value in a vector or data frame, `duplicate_detect()`
#'   tests whether there is at least one identical value. Test results are
#'   presented next to every value.
#'
#'   This function is a blunt tool designed for initial data checking. Don't put
#'   too much weight on its results.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @details This function is not very informative with many input values that
#'   only have a few characters each. Many of them may have duplicates just by
#'   chance. For example, in R's built-in `iris` data set, 99% of values have
#'   duplicates.
#'
#'   In general, the fewer values and the more characters per value there are,
#'   the more significant `duplicate_detect()`'s results will be.
#'
#' @param x Vector or data frame.
#' @param ignore Optionally, a vector of values that should not be checked. In
#'   the test result columns, they will be marked `NA`.
#' @param colname_end String. Name ending of the Boolean test result columns.
#'   Default is `"dup"`.
#' @param numeric_only [[Deprecated]] No longer used: All values are coerced to
#'   character.

#' @return A tibble (data frame). It has all the columns from `x`, and to each
#'   of these columns' right, the corresponding test result column.
#'
#'   The tibble has the `scr_dup_detect` class, which is recognized by the
#'   `audit()` generic.

#' @section Summaries with `audit()`: There is an S3 method for the `audit()`
#'   generic, so you can call `audit()` following `duplicate_detect()`. It
#'   returns a tibble with these columns ---
#'   - `term`: The original data frame's variables with at least one
#'   "duplicated" value: one that has at least one duplicate anywhere else in
#'   the data frame. For a vector, `x`.
#'   - `n_duplicated`: Number of "duplicated" values of that variable: those
#'   that have at least one duplicate anywhere in the data frame.
#'   - `dup_rate`: Rate of "duplicated" values of that variable.
#'
#'   The final row, `.total`, summarizes across all other rows: It adds up the
#'   `n_duplicated` and `n_total` columns, and calculates the average of the
#'   `dup_rate` column.
#'
#' @seealso
#'  - `duplicate_count()` provides a frequency table.
#'  - `duplicate_count_colpair()` to check each value for duplicates.
#'  - `janitor::get_dupes()` to search for duplicate rows.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # Find duplicate values in a data frame...
#' duplicate_detect(x = pigs4)
#'
#' # ...or in a single vector:
#' duplicate_detect(x = pigs4$snout)
#'
#' # Summary statistics with `audit()`:
#' pigs4 %>%
#'   duplicate_detect() %>%
#'   audit()
#'
#' # Any values can be ignored:
#' pigs4 %>%
#'   duplicate_detect(ignore = c(8.131, 7.574))


duplicate_detect <- function(x, ignore = NULL, colname_end = "dup",
                             numeric_only = TRUE) {

  if (!missing(numeric_only)) {
    cli::cli_warn(c(
      "`numeric_only` is deprecated.",
      "!" = "It no longer has any effect because all input \
      values are now coerced to character strings."
    ))
  }

  # Convert `x` to a data frame if needed:
  x_was_named <- rlang::is_named(x)
  if (!x_was_named || !is.data.frame(x)) {
    x <- tibble::as_tibble(
      x, .name_repair = if (x_was_named) {
        function(x) x
      } else if (is.atomic(x) || length(x) == 1L) {
        function(x) "value"
      } else {
        function(x) paste0("col", seq_along(x))
      }
    )
  }

  # Save the column names before the transformations that will occur below:
  colnames_original <- names(x)
  nrow_original <- nrow(x)

  # Create a reference vector with all values from `x` so that they can be
  # tested against. To make all values fit together, they are coerced to
  # character strings:
  x <- x %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "name",
      values_to = "value",
      values_transform = as.character,
      cols_vary = "slowest"
    ) %>%
    dplyr::pull(.data$value)

  # Create a Boolean vector pointing out duplicates within the test vector, both
  # from the start forward and from the end backward:
  dup <- duplicated(x) | duplicated(x, fromLast = TRUE)

  # With missing values from the input, it's not known whether they have
  # duplicates or not. `NA` should also be substituted if the user chose to
  # ignore that value:
  if (is.null(ignore)) {
    dup[is.na(x)] <- NA
  } else {
    dup[is.na(x) | x %in% ignore] <- NA
  }

  # Gather both vectors in a tibble, so that each test value is joined by a
  # Boolean value indicating whether it has any duplicates in the rest of the
  # vector (i.e., in the flattened original data frame). Split the two-column
  # tibble and rearrange it into one of the same shape as the original data
  # frame, but with every test value accompanied by its corresponding Boolean
  # value to the right, as above. Also, add the "scr_dup_detect" class added,
  # which is recognized by the `audit()` generic:
  x %>%
    tibble::tibble(dup) %>%
    split(ceiling(seq_along(x) / nrow_original)) %>%
    dplyr::bind_cols(.name_repair = function(x) {
      colnames_dup <- paste0(colnames_original, "_", colname_end)
      as.vector(rbind(colnames_original, colnames_dup))
    }) %>%
    add_class("scr_dup_detect")
}
