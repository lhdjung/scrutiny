#' Count duplicates at each observation
#'
#' @description For every value in a vector or data frame, `duplicate_tally()`
#'   counts how often it occurs in the whole input, and reports the count next
#'   to the value.
#'
#'   Call [`audit()`] on the output for summary statistics.
#'
#' @details Values with few characters are often duplicated by chance: in
#'   `iris`, 99% of values have a duplicate. Duplicates are more meaningful when
#'   there are fewer values and they have more characters.
#'
#'   All values are compared as strings, and the output returns them as such.
#'   So `1` and `"1"` are duplicates, but `2` and `"2.0"` are not. Numbers
#'   lose trailing zeros when converted: `2.0` becomes `"2"`.
#'
#' @param x Vector or data frame.
#' @param ignore Optionally, a vector of values not to check. Their tally is
#'   `NA`, as is that of missing values.
#' @param colname_end String. Suffix of the tally columns. Default is `"n"`.
#'
#' @return A tibble with the columns of `x`, each followed by its tally column.
#'   It has the class `scrutiny_dup_tally`, which is recognized by [`audit()`].
#'
#' @section Summaries with [`audit()`]: `audit()` on `duplicate_tally()` output
#'   returns summary statistics for each tally column, with a `.total` row
#'   across all of them.
#'
#' @seealso
#'  - [`duplicate_count()`] for a frequency table.
#'  - [`duplicate_count_colpair()`] for duplicates between pairs of columns.
#'  - [`janitor::get_dupes()`] for duplicate rows.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' duplicate_tally(x = pigs4)
#'
#' duplicate_tally(x = pigs4$snout)
#'
#' pigs4 |>
#'   duplicate_tally() |>
#'   audit()
#'
#' pigs4 |>
#'   duplicate_tally(ignore = c(8.13, 7.57))

duplicate_tally <- function(x, ignore = NULL, colname_end = "n") {
  if (!rlang::is_vector(x)) {
    abort_in_export(
      "`x` must be a data frame or other type of vector.",
      "x" = paste0("It is ", an_a_type(x), ".")
    )
  }

  x_was_named <- rlang::is_named(x)
  if (!x_was_named || !is.data.frame(x)) {
    x <- tibble::as_tibble(
      x,
      .name_repair = if (x_was_named) {
        function(x) x
      } else if ((is.atomic(x) && is.null(dim(x))) || length(x) == 1L) {
        function(x) "value"
      } else {
        function(x) paste0("col", seq_along(x))
      }
    )
  } else if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }

  colnames_original <- names(x)
  nrow_original <- nrow(x)

  # Each column is followed by one named after it, so a column that already
  # has such a name would appear twice:
  colnames_new <- paste0(colnames_original, "_", colname_end)
  names_taken <- intersect(colnames_new, colnames_original)
  if (length(names_taken) > 0L) {
    abort_in_export(
      "The output would have {wrap_in_backticks(names_taken)} twice.",
      "i" = "Rename {cli::qty(length(names_taken))}{?that column/those \\
      columns}, or choose another `colname_end`."
    )
  }

  # All values of `x` as strings, with numbers written out in full:
  x <- x |>
    tidyr::pivot_longer(
      cols = everything(),
      values_transform = value_to_string,
      cols_vary = "slowest"
    ) |>
    dplyr::pull(.data$value)

  # `match()` pairs each value with the first of its kind, never with `NA`, and
  # is linear rather than quadratic in `length(x)`:
  first_of_kind <- match(x, x)
  new_cols <- tabulate(first_of_kind, nbins = length(x))[first_of_kind]

  # Whether a missing value has duplicates is unknown:
  if (is.null(ignore)) {
    new_cols[is.na(x)] <- NA
  } else {
    new_cols[is.na(x) | x %in% value_to_string(ignore)] <- NA
  }

  # Each original column is followed by its test column. The flattened
  # vectors are cut by position, with a factor so that a column with no
  # rows is still a column:
  index_col <- colnames_original |>
    seq_along() |>
    rep(each = nrow_original) |>
    factor(levels = seq_along(colnames_original))
  out <- vector("list", 2L * length(colnames_original))
  out[c(TRUE, FALSE)] <- split(x, index_col)
  out[c(FALSE, TRUE)] <- split(new_cols, index_col)
  names(out) <- as.vector(rbind(colnames_original, colnames_new))
  out |>
    tibble::new_tibble(nrow = nrow_original) |>
    add_class("scrutiny_dup_tally")
}
