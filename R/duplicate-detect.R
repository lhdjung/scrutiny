#' New duplicate analysis functions
#'
#' @description `function_duplicate_cols()` is an internal (non-exported)
#'   function factory. It creates new functions that take a data frame and affix
#'   one new column to the right of each existing column.
#'
#'   These functions were made by `function_duplicate_cols()`:
#'   - [`duplicate_tally()`]
#'   - [`duplicate_detect()`]
#'
#' @param code_new_cols Expression which the factory-made function will evaluate
#'   at runtime. It computes the vector that will be split into the new columns
#'   -- those that contain the test results. Therefore, the vector must have the
#'   same length as the `x` object on which it operates, and which contains all
#'   values from the factory-made function's first argument.
#' @param default_end String. In the factory-made function, this will be the
#'   default for the `colname_end` argument, i.e., the ending of each of the
#'   newly created columns.
#' @param name_class String. Name of the class which the factory-made function
#'   will add to the tibble that it returns.
#'
#' @include utils.R
#'
#' @return Function such as [`duplicate_tally()`] or [`duplicate_detect()`].
#'
#' @noRd

function_duplicate_cols <- function(code_new_cols, default_end, name_class) {
  code_new_cols <- rlang::enexpr(code_new_cols)

  # --- Start of the manufactured function ---

  rlang::new_function(
    args = rlang::pairlist2(
      x = ,
      ignore = NULL,
      colname_end = default_end
    ),
    body = rlang::expr({
      # Type checking:
      if (!rlang::is_vector(x)) {
        abort_in_export(
          "`x` must be a data frame or other type of vector.",
          "x" = paste0("It is ", an_a_type(x), ".")
        )
      }

      # Convert `x` to a data frame if needed:
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

      # Save the column names before the transformations that will occur below:
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

      # Create a reference vector with all values from `x` so that they can be
      # tested against. To make all values fit together, they are coerced to
      # character strings, with numbers written out in full:
      x <- x |>
        tidyr::pivot_longer(
          cols = everything(),
          values_transform = value_to_string,
          cols_vary = "slowest"
        ) |>
        dplyr::pull(.data$value)

      new_cols <- `!!`(code_new_cols)

      # With missing values from the input, it's not known whether they have
      # duplicates or not. `NA` should also be substituted if the user chose to
      # ignore that value:
      if (is.null(ignore)) {
        new_cols[is.na(x)] <- NA
      } else {
        new_cols[is.na(x) | x %in% value_to_string(ignore)] <- NA
      }

      # Gather both vectors in a tibble, so that each test value is joined by a
      # Logical value indicating whether it has any duplicates in the rest of
      # the vector (i.e., in the flattened original data frame). Split the
      # two-column tibble and rearrange it into one of the same shape as the
      # original data frame, but with every test value accompanied by its
      # corresponding logical value to the right, as above. Also, add the
      # "scrutiny_dup_detect" class added, which is recognized by the `audit()`
      # generic:
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
        add_class(`!!`(name_class))
    })
  )

  # --- End of the manufactured function ---
}


# duplicate_detect() ------------------------------------------------------

#' Detect duplicate values
#'
#' @description `r lifecycle::badge('deprecated')`
#'
#'   `duplicate_detect()` is deprecated in favor of [`duplicate_tally()`] and
#'   [`duplicate_count()`], which are more informative. It will be removed in
#'   a later version.
#'
#'   For every value in a vector or data frame, it tests whether at least one
#'   other value is identical, and reports the result next to the value.
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
#' @param ignore Optionally, a vector of values not to check. Their result is
#'   `NA`.
#' @param colname_end String. Suffix of the result columns. Default is
#'   `"dup"`.
#'
#' @return A tibble with the columns of `x`, each followed by its result column.
#'   It has the class `scrutiny_dup_detect`, which is recognized by [`audit()`].
#'
#' @section Summaries with [`audit()`]: `audit()` on `duplicate_detect()`
#'   output returns a tibble with these columns:
#'   - `term`: the columns of `x`.
#'   - `dup_count`: the number of values of that column with a duplicate
#'   anywhere in `x`.
#'   - `total_count`: the number of values checked, i.e., neither `NA` nor
#'   ignored.
#'   - `dup_rate`: `dup_count / total_count`.
#'
#'   The last row, `.total`, sums the counts across columns and divides them.
#'
#' @seealso
#'  - [`duplicate_tally()`] for counts instead of a yes/no result.
#'  - [`duplicate_count()`] for a frequency table.
#'  - [`duplicate_count_colpair()`] for duplicates between pairs of columns.
#'  - [`janitor::get_dupes()`] for duplicate rows.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' duplicate_detect(x = pigs4)
#'
#' duplicate_detect(x = pigs4$snout)
#'
#' pigs4 |>
#'   duplicate_detect() |>
#'   audit()
#'
#' pigs4 |>
#'   duplicate_detect(ignore = c(8.13, 7.57))

duplicate_detect <- function(x, ignore = NULL, colname_end = "dup") {
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "duplicate_detect()",
    with = "duplicate_tally()"
  )
  duplicate_detect_impl(x, ignore = ignore, colname_end = colname_end)
}

duplicate_detect_impl <- function_duplicate_cols(
  # Create a logical vector pointing out duplicates within the vector of all
  # input values, both from the start forward and from the end backward:
  code_new_cols = duplicated(x) | duplicated(x, fromLast = TRUE),
  default_end = "dup",
  name_class = "scrutiny_dup_detect"
)


# duplicate_tally() -------------------------------------------------------

#' Count duplicates at each observation
#'
#' @description For every value in a vector or data frame, `duplicate_tally()`
#'   counts how often it occurs in the whole input, and reports the count next
#'   to the value.
#'
#'   Call [`audit()`] on the output for summary statistics.
#'
#' @param colname_end String. Suffix of the tally columns. Default is `"n"`.
#'
#' @inheritParams duplicate_detect
#' @inherit duplicate_detect details
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

duplicate_tally <- function_duplicate_cols(
  # For each input value, count how many instances of that value exist. The
  # comparison used to be `x == x[i]`, which is `NA` against a missing value,
  # and indexing by `NA` returns an element -- so every count was off by the
  # number of missing values in `x`. `match()` pairs each value with the first
  # of its kind and never a value with `NA`, and it is linear rather than
  # quadratic in `length(x)`:
  code_new_cols = {
    first_of_kind <- match(x, x)
    tabulate(first_of_kind, nbins = length(x))[first_of_kind]
  },
  default_end = "n",
  name_class = "scrutiny_dup_tally"
)
