

#' Detect duplicate values
#'
#' @description For every value in a vector or data frame, `duplicate_detect()`
#'   tests whether there is at least one identical value. Test results are
#'   presented next to every value.
#'
#'   By default, only numeric columns and string columns coercible to numeric
#'   are tested (if `x` is a data frame). Any other columns are silently
#'   dropped.
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
#' @param numeric_only Boolean. If `TRUE` (the default) and if `x` is a data
#'   frame, the function will only test numeric columns and string columns
#'   coercible to numeric. *Note*: Be careful when setting it to `FALSE`. This
#'   can lead to all kinds of coercion issues.
#' @param colname_end String. Name ending of the Boolean test result columns.
#'   Default is `"dup"`.

#' @return A tibble (data frame) —
#'   - If `x` is a vector, there are two columns: the input `value` and the
#'     Boolean `has_duplicates`.
#'   - If `x` is a data frame, the output tibble has (some of) the columns from
#'     `x`, and to each of these columns' right, the corresponding Boolean
#'     column with an index value.
#'
#' The tibble has the `scr_dup_detect` class, which is recognized by the
#' `audit()` generic.

#' @section Summaries with `audit()`: There is an S3 method for the `audit()`
#'   generic, so you can call `audit()` following `duplicate_detect()`. It
#'   returns a tibble with these columns ---
#'   - `variable`: The original data frame's variables with at least one
#'     "duplicated" value: one that has at least one duplicate anywhere else in
#'     the data frame. For a vector, `x`.
#'   - `n_duplicated`: Number of "duplicated" values of that variable: those
#'     that have at least one duplicate anywhere in the data frame.
#'   - `dup_rate`: Rate of "duplicated" values of that variable.
#'
#'   The final row, `.total`, summarizes across all other rows: It adds up the
#'   `n_duplicated` and `n_total` columns, and calculates the average of the
#'   `dup_rate` column.
#'
#' @seealso `duplicate_count()` provides a frequency table.
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
#' # If there are many values and/or few
#' # characters per value, `duplicate_detect()`
#' # can be misleading:
#' iris %>%
#'   duplicate_detect()
#'
#' iris %>%
#'   duplicate_detect() %>%
#'   audit()



duplicate_detect <- function(x, numeric_only = TRUE, colname_end = "dup") {

  # Deal with a non-data-frame vector:
  if (!is.data.frame(x)) {
    value <- tibble::as_tibble(x)
    has_duplicates <- duplicated(value) | duplicated(value, fromLast = TRUE)
    df <- tibble::tibble(value, has_duplicates) %>%
      stats::na.omit() %>%
      add_class("scr_dup_detect")
    return(df)
  }

  # (The rest is for data frames only.)

  # By default, coerce all columns to numeric with which that is possible and
  # drop the rest. In any case, lump all of the data frame's values into a
  # single vector:
  if (numeric_only) {
    x <- x %>%
      dplyr::select(where(is.numeric) | where(is.character)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
      dplyr::select(where(~ !all(is.na(.)))) %>%
      suppressWarnings()

    val <- purrr::flatten_dbl(x)
  } else {
    val <- x %>%
      purrr::flatten() %>%
      purrr::as_vector()
  }

  # Create a Boolean vector pointing out duplicates within the test vector, both
  # from the start forward and from the end backward:
  dup <- duplicated(val) | duplicated(val, fromLast = TRUE)

  # Gather both vectors in a tibble, so that each test value is joined by a
  # Boolean value indicating whether it has any duplicates in the rest of the
  # vector (i.e., in the flattened original data frame):
  df <- tibble::tibble(val, dup)

  # Prepare row index for the data frame transformation below:
  row_id <- ceiling(seq_along(val) / nrow(x))

  # Split the two-column tibble into one of the same shape as the original data
  # frame, but with every test value accompanied by its corresponding Boolean
  # value to the right, as above:
  df <- df %>%
    split(row_id) %>%
    dplyr::bind_cols(.name_repair = "minimal") %>%
    suppressMessages() %>%
    stats::na.omit()

  # Name both kinds of columns:
  colnames_dup <- paste0(names(x), "_", colname_end)   # used to have: , 1:ncol(df)
  colnames(df) <- rbind(colnames(x), colnames_dup)

  # Return the tibble, but with the "scr_dup_detect" class added, which is
  # recognized by the `audit()` generic:
  df %>%
    add_class("scr_dup_detect")
}


