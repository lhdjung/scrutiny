
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
      x =, ignore = NULL, colname_end = default_end
    ),
    body = rlang::expr({

      # Type checking:
      if (!rlang::is_vector(x)) {
        cli::cli_abort(c(
          "`x` must be a data frame or other type of vector.",
          "x" = paste0("It is ", an_a_type(x), ".")
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
      } else if (!tibble::is_tibble(x)) {
        x <- tibble::as_tibble(x)
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
          values_transform = as.character,
          cols_vary = "slowest"
        ) %>%
        dplyr::pull(.data$value)

      new_cols <- `!!`(code_new_cols)

      # With missing values from the input, it's not known whether they have
      # duplicates or not. `NA` should also be substituted if the user chose to
      # ignore that value:
      if (is.null(ignore)) {
        new_cols[is.na(x)] <- NA
      } else {
        new_cols[is.na(x) | x %in% ignore] <- NA
      }

      # Gather both vectors in a tibble, so that each test value is joined by a
      # Logical value indicating whether it has any duplicates in the rest of
      # the vector (i.e., in the flattened original data frame). Split the
      # two-column tibble and rearrange it into one of the same shape as the
      # original data frame, but with every test value accompanied by its
      # corresponding logical value to the right, as above. Also, add the
      # "scr_dup_detect" class added, which is recognized by the `audit()`
      # generic:
      x %>%
        tibble::tibble(new_cols) %>%
        split(ceiling(seq_along(x) / nrow_original)) %>%
        dplyr::bind_cols(.name_repair = function(x) {
          colnames_test <- paste0(colnames_original, "_", colname_end)
          as.vector(rbind(colnames_original, colnames_test))
        }) %>%
        add_class(`!!`(name_class))
    })
  )

  # --- End of the manufactured function ---

}



# duplicate_detect() ------------------------------------------------------

#' Detect duplicate values
#'
#' @description `r lifecycle::badge('superseded')`
#'
#'   `duplicate_detect()` is superseded because it's less informative than
#'   [`duplicate_tally()`] and [`duplicate_count()`]. Use these functions
#'   instead.
#'
#'   For every value in a vector or data frame, `duplicate_detect()` tests
#'   whether there is at least one identical value. Test results are presented
#'   next to every value.
#'
#'   This function is a blunt tool designed for initial data checking. Don't put
#'   too much weight on its results.
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @details This function is not very informative with many input values that
#'   only have a few characters each. Many of them may have duplicates just by
#'   chance. For example, in R's built-in `iris` data set, 99% of values have
#'   duplicates.
#'
#'   In general, the fewer values and the more characters per value, the more
#'   significant the results.
#'
#' @param x Vector or data frame.
#' @param ignore Optionally, a vector of values that should not be checked. In
#'   the test result columns, they will be marked `NA`.
#' @param colname_end String. Name ending of the logical test result columns.
#'   Default is `"dup"`.

#' @return A tibble (data frame). It has all the columns from `x`, and to each
#'   of these columns' right, the corresponding test result column.
#'
#'   The tibble has the `scr_dup_detect` class, which is recognized by the
#'   `audit()` generic.

#' @section Summaries with [`audit()`]: There is an S3 method for the
#'   [`audit()`] generic, so you can call [`audit()`] following
#'   `duplicate_detect()`. It returns a tibble with these columns ---
#'   - `term`: The original data frame's variables.
#'   - `dup_count`: Number of "duplicated" values of that `term` variable: those
#'   which have at least one duplicate anywhere in the data frame.
#'   - `total`: Number of all non-`NA` values of that `term` variable.
#'   - `dup_rate`: Rate of "duplicated" values of that `term` variable.
#'
#'   The final row, `.total`, summarizes across all other rows: It adds up the
#'   `dup_count` and `total_count` columns, and calculates the mean of the
#'   `dup_rate` column.
#'
#' @seealso
#'  - [`duplicate_tally()`] to count instances of a value instead of just
#'  stating whether it is duplicated.
#'  - [`duplicate_count()`] for a frequency table.
#'  - [`duplicate_count_colpair()`] to check each combination of columns for
#' duplicates.
#'  - [`janitor::get_dupes()`] to search for duplicate rows.
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

duplicate_detect <- function_duplicate_cols(
  # Create a logical vector pointing out duplicates within the vector of all
  # input values, both from the start forward and from the end backward:
  code_new_cols = duplicated(x) | duplicated(x, fromLast = TRUE),
  default_end = "dup",
  name_class = "scr_dup_detect"
)



# duplicate_tally() -------------------------------------------------------

#' Count duplicates at each observation
#'
#' @description For every value in a vector or data frame, `duplicate_tally()`
#'   counts how often it appears in total. Tallies are presented next to each
#'   value.
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @param colname_end String. Name ending of the logical test result columns.
#'   Default is `"n"`.
#'
#' @inheritParams duplicate_detect
#' @inherit duplicate_detect details
#'
#' @return A tibble (data frame). It has all the columns from `x`, and to each
#'   of these columns' right, the corresponding tally column.
#'
#'   The tibble has the `scr_dup_detect` class, which is recognized by the
#'   `audit()` generic.
#'
#' @section Summaries with [`audit()`]: There is an S3 method for the [`audit()`]
#'   generic, so you can call [`audit()`] following `duplicate_tally()`. It
#'   returns a tibble with summary statistics.
#'
#' @seealso
#'  - [`duplicate_count()`] for a frequency table.
#'  - [`duplicate_count_colpair()`] to check each combination of columns for
#' duplicates.
#'  - [`janitor::get_dupes()`] to search for duplicate rows.
#'
#' @include utils.R

#' @export
#'
#' @examples
#' # Tally duplicate values in a data frame...
#' duplicate_tally(x = pigs4)
#'
#' # ...or in a single vector:
#' duplicate_tally(x = pigs4$snout)
#'
#' # Summary statistics with `audit()`:
#' pigs4 %>%
#'   duplicate_tally() %>%
#'   audit()
#'
#' # Any values can be ignored:
#' pigs4 %>%
#'   duplicate_tally(ignore = c(8.131, 7.574))

duplicate_tally <- function_duplicate_cols(
  # For each input value, count how many other instances of that value exist:
  code_new_cols = {
    new_cols <- integer(length(x))
    for (i in seq_along(new_cols)) {
      new_cols[i] <- length(x[x == x[i]])
    }
    new_cols
  },
  default_end = "n",
  name_class = "scr_dup_tally"
)

