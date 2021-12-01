
#' Split columns by parentheses
#'
#' @description Summary statistics are often presented like `"2.65 (0.27)"`.
#'   When working with tables from PDF, it can be tedious to separate values
#'   before and inside parentheses. `split_by_parens()` does that automatically.
#'   By default, it operates on all columns. Output can optionally be
#'   transformed into a longer format by setting `.transform` to `TRUE`.
#'
#' @param .data Data frame. Only in `split_by_parens()`.
#' @param ... Optionally, name columns from `.data`. Splitting will then be
#'   restricted to these columns. This is useful if not all values contain
#'   parentheses.
#' @param .keep Boolean. If set to `TRUE`, the original columns from `.data`
#'   also appear in the output. Default is `FALSE`.
#' @param .transform Boolean. If set to `TRUE`, the output will be transformed
#'   to be better accessible for typical follow-up tasks. Default is `FALSE`.
#' @param .sep String. What to split by. Either `"parens"`, `"brackets"`, or
#'   `"braces"`. Default is `"parens"`.
#' @param .col1,.col2 Strings. Endings of the two column names that result from
#'   splitting a column. Default is `"x"` for `.col1` and `"sd"` for `.col2`.
#'
#' @include before-inside-parens.R split-transform.R
#'
#' @return A tibble with string columns.
#'
#' @seealso `before_parens()` and `inside_parens()` take a string vector and
#'   extract values from the respective position. These two functions are mapped
#'   within `split_by_parens().`
#'
#' @export
#'
#' @examples
#' # Call `split_by_parens()` on data like these:
#' pigs <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 (0.21)",    "0.19 (0.13)",
#'   "0.19 (0.28)",    "0.53 (0.10)",
#'   "0.62 (0.16)",    "0.50 (0.11)",
#'   "0.15 (0.35)",    "0.57 (0.16)",
#' )
#'
#' pigs
#'
#' # Basic usage:
#' pigs %>%
#'   split_by_parens()
#'
#' # Name specific columns to only return those:
#' pigs %>%
#'   split_by_parens(drone)
#'
#' # Pivot the data into a longer format
#' # by setting `.transform` to `TRUE`:
#' pigs %>%
#'   split_by_parens(.transform = TRUE)
#'
#' # Choose different column names or
#' # name suffixes with `.col1` and `.col2`:
#' pigs %>%
#'   split_by_parens(.col1 = "beta", .col2 = "se")
#'
#' pigs %>%
#'   split_by_parens(.transform = TRUE,
#'   .col1 = "beta", .col2 = "se")


split_by_parens <- function(.data, ..., .keep = FALSE, .transform = FALSE,
                            .sep = "parens", .col1 = "x", .col2 = "sd") {

  # Capture the names of any columns from `.data` that might have been specified
  # by the user via tidy evaluation:
  cols <- rlang::enexprs(...)

  # In case no columns were specified as per the above, prepare and defuse a
  # call that will select all columns from `.data`:
  if (length(cols) == 0) {
    cols <- rlang::exprs(dplyr::everything())
  }

  # Apply the extractor functions `before_parens()` and `inside_parens()` to all
  # selected columns from `.data` (see above), going by `.sep`, which is
  # `"parens"` by default and will thus look for parentheses:
  out <- dplyr::mutate(.data, dplyr::across(
    .cols = c(!!!cols),
    .fns = c(before_parens, inside_parens),
    sep = .sep
  ))

  # By default (`.keep = FALSE`), the original columns are dropped:
  if (!.keep) {
    out <- dplyr::select(out, -names(.data))
  }

  # Format the column name endings:
  end1 <- paste0("_", .col1)
  end2 <- paste0("_", .col2)

  # Modify the column names with the endings prepared above:
  names(out) <- stringr::str_replace(names(out), c("_1", "_2"), c(end1, end2))

  # Write new classes to inform the helper `transform_split_parens_object()`
  # about the specified column name endings:
  class_end1 <- glue::glue("scr_end1_{.col1}")
  class_end2 <- glue::glue("scr_end2_{.col2}")

  # Add these classes and a more general one to the output tibble:
  out <- add_class(out, c("scr_split_by_parens", class_end1, class_end2))

  # Return the output tibble. If desired, pivot it into a longer format using
  # the specified internal helper function from the split-transform.R file:
  if (.transform) {
    transform_split_parens_object(out)
  } else {
    out
  }

}


