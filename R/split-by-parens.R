
#' Split columns by parentheses, brackets, braces, or similar
#'
#' @description Summary statistics are often presented like `"2.65 (0.27)"`.
#'   When working with tables from PDF, it can be tedious to separate values
#'   before and inside parentheses. `split_by_parens()` does that automatically.
#'
#'   By default, it operates on all columns. Output can optionally be
#'   transformed into a longer format by setting `.transform` to `TRUE`.
#'
#'   Choose separators other than parentheses by specifying the `.sep` argument.
#'
#' @param .data Data frame. Only in `split_by_parens()`.
#' @param ... Optionally, select columns from `.data` as in `dplyr::select()`.
#'   Splitting will then be restricted to these columns. This is useful if not
#'   all values contain parentheses.
#' @param .keep Boolean. If set to `TRUE`, the original columns from `.data`
#'   also appear in the output. Default is `FALSE`.
#' @param .transform Boolean. If set to `TRUE`, the output will be transformed
#'   to be better accessible for typical follow-up tasks. Default is `FALSE`.
#' @param .sep String. What to split by. Either `"parens"`, `"brackets"`, or
#'   `"braces"`; or a length-2 vector of custom separators (see Examples).
#'   Default is `"parens"`.
#' @param .col1,.col2 Strings. Endings of the two column names that result from
#'   splitting a column. Default is `"x"` for `.col1` and `"sd"` for `.col2`.
#'
#' @include utils.R before-inside-parens.R
#'
#' @return A tibble with string columns.

#' @seealso
#'  - `before_parens()` and `inside_parens()` take a string vector and
#'  extract values from the respective position.
#'  - `dplyr::across()` powers the application of the two above functions within
#'  `split_by_parens()`, including the creation of new columns.
#'  - `tidyr::separate()` is a more general function (but does not recognize
#'  closing elements).

#' @export
#'
#' @examples
#' # Call `split_by_parens()` on data like these:
#' df1 <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 (0.21)",    "0.19 (0.13)",
#'   "0.19 (0.28)",    "0.53 (0.10)",
#'   "0.62 (0.16)",    "0.50 (0.11)",
#'   "0.15 (0.35)",    "0.57 (0.16)",
#' )
#'
#' # Basic usage:
#' df1 %>%
#'   split_by_parens()
#'
#' # Name specific columns to only return those:
#' df1 %>%
#'   split_by_parens(drone)
#'
#' # Pivot the data into a longer format
#' # by setting `.transform` to `TRUE`:
#' df1 %>%
#'   split_by_parens(.transform = TRUE)
#'
#' # Choose different column names or
#' # name suffixes with `.col1` and `.col2`:
#' df1 %>%
#'   split_by_parens(.col1 = "beta", .col2 = "se")
#'
#' df1 %>%
#'   split_by_parens(.transform = TRUE,
#'   .col1 = "beta", .col2 = "se")
#'
#' # With a different separator...
#' df2 <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 [0.21]",    "0.19 [0.13]",
#'   "0.19 [0.28]",    "0.53 [0.10]",
#'   "0.62 [0.16]",    "0.50 [0.11]",
#'   "0.15 [0.35]",    "0.57 [0.16]",
#' )
#'
#' # ... specify `.sep`:
#' df2 %>%
#'   split_by_parens(.sep = "brackets")
#'
#' # (Accordingly with `{}` and `"braces"`.)
#'
#' # If the separator is yet a different one...
#' df3 <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 <0.21>",    "0.19 <0.13>",
#'   "0.19 <0.28>",    "0.53 <0.10>",
#'   "0.62 <0.16>",    "0.50 <0.11>",
#'   "0.15 <0.35>",    "0.57 <0.16>",
#' )
#'
#' # ... `.sep` should be a length-2 vector
#' # that contains the separating elements:
#' df3 %>%
#'   split_by_parens(.sep = c("<", ">"))



split_by_parens <- function(.data, ..., .keep = FALSE, .transform = FALSE,
                            .sep = "parens", .col1 = "x", .col2 = "sd") {

  # Anything passed through the dots can only be a tidyselect specification, not
  # a formal argument. This call checks for named arguments within the dots, and
  # throws an error if there are any:
  ellipsis::check_dots_unnamed()

  # Capture any valid tidyselect specification that might have been applied by
  # the user to select columns from `.data`:
  selector <- rlang::enexprs(...)

  # Since `.sep` will be passed through the dots of `dplyr::across()`, which may
  # lead to issues with the timing of evaluation, its evaluation is forced here:
  force(.sep)

  # In case no columns were specified that way, prepare and defuse a call that
  # will select all columns:
  if (length(selector) == 0L) {
    selector <- rlang::exprs(dplyr::everything())
  }

  # Apply the extractor functions `before_parens()` and `inside_parens()` to all
  # selected columns from `.data` (see above), going by `.sep`, which is
  # `"parens"` by default and will thus look for parentheses:
  out <- dplyr::mutate(.data, dplyr::across(
    .cols = c(!!!selector),
    .fns = list(before_parens, inside_parens),
    sep = .sep
  ))

  # By default, the original columns are dropped. If the user disabled this by
  # setting `.keep` to `TRUE`, `.transform` can't also be `TRUE` because this
  # would likely lead to incommensurable data frame dimensions:
  if (!.keep) {
    out <- dplyr::select(out, -names(.data))
  } else if (.transform) {
    cli::cli_abort("`.keep` and `.transform` can't both be `TRUE`.")
  }

  # Modify the column names with the endings from the `.col*` arguments:
  names(out) <- stringr::str_replace(names(out), "_1$", paste0("_", .col1))
  names(out) <- stringr::str_replace(names(out), "_2$", paste0("_", .col2))

  # Return the output tibble. If desired, pivot it to a longer format beforehand
  # using a specified internal helper function from the utils.R file:
  if (.transform) {
    return(transform_split_parens(out, end1 = .col1, end2 = .col2))
  }

  out
}


