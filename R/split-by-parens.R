
#' Split columns by parentheses, brackets, braces, or similar
#'
#' @description Summary statistics are often presented like `"2.65 (0.27)"`.
#'   When working with tables copied into R, it can be tedious to separate
#'   values before and inside parentheses. `split_by_parens()` does this
#'   automatically.
#'
#'   By default, it operates on all columns. Output can optionally be pivoted
#'   into a longer format by setting `transform` to `TRUE`.
#'
#'   Choose separators other than parentheses by specifying the `sep` argument.
#'
#' @param data Data frame.
#' @param cols Select columns from `data` using
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, which selects all columns.
#' @param ... Optionally, select columns from `data` as in `dplyr::select()`.
#'   Splitting will then be restricted to these columns. This is useful if not
#'   all values contain parentheses.
#' @param keep Boolean. If set to `TRUE`, the original columns from `data` also
#'   appear in the output. Default is `FALSE`.
#' @param transform Boolean. If set to `TRUE`, the output will be pivoted to be
#'   better suitable for typical follow-up tasks. Default is `FALSE`.
#' @param sep String. What to split by. Either `"parens"`, `"brackets"`, or
#'   `"braces"`; or a length-2 vector of custom separators (see Examples).
#'   Default is `"parens"`.
#' @param end1,end2 Strings. Endings of the two column names that result from
#'   splitting a column. Default is `"x"` for `end1` and `"sd"` for `end2`.
#'
#' @include utils.R before-inside-parens.R
#'
#' @return A tibble with string columns.

#' @seealso
#'  - `before_parens()` and `inside_parens()` take a string vector and
#'  extract values from the respective position.
#'  - `dplyr::across()` powers the application of the two above functions within
#'  `split_by_parens()`, including the creation of new columns.
#'  - `tidyr::separate()` is a more general function, but it does not recognize
#'  closing elements (e.g., closed parentheses).

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
#' # by setting `transform` to `TRUE`:
#' df1 %>%
#'   split_by_parens(transform = TRUE)
#'
#' # Choose different column names or
#' # name suffixes with `end1` and `end2`:
#' df1 %>%
#'   split_by_parens(end1 = "beta", end2 = "se")
#'
#' df1 %>%
#'   split_by_parens(
#'     transform = TRUE,
#'     end1 = "beta", end2 = "se"
#'   )
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
#' # ... specify `sep`:
#' df2 %>%
#'   split_by_parens(sep = "brackets")
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
#' # ... `sep` should be a length-2 vector
#' # that contains the separating elements:
#' df3 %>%
#'   split_by_parens(sep = c("<", ">"))


split_by_parens <- function(data, cols = everything(), keep = FALSE,
                            transform = FALSE, sep = "parens",
                            end1 = "x", end2 = "sd", ...) {

  # Check whether the user specified any "old" arguments: those starting on a
  # dot. This check is performed by a custom function from the utils.R file. It
  # is now the only remaining purpose of the `...` dots because these are no
  # longer meant to be used. Any other arguments passed through them should
  # still lead to an error:
  check_old_args_split_by_parens(data, rlang::enquos(...))
  rlang::check_dots_empty()

  # Since `sep` will be passed through the dots of `dplyr::across()`, which may
  # lead to issues with the timing of evaluation, its evaluation is forced here:
  force(sep)

  # # Prepare the endings of the new columns -- one pair of endings for each
  # # original column:
  # endings <- rep(c(end1, end2), times = ncol(data))

  # Apply the extractor functions `before_parens()` and `inside_parens()` to all
  # selected columns from `data` (see above), going by `sep`, which is
  # `"parens"` by default and will thus look for parentheses:
  out <- dplyr::mutate(data, dplyr::across(
    .cols = {{ cols }},
    .fns = list(before_parens, inside_parens),
    # .names = "{.col}_{endings}",
    sep = sep
  ))

  # By default, the original columns are dropped. If the user disabled this by
  # setting `keep` to `TRUE`, `transform` can't also be `TRUE` because this
  # would likely lead to incommensurable data frame dimensions:
  if (!keep) {
    out <- dplyr::select(out, -names(data))
  } else if (transform) {
    cli::cli_abort(c("x" = "`keep` and `transform` can't both be `TRUE`."))
  }

  # Modify the column names with the endings from the `end*` arguments. We
  # can't use the `.names` argument of `across()` because the number of columns
  # is not yet known at that earlier point.
  names(out) <- stringr::str_replace(names(out), "_1$", paste0("_", end1))
  names(out) <- stringr::str_replace(names(out), "_2$", paste0("_", end2))

  # Return the output tibble. If desired, pivot it to a longer format beforehand
  # using a specified internal helper function from the utils.R file:
  if (transform) {
    transform_split_parens(out, end1 = end1, end2 = end2)
  } else {
    out
  }

}

