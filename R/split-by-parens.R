
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
#'   Choose separators other than parentheses with the `sep` argument.
#'
#' @param data Data frame.
#' @param cols Select columns from `data` using
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, which selects all columns.
#' @param keep Boolean. If set to `TRUE`, the original columns from `data` also
#'   appear in the output. Default is `FALSE`.
#' @param transform Boolean. If set to `TRUE`, the output will be pivoted to be
#'   better suitable for typical follow-up tasks. Default is `FALSE`.
#' @param sep String. What to split by. Either `"parens"`, `"brackets"`, or
#'   `"braces"`; or a length-2 vector of custom separators (see Examples).
#'   Default is `"parens"`.
#' @param end1,end2 Strings. Endings of the two column names that result from
#'   splitting a column. Default is `"x"` for `end1` and `"sd"` for `end2`.
#' @param ... These dots must be empty.
#'
#' @include utils.R before-inside-parens.R
#'
#' @return A tibble with string columns.

#' @seealso
#'  - `before_parens()` and `inside_parens()` take a string vector and extract
#'  values from the respective position.
#'  - `dplyr::across()` powers the application of the two above functions within
#'  `split_by_parens()`, including the creation of new columns.
#'  - `tidyr::separate()` is a more general function, but it does not recognize
#'  closing elements such as closed parentheses.

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
#' # Name specific columns with `cols` to only split those:
#' df1 %>%
#'   split_by_parens(cols = drone)
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
  # dot. This check is now the only remaining purpose of the `...` dots because
  # these are no longer meant to be used. Any other arguments passed through
  # them should still lead to an error:
  check_new_args_without_dots(
    data, dots = rlang::enquos(...),
    old_args = c(".data", ".keep", ".transform", ".sep", ".col1", ".col2"),
    name_fn = "split_by_parens"
  )

  # Since `sep` will be passed through the dots of `dplyr::across()`, which may
  # lead to issues with the timing of evaluation, its evaluation is forced here:
  force(sep)

  # Apply the extractor functions `before_parens()` and `inside_parens()` to all
  # selected columns from `data` (see above), going by `sep`, which is
  # `"parens"` by default and will thus look for parentheses:
  out <- dplyr::mutate(data, dplyr::across(
    .cols = {{ cols }},
    .fns = list(before_parens, inside_parens),
    sep = sep
  ))

  # The output should always be a tibble:
  out <- tibble::as_tibble(out)

  # Select the newly created columns and check if any of them contain nothing
  # but `NA` values. If so, this means that one or more columns in `data` don't
  # contain the `sep` elements. These columns are screened out...
  index_first_col_out <- ncol(dplyr::select(data, {{ cols }})) + 1L
  names_new_na_cols <- colnames(dplyr::select(
    out,
    all_of(index_first_col_out):ncol(out) & where(function(x) all(is.na(x)))
  ))

  # ...and the user is warned that operating on them was not successful:
  if (length(names_new_na_cols) > 0L) {
    colnames_wrong <- stringr::str_remove(names_new_na_cols, "_1$|_2$")
    colnames_wrong <- wrap_in_backticks(colnames_wrong)
    if (ncol(new_na_cols) == 1L) {
      msg_one_some <- "One column"
      msg_this_these <- "This column doesn't"
    } else {
      msg_one_some <- "Some columns"
      msg_this_these <- "These columns don't"
    }
    if (length(sep) == 2L) {
      msg_seps <- wrap_in_quotes(sep)
      msg_seps <- glue::glue("{msg_seps[1]} and {msg_seps[2]}")
    } else if (sep == "parens") {
      msg_seps <- "i.e., parentheses"
    } else if (sep == "brackets") {
      msg_seps <- "i.e., square brackets"
    } else if (sep == "braces") {
      msg_seps <- "i.e., curly braces"
    }
    cli::cli_warn(c(
      "!" = "{msg_one_some} couldn't be split.",
      "!" = "{msg_this_these} contain the `sep` elements, {msg_seps}:",
      ">" = "{colnames_wrong}."
    ))
  }

  # By default, the original columns are dropped. If the user disabled this by
  # setting `keep` to `TRUE`, `transform` can't also be `TRUE` because this
  # would likely lead to incommensurable data frame dimensions:
  if (!keep) {
    out <- dplyr::select(out, -names(data))
  } else if (transform) {
    cli::cli_abort(c("x" = "`keep` and `transform` can't both be `TRUE`."))
  }

  # Modify the column names with the endings from the `end*` arguments. We can't
  # use the `.names` argument of `dplyr::across()` because the number of columns
  # in the output data frame is not yet known at that earlier point.
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

