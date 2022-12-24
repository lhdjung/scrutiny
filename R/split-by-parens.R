
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
#'   Default is `everything()`, which selects all columns that pass `check_sep`.
#' @param check_sep Boolean. If `TRUE` (the default), columns are excluded if
#'   they don't contain the `sep` elements.
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


split_by_parens <- function(data, cols = everything(), check_sep = TRUE,
                            keep = FALSE, transform = FALSE, sep = "parens",
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

  # Determine which columns have suitable values with regards to the `sep`
  # elements and capture their names:
  names_of_cols_with_seps <- data %>%
    dplyr::select(
      function(x) {
        sep_in_order <- translate_length1_sep_keywords(sep)
        sep_in_order <- paste0(sep_in_order[1], "[^)]*", sep_in_order[2])
        x %>%
          stringr::str_detect(sep_in_order) %>%
          all()
      }
    ) %>%
    colnames()

  # By default, take care that only those columns which contain the `sep`
  # elements will be operated on:
  if (check_sep) {
    selection2 <- rlang::expr(all_of(names_of_cols_with_seps))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  # Apply the extractor functions `before_parens()` and `inside_parens()` to all
  # selected columns from `data` (see above), going by `sep`, which is
  # `"parens"` by default and will thus look for parentheses:
  out <- dplyr::mutate(data, dplyr::across(
      .cols = {{ cols }} & !!selection2,
      .fns = list(before_parens, inside_parens),
      sep = sep
    ))

  if (tibble::is_tibble(data)) {
    out <- tibble::as_tibble(out)
  }

  # From here onward, the only relevant aspect of `data` is its column names:
  data_names <- colnames(data)
  rm(data)

  # Check if any columns from `data` don't contain the `sep` elements. If so,
  # the way this was handled above depends on `check_sep`: They were either
  # excluded from splitting (`TRUE`, the default) or they were included and
  # split in a way that was likely not intended. In both cases, the user is
  # warned appropriately:
  if (!identical(names_of_cols_with_seps, data_names)) {
    names_wrong_cols <- data_names[!data_names %in% names_of_cols_with_seps]
    msg_colnames <- wrap_in_backticks(names_wrong_cols)
    msg_seps <- message_sep_if_cols_excluded(sep)
    if (check_sep) {
      if (length(names_wrong_cols) == 1L) {
        msg_col_cols <- "1 column was"
        msg_it_they <- "It doesn't"
      } else {
        msg_col_cols <- paste0(length(names_wrong_cols), " columns were")
        msg_it_they <- "They don't"
      }
      names_wrong_cols <- wrap_in_backticks(names_wrong_cols)
      cli::cli_warn(c(
        "!" = "{msg_col_cols} excluded: {names_wrong_cols}.",
        "!" = "{msg_it_they} contain the `sep` elements, {msg_seps}."
      ))
    } else {
      if (length(names_wrong_cols) == 1L) {
        msg_col_cols <- "1 column"
        msg_this_these <- "It doesn't"
      } else {
        msg_col_cols <- paste0(length(names_wrong_cols), " columns")
        msg_this_these <- "These columns don't"
      }
      cli::cli_warn(c(
        "!" = "{msg_col_cols} couldn't be split: {msg_colnames}.",
        "!" = "{msg_this_these} contain the `sep` elements, {msg_seps}."
      ))
    }
  }

  # By default, the original columns are dropped. If the user disabled this by
  # setting `keep` to `TRUE`, `transform` can't also be `TRUE` because this
  # would likely lead to incommensurable data frame dimensions:
  if (!keep) {
    out <- dplyr::select(out, -all_of(data_names))
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

