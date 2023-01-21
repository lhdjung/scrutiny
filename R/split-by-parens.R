
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
#' @param keep Boolean. If set to `TRUE`, the originally selected columns that
#'   were split by the function also appear in the output. Default is `FALSE`.
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
#' @return Data frame.

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

  # Compute a named integer vector of index locations (within `data`) of the
  # columns that will be split:
  cols_to_select <- rlang::expr({{ cols }} & !!selection2)
  cols_to_select <- tidyselect::eval_select(cols_to_select, data)

  # Prepare the endings of the split column names:
  endings <- rep(c(end1, end2), times = length(cols_to_select))

  # Apply the extractor functions `before_parens()` and `inside_parens()` to all
  # selected columns from `data` (see above), going by `sep`, which is
  # `"parens"` by default and will thus look for parentheses:
  out <- suppressWarnings(dplyr::mutate(data, dplyr::across(
    .cols = all_of(cols_to_select),
    # .fns = list(before_parens, inside_parens),
    .fns = list(
      ~ before_parens(string = .x, sep = sep),
      ~ inside_parens(string = .x, sep = sep)
    ),
    .names = "{.col}_{endings}"
    # sep = sep
  ), .before = 1L))

  # The output is meant to have the same class as the input. Since `out` is not
  # a tibble, coerce it to a tibble if and only if `data` is:
  if (tibble::is_tibble(data)) {
    out <- tibble::as_tibble(out)
  }

  # Select all "neutral" columns: those that were not selected above. They will
  # be added to `out` in the end unless it's transformed.
  names_data <- colnames(data)
  names_neutral_cols <- names_data[!names_data %in% names(cols_to_select)]
  neutral_cols <- dplyr::select(data, all_of(names_neutral_cols))

  # Save memory by removing objects that are no longer needed:
  rm(data, selection2, cols_to_select, endings)

  # By default, the original columns are dropped. If the user disabled this by
  # setting `keep` to `TRUE`, `transform` can't also be `TRUE` because this
  # would likely lead to incommensurable data frame dimensions:
  if (!keep) {
    names_original <- names_data[!names_data %in% names_neutral_cols]
    out <- dplyr::select(out, !all_of(names_original))
  }

  # Check if any columns from `data` don't contain the `sep` elements. If so,
  # the way this was handled above depends on `check_sep`: They were either
  # excluded from splitting (`TRUE`, the default) or they were included and
  # split in a way that was likely not intended. In both cases, the user is
  # warned appropriately:
  if (!identical(names_of_cols_with_seps, names_data)) {
    names_wrong_cols <- names_data[!names_data %in% names_of_cols_with_seps]
    msg_reason <- message_sep_if_cols_excluded(sep)
    msg_reason <- paste0("contain the `sep` elements, ", msg_reason)
    if (check_sep) {
      msg_exclusion <- paste0(c("was", "were"), " not split")
    } else {
      msg_exclusion <- "couldn't be split"
    }
    warn_wrong_columns_selected(names_wrong_cols, msg_exclusion, msg_reason)
  }

  # Without a special transformation, nothing is left to do except for appending
  # those columns that were never split to the output:
  if (!transform) {
    return(dplyr::mutate(out, {{ neutral_cols }}))
  }

  # Pivot the output to a longer format beforehand using a specified internal
  # helper function from the utils.R file. Since this changes the format, it
  # only works if no columns were left unsplit. If there are any, an error is
  # thrown:
  if (keep) {
    cli::cli_abort(c("!" = "`keep` and `transform` can't both be `TRUE`."))
  } else if (length(names_neutral_cols) > 0L) {
    names_neutral_cols <- wrap_in_backticks(names_neutral_cols)
    cli::cli_abort(c(
      "!" = "`transform` can't be `TRUE` if some columns are left unsplit.",
      "i" = "This concerns {names_neutral_cols}."
    ))
  }
  transform_split_parens(out, end1, end2)

}

