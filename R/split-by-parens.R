#' Split columns by parentheses, brackets, braces, or similar
#'
#' @description Summary statistics are often reported like `"2.65 (0.27)"`.
#'   `split_by_parens()` splits such columns into the part before the
#'   parentheses and the part inside them. By default, it splits every column
#'   that contains the delimiters. Set `transform` to `TRUE` to pivot the
#'   result into a longer format.
#'
#' @param data Data frame.
#' @param cols Columns to split, selected with
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, restricted by `check_delim`.
#' @param check_delim Logical. If `TRUE` (the default), columns whose non-`NA`
#'   values do not all contain the delimiters are skipped.
#' @param keep Logical. Should the original columns be kept? Default is
#'   `FALSE`.
#' @param transform Logical. Should the output be pivoted into a longer
#'   format, with one column per part and an `.origin` column naming the
#'   source? Default is `FALSE`.
#' @param delim String. The pair of delimiters around the second part:
#'   `"parens"` (the default), `"brackets"`, or `"braces"`, or two custom
#'   strings, matched literally.
#' @param end1,end2 Strings. Suffixes of the two columns that a split column
#'   produces. Defaults are `"x"` and `"sd"`.
#'
#' @include utils.R before-inside-parens.R
#'
#' @return An ungrouped data frame, a tibble if `data` is one or if
#'   `transform` is `TRUE`.
#'
#' @seealso
#'  - [`before_parens()`] and [`inside_parens()`] for the two parts of a
#'  string vector.
#'  - [`tidyr::separate_wider_delim()`], which is more general but does not
#'  recognize closing delimiters.
#'
#' @export
#'
#' @examples
#' df1 <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 (0.21)",    "0.19 (0.13)",
#'   "0.19 (0.28)",    "0.53 (0.10)",
#'   "0.62 (0.16)",    "0.50 (0.11)",
#'   "0.15 (0.35)",    "0.57 (0.16)",
#' )
#'
#' df1 |>
#'   split_by_parens()
#'
#' # Only some columns:
#' df1 |>
#'   split_by_parens(cols = drone)
#'
#' # Longer format:
#' df1 |>
#'   split_by_parens(transform = TRUE)
#'
#' # Other suffixes:
#' df1 |>
#'   split_by_parens(end1 = "beta", end2 = "se")
#'
#' # Other delimiters:
#' df2 <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 [0.21]",    "0.19 [0.13]",
#'   "0.19 [0.28]",    "0.53 [0.10]",
#' )
#'
#' df2 |>
#'   split_by_parens(delim = "brackets")
#'
#' df3 <- tibble::tribble(
#'   ~drone,           ~selfpilot,
#'   "0.09 <0.21>",    "0.19 <0.13>",
#'   "0.19 <0.28>",    "0.53 <0.10>",
#' )
#'
#' df3 |>
#'   split_by_parens(delim = c("<", ">"))

split_by_parens <- function(
  data,
  cols = everything(),
  check_delim = TRUE,
  keep = FALSE,
  transform = FALSE,
  delim = "parens",
  end1 = "x",
  end2 = "sd"
) {
  check_flag(check_delim)
  check_flag(keep)
  check_flag(transform)
  check_string(end1)
  check_string(end2)
  # `delim` goes through the dots of `dplyr::across()`, so evaluate it now:
  force(delim)
  data <- as_data_frame_input(data, "`before_parens()` or `inside_parens()`")
  check_unique_names(data, "data")

  # The function works on whole columns; with groups, `dplyr::select()` would
  # re-add the grouping columns, and `across()` would run per group:
  data <- dplyr::ungroup(data)

  # A column qualifies if every non-missing value has an opening `delim` element
  # followed by a closing one. With no rows, a character column qualifies, so
  # that the output has the same shape as for any other data:
  data_is_empty <- nrow(data) == 0L
  names_of_cols_with_delims <- data |>
    dplyr::select(
      function(x) {
        if (data_is_empty) {
          return(is.character(x))
        }
        x <- x[!is.na(x)]
        delims <- translate_delim_keywords(delim)
        after_open <- stringr::str_split_fixed(
          x,
          stringr::fixed(delims[1L]),
          n = 2L
        )[, 2L]
        length(x) > 0L &&
          all(stringr::str_detect(after_open, stringr::fixed(delims[2L])))
      }
    ) |>
    colnames()

  if (check_delim) {
    selection2 <- rlang::expr(all_of(names_of_cols_with_delims))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  cols_to_select <- rlang::expr({{ cols }} & !!selection2)
  # A rename in `cols`, as in `c(new = a)`, would name the new columns after
  # it and leave the old one unsplit:
  cols_to_select <- tidyselect::eval_select(
    cols_to_select,
    data,
    allow_rename = FALSE
  )

  endings <- rep(c(end1, end2), times = length(cols_to_select))

  # `dplyr::across()` would silently overwrite a column of the same name, so
  # the new names must be new:
  # `recycle0`: with no column to split, `paste0()` would return `"_"`:
  names_new <- cols_to_select |>
    names() |>
    rep(each = 2L) |>
    paste0("_", endings, recycle0 = TRUE)
  if (identical(end1, end2)) {
    abort_in_export(
      "!" = "`end1` and `end2` must be different.",
      "x" = "Both are \"{end1}\"."
    )
  }
  # ...and differ from each other: `a` with the ending `x_y` and `a_x` with `y`
  # would both be `a_x_y`.
  names_taken <- unique(c(
    intersect(names_new, colnames(data)),
    names_new[duplicated(names_new)]
  ))
  if (length(names_taken) > 0L) {
    abort_in_export(
      "!" = "Splitting would overwrite existing columns.",
      "x" = "{.var {names_taken}} {?is/are} already in `data`, or would be \\
      created twice.",
      "i" = "Rename {cli::qty(names_taken)}{?it/them}, or choose other `end1` and \\
      `end2`."
    )
  }

  # Defined out here: dplyr inlines functions written inside `across()`, which
  # then look up `delim` among the columns of `data` first, such as one of
  # September's values next to `aug` and `oct`:
  split_fns <- list(
    function(x) before_parens(string = x, delim = delim),
    function(x) inside_parens(string = x, delim = delim)
  )

  out <- data |>
    dplyr::mutate(
      dplyr::across(
        .cols = all_of(cols_to_select),
        .fns = split_fns,
        .names = "{.col}_{endings}"
      ),
      # A data frame without columns has no position 1:
      .before = if (ncol(data) > 0L) 1L
    ) |>
    suppressWarnings()

  # Keep the class of the input:
  if (tibble::is_tibble(data)) {
    out <- tibble::as_tibble(out)
  }

  # Columns not split, added back at the end unless `transform` is `TRUE`:
  names_data <- colnames(data)
  names_neutral_cols <- names_data[!names_data %in% names(cols_to_select)]
  neutral_cols <- data |>
    dplyr::select(all_of(names_neutral_cols))

  if (!keep) {
    names_original <- names_data[!names_data %in% names_neutral_cols]
    out <- out |>
      dplyr::select(!all_of(names_original))
  }

  # Warn about selected columns without the `delim` elements: depending on
  # `check_delim`, they were either skipped or split in a way likely not
  # intended. Only the columns that `cols` selected are worth a warning, as in
  # `decimal_places_df()`:
  names_selected <- names(tidyselect::eval_select(
    rlang::expr({{ cols }}),
    data,
    allow_rename = FALSE
  ))

  names_wrong_cols <-
    names_selected[!names_selected %in% names_of_cols_with_delims]

  if (length(names_wrong_cols) > 0L) {
    msg_reason <- delim |>
      message_delim_if_cols_excluded() |>
      call_on(\(x) paste0("contain the `delim` elements, ", x))
    if (check_delim) {
      msg_exclusion <- paste0(c("was", "were"), " not split")
    } else {
      msg_exclusion <- "couldn't be split"
    }
    warn_wrong_columns_selected(names_wrong_cols, msg_exclusion, msg_reason)
  }

  if (!transform) {
    out <- out |>
      dplyr::mutate({{ neutral_cols }})
    return(out)
  }

  # The longer format has no place for columns left unsplit, or for the
  # original columns:
  if (keep) {
    abort_in_export("!" = "`keep` and `transform` can't both be `TRUE`.")
  } else if (length(names_neutral_cols) > 0L) {
    names_neutral_cols <- wrap_in_backticks(names_neutral_cols)
    abort_in_export(
      "!" = "`transform` can't be `TRUE` if some columns are left unsplit.",
      "i" = "This concerns {names_neutral_cols}."
    )
  }

  # The longer format has a column of its own by this name:
  if (any(c(end1, end2) == ".origin")) {
    abort_in_export(
      "!" = "`end1` and `end2` can't be \".origin\" if `transform` is `TRUE`.",
      "i" = "The transformed data have an `.origin` column already."
    )
  }

  transform_split_parens(out, names(cols_to_select), end1, end2)
}
