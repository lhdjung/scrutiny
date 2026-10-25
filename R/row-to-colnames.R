#' Turn row values into column names
#'
#' @description Tables extracted from PDF, e.g. with tabulizer, sometimes have
#'   their column names in the first row (or rows) and nondescript names
#'   above. `row_to_colnames()` replaces the column names by the values of
#'   those rows and drops the rows.
#'
#' @details With several rows, their values are pasted together per column.
#'   Missing values are skipped; if all of a column's values in these rows are
#'   missing, it keeps its name. The new names must be unique: a header that
#'   repeats, as in "M, SD, M, SD", is an error unless a second row tells the
#'   columns apart.
#'
#' @param data Data frame or matrix.
#' @param row Integer. Position of the row or rows that contain the column
#'   names. Default is `1`.
#' @param collapse String. Separator between the values of several rows.
#'   Default is `" "`.
#' @param drop Logical. Should the rows be removed? Default is `TRUE`.
#'
#' @include utils.R
#'
#' @seealso `unheadr::mash_colnames()`, a more general solution to the same
#'   problem.
#'
#' @return `data` with new column names, as a data frame of the same class, or
#'   a tibble if `data` was a matrix.
#' @export

# @examples

row_to_colnames <- function(data, row = 1L, collapse = " ", drop = TRUE) {
  # Initial checks ---

  if (!is.data.frame(data)) {
    if (is.matrix(data)) {
      # Only missing names get a placeholder, quietly: a column whose header
      # cells are all `NA` keeps its name below, so an existing one must stay.
      data <- tibble::as_tibble(
        data,
        .name_repair = function(x) {
          dplyr::if_else(x == "", paste0("V", seq_along(x)), x)
        }
      )
    } else {
      cli::cli_abort(c(
        "!" = "`data` must be a data frame or a matrix.",
        "x" = "It is {an_a_type(data)}."
      ))
    }
  }

  if (length(row) < 1L) {
    cli::cli_abort(c(
      "!" = "`row` must have length 1 or greater.",
      "x" = "It has length {length(row)}."
    ))
  }

  if (!is.numeric(row)) {
    check_type(row, c("double", "integer"))
  }

  # A missing or infinite `row` used to fail in `if ()`:
  if (!all(is.finite(row)) || !all(is_whole_number(row))) {
    offenders <- row[!is.finite(row) | !is_whole_number(row)]
    offenders <- wrap_in_backticks(offenders)
    cli::cli_abort(c(
      "!" = "`row` must only have whole numbers.",
      "x" = "It includes {offenders}."
    ))
  }

  check_type(collapse, "character")
  check_length(collapse, 1L)

  if (any(row < 1L | row > nrow(data))) {
    offenders <- wrap_in_backticks(row[row < 1L | row > nrow(data)])
    cli::cli_abort(c(
      "!" = "`row` must be between 1 and the number of rows in `data`.",
      "x" = "It includes {offenders}, but `data` has {nrow(data)} row{?s}."
    ))
  }

  # Get correct column names ---

  # Restore the vector of correct column names by the values stored in the one
  # or more rows that were specified by the `row` argument. For each column, its
  # non-missing cells in these rows are pasted together. If all of them are
  # missing, the column keeps its current name:
  correct <- vapply(
    seq_along(data),
    function(i) {
      cells <- data[[i]][row] |>
        as.character() |>
        call_on(\(x) x[!is.na(x)])
      if (length(cells) == 0L) {
        colnames(data)[i]
      } else {
        paste(cells, collapse = collapse)
      }
    },
    character(1L)
  )

  # Subsequent checks ---

  # Check for empty strings in the "correct" column names:
  correct_is_empty <- stringr::str_length(correct) == 0L

  # If any of those strings really are empty, they are unsuitable as column
  # names. An error is then thrown:
  if (any(correct_is_empty)) {
    n_empty <- length(correct_is_empty[correct_is_empty])
    name_names <- dplyr::if_else(n_empty == 1L, "name", "names")
    cli::cli_abort(c(
      "!" = "Each column name must have at least one character.",
      "x" = "{n_empty} empty column {name_names}.",
      "i" = "Make sure to specify `row` in `row_to_colnames()` accordingly."
    ))
  }

  # A tibble's column names must be unique, but a table often repeats its
  # headers, as in "M, SD, M, SD" for two groups. Names that are taken twice
  # used to make an invalid tibble that only failed further down:
  names_dup <- unique(correct[duplicated(correct)])
  if (length(names_dup) > 0L) {
    abort_in_export(
      "Each column name must be unique.",
      "x" = "{wrap_in_backticks(names_dup)} {cli::qty(length(names_dup))}\\
      {?is/are} taken more than once.",
      "i" = "Specify several rows in `row` to tell the columns apart, or \\
      change the repeated cells in `data` first."
    )
  }

  # Final correction and return ---

  # Reinstate the correct column names:
  colnames(data) <- correct

  # Return the data frame. By default (`drop = TRUE`), remove the specified row
  # or rows beforehand:
  if (drop) {
    data[-row, , drop = FALSE]
  } else {
    data
  }
}
