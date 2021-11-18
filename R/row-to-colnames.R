
#' Turn row values into column names
#'
#' @description Data frames sometimes have wrong column names, while the correct
#'   column names are stored in one or more rows in the data frame itself. To
#'   remedy this issue, call `row_to_colnames()` on the data frame: It replaces
#'   the column names by the values of the specified rows (by default, only the
#'   first one). These rows are then dropped by default.
#'
#' @details If multiple rows are specified, the row values for each individual
#'   column are pasted together. Some special characters might then be missing.
#'
#'   This function might be useful when importing tables from PDF, e.g. with
#'   \href{https://cran.r-project.org/web/packages/tabulizer/vignettes/tabulizer.html}{tabulizer}.
#'    In R, these data frames (converted from matrices) do sometimes have the
#'   issue described above.
#'
#' @param data Data frame or matrix.
#' @param row Integer. Position of the rows (one or more) that jointly contain
#'   the correct column names. Default is `1`.
#' @param drop Boolean. If `TRUE` (the default), the rows specified with `row`
#'   are removed.
#'
#' @include utils.R
#'
#' @seealso `dplyr::slice()`, which the function wraps; and
#'   `unheadr::mash_colnames()`, a more sophisticated solution to the same
#'   problem.
#'
#' @return A data frame.
#' @export
#'

# @examples


row_to_colnames <- function(data, row = 1, drop = TRUE) {

  # Checks ---

  if (!is.data.frame(data)) {
    if (is.matrix(data)) {
      data <- tibble::as_tibble(data)
    } else {
      cli::cli_abort(c(
        "`data` is {an_a_type(data)}",
        "x" = "It needs to be a data frame or a matrix."
      ))
    }
  }

  if (length(row) < 1) {
    cli::cli_abort(c(
      "`row` has length {length(row)}",
      "x" = "It needs to have length 1 or more."
    ))
  }

  if (any(!is_whole_number(row))) {
    cli::cli_abort(c(
      "`row` is `{row}`",
      "x" = "It needs to be a whole number."
    ))
  }


  # Main part ---

  # Restore the vector of correct column names by the values stored in the one
  # or more rows that were specified by the `row` argument:
  correct <- dplyr::slice(data, row) %>%
    rbind(colnames(data), .) %>%
    purrr::map(remove_na)

  # If multiple rows were specified that way, the resulting vector needs to be
  # formatted to restore the correct column names:
  if (length(row) == 1) {
    correct <- correct %>%
      purrr::map(utils::tail, 1) %>%
      as.character()
  } else {
    correct <- correct %>%
      stringr::str_c() %>%
      stringr::str_squish() %>%
      stringr::str_remove_all("\"") %>%
      stringr::str_remove_all("c\\(") %>%
      stringr::str_remove_all("\\)$") %>%
      stringr::str_replace_all(", ", " ") %>%
      stringr::str_replace_all("- ", "-") %>%
      suppressWarnings()
  }

  # Check for empty strings in the "correct" column names:
  correct_empty <- stringr::str_length(correct) == 0

  # If any of those strings really are empty, they are unsuitable as column
  # names. An error is then thrown:
  if (any(correct_empty)) {
    n_empty <- length(correct_empty[correct_empty])
    name_names <- dplyr::if_else(n_empty == 1, "name", "names")
    cli::cli_abort(c(
      "{n_empty} empty column {name_names}",
      "x" = "Each column name must have at least one character.",
      ">" = "Make sure to specify `row` in `row_to_colnames()` accordingly."
    ))
  }

  # Reinstate the correct column names:
  colnames(data) <- correct

  # Return the data frame. By default (`drop = TRUE`), remove the specified row
  # or rows beforehand:
  if (drop) {
    dplyr::slice(data, -row)
  } else {
    data
  }

}

