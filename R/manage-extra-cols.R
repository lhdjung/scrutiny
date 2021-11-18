

manage_extra_cols <- function(data, extra, other_cols) {

  # Throw error if `extra` is a wrong string; i.e., if there is no extra column
  # by that name in `data`:
  if (!all(extra %in% colnames(data)) && is.character(extra)) {
    wrong_colnames <- extra[!extra %in% colnames(data)]
    is_name_are_names <- dplyr::if_else(
      length(wrong_colnames) == 1,
      "is not a column name",
      "are not column names"
    )
    cli::cli_abort(
      "{wrong_colnames} {is_name_are_names} of `data`."
    )
  }

  # Throw error if the `extra` is a wrong number; i.e., if that given number is
  # larger than the actual number of extra columns:
  if (!is.infinite(extra) && is.numeric(extra) &&
      any(c(extra, length(extra)) > length(other_cols))) {

    n_cols_info <- dplyr::case_when(
      length(other_cols) == 0  ~ glue::glue("there aren't any extra columns"),
      length(other_cols) == 1  ~ glue::glue("there's only 1 extra column"),
      TRUE                     ~ glue::glue("there are only \\
                                            {length(other_cols)} extra columns")
    )

    if (length(extra) == 1) {
      cli::cli_abort(c(
      "`extra` was specified as {extra}, for extra column number {extra}.",
      "x" = "This number is too high -- {n_cols_info} in `data`."
      ))
    } else {
      num_length_info <- dplyr::if_else(
        length(other_cols) < extra[1],
        "These numbers are",
        "That last number is"
      )
      cli::cli_abort(c(
      "`extra` was specified as `{rlang::enexprs(extra)}`.",
      "x" = "{num_length_info} too high -- {n_cols_info} in `data`."
      ))
    }
  }


  # Finally, make `other_cols` capture any and all extra columns, and return it:
  if (!is.infinite(extra) && length(other_cols) > 0) {
    dplyr::select(other_cols, all_of(extra))
  } else {
    other_cols
  }

}


