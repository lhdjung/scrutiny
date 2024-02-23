
manage_extra_cols <- function(data, extra, other_cols) {

  # Throw error if `extra` is a wrong string; i.e., if there is no extra column
  # by that name in `data`:
  if (!all(extra %in% colnames(data)) && is.character(extra)) {
    wrong_colnames <- extra[!extra %in% colnames(data)]
    is_name_are_names <- dplyr::if_else(
      length(wrong_colnames) == 1L,
      "is not a column name",
      "are not column names"
    )
    cli::cli_abort(
      "{wrong_colnames} {is_name_are_names} of `data`."
    )
  }

  # Throw error if `extra` is a wrong number; i.e., if that given number is
  # larger than the actual number of extra columns:
  if (
    !is.infinite(extra) && is.numeric(extra) &&
    any(c(extra, length(extra)) > length(other_cols))
  ) {
    # Prepare error message:
    if (length(other_cols) == 0L) {
      msg_n_cols <- "there aren't any extra columns"
    } else if (length(other_cols) == 1L) {
      msg_n_cols <- "there's only 1 extra column"
    } else {
      msg_n_cols <- glue::glue(
        "there are only {length(other_cols)} extra columns"
      )
    }
    if (length(extra) == 1L) {
      cli::cli_abort(c(
      "`extra` was specified as `{extra}`, for extra column number {extra}.",
      "x" = "This number is too high -- {msg_n_cols} in `data`."
      ))
    } else {
      if (length(other_cols) < extra[1L]) {
        msg_num_length <- "These numbers are"
      } else {
        msg_num_length <- "That last number is"
      }
      cli::cli_abort(c(
      "`extra` was specified as `{rlang::enexprs(extra)}`.",
      "x" = "{msg_num_length} too high -- {msg_n_cols} in `data`."
      ))
    }
  }

  # Finally, make `other_cols` capture any and all extra columns, and return it:
  if (!is.infinite(extra) && length(other_cols) > 0L) {
    return(dplyr::select(other_cols, dplyr::all_of(extra)))
  } else {
    return(other_cols)
  }

}

