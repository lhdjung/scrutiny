#' Explain mapper results row by row
#'
#' @description `explain_map()` takes the output of [`grim_map()`],
#'   [`grimmer_map()`], or [`debit_map()`] and adds an `explanation` column: the
#'   calculation behind each row's verdict in plain words, as printed by
#'   [`grim_explain()`], [`grimmer_explain()`], and [`debit_explain()`] for a
#'   single case. This includes the output of the sequence and total-n mappers
#'   built on them, such as [`grim_map_seq()`].
#'
#'   To read the explanations, use [`explain_print()`].
#'
#' @details Each row is tested again from its own values and the settings of
#'   the mapper call, such as `rounding` and `percent`. If the verdict differs
#'   from the row's `consistency` value, the row must have been changed after
#'   testing, and `explain_map()` throws an error.
#'
#' @param data Data frame returned by one of the mappers named above.
#'
#' @return `data` with an `explanation` column, a string per row.
#'
#' @include grim-explain.R grimmer-explain.R debit-explain.R
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   dplyr::filter(!consistency) |>
#'   explain_map()
explain_map <- function(data) {
  UseMethod("explain_map")
}


#' @export
explain_map.default <- function(data) {
  abort_in_export(
    "`data` must be the output of `grim_map()`, `grimmer_map()`, or \\
    `debit_map()`.",
    "x" = "It has class {.cls {class(data)}}.",
    "i" = "Their sequence and total-n mappers, such as `grim_map_seq()`, \\
    work as well."
  )
}


#' @export
explain_map.scrutiny_grim_map <- function(data) {
  add_explanation_col(data, grim_explain, c("x", "n", "digits_x"), "items")
}

#' @export
explain_map.scrutiny_grimmer_map <- function(data) {
  add_explanation_col(
    data,
    grimmer_explain,
    c("x", "sd", "n", "digits_x", "digits_sd"),
    "items"
  )
}

#' @export
explain_map.scrutiny_debit_map <- function(data) {
  formula <- scrutiny_meta(data)$args$formula
  if (identical(formula, "mean_n")) {
    abort_in_export(
      "`explain_map()` only covers DEBIT's default `formula = \"exact\"`.",
      "x" = "`data` was tested with `formula = \"mean_n\"`."
    )
  }
  add_explanation_col(data, debit_explain, c("x", "sd", "n", "digits_x", "digits_sd"))
}


# Calls `fun`, an `*_explain()` function, on each row of `data`: the key columns
# as values, plus the settings recorded by the mapper. A helper column like
# `items` is used if `data` has it.
add_explanation_col <- function(data, fun, cols_key, cols_helper = character()) {
  check_cols_needed(data, c(cols_key, "consistency"))

  args <- scrutiny_meta(data)$args
  if (is.null(args)) {
    abort_in_export(
      "`data` has lost the settings of the test that produced it.",
      "x" = "Some dplyr verbs, such as `summarise()`, drop them.",
      "i" = "Call `explain_map()` on the mapper's output directly, or after \\
      verbs like `filter()`, `slice()`, and `arrange()`."
    )
  }
  # The ones that `fun` can take: not `show_rec`, say.
  args <- args[intersect(names(args), names(formals(fun)))]

  cols <- c(cols_key, intersect(cols_helper, colnames(data)))

  # Plain vectors rather than the tibble, whose `[` is slow to call per row:
  columns <- as.list(data[cols])

  data$explanation <- data |>
    nrow() |>
    seq_len() |>
    vapply(
      explain_row,
      character(1L),
      columns = columns,
      consistency = data$consistency,
      fun = fun,
      args = args
    )
  data
}


explain_row <- function(i, columns, consistency, fun, args) {
  explanation <- columns |>
    lapply(`[[`, i) |>
    c(args) |>
    do.call(what = fun)

  if (!identical(explanation$consistency, consistency[[i]])) {
    abort_in_export(
      "The row with {explanation$inputs} doesn't match its own \\
      `consistency` value.",
      "x" = "It says {consistency[[i]]}, but testing the row's values \\
      again gives {explanation$consistency}.",
      "i" = "Were the values changed after testing? If so, test again."
    )
  }

  format(explanation)
}
