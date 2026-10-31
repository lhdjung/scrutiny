#' Explain mapper results row by row
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `explain_map()` takes the output of [`grim_map()`],
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
#'   DEBIT explanations cover `formula = "exact"` only.
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
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   dplyr::filter(!consistency) |>
#'   explain_map()
explain_map <- function(data) {
  UseMethod("explain_map")
}


#' @export
explain_map.default <- function(data) {
  hint <- hint_grouped(data, "explain_map(data)")
  if (is.null(hint)) {
    hint <- "Their sequence and total-n mappers, such as `grim_map_seq()`, \\
    work as well."
  }
  abort_in_export(
    "`data` must be the output of `grim_map()`, `grimmer_map()`, or \\
    `debit_map()`.",
    "x" = "It has class {.cls {class(data)}}.",
    "i" = hint
  )
}


#' @export
explain_map.scrutiny_grim_map <- function(data) {
  add_explanation_col(
    data,
    grim_explain,
    c("x", "n", "digits_x"),
    "items_averaged"
  )
}

#' @export
explain_map.scrutiny_grimmer_map <- function(data) {
  add_explanation_col(
    data,
    grimmer_explain,
    c("x", "sd", "n", "digits_x", "digits_sd"),
    "items_averaged"
  )
}

#' @export
explain_map.scrutiny_debit_map <- function(data) {
  # `debit_explain()` checks this too, but would blame a `formula` that the
  # user never passed here:
  formula <- scrutiny_args(data)$formula
  if (identical(formula, "mean_n")) {
    abort_in_export(
      "Explanations only cover DEBIT's default `formula = \"exact\"`.",
      "x" = "`data` was tested with `formula = \"mean_n\"`."
    )
  }
  add_explanation_col(
    data,
    debit_explain,
    c("x", "sd", "n", "digits_x", "digits_sd")
  )
}


# Calls `fn`, an `*_explain()` function, on each row of `data`: the key columns
# as values, plus the settings recorded by the mapper. A helper column like
# `items_averaged` is used if `data` has it.
add_explanation_col <- function(
  data,
  fn,
  cols_key,
  cols_helper = character()
) {
  check_cols_needed(data, c(cols_key, "consistency"))

  args <- scrutiny_args(data)
  if (is.null(args)) {
    abort_in_export(
      "`data` has lost the settings of the test that produced it.",
      "x" = "Some dplyr verbs, such as `summarise()`, drop them.",
      "i" = "Call `explain_map()` on the mapper's output directly, or after \\
      verbs like `filter()`, `slice()`, and `arrange()`."
    )
  }
  # The ones that `fn` can take: not `show_rec`, say.
  args <- args[intersect(names(args), names(formals(fn)))]

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
      fn = fn,
      args = args
    )
  data
}


explain_row <- function(i, columns, consistency, fn, args) {
  explanation <- columns |>
    lapply(`[[`, i) |>
    c(args) |>
    do.call(what = fn)

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
