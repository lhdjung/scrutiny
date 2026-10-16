#' Explain mapper results row by row
#'
#' @description `explain()` takes the output of [`grim_map()`],
#'   [`grimmer_map()`], or [`debit_map()`] and lays out the calculation behind
#'   each row's verdict in plain words, as [`grim_explain()`],
#'   [`grimmer_explain()`], and [`debit_explain()`] do for a single case. This
#'   includes the output of the sequence and total-n mappers built on them,
#'   such as [`grim_map_seq()`].
#'
#'   Every row is explained, so narrow down the data first, e.g. with
#'   `dplyr::filter(!consistency)` or `dplyr::slice()`.
#'
#' @details Each row is tested again from its own values and the settings of
#'   the mapper call, such as `rounding` and `percent`. If the verdict differs
#'   from the row's `consistency` value, the row must have been changed after
#'   testing, and `explain()` throws an error.
#'
#'   The generic is [`dplyr::explain()`], so loading both packages masks
#'   nothing.
#'
#' @param x Data frame returned by one of the mappers named above.
#' @param ... Not used.
#'
#' @return A list of `scrutiny_explain` objects, one per row, that prints as
#'   the explanations. See [`grim_explain()`] for the elements.
#'
#' @name explain
#' @aliases explain.scrutiny_grim_map explain.scrutiny_grimmer_map
#'   explain.scrutiny_debit_map
#'
#' @importFrom dplyr explain
#' @export explain
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   dplyr::filter(!consistency) |>
#'   explain()
NULL


#' @include grim-explain.R grimmer-explain.R debit-explain.R
#' @export
explain.scrutiny_grim_map <- function(x, ...) {
  explain_rows(x, grim_explain, c("x", "n", "digits_x"), "items")
}

#' @export
explain.scrutiny_grimmer_map <- function(x, ...) {
  explain_rows(
    x,
    grimmer_explain,
    c("x", "sd", "n", "digits_x", "digits_sd"),
    "items"
  )
}

#' @export
explain.scrutiny_debit_map <- function(x, ...) {
  formula <- scrutiny_meta(x)$args$formula
  if (identical(formula, "mean_n")) {
    abort_in_export(
      "`explain()` only covers DEBIT's default `formula = \"exact\"`.",
      "x" = "`x` was tested with `formula = \"mean_n\"`."
    )
  }
  explain_rows(x, debit_explain, c("x", "sd", "n", "digits_x", "digits_sd"))
}


# Calls `fun`, an `*_explain()` function, on each row of `data`: the key
# columns as values, plus the settings recorded by the mapper. A helper column
# like `items` is used if `data` has it.
explain_rows <- function(data, fun, cols_key, cols_helper = character()) {
  check_cols_needed(data, c(cols_key, "consistency"), name_data = "x")

  args <- scrutiny_meta(data)$args
  if (is.null(args)) {
    abort_in_export(
      "`x` has lost the settings of the test that produced it.",
      "x" = "Some dplyr verbs, such as `summarise()`, drop them.",
      "i" = "Call `explain()` on the mapper's output directly, or after \\
      verbs like `filter()`, `slice()`, and `arrange()`."
    )
  }
  # The ones that `fun` can take: not `show_rec`, say.
  args <- args[intersect(names(args), names(formals(fun)))]

  cols <- c(cols_key, intersect(cols_helper, colnames(data)))

  out <- lapply(seq_len(nrow(data)), function(i) {
    values <- lapply(data[cols], function(col) col[[i]])
    explanation <- do.call(fun, c(values, args))
    if (!identical(explanation$consistency, data$consistency[[i]])) {
      abort_in_export(
        "Row {i} doesn't match its own `consistency` value.",
        "x" = "It says {data$consistency[[i]]}, but testing the row's values \\
        again gives {explanation$consistency}.",
        "i" = "Were the values changed after testing? If so, test again."
      )
    }
    explanation
  })

  structure(out, class = "scrutiny_explain_list")
}


#' @export
print.scrutiny_explain_list <- function(x, ...) {
  if (length(x) == 0L) {
    cat("No rows to explain.\n")
  }
  for (i in seq_along(x)) {
    if (i > 1L) {
      cat("\n\n")
    }
    print(x[[i]])
  }
  invisible(x)
}
