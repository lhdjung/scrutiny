#' Summarize scrutiny objects
#'
#' @description `audit()` summarizes the output of scrutiny functions that test
#'   data frames, such as [`grim_map()`]. It is an S3 generic with methods for
#'   the functions listed below. The columns of each summary are described in
#'   the documentation of the respective function.
#'
#'   On the output of a sequence or total-n mapper, such as [`grim_map_seq()`],
#'   it calls [`audit_seq()`] or [`audit_total_n()`].
#'
#' @param data A data frame returned by one of the functions below.
#' @param ... These dots must be empty.
#' @param .by Optionally, columns of `data` to summarize by, selected as in
#'   [`dplyr::select()`], such as `.by = study`. Each group is summarized on
#'   its own, and the output has a row for each, starting with the grouping
#'   columns. Groups appear in the order of their first row. Use this rather
#'   than [`dplyr::group_by()`], which removes the class that `audit()`
#'   dispatches on.
#'
#' @section Run before `audit()`:
#'   | \strong{Function}              | \strong{Class}                      |
#'   | ---                            | ---                                 |
#'   | [`grim_map()`]                 | `"scrutiny_grim_map"`               |
#'   | [`grimmer_map()`]              | `"scrutiny_grimmer_map"`            |
#'   | [`debit_map()`]                | `"scrutiny_debit_map"`              |
#'   | [`is_percentage_of_map()`]     | `"scrutiny_is_percentage_of_map"`   |
#'   | [`add_up_to_map()`]            | `"scrutiny_add_up_to_map"`          |
#'   | `*_map_seq()` functions        | `"scrutiny_map_seq"`                |
#'   | `*_map_total_n()` functions    | `"scrutiny_map_total_n"`            |
#'   | [`duplicate_count()`]          | `"scrutiny_dup_count"`              |
#'   | [`duplicate_count_colpair()`]  | `"scrutiny_dup_count_colpair"`      |
#'   | [`duplicate_tally()`]          | `"scrutiny_dup_tally"`              |
#'   | [`audit_seq()`]                | `"scrutiny_audit_seq"`              |
#'   | [`audit_total_n()`]            | `"scrutiny_audit_total_n"`          |
#'
#' @return A tibble with summary statistics.
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   audit()
#'
#' pigs4 |>
#'   duplicate_count() |>
#'   audit()
#'
#' # Summaries by group:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   dplyr::mutate(study = rep(c("A", "B", "C"), each = 4)) |>
#'   audit(.by = study)

audit <- function(data, ..., .by = NULL) {
  rlang::check_dots_empty()
  if (!missing(.by)) {
    return(audit_by(data, rlang::enquo(.by)))
  }
  UseMethod("audit")
}


# `audit()` on each group of rows on its own. `[` keeps the class and the
# "scrutiny" attribute that `audit()` and its methods need, which
# `dplyr::group_by()` would not:
audit_by <- function(data, by) {
  cols <- tidyselect::eval_select(by, data, error_call = rlang::caller_env())
  if (length(cols) == 0L) {
    return(audit(data))
  }

  # Plain columns, so that the "scrutiny" attribute doesn't reach the summary:
  keys <- tibble::new_tibble(unclass(data)[cols], nrow = nrow(data))
  groups <- vctrs::vec_group_loc(keys)
  results <- if (nrow(groups) == 0L) {
    list(audit(data)[0L, ])
  } else {
    lapply(groups$loc, function(rows) audit(data[rows, ]))
  }

  names_taken <- intersect(names(cols), colnames(results[[1L]]))
  if (length(names_taken) > 0L) {
    abort_in_export(
      "`.by` selects {cli::qty(length(names_taken))}{?a column/columns} that \\
      the summary has as well.",
      "x" = "{wrap_in_backticks(names_taken)} would appear twice.",
      "i" = "Rename {cli::qty(length(names_taken))}{?it/them} in `data` first."
    )
  }

  nrow_results <- vapply(results, nrow, integer(1L), USE.NAMES = FALSE)
  keys <- groups$key[rep(seq_len(nrow(groups)), nrow_results), ]
  tibble::as_tibble(dplyr::bind_cols(keys, dplyr::bind_rows(results)))
}


# Without it, the generic would fail with R's bare "no applicable method":
#' @export
audit.default <- function(data, ...) {
  hint <- hint_grouped(data, "audit(data, .by = study)")
  if (is.null(hint)) {
    hint <- "See `?audit` for the functions whose output it summarizes."
  }
  abort_in_export(
    "`audit()` has no method for `data`.",
    "x" = "It has class {.cls {class(data)}}.",
    "i" = hint
  )
}


# A mapper's output that `dplyr::group_by()` went through still has the
# "scrutiny" attribute, but not the class, which no method can dispatch on.
# `dplyr::ungroup()` drops the attribute as well, so only grouped output can be
# recognized:
hint_grouped <- function(data, alternative) {
  if (inherits(data, "grouped_df") && !is.null(attr(data, "scrutiny", exact = TRUE))) {
    paste0(
      "`dplyr::group_by()` removed the class of the mapper output. ",
      "Use the output from before grouping, as in `", alternative, "`."
    )
  }
}


#' Summarize output of sequence mappers and total-n mappers
#'
#' @description `audit_seq()` summarizes the output of functions made by
#'   [`function_map_seq()`], such as [`grim_map_seq()`], and `audit_total_n()`
#'   that of functions made by [`function_map_total_n()`], such as
#'   [`grim_map_total_n()`]. The columns of each summary are described in the
#'   documentation of the respective function. [`audit()`] calls them on such
#'   output, so it can be used instead.
#'
#' @param data A data frame returned by one of the functions below.
#'
#' @name audit-special
#'
#' @section Before `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}                   |
#'   | ---                          | ---                              |
#'   | [`grim_map_seq()`]           | `"scrutiny_grim_map_seq"`        |
#'   | [`grimmer_map_seq()`]        | `"scrutiny_grimmer_map_seq"`     |
#'   | [`debit_map_seq()`]          | `"scrutiny_debit_map_seq"`       |
#'   | [`is_percentage_of_map_seq()`] | `"scrutiny_is_percentage_of_map_seq"` |
#'
#' @section Before `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}                   |
#'   | ---                          | ---                              |
#'   | [`grim_map_total_n()`]       | `"scrutiny_grim_map_total_n"`    |
#'   | [`grimmer_map_total_n()`]    | `"scrutiny_grimmer_map_total_n"` |
#'   | [`debit_map_total_n()`]      | `"scrutiny_debit_map_total_n"`   |
#'
#' @return A tibble with summary statistics.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' out <- pigs1 |>
#'   grim_map_seq(digits_x = 2, items_averaged = 1) |>
#'   audit_seq()
#' out
#'
#' # `audit()` summarizes the summary:
#' audit(out)

audit_seq <- function(data) {
  if (!inherits(data, "scrutiny_map_seq")) {
    abort_in_export(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_seq()` function, \\
      such as `grim_map_seq()`."
    )
  }

  check_dispersion_linear(data, "audit_seq")

  # `function_map_seq()` records the name of the key result column, which is
  # `"consistency"` unless the mapper was created with a different
  # `.name_key_result`. Output that has lost the attribute falls back to that,
  # and every mapper that keeps the default name is unaffected:
  meta <- attr(data, "scrutiny", exact = TRUE)
  name_key_result <- meta$name_key_result
  if (is.null(name_key_result)) {
    name_key_result <- "consistency"
  }

  # With no inconsistent case, there is nothing to count hits in, but the
  # output has the columns of any other. The dispersed variables are read off
  # the attribute, since the `var` column has no values; output that has lost
  # the attribute gets no per-variable columns:
  if (nrow(data) == 0L) {
    var_names <- meta$settings$value[[match("var", meta$settings$setting)]]
    out <- reverse_map_seq(data)
    out[[name_key_result]] <- logical(0L)
    out$hits_total <- integer(0L)
    names_var_cols <- c(
      paste0("hits_", var_names),
      paste0("diff_", rep(var_names, each = 3L), c("", "_up", "_down"))
    )
    for (name in names_var_cols) {
      out[[name]] <- integer(0L)
    }
    return(add_class(out, "scrutiny_audit_seq"))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list |>
    purrr::map(function(x) x[which(x[[name_key_result]]), ])

  # The reported value set itself, included under `include_reported = TRUE`, is
  # one value set, however many variables it is listed under:
  hits_total <- df_list_hits |>
    vapply(
      function(x) sum(x$diff_var != 0L) + any(x$diff_var == 0L),
      integer(1L),
      USE.NAMES = FALSE
    ) |>
    unname()

  # The dispersed variables, in the order of the `var` argument. The attribute
  # records them all, even one whose dispersion `out_min` or `out_max` clipped
  # away in every case; the `var` column only has those with rows:
  var_names <- meta$var
  if (is.null(var_names)) {
    var_names <- unique(data$var)
  }

  # Define some helper functions to be mapped below:
  index_hit_distance <- function(df, var_order = var_names) {
    # Splitting by a factor keeps the groups in the order of `var` rather than
    # the alphabetical one, and it keeps an empty group for a variable that has
    # no rows in this case because `out_min` or `out_max` clipped its whole
    # dispersion. Such a group counts as zero hits, like one whose rows are all
    # inconsistent. Without it, the columns of later variables shifted left:
    df |>
      split(factor(df$var, levels = var_order)) |>
      # `which()` for the same reason as in the `dplyr::filter()` call above,
      # which drops undecidable cases rather than counting them as hits:
      purrr::map(function(x) x[which(x[[name_key_result]]), ]$diff_var)
  }

  length_unless_na <- function(x) {
    if (length(x) == 1L && is.na(x)) {
      0L
    } else {
      length(x)
    }
  }

  # Prepare endings of the `diff_*` columns:
  fun_names <- c("", "_up", "_down")
  fun_names <- rep(fun_names, length(var_names))

  df_nested <- df_list |>
    purrr::map(index_hit_distance) |>
    tibble::tibble(.name_repair = function(x) "distance") |>
    tidyr::unnest_wider(col = distance)

  cols_hits <- df_nested |>
    dplyr::mutate(
      dplyr::across(
        .cols = everything(),
        .fns = function(x) {
          vapply(x, length_unless_na, integer(1L), USE.NAMES = FALSE)
        },
        .names = "hits_{.col}"
      ),
      .keep = "none"
    ) |>
    tidyr::unnest(cols = everything())

  cols_diff <- df_nested |>
    dplyr::mutate(
      dplyr::across(
        .cols = everything(),
        .fns = LIST_MIN_DISTANCE_FUNCTIONS,
        .names = "diff_{.col}{fun_names}"
      ),
      .keep = "none"
    ) |>
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = function(x) {
        x[is.infinite(x)] <- NA
        as.integer(x)
      }
    )) |>
    suppressWarnings()

  data_rev <- reverse_map_seq(data)

  # The verdicts of the reported value sets come from the sequence mapper's
  # initial test, recorded on its output for every row of the input data; `case`
  # is the row number. Reconstructed data used to be tested again here, which
  # meant finding the mapper by the name in a class string and replaying the
  # call's arguments, and a dropped argument could flip `consistency`:
  results_reported <- meta$results_reported
  if (is.null(results_reported)) {
    abort_in_export(
      "`data` has lost the settings of the test that produced it.",
      "x" = "Some dplyr verbs, such as `summarise()`, drop them.",
      "i" = "Call `audit_seq()` on the mapper's output directly, or after \\
      verbs like `filter()`, `slice()`, and `arrange()`."
    )
  }

  # `df_list` and `data_rev` are both split by `case`, so they are in this order:
  consistency <- results_reported[sort(unique(data$case))]

  cols_hits <- cols_hits |>
    dplyr::mutate(dplyr::across(
      .cols = where(is.character),
      .fns = as.numeric
    ))

  cols_diff <- cols_diff |>
    dplyr::mutate(dplyr::across(
      .cols = where(is.character),
      .fns = as.numeric
    ))

  data_rev |>
    dplyr::mutate("{name_key_result}" := consistency, hits_total) |>
    dplyr::bind_cols(cols_hits, cols_diff) |>
    add_class("scrutiny_audit_seq")
}


#' @rdname audit-special
#' @export

audit_total_n <- function(data) {
  if (!inherits(data, "scrutiny_map_total_n")) {
    abort_in_export(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_total_n()` function, \\
      such as `grim_map_total_n()`."
    )
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list |>
    purrr::map(dplyr::filter, both_consistent)

  map_nrow_half <- function(x) {
    vapply(x, nrow, integer(1L), USE.NAMES = FALSE) / 2L
  }

  hits_forth <- df_list_hits |>
    purrr::map(dplyr::filter, dir == "forth") |>
    map_nrow_half()

  hits_back <- df_list_hits |>
    purrr::map(dplyr::filter, dir == "back") |>
    map_nrow_half()

  hits_total <- hits_forth + hits_back

  # An undecidable split, with `both_consistent` missing, is neither a hit nor
  # a miss, so it is left out of the total, as `audit_seq()` leaves out
  # undecidable cases. A case with no decidable split has no hit rate:
  scenarios_total <- df_list |>
    purrr::map(dplyr::filter, !is.na(both_consistent)) |>
    map_nrow_half()
  hit_rate <- hits_total / scenarios_total
  hit_rate[scenarios_total == 0] <- NA

  data |>
    reverse_map_total_n() |>
    dplyr::mutate(
      hits_total,
      hits_forth,
      hits_back,
      scenarios_total,
      hit_rate,
      dplyr::across(
        .cols = c("n", starts_with("hits"), "scenarios_total"),
        .fns = as.integer
      )
    ) |>
    add_class("scrutiny_audit_total_n")
}


# The tier classes come first in the class vector of sequence and total-n
# output, ahead of the basic mapper's class, so `audit()` reaches these methods
# rather than counting the dispersed rows as reported cases:

#' @export
audit.scrutiny_map_seq <- function(data, ...) {
  audit_seq(data)
}

#' @export
audit.scrutiny_map_total_n <- function(data, ...) {
  audit_total_n(data)
}
