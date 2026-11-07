#' Summarize scrutiny objects
#'
#' @description `audit()` summarizes the output of scrutiny functions that test
#'   data frames, such as [`grim_map()`]. It is an S3 generic with methods for
#'   the functions listed below. The columns of each summary are described in
#'   the documentation of the respective function.
#'
#'   On the output of a sequence or total-n mapper, such as [`grim_map_seq()`],
#'   it returns one row per case; see [audit-special].
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
#'   | [`percentage_of_map()`]        | `"scrutiny_percentage_of_map"`      |
#'   | [`add_up_to_map()`]            | `"scrutiny_add_up_to_map"`          |
#'   | `*_map_seq()` functions        | `"scrutiny_map_seq"`                |
#'   | `*_map_total_n()` functions    | `"scrutiny_map_total_n"`            |
#'   | [`duplicate_count()`]          | `"scrutiny_dup_count"`              |
#'   | [`duplicate_count_colpair()`]  | `"scrutiny_dup_count_colpair"`      |
#'   | [`duplicate_tally()`]          | `"scrutiny_dup_tally"`              |
#'   | `audit()` on seq output        | `"scrutiny_audit_seq"`              |
#'   | `audit()` on total-n output    | `"scrutiny_audit_total_n"`          |
#'
#' @return A tibble with summary statistics.
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   audit()
#'
#' pigs4 |>
#'   duplicate_count() |>
#'   audit()
#'
#' # Summaries by group:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
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
  cols <- tryCatch(
    # A rename, as in `c(new = study)`, would be ignored; dplyr's `.by` rejects
    # it as well:
    tidyselect::eval_select(
      by,
      data,
      allow_rename = FALSE,
      error_call = rlang::caller_env()
    ),
    vctrs_error_subscript_oob = function(cnd) {
      # Sequence output has the tested columns and its results, but not the
      # other columns of the input. Total-n output keeps them:
      if (!is_map_seq_df(data)) {
        rlang::cnd_signal(cnd)
      }
      abort_in_export(
        "`.by` selects a column that `data` doesn't have.",
        "x" = "There is no column {wrap_in_backticks(cnd$i)}.",
        "i" = "Sequence output doesn't keep the other columns of the input. \\
        Its `case` column numbers the input rows."
      )
    }
  )
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
  if (
    inherits(data, "grouped_df") &&
      !is.null(attr(data, "scrutiny", exact = TRUE)$settings)
  ) {
    paste0(
      "`dplyr::group_by()` removed the class of the mapper output. ",
      "Use the output from before grouping, as in `",
      alternative,
      "`."
    )
  }
}


#' Classes of sequence and total-n mapper output
#'
#' @description [`audit()`] summarizes the output of functions made by
#'   [`function_map_seq()`], such as [`grim_map_seq()`], and of those made by
#'   [`function_map_total_n()`], such as [`grim_map_total_n()`], with one row
#'   per case. The columns of each summary are described in the documentation
#'   of the respective mapper.
#'
#'   Such output carries the generic class `"scrutiny_map_seq"` or
#'   `"scrutiny_map_total_n"`, which `audit()` dispatches on, and a class for
#'   the particular mapper, listed below, so that you can write your own S3
#'   methods for it. The summaries have the class `"scrutiny_audit_seq"` or
#'   `"scrutiny_audit_total_n"`, which `audit()` summarizes further.
#'
#' @name audit-special
#'
#' @section Sequence mappers:
#'   | \strong{Function}            | \strong{Class}                     |
#'   | ---                          | ---                                |
#'   | [`grim_map_seq()`]           | `"scrutiny_grim_map_seq"`          |
#'   | [`grimmer_map_seq()`]        | `"scrutiny_grimmer_map_seq"`       |
#'   | [`debit_map_seq()`]          | `"scrutiny_debit_map_seq"`         |
#'   | [`percentage_of_map_seq()`]  | `"scrutiny_percentage_of_map_seq"` |
#'
#' @section Total-n mappers:
#'   | \strong{Function}            | \strong{Class}                     |
#'   | ---                          | ---                                |
#'   | [`grim_map_total_n()`]       | `"scrutiny_grim_map_total_n"`      |
#'   | [`grimmer_map_total_n()`]    | `"scrutiny_grimmer_map_total_n"`   |
#'   | [`debit_map_total_n()`]      | `"scrutiny_debit_map_total_n"`     |
#'
#' @examples
#' out <- pigs1 |>
#'   grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   audit()
#' out
#'
#' # `audit()` summarizes the summary:
#' audit(out)
NULL


# The summaries that `audit()` returns for sequence and total-n mapper output,
# through `audit.scrutiny_map_seq()` and `audit.scrutiny_map_total_n()`.
audit_seq <- function(data) {
  if (!inherits(data, "scrutiny_map_seq")) {
    abort_in_export(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_seq()` function, \\
      such as `grim_map_seq()`."
    )
  }

  # The key result column is `"consistency"` unless the mapper was created with
  # a different `.name_key_result`, which `function_map_seq()` records:
  meta <- attr(data, "scrutiny", exact = TRUE)
  name_key_result <- meta$name_key_result
  if (is.null(name_key_result)) {
    name_key_result <- "consistency"
  }

  # Each is read below, and a missing `diff_var` would count zero hits
  # everywhere rather than fail:
  check_cols_needed(data, c("case", "var", "diff_var", name_key_result))

  var_names <- meta$settings$value[[match("var", meta$settings$setting)]]

  # With no inconsistent case, there is nothing to count hits in, but the
  # output has the columns of any other. The dispersed variables are read off
  # the attribute, since the `var` column has no values; output that has lost
  # the attribute gets no per-variable columns:
  if (nrow(data) == 0L) {
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

  # The dispersed variables, in the order of the `var` argument. The settings
  # record them all, even one whose dispersion `out_min` or `out_max` clipped
  # away in every case; the `var` column only has those with rows:
  if (is.null(var_names)) {
    var_names <- unique(data$var)
  }

  # The verdicts of the reported value sets, recorded by the sequence mapper's
  # initial test for every input row; `case` is the row number. Re-testing
  # reconstructed data instead would need the mapper and all its arguments:
  results_reported <- meta$results_reported
  if (is.null(results_reported)) {
    abort_lost_settings()
  }

  # Case numbers made strings or a factor, as for a plot, are numbers again,
  # which `sort()` and `split()` would otherwise order as 1, 10, 2:
  if (!is.numeric(data$case)) {
    case_number <- suppressWarnings(as.integer(as.character(data$case)))
    if (!anyNA(case_number)) {
      data$case <- case_number
    }
  }

  # One row per case, in the order of `case`, as `split()` below has it:
  data_rev <- reverse_map_seq(data)
  cases <- sort(unique(data$case))

  # By case number: a `case` column made a factor or strings, as for a plot,
  # would index by level code or by name. Anything that is no case number has
  # no record, which `rows_removed_by()` below reports:
  index_cases <- suppressWarnings(as.integer(as.character(cases)))
  consistency <- results_reported[index_cases]
  rows_by_case <- split(seq_len(nrow(data)), data$case)

  # A dispersed value set with an `NA` verdict may or may not be a hit, so a
  # count or a distance that depends on it is `NA`, as is everything about a
  # variable whose reported value is missing, which was not dispersed. A count
  # is the same with such value sets taken as hits or as misses only if there
  # are none; a distance, if no hit is found closer than the nearest of them:
  summarize_hits <- function(steps, verdicts) {
    hits <- steps[which(verdicts)]
    maybe <- steps[is.na(verdicts) | verdicts]
    distances <- list(
      function(x) min(abs(x)),
      function(x) min(x[x > 0]),
      function(x) max(x[x < 0])
    )
    diffs <- vapply(
      distances,
      function(f) {
        value <- suppressWarnings(f(hits))
        if (
          !identical(value, suppressWarnings(f(maybe))) || is.infinite(value)
        ) {
          NA_integer_
        } else {
          as.integer(value)
        }
      },
      integer(1L)
    )
    # The reported value set, included under `include_reported = TRUE`, is one
    # value set, however many variables it is listed under:
    count <- if (anyNA(verdicts)) NA_integer_ else length(hits)
    list(count = count, diffs = diffs)
  }

  # A variable whose rows were removed from a case, as by `filter(var == "x")`
  # or `filter(diff_var > -3)`, has unknown hits there, unlike one that
  # `out_min` or `out_max` left without any:
  is_removed <- rows_removed_by(meta$rows_dispersed, data$case, data$var)
  removed <- matrix(
    is_removed(
      rep(cases, times = length(var_names)),
      rep(var_names, each = length(cases))
    ),
    nrow = length(cases)
  )

  hits_total <- integer(length(cases))
  cols_hits <- matrix(NA_integer_, length(cases), length(var_names))
  cols_diff <- matrix(NA_integer_, length(cases), 3L * length(var_names))

  for (i in seq_along(cases)) {
    rows <- rows_by_case[[i]]
    verdicts_case <- data[[name_key_result]][rows]
    steps_case <- data$diff_var[rows]
    hits_case <- steps_case[which(verdicts_case)]
    total_unknown <- anyNA(verdicts_case)

    for (j in seq_along(var_names)) {
      var <- var_names[[j]]
      if (is.na(data_rev[[var]][[i]]) || removed[i, j]) {
        total_unknown <- TRUE
        next
      }
      in_var <- data$var[rows] == var
      summary <- summarize_hits(steps_case[in_var], verdicts_case[in_var])
      cols_hits[i, j] <- summary$count
      cols_diff[i, 3L * j - 2:0] <- summary$diffs
    }

    hits_total[[i]] <- if (total_unknown) {
      NA_integer_
    } else {
      sum(hits_case != 0L) + any(hits_case == 0L)
    }
  }

  colnames(cols_hits) <- paste0("hits_", var_names)
  colnames(cols_diff) <- paste0(
    "diff_",
    rep(var_names, each = 3L),
    c("", "_up", "_down")
  )
  cols_hits <- tibble::as_tibble(cols_hits)
  cols_diff <- tibble::as_tibble(cols_diff)

  data_rev |>
    dplyr::mutate("{name_key_result}" := consistency, hits_total) |>
    dplyr::bind_cols(cols_hits, cols_diff) |>
    add_class("scrutiny_audit_seq")
}


audit_total_n <- function(data) {
  if (!inherits(data, "scrutiny_map_total_n")) {
    abort_in_export(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_total_n()` function, \\
      such as `grim_map_total_n()`."
    )
  }

  check_cols_needed(data, c("n", "n_change", "case", "dir", "both_consistent"))

  df_list <- split(data, data$case)
  cases <- as.integer(names(df_list))

  df_list_hits <- df_list |>
    purrr::map(function(x) dplyr::filter(x, both_consistent))

  map_nrow_half <- function(x) {
    vapply(x, nrow, integer(1L), USE.NAMES = FALSE) / 2L
  }

  hits_forth <- df_list_hits |>
    purrr::map(function(x) dplyr::filter(x, dir == "forth")) |>
    map_nrow_half()

  hits_back <- df_list_hits |>
    purrr::map(function(x) dplyr::filter(x, dir == "back")) |>
    map_nrow_half()

  # An undecidable split, with `both_consistent` missing, might be a hit, so it
  # leaves the hits of its direction unknown, as in `audit_seq()`:
  has_na <- function(direction) {
    vapply(
      df_list,
      function(x) anyNA(x$both_consistent[x$dir == direction]),
      logical(1L),
      USE.NAMES = FALSE
    )
  }
  hits_forth[has_na("forth")] <- NA
  hits_back[has_na("back")] <- NA

  hits_total <- hits_forth + hits_back

  # Such a split is left out of the total of scenarios, which counts the
  # decided ones. A case with no decidable split has no hit rate:
  scenarios_total <- df_list |>
    purrr::map(function(x) dplyr::filter(x, !is.na(both_consistent))) |>
    map_nrow_half()
  hit_rate <- hits_total / scenarios_total
  hit_rate[scenarios_total == 0] <- NA

  # Rows removed from a direction, as by `filter(dir == "forth")` or
  # `audit(.by = dir)`, leave its hits unknown, and with them the totals:
  meta <- attr(data, "scrutiny", exact = TRUE)
  is_removed <- rows_removed_by(meta$rows_tested, data$case, data$dir)
  removed_forth <- is_removed(cases, "forth")
  removed_back <- is_removed(cases, "back")
  hits_forth[removed_forth] <- NA
  hits_back[removed_back] <- NA
  removed_any <- removed_forth | removed_back
  hits_total[removed_any] <- NA
  scenarios_total[removed_any] <- NA
  hit_rate[removed_any] <- NA

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
        # Not `as.integer()`, which turns a total past the integer range
        # into `NA`:
        .fns = as_integer_if_lossless
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


# The number of rows in each group of `case` and `other`, such as `var` or
# `dir`, keyed by both. Sequence and total-n mappers record it for their output:
count_rows_by <- function(case, other) {
  c(table(paste(case, other, sep = "\r")))
}

# A function telling whether rows of a group that `recorded` counts are missing
# from the data given by `case` and `other`. A group not recorded had no rows to
# begin with, and output without the record is taken as complete. Rows beyond
# the record, as from a join with other data, were never tested by the call
# whose record they would be read under, so they are an error:
rows_removed_by <- function(recorded, case, other) {
  present <- count_rows_by(case, other)
  if (!is.null(recorded)) {
    before <- unname(recorded[names(present)])
    if (any(is.na(before) | present > before)) {
      abort_in_export(
        "`data` has rows that the mapper call didn't return.",
        "x" = "Some cases have more rows than the call recorded, as after the \\
        output was bound to itself, joined with other data, or bound with \\
        `rbind()`.",
        "i" = "Rows added to mapper output would be read under its settings \\
        and results."
      )
    }
  }
  # Vectorized: looked up one group at a time, the names took quadratic time
  # in the number of cases:
  function(case, other) {
    key <- paste(case, other, sep = "\r")
    if (is.null(recorded)) {
      return(rep(FALSE, length(key)))
    }
    before <- unname(recorded[key])
    now <- unname(present[key])
    !is.na(before) & (is.na(now) | now < before)
  }
}
