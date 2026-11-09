# Development tools: run with `devtools::load_all()` and paste the output into
# a mapper's roxygen2 block. Not exported, since they serve no analysis.

#' Documentation template for [`audit()`]
#'
#' @description `write_doc_audit()` generates the roxygen2 section that
#'   documents an [`audit()`] method on the page of its mapper function, as
#'   in [`grim_map()`]. Paste the output into the mapper's roxygen2 block. To
#'   keep the list structure when indenting with `Ctrl`+`Shift`+`/`, leave
#'   empty lines around the pasted text.
#'
#' @param sample_output Data frame. The result of `audit()` on the mapper's
#'   output, such as `audit(grim_map(pigs1, digits_x = 2, items_averaged = 1,
#'   data_type = "integer"))`.
#' @param name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`.
#'
#' @noRd
#'
#' @return A string vector formatted by [`glue::glue()`].
#'
#' @examples
#' out_grim <- pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   audit()
#'
#' write_doc_audit(sample_output = out_grim, name_test = "GRIM")

write_doc_audit <- function(sample_output, name_test) {
  check_class(sample_output, "data.frame")
  check_class(sample_output, "tbl_df")
  check_length(name_test, 1L)

  if (length(sample_output) < 3L) {
    abort_in_export(
      "!" = "`sample_output` must be the output of `audit()` applied \\
      to a scrutiny-style mapper function, such as `grim_map()`.",
      "i" = "(These outputs always have at least three columns.)",
      "i" = "Create it like this:",
      "pigs1 |>
           grim_map(digits_x = 2, items_averaged = 1, data_type = \"integer\") |>
           audit() |>
           write_doc_audit(name_test = \"GRIM\")"
    )
  }

  output_name <- colnames(sample_output)
  output_number <- seq_along(output_name)

  # The columns that `audit_cols_minimal()` returns, plus `testable_cases`,
  # which the GRIM method inserts after `all_cases`. Any other column gets an
  # empty description to fill in:
  output_text_known <- c(
    all_cases = "total number of value sets.",
    testable_cases = "number of value sets with a positive `exp_incon`.",
    incon_cases = glue::glue("number of {name_test}-inconsistent value sets."),
    emp_incon = glue::glue("proportion of {name_test}-inconsistent value sets.")
  )
  output_text <- output_text_known[output_name]
  output_text[is.na(output_text)] <- ""

  output_name_expected <- c("all_cases", "incon_cases", "emp_incon")
  output_name_missing <- setdiff(output_name_expected, output_name)

  if (length(output_name_missing) > 0L) {
    abort_in_export(
      "!" = "The output must have the columns \\
      {wrap_in_backticks(output_name_expected)}.",
      "x" = "It lacks {wrap_in_backticks(output_name_missing)}."
    )
  }

  name_test_lower <- tolower(name_test)

  if (nrow(sample_output) == 1L) {
    msg_nrow <- "one row"
  } else {
    msg_nrow <- paste(nrow(sample_output), "rows")
  }

  # One element per line; `glue()` trims a trailing newline, so the lines are
  # not joined here but printed one below the other:
  intro <- c(
    glue::glue(
      "#' @section Summaries with [`audit()`]: `audit()` on \\
      `{name_test_lower}_map()` output"
    ),
    glue::glue("#'   returns a tibble with {msg_nrow} and these columns:"),
    "#'"
  )

  lines_all <- glue::glue("#' {output_number}. `{output_name}`: {output_text}")

  glue::as_glue(c(intro, lines_all))
}


#' Documentation template for the `audit()` summary of sequence output
#'
#' @description `write_doc_audit_seq()` generates the roxygen2 section that
#'   documents what `audit()` returns on the page of a function made by
#'   [`function_map_seq()`]. Paste the output into that function's roxygen2
#'   block. To keep the list structure when indenting with
#'   `Ctrl`+`Shift`+`/`, leave empty lines around the pasted text.
#'
#' @param key_args String. Names of the key columns, in the order of the
#'   function's output.
#' @param name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`.
#' @param helpers String. Names of the helper columns, such as
#'   `c("items_averaged", "data_type")`. Default is none.
#'
#' @include utils.R
#'
#' @noRd
#'
#' @return A string vector formatted by [`glue::glue()`].
#'
#' @seealso `write_doc_audit_total_n()` and
#'   `vignette("consistency-tests-in-depth")`.
#'
#' @examples
#' write_doc_audit_seq(
#'   key_args = c("x", "n"),
#'   name_test = "GRIM",
#'   helpers = c("items_averaged", "data_type")
#' )
#'
#' write_doc_audit_seq(
#'   key_args = c("x", "sd", "n"),
#'   name_test = "DEBIT",
#'   helpers = "data_type"
#' )

write_doc_audit_seq <- function(key_args, name_test, helpers = character()) {
  check_length(name_test, 1L)

  key_args_bt <- wrap_in_backticks(key_args)
  arg1 <- key_args[1L]
  hits <- paste(wrap_in_backticks(paste0("hits_", key_args)), collapse = ", ")
  vars <- commas_and(key_args_bt)
  helpers_bt <- if (length(helpers) > 0L) wrap_in_backticks(helpers)
  with_cols <- commas_and(c(helpers_bt, "their `consistency`"))
  respectively <- if (length(key_args) > 1L) ", respectively" else ""

  # One line per further variable, with its three columns spelled out:
  diff_others <- vapply(
    key_args[-1L],
    function(var) {
      cols <- paste0("diff_", var, c("", "_up", "_down"))
      paste0(
        "#'   - ",
        paste(wrap_in_backticks(cols), collapse = ", "),
        ": the same for `",
        var,
        "`.\n"
      )
    },
    character(1L)
  )
  diff_lines <- paste(diff_others, collapse = "")

  glue::glue(
    "#' @section Summaries with [`audit()`]: `audit()` on\n",
    "#'   `{tolower(name_test)}_map_seq()` output returns a tibble with one row per case and\n",
    "#'   these columns:\n",
    "#'   - {vars}: the reported values, with {with_cols}.\n",
    "#'   - `hits_total`: the number of {name_test}-consistent value sets found within\n",
    "#'   the `dispersion` range.\n",
    "#'   - {hits}: the number found by varying {vars}{respectively}. If both\n",
    "#'   `include_reported` and `include_consistent` are `TRUE`, a consistent\n",
    "#'   reported value set is counted as well: once per variable here, and\n",
    "#'   once in `hits_total`.\n",
    "#'   - `diff_{arg1}`: the distance from `{arg1}` to the nearest consistent dispersed\n",
    "#'   value, in dispersion steps. `diff_{arg1}_up` and `diff_{arg1}_down` are the\n",
    "#'   signed steps to the nearest consistent value above and below (`_down` is\n",
    "#'   negative).\n",
    "{diff_lines}",
    "#'\n",
    "#'   A count or distance is `NA` if it depends on a dispersed value set whose\n",
    "#'   consistency is `NA`, on a variable whose reported value is missing, which\n",
    "#'   is not dispersed, or on rows removed from the output, as by `filter()`.\n",
    "#'\n",
    "#'   Call `audit()` again on that summary to summarize these columns further.\n",
    "#'   `na_count` and `na_rate` there count the cases without a consistent value\n",
    "#'   within the `dispersion` range, and those that a missing value leaves\n",
    "#'   unknown.\n"
  )
}


#' Documentation template for the `audit()` summary of total-n output
#'
#' @description `write_doc_audit_total_n()` generates the roxygen2 section that
#'   documents what `audit()` returns on the page of a function made by
#'   [`function_map_total_n()`]. Paste the output into that function's
#'   roxygen2 block. To keep the list structure when indenting with
#'   `Ctrl`+`Shift`+`/`, leave empty lines around the pasted text.
#'
#' @param key_args String. Names of the key columns, without the `1` and `2`
#'   suffixes, in the order of the function's output, ending in `"n"`.
#' @param name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`.
#'
#' @noRd
#'
#' @return A string vector formatted by [`glue::glue()`].
#'
#' @seealso `write_doc_audit_seq()` and
#'   `vignette("consistency-tests-in-depth")`.
#'
#' @examples
#' write_doc_audit_total_n(key_args = c("x", "n"), name_test = "GRIM")
#'
#' write_doc_audit_total_n(key_args = c("x", "sd", "n"), name_test = "DEBIT")

write_doc_audit_total_n <- function(key_args, name_test) {
  # Checks ---

  check_length(name_test, 1L)

  if (!length(key_args) > 1L) {
    abort_in_export(
      "`key_args` must have length > 1.",
      "x" = "It has length {length(key_args)}.",
      "i" = "Consistency testing requires at least two values."
    )
  }

  if (key_args[length(key_args)] != "n") {
    abort_in_export(
      "!" = "`\"n\"` must be the last value in `key_args`.",
      "x" = "It is missing from `key_args`."
    )
  }

  # Main part ---

  key_args_num <- key_args |>
    utils::head(-1L) |>
    rep(each = 2) |>
    paste0(c("1", "2")) |>
    wrap_in_backticks()

  key_args_num1 <- commas_and(key_args_num[endsWith(key_args_num, "1`")])
  key_args_num2 <- commas_and(key_args_num[endsWith(key_args_num, "2`")])
  key_args_all <- commas_and(append(key_args_num, "`n`"))

  glue::glue(
    "#' @section Summaries with [`audit()`]: `audit()` on\n",
    "#'   `{tolower(name_test)}_map_total_n()` output returns a tibble with one row per case and\n",
    "#'   these columns:\n",
    "#'   - {key_args_all}: the reported values.\n",
    "#'   - `hits_total`: the number of splits in which both groups are\n",
    "#'   {name_test}-consistent, summing `hits_forth` and `hits_back`.\n",
    "#'   - `hits_forth`: the number of such splits with {key_args_num2} paired with\n",
    "#'   the larger group.\n",
    "#'   - `hits_back`: the same with {key_args_num1} paired with the larger group.\n",
    "#'   - `scenarios_total`: the number of splits tested, leaving out those that\n",
    "#'   could not be decided.\n",
    "#'   - `hit_rate`: `hits_total / scenarios_total`, or `NA` if no split could\n",
    "#'   be decided.\n",
    "#'\n",
    "#'   A count is `NA` if it depends on a split that could not be decided, or on\n",
    "#'   rows removed from the output, as by `filter()`.\n",
    "#'\n",
    "#'   Call `audit()` again on that summary to summarize these columns\n",
    "#'   further.\n"
  )
}
