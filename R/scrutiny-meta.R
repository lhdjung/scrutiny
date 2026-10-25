#' Read the settings a mapper recorded on its output
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Every mapper, such as [`grim_map()`], records the settings of
#'   the call that produced its output in a list attribute named `"scrutiny"`.
#'   `scrutiny_meta()` returns that list, so that a report can state under
#'   which rounding method or SD convention a verdict was reached. Classes are
#'   for method dispatch only and carry no settings.
#'
#'   Every mapper's list has one element:
#'
#' - `args`: the arguments that applied to the whole call, with defaults
#'   resolved, such as `rounding`, `percent`, or `sd_type`. Arguments that may
#'   vary by row are columns of the output instead: `digits_x`, `digits_sd`,
#'   and helper arguments such as `items_averaged`, even if given as a single
#'   value.
#'
#'   The output of sequence mappers, such as [`grim_map_seq()`], has four more:
#'
#' - `results_reported`: the verdict of every row of the input data, which
#'   [`audit_seq()`] reports.
#' - `name_key_result`: the name of the column with the verdicts, usually
#'   `"consistency"`.
#' - `var`: the dispersed variables.
#' - `dispersion_linear`: whether the dispersion was linear, which
#'   [`audit_seq()`] requires.
#'
#'   The settings of the sequence or total-n call itself, such as `dispersion`,
#'   are not recorded.
#'
#'   When the output of [`grim_map()`], [`grimmer_map()`], [`debit_map()`], or
#'   [`is_percentage_of_map()`] is printed, a line in the header names the
#'   settings that differ from the mapper's defaults, as in `# GRIM: rounding =
#'   "up"`. This includes sequence and total-n output.
#'
#'   Some dplyr verbs, such as `summarise()`, drop the attribute, and with it
#'   the header line; `filter()`, `arrange()`, and `mutate()` keep it.
#'
#' @param x Output of a mapper function.
#'
#' @return A named list, or `NULL` if `x` has no `"scrutiny"` attribute.
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, rounding = "up") |>
#'   scrutiny_meta()
#'
#' # Just the rounding method:
#' out <- grim_map(pigs1, digits_x = 2, items_averaged = 1)
#' scrutiny_meta(out)$args$rounding
scrutiny_meta <- function(x) {
  attr(x, "scrutiny", exact = TRUE)
}


# Print header ------------------------------------------------------------

# A header line under "# A tibble: ..." that names the settings in which the
# call departed from the mapper's defaults, as recorded in the "scrutiny"
# attribute. `pillar::tbl_sum()` is the documented hook for such lines. Each
# method states its mapper, whose formals hold the defaults, so the attribute
# needs no record of them. Sequence and total-n output carries the basic
# class, so it gets the same line. Mappers made by `function_map()` in other
# packages get no header unless they add a method like these.
header_settings <- function(x, name_test, mapper) {
  args <- scrutiny_meta(x)$args
  if (length(args) == 0L) {
    return(character())
  }

  defaults <- formals(mapper)[names(args)]
  is_default <- vapply(
    names(args),
    function(name) {
      default <- eval(defaults[[name]], environment(mapper))
      if (is.character(default)) {
        default <- default[[1L]]
      }
      isTRUE(all.equal(args[[name]], default))
    },
    logical(1L)
  )

  settings <- if (all(is_default)) {
    "default settings"
  } else {
    args <- args[!is_default]
    paste(names(args), "=", vapply(args, deparse, character(1L)), collapse = ", ")
  }

  rlang::set_names(settings, name_test)
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_grim_map <- function(x, ...) {
  c(NextMethod(), header_settings(x, "GRIM", grim_map))
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_grimmer_map <- function(x, ...) {
  c(NextMethod(), header_settings(x, "GRIMMER", grimmer_map))
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_debit_map <- function(x, ...) {
  c(NextMethod(), header_settings(x, "DEBIT", debit_map))
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_is_percentage_of_map <- function(x, ...) {
  c(NextMethod(), header_settings(x, "Percentage", is_percentage_of_map))
}
