#' Read the settings a mapper recorded on its output
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Every mapper, such as [`grim_map()`], records the settings of the call
#'   that produced its output. `scrutiny_meta()` returns them as a tibble, so
#'   that a report can state under which rounding method or SD convention a
#'   verdict was reached.
#'
#'   Settings are the arguments that apply to the whole call, with defaults
#'   resolved, such as `rounding`, `percent`, or `sd_type`. Arguments that may
#'   vary by row are columns of the output instead: `digits_x`, `digits_sd`,
#'   and helper arguments such as `items_averaged`, even if given as a single
#'   value.
#'
#'   Sequence and total-n mappers, such as [`grim_map_seq()`], add their own
#'   settings, such as `dispersion`, to those of the basic mapper they call.
#'
#'   When the output of [`grim_map()`], [`grimmer_map()`], [`debit_map()`], or
#'   [`is_percentage_of_map()`] is printed, a line in the header names the
#'   settings that are not at their defaults, as in `# GRIM: rounding = "up"`.
#'   This includes sequence and total-n output.
#'
#'   Some dplyr verbs, such as `summarise()`, drop the record, and with it the
#'   header line; `filter()`, `arrange()`, and `mutate()` keep it.
#'
#' @param x Output of a mapper function.
#'
#' @return A tibble with one row per setting and these columns, or `NULL` if
#'   `x` has no record of its settings:
#'   - `setting`: the name of the argument.
#'   - `value`: its value, written as R code, such as `'up'` or `1:5`.
#'   - `default`: `TRUE` if the value is the mapper's default, `FALSE` if not.
#'   - `added_by`: the mapper whose argument it is, such as `"grim_map"` or
#'   `"grim_map_seq"`.
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, rounding = "up") |>
#'   scrutiny_meta()
#'
#' # Only the settings that are not at their defaults:
#' pigs1 |>
#'   grim_map_seq(digits_x = 2, items_averaged = 1, dispersion = 1:10) |>
#'   scrutiny_meta() |>
#'   dplyr::filter(!default)
scrutiny_meta <- function(x) {
  settings <- attr(x, "scrutiny", exact = TRUE)$settings
  if (is.null(settings)) {
    return(NULL)
  }
  settings$value <- settings$value |>
    vapply(deparse_value, character(1L)) |>
    single_quotes()
  settings
}


# A value as the code that creates it, on one line:
deparse_value <- function(value) {
  paste(deparse(value, width.cutoff = 500L), collapse = " ")
}

# pillar quotes and escapes a whole character column if one of its values has
# a double quote in it, so `"up"` would print as `"\"up\""`. Single quotes
# print as they are and still make R code. Code that has a single quote or an
# escaped double quote in it keeps its double quotes:
single_quotes <- function(code) {
  is_plain <- !grepl("'", code, fixed = TRUE) & !grepl('\\"', code, fixed = TRUE)
  code[is_plain] <- gsub('"', "'", code[is_plain], fixed = TRUE)
  code
}


# The settings of the basic mapper, as a named list of their values, or `NULL`
# if `x` has no record. The sequence and total-n mappers name their rows by the
# suffixes of `name_mapper()`; a basic mapper may have recorded no rows at all.
# This is what the package itself reads: the plots, `explain_map()`, and the
# sequence mapper's re-tests.
scrutiny_args <- function(x) {
  settings <- attr(x, "scrutiny", exact = TRUE)$settings
  if (is.null(settings)) {
    return(NULL)
  }
  is_basic <- !grepl("_map_(seq|total_n)$", settings$added_by)
  rlang::set_names(settings$value[is_basic], settings$setting[is_basic])
}


# Print header ------------------------------------------------------------

# A header line under "# A tibble: ..." that names the settings that are not at
# their defaults. `pillar::tbl_sum()` is the documented hook for such lines.
# Sequence and total-n output carries the basic class, so it gets the same
# line. Mappers made by `function_map()` in other packages get no header unless
# they add a method like these.
#
# pillar pads every name in the header to the widest one, "A tibble", so the
# line would read `# GRIM:     rounding = "up"`. Only an unnamed header is
# printed as it is, so the names are written into the lines here:
header_settings <- function(header, x, name_test) {
  header <- paste0(names(header), ": ", header)
  settings <- attr(x, "scrutiny", exact = TRUE)$settings
  if (is.null(settings) || nrow(settings) == 0L) {
    return(header)
  }

  changed <- settings[!settings$default, ]
  line <- if (nrow(changed) == 0L) {
    "default settings"
  } else {
    values <- vapply(changed$value, deparse_value, character(1L))
    paste(changed$setting, "=", values, collapse = ", ")
  }

  c(header, paste0(name_test, ": ", line))
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_grim_map <- function(x, ...) {
  header_settings(NextMethod(), x, "GRIM")
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_grimmer_map <- function(x, ...) {
  header_settings(NextMethod(), x, "GRIMMER")
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_debit_map <- function(x, ...) {
  header_settings(NextMethod(), x, "DEBIT")
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.scrutiny_is_percentage_of_map <- function(x, ...) {
  header_settings(NextMethod(), x, "Percentage")
}
