# Combining the output of different mapper calls ---------------------------

# `dplyr::bind_rows()`, and everything else built on `vctrs::vec_rbind()`,
# keeps the "scrutiny" attribute of its first input. The rows of a second
# mapper call would then be read under the first call's settings by `audit()`,
# `explain_map()`, and the print header, and `audit_seq()` would read the
# first call's verdicts for the second call's cases. The vctrs methods below
# make such a combination an error; pieces of one output, as from `filter()`
# or `[`, have identical attributes and combine as before. Basic mappers record
# only settings, since each row stands alone, but sequence and total-n output
# number their cases from 1, so they record the key columns of their input as
# well: otherwise, two calls with the same settings would combine, and
# `audit_seq()` would count two cases as one.
#
# vctrs dispatches on the first class of each input, so a method is needed for
# every pair of classes that come first in a mapper's output, in either order:
# the basic mapper's own, which `function_map()` registers as it creates the
# mapper, the two tier classes, and any `.name_class` of a sequence or total-n
# mapper, which comes before them. A pair of different classes is always an
# error, since different mappers or tiers never record the same call. A
# package that creates a mapper at the top level evaluates `function_map()` at
# install time, where the registration is lost, so `.onLoad()` registers
# scrutiny's own again.

bind_classes <- new.env(parent = emptyenv())

register_bind_methods <- function(class) {
  if (is.null(class)) {
    return(invisible(NULL))
  }
  assign(class, TRUE, envir = bind_classes)
  ns <- asNamespace("vctrs")
  for (other in ls(bind_classes)) {
    for (key in unique(paste0(c(class, other), ".", c(other, class)))) {
      registerS3method("vec_ptype2", key, vec_ptype2_scrutiny, envir = ns)
      registerS3method("vec_cast", key, vec_cast_scrutiny, envir = ns)
    }
  }
}


.onLoad <- function(libname, pkgname) {
  classes <- c(
    vapply(
      c("GRIM", "GRIMMER", "DEBIT", "is_percentage_of", "add_up_to"),
      class_name_test,
      character(1L),
      suffix = "_map"
    ),
    "scrutiny_map_seq",
    "scrutiny_map_total_n"
  )
  for (class in classes) {
    register_bind_methods(class)
  }
}


vec_ptype2_scrutiny <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = rlang::caller_env()
) {
  check_same_mapper_call(x, y, x_arg, y_arg, call)
  vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg, call = call) |>
    restore_scrutiny(x)
}


vec_cast_scrutiny <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = "",
  call = rlang::caller_env()
) {
  check_same_mapper_call(x, to, x_arg, to_arg, call)
  vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg, call = call) |>
    restore_scrutiny(to)
}


# `df_ptype2()` and `df_cast()` return bare data frames; the mapper's classes
# and record are those of `template`:
restore_scrutiny <- function(out, template) {
  out <- tibble::new_tibble(
    out,
    nrow = nrow(out),
    class = setdiff(class(template), c("tbl_df", "tbl", "data.frame"))
  )
  attr(out, "scrutiny") <- attr(template, "scrutiny", exact = TRUE)
  out
}


# Output of different mappers is never combined. An output of the same mapper
# that has lost its record has nothing to compare; the functions that need the
# record say so themselves:
check_same_mapper_call <- function(x, y, x_arg, y_arg, call) {
  meta_x <- attr(x, "scrutiny", exact = TRUE)
  meta_y <- attr(y, "scrutiny", exact = TRUE)
  same_class <- identical(class(x), class(y))
  if (
    same_class &&
      (is.null(meta_x) || is.null(meta_y) || identical(meta_x, meta_y))
  ) {
    return(invisible(NULL))
  }

  settings_x <- meta_x$settings
  settings_y <- meta_y$settings
  only_one <- c(
    setdiff(settings_x$setting, settings_y$setting),
    setdiff(settings_y$setting, settings_x$setting)
  )
  common <- intersect(settings_x$setting, settings_y$setting)
  differing <- Filter(
    function(name) {
      !identical(
        settings_x$value[[match(name, settings_x$setting)]],
        settings_y$value[[match(name, settings_y$setting)]]
      )
    },
    common
  )

  msg_what <- if (!same_class) {
    "They come from different mappers."
  } else if (length(only_one) > 0L) {
    only_one <- wrap_in_backticks(only_one)
    "Only one of them records {only_one}."
  } else if (length(differing) > 0L) {
    values <- paste0(
      "`",
      differing,
      "` (",
      vapply(
        settings_x$value[match(differing, settings_x$setting)],
        deparse_value,
        character(1L)
      ),
      " vs. ",
      vapply(
        settings_y$value[match(differing, settings_y$setting)],
        deparse_value,
        character(1L)
      ),
      ")"
    )
    "They differ in {values}."
  } else {
    "They record the same settings but different value sets or verdicts."
  }

  if (x_arg == "" || y_arg == "") {
    msg_which <- "The two inputs are the output of different calls."
  } else {
    msg_which <- "{.arg {x_arg}} and {.arg {y_arg}} are the output of \\
    different calls."
  }

  cli::cli_abort(
    c(
      "Can't combine the output of different mapper calls.",
      "x" = msg_which,
      "x" = msg_what,
      "i" = "Each output records the settings of its call, which \\
      {.fn audit}, {.fn explain_map}, and others read. Combined rows would \\
      be read under the first call's settings.",
      "i" = "Test all the value sets in one call instead."
    ),
    call = call
  )
}
