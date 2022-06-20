

# Internal helpers; not exported ------------------------------------------

check_key_args_in_colnames <- function(data, reported) {
  offenders <- reported[!reported %in% colnames(data)]
  if (length(offenders) > 0) {
    if (length(offenders) == 1) {
      msg_cols <- "Column"
      msg_it_they <- "It's"
    } else {
      msg_cols <- "Columns"
      msg_it_they <- "They are"
    }
    if (length(offenders) > 1) {
      msg_each_other <- "each other"
    } else {
      msg_each_other <- ""
    }
    non_offenders <- reported[!reported %in% offenders]
    if (length(non_offenders) > 0) {
      if (length(offenders) > 1) {
        msg_each_other <- paste0(msg_each_other, " and with ")
      }
      non_offenders <- backticks(non_offenders)
    }
    offenders <- backticks(offenders)
    cli::cli_abort(c(
      "{msg_cols} {offenders} missing from `data`.",
      "{msg_it_they} meant to be tested for consistency with \\
      {msg_each_other}{non_offenders}."
    ))
  }
}


check_consistency_not_in_colnames <- function(data, name_test) {
  if ("consistency" %in% colnames(data)) {
    dc <- class(data)
    class_basic   <- dc[stringr::str_detect(dc, "_map$")]
    class_seq     <- dc[stringr::str_detect(dc, "_map_seq$")]
    class_total_n <- dc[stringr::str_detect(dc, "_map_total_n$")]
    if (length(class_basic) > 0) {
      fun_name_basic <- stringr::str_remove(class_basic, "scr_")
    } else {
      fun_name_basic <- NULL
    }
    if (length(class_seq) > 0) {
      fun_name_basic <- NULL
      fun_name_seq <- stringr::str_remove(class_seq, "scr_")
    } else {
      fun_name_seq <- NULL
    }
    if (length(class_total_n) > 0) {
      fun_name_basic <- NULL
      fun_name_total_n <- stringr::str_remove(class_total_n, "scr_")
    } else {
      fun_name_total_n <- NULL
    }
    non_fun_name_classes <- c("map_seq", "map_total_n")
    fun_name_all <- c(fun_name_basic, fun_name_seq, fun_name_total_n)
    fun_name_all <- fun_name_all[!fun_name_all %in% non_fun_name_classes]
    fun_name_all <- fun_name_all[length(fun_name_all) > 0]
    if (length(fun_name_all) == 0) {
      fun_name_all <- ""
    }
    if (stringr::str_detect(fun_name_all, "_seq")) {
      msg_special <- "sequence "
    } else if (stringr::str_detect(fun_name_all, "_total_n")) {
      msg_special <- "total-n "
    } else {
      msg_special <- ""
    }
    if (length(fun_name_all) > 0) {
      msg_fun_name <- paste0(", `", fun_name_all, "()`,")
    } else {
      msg_fun_name <- ""
    }
    cli::cli_abort(c(
      "`data` already includes a \"consistency\" column.",
      "x" = "This shouldn't be the case before {name_test}-testing.",
      "x" = "Did you use the output of a consistency test \\
      {msg_special}mapper function for {name_test}{msg_fun_name} \\
      as an input here?"
    ))
  }
}





#' Check that a mapper's input has correct column names
#'
#' @description When called within a consistency test mapper function,
#'   `check_mapper_input_colnames()` makes sure that the input data frame has
#'   correct column names:

#'  - They include all the key columns corresponding to the test applied by the
#'  mapper.
#'  - They don't already include `"consistency"`.
#'
#'   If either check fails, the function throws an informative error.

#' @param data Data frame. Input to the mapper function.
#' @param reported String vector of the "key" column names that `data` needs to
#'   have, such as `c("x", "n")` for `grim_map()`.
#' @param name_test String (length 1). Short, plain-text name of the consistency
#'   test that the mapper function applies, such as `"GRIM"`.
#'
#' @include utils.R
#'
#' @export
#'
#' @seealso `vignette("consistency-tests")` for context and the "key columns"
#'   terminology.


check_mapper_input_colnames <- function(data, reported, name_test) {
  check_key_args_in_colnames(data, reported)
  check_consistency_not_in_colnames(data, name_test)
}




#' Alert user if more specific `audit_*()` summaries are available
#'
#' @description (Note: Ignore this function if your `audit()` method calls
#'   `required_audit_cols()`.)
#'
#'   Call `check_audit_special()` within an `audit()` method for a consistency
#'   test mapper function, such as `audit.scr_grim_map()`. It checks if the
#'   input data frame was the product of a function produced by
#'   `function_map_seq()` or `function_map_total_n()`.
#'
#'   If so, the function issues a gentle alert to the user that points to
#'   `audit_seq()` or `audit_total_n()`, respectively.
#'
#' @param data The `audit()` method's input data frame.
#' @param name_test String (length 1). Short, plain-text name of the consistency
#'   test, such as `"GRIM"`.
#'
#' @export


check_audit_special <- function(data, name_test) {

  class_name_root <- paste0("scr_", tolower(name_test), "_map_")

  class_seq     <- paste0(class_name_root, "seq")
  class_total_n <- paste0(class_name_root, "total_n")

  # If `data` is the output of a function like `grim_map_seq()`, point the user
  # to the dedicated summary function for such output, `audit_seq()`:
  if (inherits(data, class_seq)) {
    cli::cli_alert_info(
      "More specialized {name_test} summaries available with `audit_seq()`."
    )
  }

  # Likewise, if `data` is the output of a function like `grim_map_total_n()`,
  # point the user to `audit_total_n()`:
  if (inherits(data, class_total_n)) {
    cli::cli_alert_info(
      "More specialized {name_test} summaries available with `audit_total_n()`."
    )
  }

}


