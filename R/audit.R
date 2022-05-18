
#' Summaries of scrutiny objects
#'
#' @description `audit()` is an S3 generic to follow up on those scrutiny
#'   functions that perform tests on a data frame. It summarizes results of
#'   those tests and presents the summaries in a tibble. `audit_seq()` and
#'   `audit_total_n()` summarize the results of functions that end on `_seq` and
#'   `_total_n`, respectively.
#'
#'   Below is a list of functions that return objects with classes for which
#'   there are `audit()` methods. This means you can run `audit()` on the output
#'   returned by any of these functions. The same is true for `audit_seq()` and
#'   `audit_total_n()`.
#'
#'   Go to the documentation of any function below to learn about its particular
#'   `audit()` method.

#   @section `audit()`: | \strong{Function}            | \strong{Class}            |
#   | ---                          | ---                       |
#   | `grim_map()`                 | `scr_grim_map`            |
#   | `grim_map_total_n()`         | `scr_grim_map_total_n`    |
#   | `debit_map()`                | `scr_debit_map`           |
#   | `debit_map_total_n()`        | `scr_debit_map_total_n`   |
#   | `duplicate_count()`          | `scr_dup_count`           |
#   | `duplicate_detect()`         | `scr_dup_detect`          |

#' @param data A data frame with one of the classes named below.

#' @section `audit()`:
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map()`                 | `scr_grim_map`            |
#'   | `debit_map()`                | `scr_debit_map`           |
#'   | `duplicate_count()`          | `scr_dup_count`           |
#'   | `duplicate_detect()`         | `scr_dup_detect`          |

#' @section `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map_seq()`             | `scr_grim_map_seq`    |
#'   | `debit_map_seq()`            | `scr_debit_map_seq`   |

#' @section `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map_total_n()`         | `scr_grim_map_total_n`    |
#'   | `debit_map_total_n()`        | `scr_debit_map_total_n`   |


#'
#' @return A tibble (data frame) with test summary statistics.
#' @export
#'
#' @examples
#' # For GRIM-testing:
#' pigs1 %>%
#'   grim_map() %>%
#'   audit()
#'
#' # For detecting duplicates:
#' pigs4 %>%
#'   duplicate_detect() %>%
#'   audit()


audit <- function(data) {
  UseMethod("audit")
}


#' @rdname audit
#' @export

audit_seq <- function(data) {
  UseMethod("audit_seq")
}


#' @rdname audit
#' @export

audit_total_n <- function(data) {
  UseMethod("audit_total_n")
}

