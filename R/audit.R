
#' Summaries of scrutiny objects
#'
#' @description `audit()` is an S3 generic to follow up on those scrutiny
#'   functions that perform tests on a data frame. It summarizes results of
#'   those tests and presents the summaries in a tibble.
#'
#'   Below is a list of functions that return objects with classes for which
#'   there are `audit()` methods. This means you can run `audit()` on the output
#'   returned by any of these functions.
#'
#'   Go to the documentation of any function below to learn about its particular
#'   `audit()` method.
#'
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map()`                 | `scr_grim_map`            |
#'   | `grim_map_disperse()`        | `scr_grim_map_disperse`   |
#'   | `debit_map()`                | `scr_debit_map`           |
#'   | `debit_map_disperse()`       | `scr_debit_map_disperse`  |
#'   | `duplicate_count()`          | `scr_dup_count`           |
#'   | `duplicate_detect()`         | `scr_dup_detect`          |

#' @param data A data frame with one of the classes named above.
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

