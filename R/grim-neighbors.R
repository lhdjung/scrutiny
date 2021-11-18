

# Prepare data frame with sequence for tests
#
# @description `prepare_seq_test()` creates a data frame with a sequence of
#   decimal numbers following an input value. Further columns can be added as
#   in `dplyr::mutate()`. Sequence generation can be modified via arguments
#   passed on to `seq_decimal_length()`.
#
#   Use this function in preparation for scrutiny's mapping functions like
#   `grim_map()` and `debit_map()`.
#
# @param .from Numeric (or string coercible to numeric). Starting point of the
#   sequence.
# @param ... Name-value pairs. These will be columns in the output tibble.
# @param .length_out Integer. Length of the output vector (i.e., the number of
#   its values). Default is `10`.
# @param .dir Integer. If set to `-1`, the sequence goes backward. Default is
#   `1`.
# @param .offset Integer. If set to a non-zero number, the starting point will
#   be offset by that many units on the level of the last decimal digit.
#   Default is `0`.
#
# @include seq-decimal.R
#
# @return A tibble with string column `x` and possibly other columns.
#
# @seealso `seq_decimal_length()`, the underlying sequence-generating function.
#
# @export
#
# @examples
# # REDO ALL THESE EXAMPLES
# # GRIM-test the five next neighbors:
# grim_neighbors(x = "0.19", n = 24, .length_out = 5)
#
# # Go backwards with `.dir = -1`:
# grim_neighbors(x = "0.19", n = 24, .length_out = 5, .dir = -1)
#
# # The difference between the neighbors
# # depends on the number of decimal places.
# # Compare these tibbles with those above:
# grim_neighbors(x = "9.384", n = 350)
# grim_neighbors(x = "9.384", n = 350, .dir = -1)



# seq_endpoint_df <- function(.from, .to, ..., .offset = 0,
#                             .string_output = TRUE) {
#
#   x <- seq_endpoint(from = .from, to = .to, offset = .offset,
#                     string_output = .string_output)
#
#   further_cols <- rlang::enquos(...)
#
#   tibble::tibble(x, !!!further_cols)
# }
#
#
#
# #' @rdname seq_endpoint_df
# #' @export
#
# seq_distance_df <- function(.from, ..., .length_out = 10, .dir = 1,
#                               .offset = 0, .string_output = TRUE) {
#
#   x <- seq_distance(from = .from, length_out = .length_out, dir = .dir,
#                     offset = .offset, string_output = .string_output)
#
#   further_cols <- rlang::enquos(...)
#
#   tibble::tibble(x, !!!further_cols)
# }




# # THIS FUNCTION IS IN SHAMBLES!
#
# grim_neighbors <- function(..., .func = grim, .length_out = 10,
#                           .dir = 1, .offset = 0, .include_index = FALSE) {
#
#   v1 <- rlang::enexprs(...)[1]
#
#   neighbor <- seq_decimal_length(
#     from = v1, length_out = .length_out, offset = .offset, string_output = TRUE
#     )
#
# #  neighbor <- restore_zeros(neighbor)
#   consistency <- purrr::map_lgl(.x = neighbor, .f = .func, ...)
#   n <- as.integer(n)
#
#   out <- tibble::tibble(neighbor, n, consistency)
#
#   tibble::new_tibble(out, nrow = nrow(out), class = "scr_grim_neighbors")
# }

