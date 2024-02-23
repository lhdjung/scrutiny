#
# # Note: This function was made by `function_map()` using the code right below.
# # Its purpose is to debug the `code_col_control` part of `function_map()`.
#
# # function_map(
# #   .fun = grim_scalar,
# #   .reported = c("x", "n"),
# #   .name_test = "GRIM",
# #   .col_names = c(
# #     "rec_sum", "rec_x_upper", "rec_x_lower",
# #     "rec_x_upper_rounded_up", "rec_x_upper_rounded_down",
# #     "rec_x_lower_rounded_up", "rec_x_lower_rounded_down"
# #   ),
# #   .col_control = "show_rec",
# #   .col_filler = NA
# # )
#
#
# # Test the factory-made function like this:
#
# # pigs1 %>%
# #   grim_map_new(show_rec = TRUE)
#
#
# # These are the 11 column names returned with `grim_map(show_rec = TRUE)`:
#
# # c(
# #   "x", "n", "consistency", "rec_sum", "rec_x_upper", "rec_x_lower",
# #   "rec_x_upper_rounded_up", "rec_x_upper_rounded_down",
# #   "rec_x_lower_rounded_up", "rec_x_lower_rounded_down", "ratio"
# # )
#
# # # Test with:
# # data <- pigs1
# # x <- NULL
# # n <- NULL
# # show_rec <- TRUE
#
# grim_map_new <- function (data, x = NULL, n = NULL, ...)
# {
#   fun <- function (x, n, items = 1, percent = FALSE, show_rec = FALSE,
#                    rounding = "up_or_down", threshold = 5, symmetric = FALSE,
#                    tolerance = .Machine$double.eps^0.5)
#   {
#     check_type(items, c("double", "integer"))
#     check_type(percent, "logical")
#     if (!is.character(x)) {
#       cli::cli_abort(c(
#         `!` = "`x` must be a string.",
#         x = "It is {an_a_type(x)}."
#       ))
#     }
#     x_num <- as.numeric(x)
#     digits <- decimal_places_scalar(x)
#     if (percent) {
#       x_num <- x_num/100
#       digits <- digits + 2L
#     }
#     n_items <- n * items
#     rec_sum <- x_num * n_items
#     rec_x_upper <- dustify(ceiling(rec_sum)/n_items)
#     rec_x_lower <- dustify(floor(rec_sum)/n_items)
#     grains_rounded <- reround(
#       x = c(rec_x_upper, rec_x_lower),
#       digits = digits, rounding = rounding, threshold = threshold,
#       symmetric = symmetric
#     )
#     grain_is_x <- any(dplyr::near(grains_rounded, x_num,
#                                   tol = tolerance))
#     if (!show_rec) {
#       return(grain_is_x)
#     } else {
#       consistency <- grain_is_x
#       length_2ers <- c("up_or_down", "up_from_or_down_from",
#                        "ceiling_or_floor")
#       if (any(length_2ers == rounding)) {
#         return(list(consistency, rec_sum, rec_x_upper,
#                     rec_x_lower, grains_rounded[1L], grains_rounded[2L],
#                     grains_rounded[5L], grains_rounded[6L]))
#       } else {
#         return(list(consistency, rec_sum, rec_x_upper,
#                     rec_x_lower, grains_rounded[1L], grains_rounded[3L]))
#       }
#     }
#   }
#   name_class <- NULL
#   if (!missing(x) || !missing(n)) {
#     data <- absorb_key_args(data, c("x", "n"))
#   }
#   check_args_disabled(NULL)
#   check_factory_dots(fun, "grim_scalar", ...)
#   check_mapper_input_colnames(data, c("x", "n"), "GRIM")
#   check_tibble(data)
#   data_tested <- data[, c("x", "n")]
#   data_non_tested <- data[!colnames(data) %in% c("x", "n")]
#   consistency <- purrr::pmap(data_tested, fun, ...)
#   dots <- rlang::enexprs(...)
#   if (any("rounding" == names(dots))) {
#     rounding_class <- dots$rounding
#   } else {
#     rounding_class <- formals(fun)$rounding
#   }
#   rounding_class <- paste0("scr_rounding_", rounding_class)
#   name_class <- c(name_class, rounding_class)
#   out <- tibble::tibble(data_tested, consistency, data_non_tested) %>%
#     add_class(c("scr_grim_map", rounding_class))
#   if (!all(vapply(consistency, length, integer(1L)) == 1L)) {
#     extend_if_length1 <- function(x, value_if_length1) {
#       if (length(x) == 1L)
#         list(list(x, value_if_length1))
#       else x
#     }
#     out$consistency <- purrr::map(out$consistency, extend_if_length1,
#                                   value_if_length1 = NA)
#     out <- unnest_consistency_cols(
#       out, col_names = c(
#         "consistency",
#         "rec_sum", "rec_x_upper", "rec_x_lower", "rec_x_upper_rounded_up",
#         "rec_x_upper_rounded_down", "rec_x_lower_rounded_up",
#         "rec_x_lower_rounded_down"
#       ), index = FALSE
#     )
#   } else {
#     out <- tidyr::unnest(out, cols = consistency)
#   }
#   if (!is.list(out$consistency)) {
#     return(out)
#   }
#   `$<-`(out, "consistency", unlist(out$consistency, use.names = FALSE))
# }
