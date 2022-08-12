
# # This will go into a higher-level function:
# if (inherits(df, "scr_percent_true")) {
#   mean_percentage <- "percentage"
# } else {
#   mean_percentage <- "mean"
# }


# x <- "2.71"
# n <- 32
# items <- 1
# consistency <- FALSE
# rec_sum <- 130
# rec_x_upper <- 2.74
# rec_x_lower <- 2.7
# data_classes <- class(grim_map(pigs1))


# grim_explain <- function(x, n, items, consistency, rec_sum,
#                          rec_x_upper, rec_x_lower,
#                          rec_x_upper_rounded, rec_x_lower_rounded,
#                          data_classes) {
#
#   if (consistency) {
#     out <- glue::glue(
#       "This is fine -- {x} (`x`) and {n} (`n`) are GRIM-consistent \\
#       given the other parameters."
#     )
#     return(out)
#   }
#
#   x_orig <- x
#   x <- as.numeric(x)
#
#   if ("scr_percent_true" %in% data_classes) {
#     mean_percentage <- "percentage"
#     conv_back <- " converted back from a percentage"
#     dp_orig <- decimal_places_scalar(x)
#     x <- x / 100
#     x <- restore_zeros(x, width = dp_orig + 2)
#   } else {
#     mean_percentage <- "mean"
#     conv_back <- ""
#   }
#
#   lab_rounding <- data_classes[stringr::str_detect(data_classes,
#                                                    "scr_rounding_")]
#   lab_rounding <- stringr::str_remove(lab_rounding, "scr_rounding_")
#
#
#   if (lab_rounding == "up_or_down") {
#     lab_rounding_end <- "rounded up or down"
#   } else if (lab_rounding == "ceiling_or_floor") {
#     lab_rounding_end <- "ceiled or floored (at the appropriate decimal level)"
#   } else if (lab_rounding == "even") {
#     lab_rounding_end <-
#       "rounded to the next even number using base R's `round()`"
#   } else if (lab_rounding %in% c("up", "down")) {
#     lab_rounding_end <- paste("rounded", lab_rounding)
#   } else if (rounding == "ceiling") {
#     lab_rounding_end <- "ceiled"
#   } else if (lab_rounding == "floor") {
#     lab_rounding_end <- "floored"
#   } else if (lab_rounding == "trunc") {
#     lab_rounding_end <- "truncated"
#   } else if (lab_rounding == "anti_trunc") {
#     lab_rounding_end <-  glue::glue("rounded in an \"anti-truncated\" \\
#     way, i.e., rounded away from zero")
#   }
#
#
#   # lab_rounding_arg <- paste0("\"", lab_rounding, "\"")
#   lab_rounding_arg <- lab_rounding
#
#   if (lab_rounding == "up_or_down") {
#     # lab_rounding_arg <- paste0(lab_rounding_arg, "; the default")
#     lab_rounding_default <- "; the default"
#   } else {
#     lab_rounding_default <- ""
#   }
#
#   if (stringr::str_detect(lab_rounding, "_or_")) {
#     two_four <- "four"
#     rounding_split <- stringr::str_split(lab_rounding, "_or_")
#     rounding_split <- rounding_split[[1]]
#     lab_rounding_1 <- rounding_split[1]
#     lab_rounding_2 <- rounding_split[2]
#     # lab_rec_x_rounded <- list(  # INDEX THESE FOUR ELEMENTS DIRECTLY IN THE OUTPUT
#     #   rlang::expr_text(glue::glue("rec_x_upper_rounded_{lab_rounding_1}")),
#     #   rlang::expr_text(glue::glue("rec_x_upper_rounded_{lab_rounding_2}")),
#     #   rlang::expr_text(glue::glue("rec_x_lower_rounded_{lab_rounding_1}")),
#     #   rlang::expr_text(glue::glue("rec_x_lower_rounded_{lab_rounding_2}"))
#     # )
#
#     # ATTEMPT TO FIX THE BUG: CONDITIONAL ON THE ROUNDING VALUE BEING PLURAL
#     # (SEE ABOVE), THE ROUNDED RECONSTRUCTED VALUES ARGUMENTS WILL BE LISTS WITH
#     # TWO ELEMENTS. BREAK THEM APART HERE:
#     rec_x_lower_rounded_1 <- rec_x_lower_rounded[[1]]
#     rec_x_lower_rounded_2 <- rec_x_lower_rounded[[2]]
#     rec_x_upper_rounded_1 <- rec_x_upper_rounded[[1]]
#     rec_x_upper_rounded_2 <- rec_x_upper_rounded[[2]]
#
#     lab_rec_x_rounded_1 <- glue::glue("rec_x_upper_rounded_{lab_rounding_1}")
#     lab_rec_x_rounded_2 <- glue::glue("rec_x_upper_rounded_{lab_rounding_2}")
#     lab_rec_x_rounded_3 <- glue::glue("rec_x_lower_rounded_{lab_rounding_1}")
#     lab_rec_x_rounded_4 <- glue::glue("rec_x_lower_rounded_{lab_rounding_2}")
#
#     # vals_rec_x_rounded <- lab_rec_x_rounded %>%
#     #   purrr::map(list(rlang::parse_expr, eval))
#
#   # HERE IS THE ERROR: EVALUATION (WITH `eval()`) FAILS BECAUSE NO OBJECTS BY
#   # THOSE NAMES ARE FOUND. MAYBE SUPPLY THESE OBJECTS AS ARGUMENTS? HOWEVER, IT
#   # CAN'T BE THE OUTCOMMENTED ARGUMENTS AT THE TOP BECAUSE THESE ARE SPECIFIC TO
#   # ROUNDING UP AND DOWN! SO THE WHOLE APPROACH OF PARSING AND EVALUATION SEEMS
#   # WRONG, AND WHAT'S NEEDED INSTEAD ARE SIMPLY ARGUMENTS. THE SAME WILL BE
#   # NECESSARY FOR `rec_x_upper_rounded` AND `rec_x_lower_rounded`, WHICH MAY BE
#   # `NULL` IF `rounding` HAS ONE OF THE `_or_` VALUES.
#     vals_rec_x_rounded <- list(
#       eval(rlang::parse_expr(glue::glue("rec_x_upper_rounded_{lab_rounding_1}"))),
#       eval(rlang::parse_expr(glue::glue("rec_x_upper_rounded_{lab_rounding_2}"))),
#       eval(rlang::parse_expr(glue::glue("rec_x_lower_rounded_{lab_rounding_1}"))),
#       eval(rlang::parse_expr(glue::glue("rec_x_lower_rounded_{lab_rounding_2}")))
#     )
#   } else {
#     two_four <- "two"
#     lab_rec_x_rounded <- "`rec_x_upper_rounded` and `rec_x_lower_rounded`"
#     vals_rec_x_rounded <- list(
#       eval(rlang::parse_expr("rec_x_upper_rounded")),
#       eval(rlang::parse_expr("rec_x_lower_rounded"))
#     )
#   }
#
#
#
#   out <- glue::glue(
#     "A {mean_percentage} (`x`) value of {x_orig} is not \\
#       GRIM-consistent with an `n` of {n} given the other \\
#       parameters. If {x} is multiplied with {n}, it returns \\
#       {rec_sum} (`rec_sum` column). Ceiling this value and \\
#       dividing the result by {n} (`n`) returns {rec_x_upper} \\
#       (`rec_x_upper`). Conversely, flooring it and then \\
#       dividing by {n} returns {rec_x_lower} (`rec_x_lower`). \\
#       This leads to lower and upper bounds of the putative \\
#       original {mean_percentage} which was subsequently rounded \\
#       to {x}. To reproduce the rounding of {rec_x_upper} and \\
#       {rec_x_lower}, they were {lab_rounding_end}, following \\
#       the `rounding` argument \\
#       (\"{lab_rounding_arg}\"{lab_rounding_default}). The rounded \\
#       numbers corresponding to the upper bound (or ceiling), \\
#       {vals_rec_x_rounded[1]} and {vals_rec_x_rounded[2]}, \\
#       are in the `{lab_rec_x_rounded_1}` and `{lab_rec_x_rounded_2}` \\
#       columns. Those corresponding to the lower bound (or floor), \\
#       {vals_rec_x_rounded[3]} and {vals_rec_x_rounded[4]}, are in the \\
#       `{lab_rec_x_rounded_3}` and `{lab_rec_x_rounded_4}` columns. \\
#       Now, {x} and {n} \\
#       (`x`{conv_back} and `n`) are GRIM-consistent if and only if \\
#       {x} is near-equal to any of these {two_four} final rounded numbers. \\
#       For this comparison, `dplyr::near()` is used to avoid exact \\
#       numerical identity as a criterion. This would be error-prone \\
#       with floating-point numbers such as those examined here, \\
#       and thus overly strict."
#   )
#
#   return(out)
# }


# # Old:
# {vals_rec_x_rounded[3]}, and {vals_rec_x_rounded[4]},
# are in the {two_four} columns {lab_rec_x_rounded}.


# Test --------------------------------------------------------------------

# out <- pigs1 %>%
#   grim_map(show_rec = TRUE)   # DOESN'T YET WORK FOR "UP" ETC.
#
#
# out %>%
#   dplyr::mutate(
#     data_classes = class(out)[stringr::str_detect(class(out), "scr_rounding")],
#     ratio = NULL
#   ) %>%
#   purrr::pmap_chr(grim_explain) %>%
#   .[2]


