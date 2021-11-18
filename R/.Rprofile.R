
if (interactive()) {
  suppressMessages(require(devtools))
}


# utils::globalVariables(c(
#   "where", "desc", "all_of", "contains", "everything", "."
# ))



# To format the string of "global variables" that show up when using
# `devtools::check()`; at the bottom of the output following "Undefined global
# functions or variables":

# global_vars_raw <- "    . across all_of consistency contains count desc dupe_count everything
#     frac items key my_vec n n_duplicated n_total origin origin_2 ratio sd
#     sd_incl_lower sd_incl_upper sd_lower sd_upper value_duplicated
#     variable where x x_lower x_upper
# "
#
# format_global_vars_raw <- function(string) {
#
#   string %>%
#     stringr::str_replace_all("\\s", ", ") %>%
#     stringr::str_replace_all(", , , , ", " ") %>%
#     stringr::str_replace_all(" , ", ", ") %>%
#     stringr::str_remove("^ ") %>%
#     stringr::str_remove(", $") %>%
#     stringr::str_split(",") %>%
#     purrr::map(stringr::str_trim)
# }
#
# global_vars <- format_global_vars_raw(global_vars_raw)
# global_vars
#
# # Declare those objects to be global variables:
# globalVariables(global_vars)
