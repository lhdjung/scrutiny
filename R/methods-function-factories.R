

#' @include audit.R reverse-map-seq.R reverse-map-total-n.R
#' @export



# For output of `function_map_seq()` --------------------------------------

audit.scr_grim_map_seq <- function(data) {
  summarize_map_seq(data)
}

audit.scr_debit_map_seq <- function(data) {
  summarize_map_seq(data)
}



# For output of `function_map_total_n()` ----------------------------------

audit.scr_grim_map_total_n <- function(data) {
  summarize_map_total_n(data)
}

audit.scr_debit_map_total_n <- function(data) {
  summarize_map_total_n(data)
}

