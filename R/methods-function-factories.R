

#' @include audit.R reverse-map-seq.R reverse-map-total-n.R
#' @export



# For output of `function_map_seq()` --------------------------------------

audit_seq.scr_grim_map_seq <- function(data) {
  summarize_map_seq(data)
}

audit_seq.scr_debit_map_seq <- function(data) {
  summarize_map_seq(data)
}



# For output of `function_map_total_n()` ----------------------------------

audit_total_n.scr_grim_map_total_n <- function(data) {
  summarize_map_total_n(data)
}

audit_total_n.scr_debit_map_total_n <- function(data) {
  summarize_map_total_n(data)
}

