#' @include audit-cols-minimal.R
#' @export
audit.scrutiny_is_percentage_of_map <- function(data) {
  audit_cols_minimal(data, "is_percentage_of")
}

#' @export
audit.scrutiny_add_up_to_map <- function(data) {
  audit_cols_minimal(data, "add_up_to")
}
