
# TODO: Not really debugging but just exploring if `case` could be a factor,
# with the string versions of the original values being the levels, and the
# current values -- the integers -- receding to the background. This would make
# `case` more informative to look at, and it would lift the requirement for
# `dispersion` to be a linear sequence in order for `audit_seq()` to work.

# # Example data:
# data <- pigs1
# x <-  NULL
# n <-  NULL
# var <-  Inf
# dispersion <-  1:5
# out_min <-  "auto"
# out_max <-  NULL
# include_reported <-  FALSE
# include_consistent <-  FALSE

function (data, x = NULL, n = NULL, var = Inf, dispersion = 1:5,
          out_min = "auto", out_max = NULL, include_reported = FALSE,
          include_consistent = FALSE, ...)
{
  name_test <- "GRIM"
  name_fun <- "grim_map"
  reported <- c("x", "n")
  name_class <- NULL
  args_disabled <- NULL
  fun <- grim_map   # simplifying here
  data <- absorb_key_args(data, reported)
  check_factory_dots(fun, name_fun, ...)
  args_excluded <- c(reported, args_disabled)
  arg_list <- call_arg_list()
  arg_list <- arg_list[!names(arg_list) %in% args_excluded]
  check_mapper_input_colnames(data, reported)
  check_consistency_not_in_colnames(data, name_test)
  check_tibble(data)
  data <- fun(data, ...)
  if (!include_consistent) {
    data <- data[!data$consistency, ]
  }
  if (all(is.infinite(var))) {
    var <- reported
  }

  # Enter `function_map_seq_proto()` with these variables assigned:
  var <- var[1]
  # .fun <-  fun
  # .name_test <-  name_test
  # .name_class <-  name_class
  # .dispersion <-  dispersion
  # .out_min <-  out_min
  # .out_max <-  out_max
  # .include_reported <-  include_reported

  map_seq_proto <- function_map_seq_proto(.fun = fun, .name_test = name_test,
                                          .name_class = name_class, .dispersion = dispersion, .out_min = out_min,
                                          .out_max = out_max, .include_reported = include_reported,
                                          ...)
  out <- purrr::map(var, ~map_seq_proto(data = data, var = .x))
  out[vapply(out, is.null, logical(1L))] <- NULL
  if (length(out) == 0L) {
    msg_setting <- if (interactive()) {
      "`include_consistent = TRUE`"
    } else {
      "unchecking \"Inconsistent cases only\""
    }
    cli::cli_warn(c(`!` = "No inconsistent cases to disperse from.",
                    i = "Try {msg_setting} to disperse from consistent cases, as well."))
    return(tibble::tibble())
  }
  nrow_out <- vapply(out, nrow, integer(1L), USE.NAMES = FALSE)
  var <- var %>% purrr::map2(nrow_out, rep) %>% unlist(use.names = FALSE)
  out <- out %>% dplyr::bind_rows() %>% dplyr::mutate(var,
                                                      n = as.integer(n))
  class_dispersion_ascending <- if (is_seq_ascending(dispersion)) {
    NULL
  } else {
    "scr_map_seq_disp_nonlinear"
  }
  classes_seq <- c("scr_map_seq", paste0("scr_", tolower(name_test),
                                         "_map_seq"), class_dispersion_ascending)
  out <- add_class(out, classes_seq)
  dots <- rlang::enexprs(...)
  if (length(dots$rounding) > 0L) {
    class(out)[stringr::str_detect(class(out), "^scr_rounding_")] <- paste0("scr_rounding_",
                                                                            dots$rounding)
  }
  if (!is.list(out$consistency)) {
    return(out)
  }
  `$<-`(out, "consistency", unlist(out$consistency, use.names = FALSE))
}
