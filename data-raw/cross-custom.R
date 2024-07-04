
# Use `cross2_custom()` in data-gen.R to generate GRIM rasters after any change
# in the `grim_scalar()` implementation. This requires `.vary` to be its
# default, `"fastest"`.

# These functions are copied from an MIT-licensed repo:
# https://github.com/lhdjung/lukas_jung_blog/blob/17a835e07d7096026ec8a88da994157afa393939/posts/purrr_cross_replacement/index.qmd
# They are rewritten versions of `purrr::cross2()` and the lower-level
# `purrr::cross()`, which were deprecated, so they are going to be removed from
# purrr at some point.

# Using custom rewritten versions is an experimental alternative to the plan of
# temporarily reinstalling an older version of purrr. This plan is outlined at:
# https://github.com/lhdjung/scrutiny/issues/53


# Basic function
cross_custom <- function(.l, .filter = NULL,
                         .vary = c("fastest", "slowest")) {
  out <- vctrs::vec_expand_grid(!!!.l, .vary = .vary)
  if (is.null(.filter)) {
    return(as.list(out))
  } else if (!is.function(.filter)) {
    cli::cli_abort(c(
      "`.filter` must be a function.",
      "i" = "(Also, it needs to return a single logical value.)"
    ))
  }
  out %>%
    dplyr::filter(!.filter(.x, .y)) %>%
    as.list()
}

# Special cases

cross2_custom <- function(.x, .y, .filter = NULL,
                          .vary = c("fastest", "slowest")) {
  cross_custom(
    list(.x = .x, .y = .y), .filter = .filter, .vary = .vary
  )
}
