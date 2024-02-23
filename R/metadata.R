#
#
# # NOTE: This file should normally be outcommented. It contains code for package
# # metadata analysis. In particular, it counts the lines of code in all R files.
#
#
# # Installation ------------------------------------------------------------
#
# # The cloc package has never been on CRAN, so install it like this:
# remotes::install_github("hrbrmstr/cloc")
#
# # Analysis ----------------------------------------------------------------
#
# # Find the scrutiny/R file:
# path <- paste0(here::here(), "/R/")
#
# # Count lines of code (loc) from that file, removing the "SUM" line because it
# # goes against tidy data principles, and adding a total number of lines. Also,
# # some other files are ignored because they don't include code that is part of
# # the package's current source code or because their code is part of a standard
# # suite that was automatically generated ba a workflow package; e.g.,
# # "utils-pipe".
# df_files <- path %>%
#   cloc::cloc_by_file() %>%
#   dplyr::filter(
#     language != "SUM",
#     !stringr::str_detect(filename, "rivets"),
#     !stringr::str_detect(filename, "debug"),
#     !stringr::str_detect(filename, "import-reexport"),
#     !stringr::str_detect(filename, "utils-pipe"),
#     !stringr::str_detect(filename, "utils-tidy-eval"),
#     !stringr::str_detect(filename, "scrutiny-package"),
#     !stringr::str_detect(filename, "Rprofile"),
#   ) %>%
#   # dplyr::mutate(total = loc + blank_lines + comment_lines) %>%
#   dplyr::mutate(
#     total = loc + blank_lines + comment_lines,
#     filename_short = stringr::str_remove(filename, path),
#     filename_short = stringr::str_remove(filename_short, ".R$")
#   ) %>%
#   dplyr::relocate(filename_short, .after = "filename") %>%
#   dplyr::mutate(filename = NULL) %>%
#   dplyr::mutate(
#     type = dplyr::case_when(
#       stringr::str_detect(filename_short, "grim|debit|consistency") ~ "consistency_test",
#       stringr::str_detect(filename_short, "round") ~ "rounding",
#       .default = "other"
#     ), .after = filename_short
#   )
#
#
# df_files
#
# # Sum up loc and other types of lines. All the dplyr code below is actually
# # superfluous because the last line of `df_files` (removed by the
# # `dplyr::filter(language != "SUM")` call above) contains the sums already.
# # Extract its statistics of interest with: `df_files[nrow(df_files), 4:6]`
# df_files_summary <- df_files %>%
#   dplyr::summarise(dplyr::across(
#     .cols = where(scrutiny::is_numeric_like),
#     .fns = sum
#   ))
#
# df_files_summary
#
#
#
# # Visualization -----------------------------------------------------------
#
# ggplot2::theme_set(ggplot2::theme_minimal())
#
# # This plot is meant to be viewed in fullscreen, or else the labels won't be
# # legible:
# ggplot2::ggplot(df_files, ggplot2::aes(
#   x = forcats::fct_reorder(filename_short, loc, .desc = TRUE),
#   y = loc
# )) +
#   ggplot2::geom_col(fill = "royalblue1", alpha = 0.8) +
#   ggplot2::geom_text(
#     ggplot2::aes(label = loc), nudge_y = 10, size = 2.5
#   ) +
#   ggplot2::labs(
#     x = "File name (.R)",
#     y = "Lines of code"
#   ) +
#   ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0.1, 20))) +
#   ggplot2::theme(
#     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.95),
#     panel.grid.minor = ggplot2::element_blank(),
#     panel.grid.major.x = ggplot2::element_blank(),
#     panel.grid.minor.x = ggplot2::element_blank()
#   )
#
