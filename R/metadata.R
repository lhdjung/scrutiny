#
#
# # NOTE: This file should normally be outcommented. It contains code for package
# # metadata analysis. In particular, it counts the lines of code in all R files.
#
#
# # Analysis ----------------------------------------------------------------
#
# # Find the scrutiny/R file:
# path <- paste0(here::here(), "/R/")
#
# # Count lines of code (loc) from that file, removing the "SUM" line because it
# # goes against tidy data principles, and adding a total number of lines.
# df_files <- path %>%
#   cloc::cloc_by_file() %>%
#   dplyr::filter(language != "SUM") %>%
#   dplyr::mutate(total = loc + blank_lines + comment_lines) %>%
#   dplyr::mutate(
#     filename_short = stringr::str_remove(filename, path),
#     filename_short = stringr::str_remove(filename_short, ".R$")
#   ) %>%
#   dplyr::relocate(filename_short, .after = "filename")
#
# # Sum up loc and other types of lines. All the dplyr code below is actually
# # superfluous because the last line of `df_files` (removed by the
# # `dplyr::filter(language != "SUM")` call above) contains the sums already.
# # Extract its statistics of interest with: `df_files[nrow(df_files), 4:6]`
# df_files_summary <- df_files %>%
#   dplyr::summarise(dplyr::across(
#     .cols = matches(c("loc", "blank_lines", "comment_lines", "total")),
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
# ggplot2::ggplot(df_files, ggplot2::aes(
#   x = forcats::fct_reorder(filename_short, loc),
#   y = loc
# )) +
#   ggplot2::geom_col() +
#   ggplot2::labs(
#     x = "File name",
#     y = "Lines of code"
#   ) +
#   ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(0.1, 0.1))) +
#   ggplot2::theme(
#     axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.95)
#   )
#
