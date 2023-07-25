#
# df1 <- tibble::tibble(
#   absolute = c(40, 37, 24),
#   percentage = c(33, 31, 20),
#   total = 120
# )
#
# # Copy `df1`, but replace the correct absolutes by incorrect ones:
# df2 <- df1
# df2$absolute <- c(41, 38, 25)
#
# # Made with `dput()`:
# df1_tested_exp <- structure(
#   list(
#     absolute = c(40, 37, 24),
#     percentage = c(33, 31, 20),
#     total = c(120, 120, 120),
#     consistency = c(TRUE, TRUE, TRUE)
#   ),
#   row.names = c(NA, -3L),
#   class = c(
#     "scr_percentage_map", "scr_rounding_up_or_down",
#     "tbl_df", "tbl", "data.frame"
#   )
# )
#
#
# test_that("`is_percentage_of_map()` works correctly", {
#   df1 %>% is_percentage_of_map() %>% expect_equal(df1_tested_exp)
#   df2 %>% is_percentage_of_map() %>% identical(df1_tested_exp) %>% expect_false()
#   df1[2:3] %>% expect_equal(df2[2:3])
# })
#
