df <- tibble::tibble(x = c(0.3, 5.2), n = c(4, 28))
out_up <- df |>
  grim_map(
    digits_x = 1,
    items_averaged = 1,
    data_type = "integer",
    rounding = "up"
  )
out_down <- df |>
  grim_map(
    digits_x = 1,
    items_averaged = 1,
    data_type = "integer",
    rounding = "down"
  )

test_that("output of calls with different settings can't be combined", {
  dplyr::bind_rows(out_up, out_down) |> expect_error("Can't combine the output of different mapper calls")
  dplyr::bind_rows(out_up, out_down) |> expect_error("`rounding` \\(\"up\" vs. \"down\"\\)")
  vctrs::vec_rbind(out_up, out_down) |> expect_error("Can't combine the output of different mapper calls")
  list(out_up, out_down) |> purrr::list_rbind() |> expect_error("Can't combine the output of different mapper calls")
  dplyr::bind_rows(out_up, out_down |> dplyr::filter(n > 10)) |> expect_error("different mapper calls")
})

test_that("pieces of one output combine as before, with the record kept", {
  out <- dplyr::bind_rows(out_up[1, ], out_up[2, ])
  out |> expect_equal(out_up)
  out |> class() |> expect_equal(class(out_up))
  out |> attr("scrutiny") |> expect_identical(attr(out_up, "scrutiny"))
  vctrs::vec_rbind(out_up, out_up |> dplyr::filter(n > 10)) |> class() |> expect_equal(class(out_up))
  dplyr::bind_rows(out_up, out_up |> dplyr::mutate(z = 1)) |> nrow() |> expect_equal(4L)
})

test_that("the sequence and total-n tiers reject different calls, too", {
  seq_1 <- tibble::tibble(x = 5.19, n = 28) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  seq_2 <- tibble::tibble(x = c(5.17, 4.2), n = c(28, 30)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  dplyr::bind_rows(seq_1, seq_2) |> expect_error("different value sets or results")
  dplyr::bind_rows(seq_1, seq_1[1:3, ]) |> expect_no_error()
  seq_1 |> dplyr::filter(var == "n") |> dplyr::bind_rows(seq_1 |> dplyr::filter(var == "x")) |> audit_seq() |> expect_equal(audit_seq(seq_1))
  tot_1 <- tibble::tibble(x1 = 5.19, x2 = 4.2, n = 58) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 0:1)
  tot_2 <- tibble::tibble(x1 = 5.19, x2 = 4.2, n = 58) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 0:2)
  dplyr::bind_rows(tot_1, tot_2) |> expect_error("`dispersion`")
})

test_that("a mapper made in the session is covered as well", {
  schlim_scalar <- function(y, n, strict = FALSE) (y / 3) > n
  schlim_map <- function_map(.fn = schlim_scalar, .reported = c("y", "n"), .name_test = "SCHLIM")
  df <- tibble::tibble(y = 16:18, n = 3:5)
  dplyr::bind_rows(schlim_map(df), schlim_map(df, strict = TRUE)) |> expect_error("`strict` \\(FALSE vs. TRUE\\)")
  dplyr::bind_rows(schlim_map(df), schlim_map(df)) |> nrow() |> expect_equal(6L)
})

test_that("output that has lost its record combines as before", {
  stripped <- out_up
  attr(stripped, "scrutiny") <- NULL
  dplyr::bind_rows(stripped, out_down) |> nrow() |> expect_equal(4L)
})

test_that("output of different mappers or tiers can't be combined", {
  df <- tibble::tibble(x = c(5.19, 4.2), n = c(28, 30))
  out_grim <- df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  out_grimmer <- tibble::tibble(x = 5.21, sd = 1.6, n = 28) |> grimmer_map(digits_x = 2, digits_sd = 1, items_averaged = 1, data_type = "integer")
  out_seq <- df |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  dplyr::bind_rows(out_grim, out_grimmer) |> expect_error("different mappers")
  dplyr::bind_rows(out_grimmer, out_grim) |> expect_error("different mappers")
  dplyr::bind_rows(out_grim, out_seq)     |> expect_error("different mappers")
  dplyr::bind_rows(out_seq, out_grim)     |> expect_error("different mappers")
  # A class of the user's own comes first:
  seq_custom <- function_map_seq(.fn = grim_map, .reported = c("x", "n"), .name_test = "GRIM", .name_class = "my_class")
  dplyr::bind_rows(seq_custom(df, digits_x = 2, items_averaged = 1, data_type = "integer"), seq_custom(df, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up")) |> expect_error("`rounding`")
})

test_that("output of sequence and total-n calls on different data is not combined", {
  seq1 <- tibble::tibble(x = 5.19, n = 28) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  seq2 <- tibble::tibble(x = 6.24, n = 30) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  seq1 |> dplyr::bind_rows(seq2) |> expect_error("different value sets")
  seq1 |> dplyr::bind_rows(seq1[0, ]) |> nrow() |> expect_equal(nrow(seq1))

  total1 <- tibble::tibble(x1 = 3.43, x2 = 5.28, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  total2 <- tibble::tibble(x1 = 2.97, x2 = 4.42, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  total1 |> dplyr::bind_rows(total2) |> expect_error("different value sets")

  data <- tibble::tibble(a = 1, b = 2, c = 3, total = 3)
  sum3 <- data |> add_up_to_map(summands = c(a, b, c), total = total)
  sum2 <- data |> add_up_to_map(summands = c(a, b), total = total)
  sum3 |> dplyr::bind_rows(sum2) |> expect_error("Can't combine")
})

test_that("calls that differ in a helper or a decimal count can't be combined", {
  df <- tibble::tibble(x = 5.19, n = 28)
  seq_1 <- df |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  dplyr::bind_rows(seq_1, df |> grim_map_seq(digits_x = 2, items_averaged = 2, data_type = "integer")) |> expect_error("different calls")
  dplyr::bind_rows(seq_1, df |> grim_map_seq(digits_x = 3, items_averaged = 1, data_type = "integer")) |> expect_error("different calls")
  df_tot <- tibble::tibble(x1 = 3.1, x2 = 2.9, n = 41)
  tot_1 <- df_tot |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  dplyr::bind_rows(tot_1, df_tot |> grim_map_total_n(digits_x = 2, items_averaged = 2, data_type = "integer")) |> expect_error("different calls")
  dplyr::bind_rows(tot_1, df_tot |> dplyr::mutate(items_averaged = 2) |> grim_map_total_n(digits_x = 2, data_type = "integer")) |> expect_error("different calls")
  dplyr::bind_rows(tot_1[1:3, ], tot_1[-(1:3), ]) |> expect_equal(tot_1)
})

test_that("a setting given as an integer is the same as a double", {
  df <- tibble::tibble(x = 5.19, n = 28)
  dplyr::bind_rows(df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer"), df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", threshold = 5L)) |> nrow() |> expect_equal(2L)
  dplyr::bind_rows(df |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 1:5), df |> grim_map_seq(digits_x = 2L, items_averaged = 1L, data_type = "integer", dispersion = c(1, 2, 3, 4, 5))) |> nrow() |> expect_equal(40L)
})

test_that("the errors name what differs, in the order given", {
  df <- tibble::tibble(n_control = 10, n_treatment = 12, n_all = 22)
  dplyr::bind_rows(add_up_to_map(df, summands = c(n_control, n_treatment), total = n_all), add_up_to_map(df, summands = n_control, total = n_all)) |> expect_error("test different columns")
  df_grim <- tibble::tibble(x = 5.19, n = 28)
  dplyr::rows_append(grim_map(df_grim, digits_x = 2, items_averaged = 1, data_type = "integer"), grim_map(df_grim, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up")) |> expect_error("`x` and `y`")
})

test_that("mapper output doesn't combine with a plain data frame, in either order", {
  # `bind_rows()` kept the mapper's record for rows it never tested:
  out <- tibble::tibble(x = 5.19, n = 28) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  plain <- tibble::tibble(x = 1, n = 5, consistency = TRUE)
  dplyr::bind_rows(out, plain)                    |> expect_error("`..2` is not mapper output")
  dplyr::bind_rows(plain, out)                    |> expect_error("`..1` is not mapper output")
  dplyr::bind_rows(out, as.data.frame(plain))     |> expect_error("is not mapper output")
  dplyr::bind_rows(tibble::as_tibble(out), plain) |> nrow() |> expect_equal(2L)
  dplyr::rows_append(out, plain)                  |> expect_error("`y` is not mapper output")
  dplyr::bind_rows(out, tibble::tibble(.rows = 3)) |> expect_error("is not mapper output")
  # An empty data frame adds no rows, whatever its columns:
  dplyr::bind_rows(out, tibble::tibble(x = numeric())) |> audit() |> expect_equal(audit(out))
  # This failed, since the type was taken from the plain data frame. dplyr
  # restores the classes of its first input, so only vctrs keeps the record:
  tibble::tibble() |> vctrs::vec_rbind(out) |> audit() |> expect_equal(audit(out))
  tibble::tibble() |> dplyr::bind_rows(out) |> class()  |> expect_identical(c("tbl_df", "tbl", "data.frame"))
})

test_that("columns can be added to mapper output, and nothing but columns", {
  # These hit the guard against rows since vctrs asks for the type of the
  # inputs when binding columns, too:
  data <- tibble::tibble(x = c(5.19, 3.2), n = c(28, 40), study = c("a", "b"))
  out <- data |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  wider <- out |> dplyr::bind_cols(tibble::tibble(z = 1:2))
  wider |> class()                             |> expect_equal(class(out))
  wider |> attr("scrutiny", exact = TRUE)      |> expect_identical(attr(out, "scrutiny", exact = TRUE))
  wider |> audit()                             |> expect_equal(audit(out))
  out |> dplyr::bind_cols(data.frame(w = 1:2)) |> ncol() |> expect_equal(ncol(out) + 1L)
  out |> tidyr::nest(data = -study)            |> nrow() |> expect_equal(2L)
  out |> dplyr::bind_rows(tibble::tibble())    |> expect_equal(out)
  data |> dplyr::group_by(study) |> dplyr::group_modify(~ grim_map(.x, digits_x = 2, items_averaged = 1, data_type = "integer")) |> nrow() |> expect_equal(2L)
})

test_that("an output bound to itself can't be audited", {
  # Each row was counted twice:
  seq_out <- tibble::tibble(x = c(5.19, 3.45), n = c(28, 30)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  tot_out <- tibble::tibble(x1 = 3.84, x2 = 4.21, n = 41) |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  dplyr::bind_rows(seq_out, seq_out) |> audit_seq()     |> expect_error("occur more than once")
  dplyr::bind_rows(tot_out, tot_out) |> audit_total_n() |> expect_error("bound to itself")
})

test_that("total-n calls record a helper or `digits_*` the same whichever way it was given", {
  df <- tibble::tibble(x1 = c(3.84, 5.1), x2 = c(4.21, 3.9), n = c(41L, 30L))
  out <- df |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  dplyr::bind_rows(out, df |> dplyr::mutate(items_averaged = 1L) |> grim_map_total_n(digits_x = 2, data_type = "integer")) |> nrow() |> expect_equal(2L * nrow(out))
  dplyr::bind_rows(out, df |> grim_map_total_n(digits_x = c(2, 2), items_averaged = 1, data_type = "integer"))             |> nrow() |> expect_equal(2L * nrow(out))
})

test_that("the sequence and total-n audits reject rows added by a join", {
  # `full_join()` is no row binding, so the vctrs methods never see it:
  seq_out   <- tibble::tibble(x = c(5.19, 3.41), n = c(28, 30))           |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  seq_other <- tibble::tibble(x = c(5.19, 3.41, 6.22), n = c(28, 30, 31)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  tot_out   <- tibble::tibble(x1 = 5.19, x2 = 4.06, n = 65)               |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")
  suppressMessages(dplyr::full_join(seq_out, dplyr::filter(seq_other, case == 3))) |> audit_seq()     |> expect_error("rows that the mapper call didn't return")
  rbind(tot_out, tot_out[1, ])                                                     |> audit_total_n() |> expect_error("rows that the mapper call didn't return")
})


test_that("`rows_delete()`, `rows_update()`, and `rows_patch()` pass, as they add no rows", {
  # They cast `y` to the output to match its rows, which the guard took for
  # combining the two:
  out <- tibble::tibble(x = c(5.19, 5.18), n = c(28, 28)) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  key <- tibble::tibble(x = 5.19, n = 28L)

  out |> dplyr::rows_delete(key, by = c("x", "n")) |> nrow() |> expect_equal(1L)
  out |> dplyr::rows_update(dplyr::mutate(key, exp_incon = 0), by = c("x", "n")) |> dplyr::pull(exp_incon) |> expect_equal(c(0, 0.72))
  out |> dplyr::rows_delete(key, by = c("x", "n")) |> scrutiny_meta() |> expect_equal(scrutiny_meta(out))
  out |> dplyr::rows_patch(dplyr::mutate(key, exp_incon = 0), by = c("x", "n")) |> nrow() |> expect_equal(2L)

  # Those that add rows still don't:
  new <- tibble::tibble(x = 9.99, n = 28L)
  out |> dplyr::rows_insert(new, by = c("x", "n")) |> expect_error("Can't combine mapper output")
  out |> dplyr::rows_upsert(new, by = c("x", "n")) |> expect_error("Can't combine mapper output")
  out |> dplyr::rows_append(new)                   |> expect_error("Can't combine mapper output")
})
