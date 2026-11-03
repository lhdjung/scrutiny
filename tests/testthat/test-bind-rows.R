df <- tibble::tibble(x = c(0.3, 5.2), n = c(4, 28))
out_up <- df |> grim_map(digits_x = 1, items_averaged = 1, rounding = "up")
out_down <- df |> grim_map(digits_x = 1, items_averaged = 1, rounding = "down")

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
  seq_1 <- tibble::tibble(x = 5.19, n = 28) |> grim_map_seq(digits_x = 2, items_averaged = 1)
  seq_2 <- tibble::tibble(x = c(5.17, 4.2), n = c(28, 30)) |> grim_map_seq(digits_x = 2, items_averaged = 1)
  dplyr::bind_rows(seq_1, seq_2) |> expect_error("different value sets or verdicts")
  dplyr::bind_rows(seq_1, seq_1[1:3, ]) |> expect_no_error()
  seq_1 |> dplyr::filter(var == "n") |> dplyr::bind_rows(seq_1 |> dplyr::filter(var == "x")) |> audit_seq() |> expect_equal(audit_seq(seq_1))
  tot_1 <- tibble::tibble(x1 = 5.19, x2 = 4.2, n = 58) |> grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:1)
  tot_2 <- tibble::tibble(x1 = 5.19, x2 = 4.2, n = 58) |> grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:2)
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
  out_grim <- df |> grim_map(digits_x = 2, items_averaged = 1)
  out_grimmer <- tibble::tibble(x = 5.21, sd = 1.6, n = 28) |> grimmer_map(digits_x = 2, digits_sd = 1, items_averaged = 1)
  out_seq <- df |> grim_map_seq(digits_x = 2, items_averaged = 1)
  dplyr::bind_rows(out_grim, out_grimmer) |> expect_error("different mappers")
  dplyr::bind_rows(out_grimmer, out_grim) |> expect_error("different mappers")
  dplyr::bind_rows(out_grim, out_seq)     |> expect_error("different mappers")
  dplyr::bind_rows(out_seq, out_grim)     |> expect_error("different mappers")
  # A class of the user's own comes first:
  seq_custom <- function_map_seq(.fn = grim_map, .reported = c("x", "n"), .name_test = "GRIM", .name_class = "my_class")
  dplyr::bind_rows(seq_custom(df, digits_x = 2, items_averaged = 1), seq_custom(df, digits_x = 2, items_averaged = 1, rounding = "up")) |> expect_error("`rounding`")
})

test_that("output of sequence and total-n calls on different data is not combined", {
  seq1 <- tibble::tibble(x = 5.19, n = 28) |> grim_map_seq(digits_x = 2, items_averaged = 1)
  seq2 <- tibble::tibble(x = 6.24, n = 30) |> grim_map_seq(digits_x = 2, items_averaged = 1)
  seq1 |> dplyr::bind_rows(seq2) |> expect_error("different value sets")
  seq1 |> dplyr::bind_rows(seq1[0, ]) |> nrow() |> expect_equal(nrow(seq1))

  total1 <- tibble::tibble(x1 = 3.43, x2 = 5.28, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = 1)
  total2 <- tibble::tibble(x1 = 2.97, x2 = 4.42, n = 90) |> grim_map_total_n(digits_x = 2, items_averaged = 1)
  total1 |> dplyr::bind_rows(total2) |> expect_error("different value sets")

  data <- tibble::tibble(a = 1, b = 2, c = 3, total = 3)
  sum3 <- data |> add_up_to_map(summands = c(a, b, c), total = total)
  sum2 <- data |> add_up_to_map(summands = c(a, b), total = total)
  sum3 |> dplyr::bind_rows(sum2) |> expect_error("Can't combine")
})

test_that("calls that differ in a helper or a decimal count can't be combined", {
  df <- tibble::tibble(x = 5.19, n = 28)
  seq_1 <- df |> grim_map_seq(digits_x = 2, items_averaged = 1)
  dplyr::bind_rows(seq_1, df |> grim_map_seq(digits_x = 2, items_averaged = 2)) |> expect_error("different calls")
  dplyr::bind_rows(seq_1, df |> grim_map_seq(digits_x = 3, items_averaged = 1)) |> expect_error("different calls")
  df_tot <- tibble::tibble(x1 = 3.1, x2 = 2.9, n = 41)
  tot_1 <- df_tot |> grim_map_total_n(digits_x = 2, items_averaged = 1)
  dplyr::bind_rows(tot_1, df_tot |> grim_map_total_n(digits_x = 2, items_averaged = 2)) |> expect_error("different calls")
  dplyr::bind_rows(tot_1, df_tot |> dplyr::mutate(items_averaged = 2) |> grim_map_total_n(digits_x = 2)) |> expect_error("different calls")
  dplyr::bind_rows(tot_1[1:3, ], tot_1[-(1:3), ]) |> expect_equal(tot_1)
})
