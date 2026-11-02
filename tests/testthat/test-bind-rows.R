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
