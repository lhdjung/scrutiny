test_that("`scrutiny_meta()` returns the recorded settings, or `NULL`", {
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, rounding = "up")
  out |> scrutiny_meta() |> _$args$rounding |> expect_equal("up")
  out |> scrutiny_meta() |> _$args$percent  |> expect_false()
  out |> dplyr::filter(!consistency) |> scrutiny_meta() |> expect_equal(scrutiny_meta(out))
  out |> dplyr::summarise(m = mean(x)) |> scrutiny_meta() |> expect_null()
  pigs1 |> scrutiny_meta() |> expect_null()

  out_seq <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1)
  out_seq |> scrutiny_meta() |> names() |>
    expect_setequal(c("args", "results_reported", "name_key_result", "var", "dispersion_linear"))
  out_seq |> scrutiny_meta() |> _$results_reported |> expect_equal(out$consistency)
})
