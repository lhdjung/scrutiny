

x <- rnorm(50, 20, 10)


test_that(glue::glue("With 1 decimal place, `reround()` internally calls \\
          the correct rounding functions"), {
            expect_true(all(dplyr::near(reround(x, 1, "up"), round_up(x, 1))))
            expect_true(all(dplyr::near(reround(x, 1, "down"), round_down(x, 1))))
            expect_true(all(dplyr::near(reround(x, 1, "even"), round(x, 1))))
            expect_true(all(dplyr::near(reround(x, 1, "ceiling"), round_ceiling(x, 1))))
            expect_true(all(dplyr::near(reround(x, 1, "floor"), round_floor(x, 1))))
            expect_true(all(dplyr::near(reround(x, 1, "trunc"), round_trunc(x, 1))))
            expect_true(all(dplyr::near(reround(x, 1, "anti_trunc"), round_anti_trunc(x, 1))))
          })

test_that(glue::glue("With 2 decimal places, `reround()` internally calls \\
                     the correct rounding functions"), {
            expect_true(all(dplyr::near(reround(x, 2, "up"), round_up(x, 2))))
            expect_true(all(dplyr::near(reround(x, 2, "down"), round_down(x, 2))))
            expect_true(all(dplyr::near(reround(x, 2, "even"), round(x, 2))))
            expect_true(all(dplyr::near(reround(x, 2, "ceiling"), round_ceiling(x, 2))))
            expect_true(all(dplyr::near(reround(x, 2, "floor"), round_floor(x, 2))))
            expect_true(all(dplyr::near(reround(x, 2, "trunc"), round_trunc(x, 2))))
            expect_true(all(dplyr::near(reround(x, 2, "anti_trunc"), round_anti_trunc(x, 2))))
          })

test_that(glue::glue("With 3 decimal places, `reround()` internally calls \\
                     the correct rounding functions"), {
            expect_true(all(dplyr::near(reround(x, 3, "up"), round_up(x, 3))))
            expect_true(all(dplyr::near(reround(x, 3, "down"), round_down(x, 3))))
            expect_true(all(dplyr::near(reround(x, 3, "even"), round(x, 3))))
            expect_true(all(dplyr::near(reround(x, 3, "ceiling"), round_ceiling(x, 3))))
            expect_true(all(dplyr::near(reround(x, 3, "floor"), round_floor(x, 3))))
            expect_true(all(dplyr::near(reround(x, 3, "trunc"), round_trunc(x, 3))))
            expect_true(all(dplyr::near(reround(x, 3, "anti_trunc"), round_anti_trunc(x, 3))))
          })

test_that(glue::glue("With 4 decimal places, `reround()` internally calls \\
                     the correct rounding functions"), {
            expect_true(all(dplyr::near(reround(x, 4, "up"), round_up(x, 4))))
            expect_true(all(dplyr::near(reround(x, 4, "down"), round_down(x, 4))))
            expect_true(all(dplyr::near(reround(x, 4, "even"), round(x, 4))))
            expect_true(all(dplyr::near(reround(x, 4, "ceiling"), round_ceiling(x, 4))))
            expect_true(all(dplyr::near(reround(x, 4, "floor"), round_floor(x, 4))))
            expect_true(all(dplyr::near(reround(x, 4, "trunc"), round_trunc(x, 4))))
            expect_true(all(dplyr::near(reround(x, 4, "anti_trunc"), round_anti_trunc(x, 4))))
          })

test_that(glue::glue("With 5 decimal places, `reround()` internally calls \\
                     the correct rounding functions"), {
            expect_true(all(dplyr::near(reround(x, 5, "up"), round_up(x, 5))))
            expect_true(all(dplyr::near(reround(x, 5, "down"), round_down(x, 5))))
            expect_true(all(dplyr::near(reround(x, 5, "even"), round(x, 5))))
            expect_true(all(dplyr::near(reround(x, 5, "ceiling"), round_ceiling(x, 5))))
            expect_true(all(dplyr::near(reround(x, 5, "floor"), round_floor(x, 5))))
            expect_true(all(dplyr::near(reround(x, 5, "trunc"), round_trunc(x, 5))))
            expect_true(all(dplyr::near(reround(x, 5, "anti_trunc"), round_anti_trunc(x, 5))))
          })

