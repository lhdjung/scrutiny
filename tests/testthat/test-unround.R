
df1 <- unround(c(3.6, "5.20", 5.174)) %>%
  suppressMessages()


test_that("The output is a tibble", {

  df1 %>% expect_s3_class("tbl_df")
})

test_that("It has correct dimensions", {
  df1 %>% dim() %>% expect_equal(c(3, 7))
})


colnames_expected <- c(
  "range", "rounding", "lower", "incl_lower", "x", "incl_upper", "upper"
)

test_that("It has correct column names", {
  df1 %>% colnames() %>% expect_setequal(colnames_expected)
})


test_that("Its columns have the correct types", {
  df1[[1]] %>% expect_type("character")
  df1[[2]] %>% expect_type("character")
  df1[[3]] %>% expect_type("double")
  df1[[4]] %>% expect_type("logical")
  df1[[5]] %>% expect_type("character")
  df1[[6]] %>% expect_type("logical")
  df1[[7]] %>% expect_type("double")
})


test_that("A non-string `x` specification throws an error
          (if `digits` is `NULL`; the default)", {
  unround(4.5) %>% expect_error()
})

test_that("The function throws an error if `rounding` is misspecified", {
  unround("4.50", rounding = "doesn't exist") %>% expect_error()
})



df2 <- unround("4.50", digits = 1)
df3 <- unround("4.50", digits = 1:5)


test_that("", {
  df2$lower %>% expect_equal(4.45)
  df2$upper %>% expect_equal(4.55)
})


test_that("", {
  df3$lower %>% expect_equal(c(4.450000, 4.495000, 4.499500, 4.499950, 4.499995))
  df3$upper %>% expect_equal(c(4.550000, 4.505000, 4.500500, 4.500050, 4.500005))
})



rbs_up_or_down <- rounding_bounds_scalar("up_or_down", 8.2, 0.05, 0.5)
rbs_up         <- rounding_bounds_scalar("up"        , 8.2, 0.05, 0.5)
rbs_down       <- rounding_bounds_scalar("down"      , 8.2, 0.05, 0.5)
rbs_even       <- rounding_bounds_scalar("even"      , 8.2, 0.05, 0.5)
rbs_ceiling    <- rounding_bounds_scalar("ceiling"   , 8.2, 0.05, 0.5)
rbs_floor      <- rounding_bounds_scalar("floor"     , 8.2, 0.05, 0.5)
rbs_trunc      <- rounding_bounds_scalar("trunc"     , 8.2, 0.05, 0.5)
rbs_anti_trunc <- rounding_bounds_scalar("anti_trunc", 8.2, 0.05, 0.05)
rbs_error      <- rounding_bounds_scalar("doesn't exist", 6, 6, 6)

rbs_types_exp <- c("double", "double", "character", "character")


test_that("The list returned by `rounding_bounds_scalar()`
          has correct types", {
  rbs_up_or_down %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_up         %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_down       %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_even       %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_ceiling    %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_floor      %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_trunc      %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
  rbs_anti_trunc %>% purrr::map_chr(typeof) %>% expect_equal(rbs_types_exp)
})


test_that("The list has correct values", {
  rbs_up_or_down %>% expect_equal(list(8.15, 8.25, "<=", "<="))
  rbs_up         %>% expect_equal(list(8.15, 8.25, "<=", "<" ))
  rbs_down       %>% expect_equal(list(8.15, 8.25, "<" , "<="))
  rbs_even       %>% expect_equal(list(7.7 , 8.7 , "<" , "<" ))
  rbs_ceiling    %>% expect_equal(list(7.2 , 8.2 , "<" , "<="))
  rbs_floor      %>% expect_equal(list(8.2 , 9.2 , "<=", "<" ))
  rbs_trunc      %>% expect_equal(list(8.2 , 9.2 , "<=", "<" ))
  rbs_anti_trunc %>% expect_equal(list(8.1 , 8.2 , "<=",  "<"))
})


test_that("", {
  rounding_bounds %>% expect_type("closure")
})

test_that("Wrong `rounding` specifications return the string (list)
          that will trigger an error within `unround()", {
  rbs_error %>% expect_setequal("error_trigger")
})


