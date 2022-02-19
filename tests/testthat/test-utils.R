

test_that("`wrong_spec_string()` returns a string", {
  wrong_spec_string(4) %>% expect_type("character")
})


sd_rec_scalar <- reconstruct_sd_scalar("mean_n", "0.3", 30, 12, 15)

test_that("`reconstruct_sd_scalar()` returns correct values", {
  sd_rec_scalar %>% expect_type("double")
  sd_rec_scalar %>% expect_equal(0.4660916)
})


sd_rec <- reconstruct_sd("mean_n", "0.3", 30, 12, 15)

test_that("`reconstruct_sd()` returns correct values", {
  sd_rec %>% expect_type("double")
  sd_rec %>% expect_equal(0.4660916)
})


test_that("`integer_places()` returns correct values", {
  integer_places(1.2)     %>% expect_equal(1)
  integer_places(11.2)    %>% expect_equal(2)
  integer_places(111.2)   %>% expect_equal(3)
  integer_places(1111.2)  %>% expect_equal(4)
  integer_places(11111.2) %>% expect_equal(5)

  integer_places(1.22222) %>% expect_equal(1)
  integer_places(11.2222) %>% expect_equal(2)
  integer_places(111.222) %>% expect_equal(3)
  integer_places(1111.22) %>% expect_equal(4)
  integer_places(11111.2) %>% expect_equal(5)
})


test_that("`straighten_out()` returns correct values", {
  straighten_out(1, 2, 3)           %>% expect_equal(list(1, 2, 3))
  straighten_out("a", "b", "c")     %>% expect_equal(list("a", "b", "c"))
  straighten_out(TRUE, FALSE, TRUE) %>% expect_equal(list(TRUE, FALSE, TRUE))
})



test_that("`an_a()` returns correct values", {
  an_a("start") %>% expect_equal("a")
  an_a("end")   %>% expect_equal("an")
})



test_that("`an_a_type()` returns correct values", {
  an_a_type("bla") %>% expect_equal("a character")
  an_a_type(4)     %>% expect_equal("a double (numeric value)")
})


test_that("`is_whole_number()` returns correct values", {
  is_whole_number(1)   %>% expect_true()
  is_whole_number(985) %>% expect_true()
  is_whole_number(37)  %>% expect_true()

  is_whole_number(0.2)    %>% expect_false()
  is_whole_number(25.05)  %>% expect_false()
  is_whole_number(75.489) %>% expect_false()
})



test_that(glue::glue("The super-short functions used within \\
`decimal_places()` return correct values"), {
  is_length_1_and_not_na("is this?") %>% expect_true()
  is_length_1_and_not_na(123) %>% expect_true()
  is_length_1_and_not_na(NA) %>% expect_false()

  is_length_greater_1(letters) %>% expect_true()
  is_length_greater_1(1:5)     %>% expect_true()
  is_length_greater_1(c("Huey", "Dewey", "Louie")) %>% expect_true()
  is_length_greater_1(1) %>% expect_false()
  is_length_greater_1("not really") %>% expect_false()

  set_to_0(letters) %>% expect_equal(0)
  set_to_0(1:5) %>% expect_equal(0)
  set_to_0(c("Huey", "Dewey", "Louie")) %>% expect_equal(0)
})



parcel_letters_expected <- c(
  "a", "c", "e", "g", "i", "k", "m", "o", "q", "s", "u", "w", "y"
)

test_that("`parcel_nth_elements()` returns correct values", {
  parcel_nth_elements(letters, 2) %>% expect_equal(parcel_letters_expected)
  parcel_nth_elements(1:10, 2) %>% expect_equal(c(1, 3, 5, 7, 9))
  parcel_nth_elements(1:10, 5) %>% expect_equal(c(1, 6))
})



df_test <- tibble::tibble(
  a = c(1, 2, 3, 1, 2, 3),
  b = c(1, 2, 3, 1, 2, 3)
) %>%
  remove_equivalent_rows()

df_expected_1 <- tibble::tibble(
  a = 1:3,
  b = 1:3
)

test_that("`remove_equivalent_rows()` returns correct values", {
  df_test %>% expect_equal(df_expected_1)
})



df_test_2 <- tibble::tibble(
  a = 1,
  b = 2,
  c = 3
) %>%
  reverse_column_order()

df_expected_2 <- tibble::tibble(
  c = 3,
  b = 2,
  a = 1
)

test_that("`reverse_column_order()` returns correct values", {
  df_test_2 %>% expect_equal(df_expected_2)
})



vec_test_1 <- 1:50 %>%
  as.double() %>%
  censor(25, 40)

vec_expected_1 <- rep(25, 25) %>%
  append(26:39) %>%
  append(rep(40, 11))

test_that("`censor()` returns correct values", {
  vec_test_1 %>% expect_equal(vec_expected_1)
})



vec_test_2 <- 1:7
vec_test_2 <- add_class(vec_test_2, "silly test")

test_that("", {
  vec_test_2 %>% expect_s3_class("silly test")
})



vec_test_3 <- remove_na(c(1, NA, 3, 4, NA))
vec_expected_3 <- c(1, 3, 4)

test_that("", {
  vec_test_3 %>% expect_equal(vec_expected_3)
})



test_that("`proto_rounding_singular()` throws an error iff it should", {
  proto_rounding_singular(
    c("up_or_down", "floor"),
    "up_or_down",
    "up",
    "down"
  ) %>% expect_error()

  proto_rounding_singular(
    c("not a rounding procedure", "floor"),
    "up_or_down",
    "up",
    "down"
  ) %>% expect_silent()

})



test_that("", {
  check_rounding_singular(c("up_or_down", "up")) %>% expect_error()
  check_rounding_singular(c("ceiling_or_floor", "trunc")) %>% expect_error()
  check_rounding_singular(c("ceiling", "up")) %>% expect_silent()
  check_rounding_singular("up") %>% expect_silent()
  check_rounding_singular("bla") %>% expect_silent()
})




test_that("`proto_lengths_congruent()` throws a warning when it should", {
  proto_lengths_congruent(1, 2, 3, 4, 5) %>% expect_warning()
  proto_lengths_congruent("a", "b", "c", "d", "e") %>% expect_warning()
})


test_that("`proto_lengths_congruent()` throws an error when it should", {
  proto_lengths_congruent(1, c(1, 2), 3, 4, 5) %>% expect_error()
  proto_lengths_congruent("a", c("a", "b"), "c", "d", "e") %>% expect_error()
})



numbers <- 1:5
nephews <- c("Huey", "Dewey", "Louie")

test_that("`check_lengths_congruent()` throws an error when it should", {
  check_lengths_congruent(list(numbers, nephews)) %>% expect_error()
})




test_that("`check_length()` throws an error iff it should", {
  check_length(numbers, "numbers", 1) %>% expect_error()
  check_length(nephews, "nephews", 1) %>% expect_error()

  check_length(numbers, "numbers", 5) %>% expect_silent()
  check_length(nephews, "nephews", 3) %>% expect_silent()
})




