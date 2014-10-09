
context("rpin")

test_that("Guards for incorrect input format", {
  expect_that(rpin(list(3)), throws_error())
  expect_that(rpin(Sys.Date()), throws_error())
  expect_that(rpin("Halleluja!"), throws_error())
})


test_that("Correct output format", {
  expect_that(x <- rpin(10), is_a("pin"))
  expect_that(rpin(x), is_a("pin"))
  expect_that(attr(rpin(x), "non_personal"), is_true())
})


test_that("pin_birthplace non-informative", {
  expect_that(sort(as.character(unique(pin_birthplace(x <- rpin(1000))))), 
              is_equivalent_to(t <- c("Born after 31 december 1989",
                                      "Extra number and immigrants (immigrated after 1946)"
                                      )))
  expect_that(sort(as.character(unique(pin_birthplace(rpin(x))))), 
              is_equivalent_to(t))
})


  