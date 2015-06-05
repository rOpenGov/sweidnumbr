
context("pin_interimn")

pin_test <- c("190102239803", "19640228P367", "18990716T818")
pin_test_res <- c(FALSE, TRUE, TRUE)

test_that(desc="interim number",{
  skip_on_cran()
  skip_on_travis()
  expect_is(pin_interimn(pin = pin_test), "logical")
  expect_equal(pin_interimn(pin = pin_test), expected = pin_test_res)
})

test_that(desc="Handle NA",{
  skip_on_cran()
  skip_on_travis()
  expect_true(is.na(pin_interimn(as.pin(c(NA, pin_test)))[1]))
  expect_false(is.na(pin_interimn(as.pin(c(NA, pin_test)))[2]))
})
