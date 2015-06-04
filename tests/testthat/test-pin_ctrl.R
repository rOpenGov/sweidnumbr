
context("pin_ctrl")

pin_test <- c("196408233234", "196408233235")
pin_test_res <- c(TRUE, FALSE)

test_that(desc="control number",{
  expect_is(pin_ctrl(pin = pin_test), "logical")
  expect_equal(pin_ctrl(pin = pin_test), expected = pin_test_res)
})

test_that(desc="Handle NA",{
  skip_on_cran()
  skip_on_travis()
  expect_true(is.na(pin_ctrl(as.pin(c(NA,"198501169885")))[1]))
  expect_false(is.na(pin_ctrl(as.pin(c(NA,"198501169885")))[2]))
})
